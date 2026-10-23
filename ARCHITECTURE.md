# mates Architecture

mates is a command line vCard and iCalendar manipulation tool for managing
contacts, calendar entries, and TODO items through DAV protocols. The intended
model is online-first with opportunistic local caching: the remote DAV
collections are the durable source of truth when online, and remote reads are
cached so recently seen data remains available when offline. The service stores DAV objects and normalized projections in
PostgreSQL using the schema in `db/schema.sql`.

## Product Boundary

The primary product boundary is the CardDAV and CalDAV service. Existing DAV
clients, especially phone and desktop contact/calendar applications, are the
first-class clients. If a native client and `mates-web` want the same data, the
native client path wins and `mates-web` must adapt to the exposed DAV behavior.

`mates-web` is intentionally secondary. It exists as a convenient inspection and
testing client, not as a privileged application tier. It must use only the
public CardDAV and CalDAV HTTP interfaces under `/dav/`; it must not call
private JSON data routes, bypass DAV into SQL, depend on process-local state, or
receive server behavior that cannot also be expressed as ordinary DAV semantics.
When the web UI needs less data, it should use standard DAV mechanisms such as
property selection, `addressbook-query`, `calendar-query`, `*-multiget`, ETags,
and sync tokens rather than a mates-specific side channel.

## Current Components

- `Mates.App` owns command dispatch and terminal output.
- `Mates.Config` stores account and authentication configuration in the XDG
  config directory.
- `Mates.VCard` parses basic vCard blocks and extracts common display fields.
- `Mates.Cache` stores one cached `.vcf` file per full contact and one cached
  raw response per CardDAV request in the XDG cache directory.
- `Mates.CardDAV` owns remote CardDAV requests, challenge-driven HTTP auth through
  the managed libcurl transport, address book discovery, remote listing, and remote show.
- `Mates.Server` starts the Servant/Warp HTTP service.
- `Mates.Server.API` exposes health/static mounts and mounts the DAV
  application. It no longer exposes JSON data routes.
- `Mates.Server.DAV` handles WebDAV/CardDAV/CalDAV HTTP method entry points,
  request transactions, representation validators, and conditional writes.
- `Mates.Server.Security` authenticates proxy identities and scopes collection
  access; `Mates.Server.Database` stores local principals, grants, and sync state.
- `Mates.Server.Protocol` and `Mates.Server.Xml` parse DAV XML by namespace and
  render discovery, property selection, queries, multiget, and sync responses.
- `Mates.Transport` wraps libcurl without invoking a subprocess.
- `Mates.CalendarNative` wraps libical parsing, timezone conversion, and bounded
  recurrence iteration. Native resources are released at the Haskell boundary.
- `Mates.ICalendar` parses iCalendar components for projection and CalDAV
  `time-range` matching, including recurrence expansion and VTODO overlap
  semantics.
- `Mates.Server.Static` serves the compiled Elm frontend from `/ui/`.
- `Mates.Server.ClientProfile` recognizes client families from request headers.
- `Mates.Server.Render` renders vCard and iCalendar payloads through the client
  profile layer so normalized data can be shaped for Apple or generic DAV
  clients.
- `Mates.Server.Store` is the storage boundary. It opens PostgreSQL
  connections per operation, applies `db/schema.sql` as the development schema
  source of truth, appends immutable raw DAV revisions, and writes normalized
  projections for contacts, email types, relationships, media, calendars,
  event timing, TODO timing, and recurrence metadata.
- `frontend/` contains the Elm inspection UI. It is a DAV client, not an
  internal frontend tier: it reads and writes through CardDAV and CalDAV
  directly, parsing DAV XML, vCard, and iCalendar in the browser. Contact
  editing is also a CardDAV operation: the browser fetches full address data,
  edits a constrained set of visible fields, preserves unedited vCard
  properties, and writes the result back with `PUT`.
- `db/schema.sql` defines the PostgreSQL schema for normalized contact and
  calendar storage plus shared DAV sync metadata. During this pre-migration
  development phase, runtime bootstrapping and test resets both use this file.

## Data Locations

- Config: `$XDG_CONFIG_HOME/mates/config`, or the platform default returned by
  Haskell's `getXdgDirectory XdgConfig`.
- Contact cache: `$XDG_CACHE_HOME/mates/cards`, or the platform default
  returned by `getXdgDirectory XdgCache`.
- Remote response cache: `$XDG_CACHE_HOME/mates/responses`.
- Service database: PostgreSQL. `MATES_DATABASE_URL` is a libpq connection
  string; when it is unset the service uses `dbname=mates`. `MATES_SCHEMA_PATH`
  can point at an alternate schema file; otherwise `db/schema.sql` is read from
  the current working directory.

The cache formats are deliberately simple in the first version. Each full
contact is stored as a raw `.vcf` file named from its `UID`, or from a
deterministic local hash when the card has no `UID`. Each remote response is
stored as a raw body file named from a deterministic hash of the request
identity, including account, URL, method, Depth, and request body.

## PostgreSQL Store

The PostgreSQL design treats a contact as the stable root entity and stores
repeatable vCard facts in child tables. We are deliberately not splitting
`person` and `contact` into separate first-level domain records at this stage:
the contact row is the person row. Genealogy-oriented facts such as birth date,
death date, places, notes, and relationships attach to that same root entity as
structured rows or extension key/value properties.

- `contacts` holds the stable contact id, vCard `UID`, vCard `KIND`, display
  name, sort name, and soft-delete timestamp.
- `contact_names`, `contact_nicknames`, and `contact_organizations` model
  person and organization identity details.
- `contact_categories` stores repeatable vCard `CATEGORIES` values.
- `contact_emails`, `contact_phones`, `contact_addresses`, `contact_urls`, and
  `contact_impps` hold repeatable communication endpoints.
- `contact_*_types` tables store vCard type labels such as `HOME`, `WORK`, and
  `INTERNET` as queryable rows instead of comma-separated strings.
- `contact_dates`, `contact_notes`, and `contact_media` cover birthdays, death
  dates, anniversaries, notes, photos, and logos. Dates that parse cleanly are
  stored as SQL dates; partial or textual dates remain textual so the original
  fact is not lost.
- `contact_relationships` stores normalized relationship labels, asserted
  related names, optional resolved contact UUIDs, resolution state, and whether
  the row was asserted by a vCard or inferred as the inverse of another row.
  Apple `X-ABRELATEDNAMES` plus grouped `X-ABLabel` pairs are projected here
  as first-class relationship rows. The asserted label is preserved when it is
  more specific than the broad family category, so `mother`, `father`,
  `daughter`, `son`, `husband`, and `wife` remain visible as such. Broader
  categories such as `parent`, `child`, `sibling`, and `spouse` are derived
  only for grouping and inferred inverse relationships.
- `contact_relationship_suppressions` records inferred inverse relationships
  that a user removed from one contact. This prevents the resolver from
  recreating the same inverse row while preserving the original asserted
  relationship and raw DAV revision on the other contact.
- `contact_properties` and `contact_property_parameters` are the extension
  layer for less-structured and vendor-specific properties after first-class
  projections have been populated. Genealogy properties that do not yet need a
  dedicated table, such as birth and death places, live here as ordinary
  contact/person facts.

Calendar and TODO data is stored in its own normalized tables:

- `calendars` stores local calendar identity, display color, and timezone.
- `calendar_components` is the shared root table for `VEVENT` entries and
  `VTODO` items.
- `calendar_events` stores event-specific timing, all-day state, duration, and
  transparency.
- `calendar_todos` stores task-specific start/due/completed/created state,
  duration, and percent complete.
- `calendar_participants`, `calendar_recurrence_rules`,
  `calendar_recurrence_dates`, and `calendar_alarms` model common iCalendar
  child components that are important for queries and reminders.
- `calendar_properties` and `calendar_property_parameters` are the extension
  layer for less-common and vendor-specific iCalendar properties.

DAV and round-trip state is intentionally separate from normalized facts. The
raw DAV revision stream is authoritative; normalized contact and calendar rows
are projections of the latest non-deleted revision:

- `dav_accounts` stores server URL, username, and optional authorization
  identity, but no authentication secret.
- `dav_collections` stores discovered address book and calendar collection URLs
  plus collection metadata such as `ctag` and sync token.
- `dav_objects` maps remote hrefs to the current revision and tombstone state.
- `dav_object_revisions` is append-only history. Each `PUT`, import, repair,
  or `DELETE` records an operation, ETag, parser status, content metadata,
  SHA-256, and the exact raw vCard/iCalendar bytes when the operation has a
  body.
- `dav_object_projections` records which revision produced the current
  normalized row, or why projection was skipped/failed.
- `contact_dav_objects` and `calendar_component_dav_objects` map parsed domain
  rows back to their remote DAV objects.
- `remote_response_cache` is the database-backed replacement for the current
  raw response cache.

Deliberate storage choices for this version:

- One served address book; contact UIDs remain unique in that shared book.
- Embedded contact media stays in PostgreSQL, with decoded photo bytes in
  `contact_media` and the exact original encoding retained in raw revisions.
- Type labels remain free text. Common fields use the existing normalized
  tables; original properties, groups, and parameters also remain queryable.
- Recurrence rules remain raw source values. libical performs bounded expansion;
  exceeding the work limit produces a DAV error rather than incomplete matches.
- Operator history/revert tooling and a general migration framework are outside
  this version. Schema upgrades and projection backfills are idempotent and
  preserve raw history. A schema fingerprint avoids rerunning unchanged DDL
  during ordinary startup and operator commands.

## Command Flow

1. `mates init` writes the CardDAV server, user, optional authorization
   identity, and optional address book path.
2. `mates import` parses one or more local vCard files and writes them to the
   local cache.
3. `mates list`, `mates email`, and `mates export` read only from the local
   cache and therefore work offline. List-style queries match partial names and
   partial email addresses. `mates show` renders a human-readable contact view
   from the cache first, then falls back to remote lookup when CardDAV is
   configured. `mates show --remote` forces remote lookup. Raw vCard output
   remains available through `mates export`.
4. `mates list --remote` and `mates remote list` query the configured CardDAV
   URL and print returned vCard summaries. If the URL is a home/principal
   collection rather than an address book, the CardDAV layer discovers child
   address book collections and queries those.
5. `mates email --remote` and `mates remote email` use the same remote matching
   path as remote listing and print tab-separated email, full name, and email
   type labels.
6. `mates show --remote` resolves the requested partial name, partial email,
   UID, or href from a remote address book query, then fetches the full card
   with `addressbook-multiget`. Full vCards returned by this path are written
   into the contact cache.

## Service Flow

`mates serve [--port PORT]` starts a Servant/Warp service. The HTTP service
exposes:

- `GET /health`.
- `GET /ui/`.
- DAV methods under `/dav/`.

The `/ui/` mount serves static frontend assets from `frontend/`. The source app
is written in Elm and compiled to `frontend/main.js` by `npm run build:ui`; the
generated JavaScript is intentionally ignored by git. Elm is installed as a
local npm development dependency, and `ELM_HOME` is redirected to an ignored
`.elm-home/` directory for sandbox-friendly local builds.

The frontend is deliberately constrained to the same DAV contract exposed to
external clients. It uses `addressbook-query` to fetch contacts, CardDAV `PUT`
to create contacts, and `calendar-query` to fetch calendar entries. Elm parses
the DAV multi-status XML and then parses embedded vCard and iCalendar data into
UI models. For contacts, the list request asks for summary vCard properties
through `C:address-data` property selection, and the selected contact is fetched
with `addressbook-multiget` for full detail. It must not grow a private data
API, even if that would be simpler for the web page. Contact detail panes render
ordered parsed vCard fields: email, phone, address, URL, organization, title,
role, nickname, birthday, death date, anniversary, birth place, death place,
related names, and notes. It also renders a compact family tree under the
selected person from the relationship facts exposed by CardDAV, and exposes an
edit pane for common contact/person fields. Relationship edits are modeled as
individual rows with explicit remove buttons, then serialized back into
`RELATED` vCard properties. When a PHOTO or LOGO is displayable, the frontend
renders it above the contact name. Calendar
views are UI projections over CalDAV `VEVENT` data: agenda shows the next
upcoming entries from the browser's current date, day and week place timed
entries on a 30-minute grid, and month shows a conventional six-week calendar
grid. Day, week, and month views share previous/next navigation over a selected
date.

DAV traffic is mounted at `/dav` through Servant's `Raw` escape hatch because
CardDAV and CalDAV use WebDAV methods such as `PROPFIND` and `REPORT`, XML
request bodies, and multi-status responses. The CardDAV contact path currently:

- answers `OPTIONS` with DAV capabilities;
- answers `PROPFIND /dav/` and `/dav/contacts/` with collection properties;
- answers depth-1 `PROPFIND /dav/contacts/` with address object hrefs, ETags,
  content type, and content length;
- answers `addressbook-query`, `addressbook-multiget`, and `sync-collection`
  reports with vCard address data; `addressbook-query` supports basic
  `text-match` filtering against normalized contact names, UIDs, and email
  addresses, and `C:address-data` property selection for smaller vCard bodies;
- routes `GET` and `HEAD` for `/dav/contacts/*.vcf` to stored raw vCard bodies;
- accepts `PUT` for one-vCard address objects and updates normalized contact
  projections from the parsed vCard after appending a raw revision;
- accepts `DELETE` for address objects by appending a tombstone revision and
  removing the current projection.

The CalDAV calendar path currently:

- answers `PROPFIND /dav/calendars/` and arbitrary
  `/dav/calendars/COLLECTION/` collection paths;
- answers depth-1 collection `PROPFIND` with calendar object hrefs, ETags,
  content type, and content length;
- answers `calendar-query`, `calendar-multiget`, and `sync-collection` reports
  with iCalendar data; `calendar-query` supports `CAL:time-range` filtering for
  `VEVENT` and `VTODO`. Event filtering accounts for timed and all-day spans,
  `DTEND`, `DURATION`, `RRULE`, `RDATE`, `EXDATE`, and detached recurrence
  overrides. TODO filtering follows the CalDAV `VTODO` cases for `DTSTART`,
  `DUE`, `DURATION`, `COMPLETED`, and `CREATED`;
- routes `GET` and `HEAD` for `/dav/calendars/COLLECTION/*.ics` to stored raw
  iCalendar bodies;
- accepts `PUT` for raw `VCALENDAR` objects and updates normalized calendar
  projections from parsable `VEVENT` and `VTODO` data after appending a raw
  revision;
- accepts `DELETE` for calendar objects by appending a tombstone revision and
  removing the current projection.

The implementation is sufficient for basic vdirsyncer CardDAV and CalDAV sync
into PostgreSQL. It enforces simple `If-Match` and
`If-None-Match` preconditions and emits deterministic ETags derived from raw
object bodies. The latest visible object is read through `dav_objects`'
`current_revision_id`; older revisions remain available in PostgreSQL for
audit and future revert tooling. Collection change sequences and a durable
change journal drive sync tokens, including deletions and changes to inferred
relationships. Conditional writes and journal updates share a transaction;
responses are sent after commit. Representation ETags use SHA-256.

Client-specific behavior belongs behind `Mates.Server.ClientProfile` and
`Mates.Server.Render`. The storage layer keeps normalized facts; renderers are
responsible for client-specific wire formats. For example, relationships are
stored as first-class rows using the asserted relationship label, preserving
genealogy-specific information such as `mother` and `father`. Generic clients
receive `RELATED;VALUE=text` properties with that label, while recognized Apple
Contacts clients can receive the same normalized rows rendered as Apple
`X-ABRELATEDNAMES`/`X-ABLabel` pairs. Imported Apple extensions are input
compatibility, not the server's canonical storage model.

Relationship resolution is deliberately conservative. A related name is linked
to a contact UUID only when the current database contains exactly one
non-deleted contact with that display name. A resolved asserted row generates
one inferred inverse row, using generic inverse labels where gender or exact
role cannot be known, unless the target contact has explicitly suppressed that
inferred row through an edit. Duplicate names are marked `ambiguous`; missing
names are marked `unresolved`. In both cases mates preserves the original text
but does not manufacture a UUID link.

## Authentication and Authorization

Proxy authentication is the default. nginx authenticates DAV and UI clients and
overwrites `X-REMOTE-USER` with the authenticated username. mates binds to
loopback by default; the backend must be reachable only through the trusted
proxy. `--public-url` supplies the external origin for canonical DAV links.
Missing, empty, or duplicate identity headers return 401. Unknown or disabled
local principals return 403. The backend health endpoint remains public.
`--auth none` is an explicit loopback-only development mode and ignores proxy
identity headers.

Authorization is a separate collection-level decision. `local_principals` and
`collection_grants` hold local identities and read/write grants; write includes
read. There are no implicit grants. Operator CLI commands provision principals,
calendar collections, timezones, and grants using direct PostgreSQL access.
New calendars default to UTC. Floating-time filtering uses the collection
timezone unless a query supplies `CAL:timezone`; archived floating values remain
floating, and `CAL:calendar-timezone` exposes the collection definition. Existing DAV
account rows continue to describe outbound connections, not local users.

Every DAV request uses a scoped store. Collection discovery, property queries,
multiget, sync, and object reads reveal only accessible collections. Writes
recheck permissions while holding collection locks and record the principal and
user agent in the raw revision. The current implementation serializes writes
across local collections to keep derived relationship changes atomic.

Explicit relationship choices use standard URI-valued `RELATED` properties,
written through CardDAV. URI assertions resolve before name matching; they are
retained through generic and Apple relationship rendering. Ambiguous names stay
ambiguous until a user selects a contact. Inverse suppression remains stored
independently from the other contact's assertion.

## Remote Read Cache

There is no separate sync operation. CardDAV reads populate a raw response
cache automatically:

- Before a CardDAV request is sent, `Mates.CardDAV` looks for a matching cached
  response younger than 15 seconds. Fresh responses are returned without
  contacting the server.
- Successful 2xx responses are cached by request identity.
- Failed live requests fall back to the most recent cached response for that
  request, even if it is older than 15 seconds.
- Remote show can fall back to the lightweight vCard data from a cached remote
  list response if the full-card request has never succeeded.

Authentication is part of the remote request boundary rather than the cache
layer. The configuration does not store an HTTP authentication scheme. The
backend uses libcurl with `CURLAUTH_ANY` so the server's `WWW-Authenticate` challenge
drives the selected mechanism. For HTTP Negotiate, the mechanism underneath is
GSSAPI/Kerberos using the platform credential cache when libcurl supports it.

The config can carry an optional `authzid` for servers that support requesting a
specific authorization identity. Legacy `auth=...`, `negotiate-authzid`, and
`gssapi-authzid` config entries are accepted by the parser for compatibility,
but rendered configuration writes only `authzid`.

The transport uses managed C interfaces to libcurl with challenge-driven HTTP
authentication, TLS verification, bounded responses, redirects, and the existing
15-second response cache/offline fallback. Credentials are never placed in a
subprocess command line. Haskell and Elm clients parse DAV XML by namespace and
honor successful property statuses.

## Architecture Completion and Verification

The nine specified workstreams now map to implementation and automated checks:

1. DAV compatibility is checked through HTTP fixtures, nginx, and vdirsyncer.
   Native phone/desktop testing remains unverified and is not claimed.
2. The documented method/report surface uses namespace-aware XML, principal and
   collection discovery, property selection, compound filtering, scoped
   multiget, and durable collection sync state.
3. The web UI discovers accessible collections and uses standard DAV requests,
   selections, and ETag preconditions. There are no private data routes.
4. Contact projections populate common normalized tables and retain original
   groups and parameters. Rendering reads projected properties and preserves
   raw fallback when fidelity is uncertain; archived revisions are unchanged.
5. libical handles calendar parsing, timezones, and recurrence. Projections cover
   floating/date/zoned timing, participants, organizers, alarms, recurrence,
   statuses, and extension properties. Queries retain VEVENT/VTODO overlap
   semantics and report expansion limits explicitly.
6. nginx authentication and SQL collection authorization are implemented as
   separate concerns, with CLI-managed principals and grants.
7. Relationships support explicit URI targets, conservative name resolution,
   inferred inverses, suppressions, and a DAV-backed contact picker.
8. Elm uses namespace-aware DAV parsing, respects collection privileges, and
   surfaces authentication, permission, and edit conflicts.
9. CLI remote access uses libcurl directly and preserves configuration, cache,
   and offline behavior.

`./scripts/check.sh` builds the backend and optimized Elm UI, runs parser and
PostgreSQL tests, runs Elm tests, and runs disposable HTTP/nginx/vdirsyncer
integration tests. `PGHOST`/`PGPORT` select the development database instance.
Test resets require a database name ending in `_test`.

The supported service surface remains object CRUD, discovery, query, multiget,
and sync for the existing address book and provisioned calendar collections.
Collection/grant administration uses the CLI. DAV ACL administration, scheduling
message delivery, server-side reminder execution, and history/revert tooling are
outside this version. CAL:expand and VALARM query filters are not advertised;
calendar-data supports component/property selection. No full RFC conformance or
native-device certification is implied by the automated compatibility suite.
