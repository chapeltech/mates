# mates

mates is a command line and service-oriented vCard/iCalendar manipulation tool
for managing contacts, calendar entries, and TODO items with a DAV-backed
workflow. It keeps local caches of imported vCards and remote DAV responses so
common reads continue to work when the server is temporarily unavailable.

The service stores contacts, calendar objects, DAV metadata, immutable raw DAV
revisions, and normalized query projections in PostgreSQL. The CLI cache
commands still use the flat-file cache described below for offline remote-read
behavior.

## Build

```sh
cabal build
```

Run the tool from the build tree:

```sh
cabal run mates -- --help
```

The backend requires GHC 9.6 or newer, Cabal, PostgreSQL with `pgcrypto` and
`citext`, and development libraries for libcurl 7.85 or newer and libical 3. The UI uses the
Elm toolchain installed by npm. `cabal.project` retains compatibility overrides
for conservative dependency bounds on newer GHC releases.

## Configure

```sh
cabal run mates -- init \
  --server https://carddav.example.com \
  --user alice \
  --addressbook /addressbooks/users/alice/default/
```

If your server supports an authorization identity, record it with `--authzid`:

```sh
cabal run mates -- init \
  --server https://carddav.example.com \
  --user alice \
  --authzid alice@EXAMPLE.COM
```

Do not configure an HTTP authentication scheme. The remote backend discovers
what the server requires from `WWW-Authenticate` challenges and uses an
available client mechanism. `--authzid` is optional and should be omitted when
the credential identity should be used directly.

This writes configuration under the XDG config directory and prepares the local
cache under the XDG cache directory.

## Run The Service

Create a PostgreSQL database first:

```sh
createdb mates
```

By default the service connects with `dbname=mates`. Override that with a
libpq connection string in `MATES_DATABASE_URL`:

```sh
MATES_DATABASE_URL='host=/var/run/postgresql dbname=mates' \
  cabal run mates -- serve --auth none --port 8099
```

On startup, `mates` reads `db/schema.sql` and creates any missing PostgreSQL
tables and indexes. Override the schema path with `MATES_SCHEMA_PATH` when
running from somewhere other than the repository root. In development, test
reset drops and recreates the `public` schema from this same file; there is no
schema migration framework yet.

```sh
cabal run mates -- serve --auth none
cabal run mates -- serve --auth none --port 8099
```

The service is a Servant/Warp application exposing:

```text
GET /health
GET /ui/
DAV /dav/
```

DAV protocol traffic is mounted at `/dav`. The current contacts CardDAV
implementation supports collection `PROPFIND`, `addressbook-query`,
`addressbook-multiget`, `GET`, `HEAD`, `PUT`, and `DELETE` below
`/dav/contacts/`. The CalDAV implementation supports collection `PROPFIND`,
`calendar-query`, `calendar-multiget`, `sync-collection`, `GET`, `HEAD`, `PUT`,
and `DELETE` below `/dav/calendars/COLLECTION/`. Uploaded `.ics` objects are
stored raw for DAV round trips. `calendar-query` supports `CAL:time-range`
filtering for `VEVENT` and `VTODO`; events account for `DTEND`, `DURATION`,
all-day date spans, `RRULE`, `RDATE`, `EXDATE`, and detached recurrence
overrides, and TODO matching follows the CalDAV `VTODO` time-range cases for
`DTSTART`, `DUE`, `DURATION`, `COMPLETED`, and `CREATED`. The web UI reads
calendar objects back through CalDAV.

Every CardDAV and CalDAV write appends a row to `dav_object_revisions` before
updating the current object pointer. The raw vCard or iCalendar bytes received
from the client are kept as the authoritative copy. Contact and calendar tables
are projections of the latest non-deleted revision, so they can be rebuilt if a
parser changes. `DELETE` creates a tombstone revision instead of deleting the
history.

The server recognizes client families from request headers. Rendering goes
through a client profile layer so stored normalized data can be emitted in the
shape a client expects. For example, generic vCard relationship output can
differ from Apple `X-ABRELATEDNAMES`/`X-ABLabel` output without changing the
stored representation. Generic clients receive standard `RELATED;VALUE=text`
properties; Apple relationship extensions are accepted as input compatibility
and projected to normalized relationship rows.

Contacts are also the person records. Genealogy-style facts such as `BDAY`,
`DEATHDATE`, `BIRTHPLACE`, `DEATHPLACE`, notes, and relationships stay attached
to the same contact root rather than moving into a separate person model.
Birth, death, and anniversary values are projected into `contact_dates` when
possible, with textual values preserved when they are partial or non-date
strings.

Relationships are modeled as graph facts, not only display strings. When a
vCard supplies an Apple related name, mates stores the asserted name with a
specific relationship label such as `mother`, `father`, `daughter`, `son`,
`husband`, or `wife`; it does not collapse those facts to `parent`, `child`,
or `spouse` in storage or display. Broader categories are derived only for
family-tree grouping and inferred inverse labels. If that name resolves to
exactly one local contact display name, mates links it to that contact UUID and
creates an inferred inverse relationship. If no local contact matches, or if
more than one local contact has the same name, the relationship remains
unresolved or ambiguous and mates does not guess. If an inferred inverse row is
removed from a contact in the web editor, mates records that suppression so the
resolver does not immediately recreate the row from the other contact's
asserted relationship.

## Web UI

The contact browser is an Elm application in `frontend/`. Install the local
Elm toolchain and build the generated JavaScript before starting the service:

```sh
npm install
npm run build:ui
cabal run mates -- serve --auth none --port 8099
```

Then visit:

```text
http://localhost:8099/ui/
```

The UI uses CardDAV and CalDAV directly on the same origin as the service. It
fetches the contact list with `addressbook-query` plus `C:address-data`
property selection, fetches the selected contact with `addressbook-multiget`,
creates contacts with CardDAV `PUT`, discovers accessible calendars with
`PROPFIND`, and fetches each calendar with CalDAV
`calendar-query`; it parses the DAV multi-status XML, vCard, and iCalendar
responses in Elm. It currently provides contact search by name or email, a
contact list, a detail pane with parsed vCard fields, contact photos, and a
family tree view built from relationship fields, a compact add-contact form, an
edit pane for selected contacts, and searchable calendar agenda day, week, and
month views. Contact edits use the loaded ETag in CardDAV `PUT` preconditions; the browser
preserves unedited vCard properties while changing the visible fields in the
form. Relationships are edited as individual rows with remove buttons and an
explicit contact picker. A selected contact is written as a URI-valued
`RELATED`; name-only assertions remain supported. Read-only collections expose
no contact editing controls. The contacts list scrolls independently beside the detail pane on
desktop. When a contact has an embedded or web-reachable vCard photo, the
detail pane displays it above the contact name. The calendar agenda shows the
next upcoming entries; day, week, and month views share previous/next
navigation around a selected date. Day and week
views lay out timed entries on 30-minute time slots, while month view shows
entries in a conventional calendar grid. Created contacts and DAV-synced
objects are persisted in PostgreSQL. `npm run build:ui` sets `ELM_HOME` to the
project-local ignored `.elm-home/` directory so it does not need to write to the
user's home directory.

## Sync With vdirsyncer

Start `mates` first:

```sh
cabal run mates -- serve --auth none --port 8099
```

Then point vdirsyncer at the local CardDAV address book:

```ini
[general]
status_path = "~/.local/state/vdirsyncer/status/"

[pair mates_contacts]
a = "mates_contacts"
b = "contacts_remote"
collections = null
conflict_resolution = "b wins"

[storage mates_contacts]
type = "carddav"
url = "http://127.0.0.1:8099/dav/contacts/"

[storage contacts_remote]
type = "carddav"
url = "https://meerclar.imrryr.org:5232/elric/"
auth = "basic"
# username = "..."
# password = "..."
```

Then run:

```sh
vdirsyncer discover mates_contacts
vdirsyncer sync mates_contacts
```

For CalDAV, point each calendar collection at its own URL:

```ini
[pair mates_calendar]
a = "local_calendar"
b = "mates_calendar"
collections = null
conflict_resolution = "a wins"

[storage local_calendar]
type = "filesystem"
path = "/path/to/vdir/calendar"
fileext = ".ics"

[storage mates_calendar]
type = "caldav"
url = "http://127.0.0.1:8099/dav/calendars/personal/"
```

Then run:

```sh
vdirsyncer discover mates_calendar
vdirsyncer sync mates_calendar
```

The examples above use explicit, loopback-only development access. Normal
service startup requires proxy authentication as described below.

Contacts synced through CardDAV appear in the web UI through CardDAV queries.
Calendar objects synced through CalDAV appear in the web UI through CalDAV
queries. Service data is durable in PostgreSQL and survives a `mates` process
restart.

## Authentication and Collection Permissions

The default service mode requires an authenticating reverse proxy. Start the
backend on loopback with the externally visible origin:

```sh
cabal run mates -- serve --port 8099 --public-url https://mates.example.com
```

nginx authenticates the client and overwrites `X-REMOTE-USER` with the
identity it authenticated. The backend must be reachable only through the
trusted proxy. A deployment example is in
[`deploy/nginx.conf.example`](deploy/nginx.conf.example). mates does not store
passwords or implement login challenges. Missing, empty, or duplicate identity
headers return 401; unknown or disabled principals return 403. `/health` remains
available on the backend without authentication.

Authorization is separate from authentication. Provision principals and
collections, then grant each principal read or write access:

```sh
cabal run mates -- principal add alice
cabal run mates -- collection list
cabal run mates -- collection create /dav/calendars/personal/ Personal
cabal run mates -- collection timezone /dav/calendars/personal/ Europe/London
cabal run mates -- access grant alice /dav/contacts/ write
cabal run mates -- access grant alice /dav/calendars/personal/ read
cabal run mates -- access list
cabal run mates -- access revoke alice /dav/calendars/personal/
cabal run mates -- principal disable alice
```

Operator commands use `MATES_DATABASE_URL` and require direct database access.
Write access includes read access. There are no automatic grants; inaccessible
collections are absent from discovery and cannot be fetched through multiget.
DAV object PUT does not create an unprovisioned calendar. Collection lifecycle
and grant administration use the CLI. New calendars default to UTC; their
timezone controls floating-time queries, and a CalDAV query can supply its own
`CAL:timezone` override.

For local development, `serve --auth none` ignores identity headers and permits
access to all provisioned collections. This mode requires a loopback host.
`--host` selects the backend bind address; proxy mode also defaults to loopback.
The public URL identifies the external origin, not a URL path prefix.

## Verification

Install npm dependencies and create a dedicated test database on a development
PostgreSQL instance:

```sh
npm install
createdb mates_test
./scripts/check.sh
```

Set `PGHOST` and `PGPORT` to select that instance. The Haskell database tests
reset only `mates_test`; the reset function refuses database names without an
`_test` suffix. The HTTP integration harness creates and drops its own temporary
database and starts its own backend and unprivileged nginx. It requires nginx,
vdirsyncer, and PostgreSQL command-line tools.

Parser and rendering checks can also run without PostgreSQL:

```sh
cabal test --test-options=--unit
npm run test:ui
```

Automated coverage includes proxy authentication, collection permissions,
namespace-aware DAV XML, property selection, conditional writes, revision
history (including a database append-only guard), persisted sync tokens and
deletions, relationships, libical recurrence
and timezones, libcurl caching, and vdirsyncer interoperability. Native phone and
desktop clients have not been verified.

## Import vCards

```sh
cabal run mates -- import contacts.vcf
```

Each imported vCard is cached as an individual `.vcf` file. Cards with a `UID`
use that value as their cache identifier. Cards without a `UID` get a stable
local identifier.

## List Contacts

```sh
cabal run mates -- list
cabal run mates -- list ada
cabal run mates -- list example.com
```

Listing reads only from the local cache, so it works offline. Queries match
partial names and partial email addresses.

## List Remote Contacts

```sh
cabal run mates -- list --remote
cabal run mates -- remote list ada
cabal run mates -- remote list example.com
```

Remote listing sends a CardDAV `addressbook-query` to the configured address
book and prints contact names. Queries use the same partial name and email
matching as local listing.

The configured `--server` can be either an address book collection URL or a
CardDAV home/principal URL. If the first query returns no cards, `mates` tries
to discover address book collections below that URL and lists cards from those
collections.

HTTP authentication is handled directly by libcurl using the server's
`WWW-Authenticate` challenge. If the chosen authentication scheme needs a
password, set `MATES_PASSWORD` in the environment before running the command.
Negotiate uses the platform Kerberos credential cache when libcurl supports
GSS-API or SSPI.

## Remote Cache

Every successful CardDAV response is cached under the XDG cache directory. When
the same remote request is repeated within 15 seconds, `mates` uses the cached
response instead of refetching it. If a live remote request fails because the
server is unavailable or returns an error, `mates` falls back to the most recent
cached response for that exact request.

Full vCards fetched by `mates show --remote` are also stored in the local card
cache, so later `mates show NAME` calls can render them without contacting the
server. Remote list results are cached as CardDAV responses rather than written
as full local vCards, because list responses usually contain only lightweight
contact data.

## List Email Addresses

```sh
cabal run mates -- email
cabal run mates -- email vera
cabal run mates -- email --remote example.com
cabal run mates -- remote email example.com
```

`email` prints one row per email address in the form:

```text
email-address<TAB>full name<TAB>EMAIL type labels
```

The query matches contacts the same way as `list` and `show`: partial names,
partial email addresses, identifiers, and cached local metadata.

## Show Or Export A Contact

```sh
cabal run mates -- show friend-1
cabal run mates -- show Ada
cabal run mates -- show ada@example.test
cabal run mates -- show --remote Ada
cabal run mates -- export friend-1 friend-1.vcf
```

`show` accepts a vCard identifier, partial contact name, or partial email
address. If no local cached card matches and CardDAV is configured, `show` tries
the remote server. Use `show --remote` to force remote lookup. Remote lookup
resolves the query from the lightweight remote list, then fetches the full card
with CardDAV `addressbook-multiget`.

`show` prints a human-readable view. It drops console-hostile payloads such as
embedded photos and prints a note when an image is present. Use `export` when
you need the raw vCard.

`export` currently requires a local vCard identifier.
