-- Development PostgreSQL schema for mates.
--
-- Goals:
--   * Store contact facts in normalized tables.
--   * Preserve enough DAV/vCard/iCalendar metadata for sync and round-tripping.
--   * Avoid storing authentication secrets.
--   * Keep common labels/types queryable instead of burying them in JSON.
--   * Preserve less-structured vCard properties in an extension table.

CREATE EXTENSION IF NOT EXISTS pgcrypto;
CREATE EXTENSION IF NOT EXISTS citext;

CREATE TABLE IF NOT EXISTS contacts (
    id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    vcard_uid text UNIQUE,
    kind text NOT NULL DEFAULT 'individual',
    display_name text NOT NULL,
    sort_name text,
    created_at timestamptz NOT NULL DEFAULT now(),
    updated_at timestamptz NOT NULL DEFAULT now(),
    deleted_at timestamptz
);

CREATE TABLE IF NOT EXISTS contact_names (
    id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    contact_id uuid NOT NULL REFERENCES contacts(id) ON DELETE CASCADE,
    is_primary boolean NOT NULL DEFAULT true,
    family_name text,
    given_name text,
    additional_names text,
    honorific_prefixes text,
    honorific_suffixes text,
    formatted_name text
);

CREATE TABLE IF NOT EXISTS contact_nicknames (
    id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    contact_id uuid NOT NULL REFERENCES contacts(id) ON DELETE CASCADE,
    nickname text NOT NULL,
    UNIQUE (contact_id, nickname)
);

CREATE TABLE IF NOT EXISTS contact_categories (
    contact_id uuid NOT NULL REFERENCES contacts(id) ON DELETE CASCADE,
    category text NOT NULL,
    PRIMARY KEY (contact_id, category)
);

CREATE TABLE IF NOT EXISTS contact_organizations (
    id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    contact_id uuid NOT NULL REFERENCES contacts(id) ON DELETE CASCADE,
    organization_name text NOT NULL,
    organization_unit text,
    title text,
    role text,
    sort_order integer NOT NULL DEFAULT 0
);

CREATE TABLE IF NOT EXISTS contact_emails (
    id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    contact_id uuid NOT NULL REFERENCES contacts(id) ON DELETE CASCADE,
    email citext NOT NULL,
    is_primary boolean NOT NULL DEFAULT false,
    label text,
    sort_order integer NOT NULL DEFAULT 0,
    UNIQUE (contact_id, email)
);

CREATE TABLE IF NOT EXISTS contact_email_types (
    email_id uuid NOT NULL REFERENCES contact_emails(id) ON DELETE CASCADE,
    type text NOT NULL,
    PRIMARY KEY (email_id, type)
);

CREATE TABLE IF NOT EXISTS contact_phones (
    id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    contact_id uuid NOT NULL REFERENCES contacts(id) ON DELETE CASCADE,
    phone text NOT NULL,
    is_primary boolean NOT NULL DEFAULT false,
    label text,
    sort_order integer NOT NULL DEFAULT 0,
    UNIQUE (contact_id, phone)
);

CREATE TABLE IF NOT EXISTS contact_phone_types (
    phone_id uuid NOT NULL REFERENCES contact_phones(id) ON DELETE CASCADE,
    type text NOT NULL,
    PRIMARY KEY (phone_id, type)
);

CREATE TABLE IF NOT EXISTS contact_addresses (
    id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    contact_id uuid NOT NULL REFERENCES contacts(id) ON DELETE CASCADE,
    is_primary boolean NOT NULL DEFAULT false,
    label text,
    post_office_box text,
    extended_address text,
    street_address text,
    locality text,
    region text,
    postal_code text,
    country_name text,
    sort_order integer NOT NULL DEFAULT 0
);

CREATE TABLE IF NOT EXISTS contact_address_types (
    address_id uuid NOT NULL REFERENCES contact_addresses(id) ON DELETE CASCADE,
    type text NOT NULL,
    PRIMARY KEY (address_id, type)
);

CREATE TABLE IF NOT EXISTS contact_urls (
    id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    contact_id uuid NOT NULL REFERENCES contacts(id) ON DELETE CASCADE,
    url text NOT NULL,
    is_primary boolean NOT NULL DEFAULT false,
    label text,
    sort_order integer NOT NULL DEFAULT 0,
    UNIQUE (contact_id, url)
);

CREATE TABLE IF NOT EXISTS contact_url_types (
    url_id uuid NOT NULL REFERENCES contact_urls(id) ON DELETE CASCADE,
    type text NOT NULL,
    PRIMARY KEY (url_id, type)
);

CREATE TABLE IF NOT EXISTS contact_impps (
    id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    contact_id uuid NOT NULL REFERENCES contacts(id) ON DELETE CASCADE,
    uri text NOT NULL,
    is_primary boolean NOT NULL DEFAULT false,
    label text,
    sort_order integer NOT NULL DEFAULT 0,
    UNIQUE (contact_id, uri)
);

CREATE TABLE IF NOT EXISTS contact_impp_types (
    impp_id uuid NOT NULL REFERENCES contact_impps(id) ON DELETE CASCADE,
    type text NOT NULL,
    PRIMARY KEY (impp_id, type)
);

CREATE TABLE IF NOT EXISTS contact_dates (
    id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    contact_id uuid NOT NULL REFERENCES contacts(id) ON DELETE CASCADE,
    kind text NOT NULL,
    date_value date,
    text_value text,
    label text,
    CHECK (date_value IS NOT NULL OR text_value IS NOT NULL)
);

CREATE TABLE IF NOT EXISTS contact_relationships (
    id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    contact_id uuid NOT NULL REFERENCES contacts(id) ON DELETE CASCADE,
    relationship_label text NOT NULL,
    related_name text NOT NULL,
    related_contact_id uuid REFERENCES contacts(id) ON DELETE SET NULL,
    resolution_status text NOT NULL DEFAULT 'unresolved',
    relationship_source text NOT NULL DEFAULT 'asserted',
    inferred_from_relationship_id uuid
        REFERENCES contact_relationships(id) ON DELETE CASCADE,
    sort_order integer NOT NULL DEFAULT 0,
    CHECK (resolution_status IN ('unresolved', 'resolved', 'ambiguous')),
    CHECK (relationship_source IN ('asserted', 'inferred'))
);

CREATE TABLE IF NOT EXISTS contact_relationship_suppressions (
    id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    contact_id uuid NOT NULL REFERENCES contacts(id) ON DELETE CASCADE,
    related_contact_id uuid NOT NULL REFERENCES contacts(id) ON DELETE CASCADE,
    relationship_label text NOT NULL,
    related_name text NOT NULL,
    created_at timestamptz NOT NULL DEFAULT now()
);

-- Extension rows preserve less-structured vCard properties. This is where
-- uncommon scalar or repeated properties live after first-class projections
-- have been populated. Apple grouped X-ABLabel companions are projected into
-- contact_relationships when they describe X-ABRELATEDNAMES. Exact unique
-- name matches are resolved to contact UUIDs; ambiguous names stay unresolved
-- rather than guessing.
CREATE TABLE IF NOT EXISTS contact_properties (
    id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    contact_id uuid NOT NULL REFERENCES contacts(id) ON DELETE CASCADE,
    property_group text,
    property_key text NOT NULL,
    value text,
    value_type text NOT NULL DEFAULT 'text',
    sort_order integer NOT NULL DEFAULT 0,
    CHECK (property_key <> '')
);

CREATE TABLE IF NOT EXISTS contact_property_parameters (
    id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    property_id uuid NOT NULL
        REFERENCES contact_properties(id) ON DELETE CASCADE,
    parameter_key text NOT NULL,
    parameter_value text NOT NULL,
    sort_order integer NOT NULL DEFAULT 0,
    CHECK (parameter_key <> '')
);

CREATE TABLE IF NOT EXISTS contact_notes (
    id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    contact_id uuid NOT NULL REFERENCES contacts(id) ON DELETE CASCADE,
    note text NOT NULL,
    sort_order integer NOT NULL DEFAULT 0
);

CREATE TABLE IF NOT EXISTS contact_media (
    id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    contact_id uuid NOT NULL REFERENCES contacts(id) ON DELETE CASCADE,
    kind text NOT NULL,
    media_type text,
    uri text,
    data bytea,
    sort_order integer NOT NULL DEFAULT 0,
    CHECK (uri IS NOT NULL OR data IS NOT NULL)
);

CREATE TABLE IF NOT EXISTS calendars (
    id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    display_name text NOT NULL,
    description text,
    color text,
    timezone text,
    created_at timestamptz NOT NULL DEFAULT now(),
    updated_at timestamptz NOT NULL DEFAULT now(),
    deleted_at timestamptz
);

CREATE TABLE IF NOT EXISTS calendar_components (
    id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    calendar_id uuid NOT NULL REFERENCES calendars(id) ON DELETE CASCADE,
    component_kind text NOT NULL,
    ical_uid text NOT NULL,
    recurrence_id text,
    sequence integer NOT NULL DEFAULT 0,
    summary text,
    description text,
    location text,
    status text,
    priority integer,
    created_at timestamptz NOT NULL DEFAULT now(),
    updated_at timestamptz NOT NULL DEFAULT now(),
    deleted_at timestamptz,
    CHECK (component_kind IN ('VEVENT', 'VTODO')),
    UNIQUE (id, component_kind)
);

CREATE TABLE IF NOT EXISTS calendar_events (
    component_id uuid PRIMARY KEY,
    component_kind text NOT NULL DEFAULT 'VEVENT',
    dtstart_at timestamptz,
    dtstart_date date,
    dtstart_tzid text,
    dtend_at timestamptz,
    dtend_date date,
    dtend_tzid text,
    duration interval,
    is_all_day boolean NOT NULL DEFAULT false,
    transparency text,
    FOREIGN KEY (component_id, component_kind)
        REFERENCES calendar_components(id, component_kind) ON DELETE CASCADE,
    CHECK (component_kind = 'VEVENT'),
    CHECK (dtstart_at IS NOT NULL OR dtstart_date IS NOT NULL),
    CHECK (dtstart_at IS NULL OR dtstart_date IS NULL),
    CHECK (dtend_at IS NULL OR dtend_date IS NULL)
);

CREATE TABLE IF NOT EXISTS calendar_todos (
    component_id uuid PRIMARY KEY,
    component_kind text NOT NULL DEFAULT 'VTODO',
    dtstart_at timestamptz,
    dtstart_date date,
    dtstart_tzid text,
    due_at timestamptz,
    due_date date,
    due_tzid text,
    completed_at timestamptz,
    created_at_ical timestamptz,
    duration interval,
    percent_complete integer,
    FOREIGN KEY (component_id, component_kind)
        REFERENCES calendar_components(id, component_kind) ON DELETE CASCADE,
    CHECK (component_kind = 'VTODO'),
    CHECK (dtstart_at IS NULL OR dtstart_date IS NULL),
    CHECK (due_at IS NULL OR due_date IS NULL),
    CHECK (percent_complete IS NULL OR percent_complete BETWEEN 0 AND 100)
);

ALTER TABLE calendar_todos
    ADD COLUMN IF NOT EXISTS created_at_ical timestamptz;

ALTER TABLE calendar_todos
    ADD COLUMN IF NOT EXISTS duration interval;

CREATE TABLE IF NOT EXISTS calendar_participants (
    id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    component_id uuid NOT NULL
        REFERENCES calendar_components(id) ON DELETE CASCADE,
    contact_id uuid REFERENCES contacts(id) ON DELETE SET NULL,
    uri text,
    email citext,
    common_name text,
    role text,
    participation_status text,
    rsvp boolean,
    sort_order integer NOT NULL DEFAULT 0,
    CHECK (uri IS NOT NULL OR email IS NOT NULL OR contact_id IS NOT NULL)
);

CREATE TABLE IF NOT EXISTS calendar_recurrence_rules (
    id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    component_id uuid NOT NULL
        REFERENCES calendar_components(id) ON DELETE CASCADE,
    rule text NOT NULL,
    sort_order integer NOT NULL DEFAULT 0
);

CREATE TABLE IF NOT EXISTS calendar_recurrence_dates (
    id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    component_id uuid NOT NULL
        REFERENCES calendar_components(id) ON DELETE CASCADE,
    recurrence_kind text NOT NULL,
    occurs_at timestamptz,
    occurs_date date,
    tzid text,
    sort_order integer NOT NULL DEFAULT 0,
    CHECK (recurrence_kind IN ('RDATE', 'EXDATE')),
    CHECK (occurs_at IS NOT NULL OR occurs_date IS NOT NULL),
    CHECK (occurs_at IS NULL OR occurs_date IS NULL)
);

CREATE TABLE IF NOT EXISTS calendar_alarms (
    id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    component_id uuid NOT NULL
        REFERENCES calendar_components(id) ON DELETE CASCADE,
    action text NOT NULL,
    trigger_at timestamptz,
    trigger_offset interval,
    trigger_relation text,
    repeat_count integer,
    repeat_interval interval,
    summary text,
    description text,
    sort_order integer NOT NULL DEFAULT 0,
    CHECK (trigger_at IS NOT NULL OR trigger_offset IS NOT NULL),
    CHECK (trigger_at IS NULL OR trigger_offset IS NULL)
);

-- Extension rows preserve less-structured iCalendar properties for VEVENT and
-- VTODO components, including X-* properties and uncommon repeated values.
CREATE TABLE IF NOT EXISTS calendar_properties (
    id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    component_id uuid NOT NULL
        REFERENCES calendar_components(id) ON DELETE CASCADE,
    property_key text NOT NULL,
    value text,
    value_type text NOT NULL DEFAULT 'text',
    sort_order integer NOT NULL DEFAULT 0,
    CHECK (property_key <> '')
);

CREATE TABLE IF NOT EXISTS calendar_property_parameters (
    id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    property_id uuid NOT NULL
        REFERENCES calendar_properties(id) ON DELETE CASCADE,
    parameter_key text NOT NULL,
    parameter_value text NOT NULL,
    sort_order integer NOT NULL DEFAULT 0,
    CHECK (parameter_key <> '')
);

CREATE TABLE IF NOT EXISTS dav_accounts (
    id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    server_url text NOT NULL,
    username text NOT NULL,
    authzid text,
    created_at timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS dav_collections (
    id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    account_id uuid NOT NULL REFERENCES dav_accounts(id) ON DELETE CASCADE,
    calendar_id uuid REFERENCES calendars(id) ON DELETE SET NULL,
    collection_kind text NOT NULL,
    url text NOT NULL,
    display_name text,
    ctag text,
    sync_token text,
    last_seen_at timestamptz,
    CHECK (collection_kind IN ('addressbook', 'calendar', 'unknown')),
    UNIQUE (account_id, url)
);

-- etag and raw_payload are retained on dav_objects only as legacy/backfill
-- columns for pre-revision development databases. New writes use the
-- append-only dav_object_revisions table as the authoritative body store.
CREATE TABLE IF NOT EXISTS dav_objects (
    id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    collection_id uuid NOT NULL
        REFERENCES dav_collections(id) ON DELETE CASCADE,
    object_kind text NOT NULL,
    href text NOT NULL,
    current_revision_id uuid,
    resource_uid text,
    deleted_at timestamptz,
    created_at timestamptz NOT NULL DEFAULT now(),
    updated_at timestamptz NOT NULL DEFAULT now(),
    etag text,
    raw_payload text,
    last_fetched_at timestamptz,
    last_pushed_at timestamptz,
    dirty boolean NOT NULL DEFAULT false,
    deleted_remote boolean NOT NULL DEFAULT false,
    deleted_local boolean NOT NULL DEFAULT false,
    CHECK (object_kind IN ('VCARD', 'VEVENT', 'VTODO', 'VJOURNAL', 'UNKNOWN')),
    UNIQUE (collection_id, href)
);

ALTER TABLE dav_objects
    ADD COLUMN IF NOT EXISTS current_revision_id uuid;

ALTER TABLE dav_objects
    ADD COLUMN IF NOT EXISTS deleted_at timestamptz;

ALTER TABLE dav_objects
    ADD COLUMN IF NOT EXISTS created_at timestamptz NOT NULL DEFAULT now();

ALTER TABLE dav_objects
    ADD COLUMN IF NOT EXISTS updated_at timestamptz NOT NULL DEFAULT now();

CREATE TABLE IF NOT EXISTS dav_object_revisions (
    id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    dav_object_id uuid NOT NULL REFERENCES dav_objects(id) ON DELETE CASCADE,
    revision_no bigint NOT NULL,
    previous_revision_id uuid REFERENCES dav_object_revisions(id),
    operation text NOT NULL,
    source text NOT NULL DEFAULT 'dav',
    actor_principal text,
    client_user_agent text,
    content_type text,
    object_kind text NOT NULL,
    resource_uid text,
    raw_payload bytea,
    raw_payload_sha256 bytea,
    raw_payload_size bigint,
    etag text NOT NULL,
    parse_status text NOT NULL DEFAULT 'pending',
    parse_error text,
    parser_version text,
    created_at timestamptz NOT NULL DEFAULT now(),
    CHECK (operation IN ('put', 'delete', 'import', 'repair', 'server-rewrite')),
    CHECK (source IN ('dav', 'cli', 'import', 'system')),
    CHECK (object_kind IN ('VCARD', 'VEVENT', 'VTODO', 'VJOURNAL', 'UNKNOWN')),
    CHECK (parse_status IN ('pending', 'ok', 'failed', 'skipped')),
    CHECK (operation = 'delete' OR raw_payload IS NOT NULL),
    UNIQUE (dav_object_id, revision_no)
);

CREATE TABLE IF NOT EXISTS dav_object_projections (
    dav_object_id uuid PRIMARY KEY REFERENCES dav_objects(id) ON DELETE CASCADE,
    revision_id uuid NOT NULL REFERENCES dav_object_revisions(id) ON DELETE CASCADE,
    contact_id uuid REFERENCES contacts(id) ON DELETE SET NULL,
    calendar_component_id uuid REFERENCES calendar_components(id) ON DELETE SET NULL,
    projection_kind text NOT NULL,
    status text NOT NULL,
    error text,
    parser_version text,
    projected_at timestamptz NOT NULL DEFAULT now(),
    CHECK (projection_kind IN ('contact', 'calendar-component', 'none')),
    CHECK (status IN ('ok', 'failed', 'skipped')),
    CHECK (
        (contact_id IS NOT NULL AND calendar_component_id IS NULL)
        OR (contact_id IS NULL AND calendar_component_id IS NOT NULL)
        OR (contact_id IS NULL AND calendar_component_id IS NULL)
    )
);

CREATE TABLE IF NOT EXISTS contact_dav_objects (
    contact_id uuid NOT NULL REFERENCES contacts(id) ON DELETE CASCADE,
    dav_object_id uuid NOT NULL REFERENCES dav_objects(id) ON DELETE CASCADE,
    PRIMARY KEY (contact_id, dav_object_id),
    UNIQUE (dav_object_id)
);

CREATE TABLE IF NOT EXISTS calendar_component_dav_objects (
    component_id uuid NOT NULL
        REFERENCES calendar_components(id) ON DELETE CASCADE,
    dav_object_id uuid NOT NULL REFERENCES dav_objects(id) ON DELETE CASCADE,
    PRIMARY KEY (component_id, dav_object_id)
);

ALTER TABLE calendar_component_dav_objects
    DROP CONSTRAINT IF EXISTS calendar_component_dav_objects_dav_object_id_key;

CREATE TABLE IF NOT EXISTS remote_response_cache (
    cache_key text PRIMARY KEY,
    account_id uuid REFERENCES dav_accounts(id) ON DELETE CASCADE,
    method text NOT NULL,
    url text NOT NULL,
    depth text,
    request_body text,
    response_body text NOT NULL,
    status integer NOT NULL,
    fetched_at timestamptz NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS contacts_display_name_idx
    ON contacts (lower(display_name));

CREATE INDEX IF NOT EXISTS contacts_sort_name_idx
    ON contacts (lower(sort_name));

CREATE INDEX IF NOT EXISTS contacts_kind_idx
    ON contacts (kind);

CREATE UNIQUE INDEX IF NOT EXISTS contact_names_one_primary_idx
    ON contact_names (contact_id)
    WHERE is_primary;

CREATE INDEX IF NOT EXISTS contact_emails_email_idx
    ON contact_emails (email);

CREATE INDEX IF NOT EXISTS contact_emails_email_pattern_idx
    ON contact_emails (lower(email::text) text_pattern_ops);

CREATE UNIQUE INDEX IF NOT EXISTS contact_emails_one_primary_idx
    ON contact_emails (contact_id)
    WHERE is_primary;

CREATE INDEX IF NOT EXISTS contact_phones_phone_idx
    ON contact_phones (phone);

CREATE UNIQUE INDEX IF NOT EXISTS contact_phones_one_primary_idx
    ON contact_phones (contact_id)
    WHERE is_primary;

CREATE UNIQUE INDEX IF NOT EXISTS contact_addresses_one_primary_idx
    ON contact_addresses (contact_id)
    WHERE is_primary;

CREATE UNIQUE INDEX IF NOT EXISTS contact_urls_one_primary_idx
    ON contact_urls (contact_id)
    WHERE is_primary;

CREATE UNIQUE INDEX IF NOT EXISTS contact_impps_one_primary_idx
    ON contact_impps (contact_id)
    WHERE is_primary;

CREATE INDEX IF NOT EXISTS contact_dates_contact_id_idx
    ON contact_dates (contact_id);

CREATE INDEX IF NOT EXISTS contact_dates_kind_idx
    ON contact_dates (kind);

CREATE INDEX IF NOT EXISTS contact_dates_contact_kind_idx
    ON contact_dates (contact_id, kind);

CREATE INDEX IF NOT EXISTS contact_properties_contact_id_idx
    ON contact_properties (contact_id);

CREATE INDEX IF NOT EXISTS contact_properties_key_idx
    ON contact_properties (property_key);

CREATE INDEX IF NOT EXISTS contact_properties_contact_key_idx
    ON contact_properties (contact_id, property_key);

CREATE INDEX IF NOT EXISTS contact_property_parameters_key_idx
    ON contact_property_parameters (parameter_key);

CREATE INDEX IF NOT EXISTS contact_property_parameters_property_id_idx
    ON contact_property_parameters (property_id);

CREATE INDEX IF NOT EXISTS contact_relationships_contact_id_idx
    ON contact_relationships (contact_id);

CREATE INDEX IF NOT EXISTS contact_relationships_label_idx
    ON contact_relationships (relationship_label);

CREATE INDEX IF NOT EXISTS contact_relationships_related_contact_id_idx
    ON contact_relationships (related_contact_id);

CREATE INDEX IF NOT EXISTS contact_relationships_resolution_status_idx
    ON contact_relationships (resolution_status);

CREATE UNIQUE INDEX IF NOT EXISTS contact_relationships_inferred_source_idx
    ON contact_relationships (inferred_from_relationship_id)
    WHERE inferred_from_relationship_id IS NOT NULL;

CREATE UNIQUE INDEX IF NOT EXISTS contact_relationship_suppressions_unique_idx
    ON contact_relationship_suppressions
       (contact_id, related_contact_id, relationship_label, lower(related_name));

CREATE INDEX IF NOT EXISTS contact_relationship_suppressions_contact_id_idx
    ON contact_relationship_suppressions (contact_id);

CREATE INDEX IF NOT EXISTS contact_relationship_suppressions_related_contact_id_idx
    ON contact_relationship_suppressions (related_contact_id);

CREATE INDEX IF NOT EXISTS calendars_display_name_idx
    ON calendars (lower(display_name));

CREATE INDEX IF NOT EXISTS calendar_components_calendar_id_idx
    ON calendar_components (calendar_id);

CREATE INDEX IF NOT EXISTS calendar_components_kind_idx
    ON calendar_components (component_kind);

CREATE INDEX IF NOT EXISTS calendar_components_status_idx
    ON calendar_components (status);

CREATE INDEX IF NOT EXISTS calendar_components_summary_idx
    ON calendar_components (lower(summary));

CREATE UNIQUE INDEX IF NOT EXISTS calendar_components_uid_idx
    ON calendar_components
       (calendar_id, component_kind, ical_uid, coalesce(recurrence_id, ''));

CREATE INDEX IF NOT EXISTS calendar_events_dtstart_at_idx
    ON calendar_events (dtstart_at);

CREATE INDEX IF NOT EXISTS calendar_events_dtstart_date_idx
    ON calendar_events (dtstart_date);

CREATE INDEX IF NOT EXISTS calendar_events_dtend_at_idx
    ON calendar_events (dtend_at);

CREATE INDEX IF NOT EXISTS calendar_todos_due_at_idx
    ON calendar_todos (due_at);

CREATE INDEX IF NOT EXISTS calendar_todos_due_date_idx
    ON calendar_todos (due_date);

CREATE INDEX IF NOT EXISTS calendar_todos_completed_at_idx
    ON calendar_todos (completed_at);

CREATE INDEX IF NOT EXISTS calendar_todos_created_at_ical_idx
    ON calendar_todos (created_at_ical);

CREATE INDEX IF NOT EXISTS calendar_participants_component_id_idx
    ON calendar_participants (component_id);

CREATE INDEX IF NOT EXISTS calendar_participants_email_idx
    ON calendar_participants (email);

CREATE INDEX IF NOT EXISTS calendar_recurrence_rules_component_id_idx
    ON calendar_recurrence_rules (component_id);

CREATE INDEX IF NOT EXISTS calendar_recurrence_dates_component_id_idx
    ON calendar_recurrence_dates (component_id);

CREATE INDEX IF NOT EXISTS calendar_recurrence_dates_occurs_at_idx
    ON calendar_recurrence_dates (occurs_at);

CREATE INDEX IF NOT EXISTS calendar_recurrence_dates_occurs_date_idx
    ON calendar_recurrence_dates (occurs_date);

CREATE INDEX IF NOT EXISTS calendar_alarms_component_id_idx
    ON calendar_alarms (component_id);

CREATE INDEX IF NOT EXISTS calendar_properties_component_id_idx
    ON calendar_properties (component_id);

CREATE INDEX IF NOT EXISTS calendar_properties_key_idx
    ON calendar_properties (property_key);

CREATE INDEX IF NOT EXISTS calendar_properties_component_key_idx
    ON calendar_properties (component_id, property_key);

CREATE INDEX IF NOT EXISTS calendar_property_parameters_key_idx
    ON calendar_property_parameters (parameter_key);

CREATE INDEX IF NOT EXISTS calendar_property_parameters_property_id_idx
    ON calendar_property_parameters (property_id);

CREATE UNIQUE INDEX IF NOT EXISTS dav_accounts_unique_idx
    ON dav_accounts (server_url, username, coalesce(authzid, ''));

CREATE INDEX IF NOT EXISTS dav_collections_account_id_idx
    ON dav_collections (account_id);

CREATE INDEX IF NOT EXISTS dav_collections_kind_idx
    ON dav_collections (collection_kind);

CREATE INDEX IF NOT EXISTS dav_collections_calendar_id_idx
    ON dav_collections (calendar_id);

CREATE INDEX IF NOT EXISTS dav_objects_collection_id_idx
    ON dav_objects (collection_id);

CREATE INDEX IF NOT EXISTS dav_objects_resource_uid_idx
    ON dav_objects (resource_uid);

CREATE INDEX IF NOT EXISTS dav_objects_kind_idx
    ON dav_objects (object_kind);

CREATE INDEX IF NOT EXISTS dav_objects_current_revision_idx
    ON dav_objects (current_revision_id);

CREATE INDEX IF NOT EXISTS dav_objects_deleted_at_idx
    ON dav_objects (deleted_at);

CREATE INDEX IF NOT EXISTS dav_object_revisions_object_idx
    ON dav_object_revisions (dav_object_id, revision_no DESC);

CREATE INDEX IF NOT EXISTS dav_object_revisions_created_at_idx
    ON dav_object_revisions (created_at DESC);

CREATE INDEX IF NOT EXISTS dav_object_revisions_sha256_idx
    ON dav_object_revisions (raw_payload_sha256);

CREATE INDEX IF NOT EXISTS dav_object_revisions_uid_idx
    ON dav_object_revisions (resource_uid);

CREATE INDEX IF NOT EXISTS dav_object_projections_revision_idx
    ON dav_object_projections (revision_id);

CREATE INDEX IF NOT EXISTS contact_dav_objects_contact_id_idx
    ON contact_dav_objects (contact_id);

CREATE INDEX IF NOT EXISTS calendar_component_dav_objects_component_id_idx
    ON calendar_component_dav_objects (component_id);

CREATE INDEX IF NOT EXISTS remote_response_cache_fetched_at_idx
    ON remote_response_cache (fetched_at);

-- Local identities are supplied by an authenticating reverse proxy. They are
-- deliberately separate from outbound DAV accounts and contain no secrets.
CREATE TABLE IF NOT EXISTS local_principals (
    username text PRIMARY KEY CHECK (username <> ''),
    enabled boolean NOT NULL DEFAULT true
);
CREATE TABLE IF NOT EXISTS collection_grants (
    username text NOT NULL REFERENCES local_principals(username) ON DELETE CASCADE,
    collection_id uuid NOT NULL REFERENCES dav_collections(id) ON DELETE CASCADE,
    access text NOT NULL CHECK (access IN ('read', 'write')),
    PRIMARY KEY (username, collection_id)
);
ALTER TABLE dav_collections ADD COLUMN IF NOT EXISTS sync_generation uuid NOT NULL DEFAULT gen_random_uuid();
ALTER TABLE dav_collections ADD COLUMN IF NOT EXISTS change_sequence bigint NOT NULL DEFAULT 0;
CREATE TABLE IF NOT EXISTS dav_collection_changes (
    collection_id uuid NOT NULL REFERENCES dav_collections(id) ON DELETE CASCADE,
    sequence bigint NOT NULL,
    href text NOT NULL,
    deleted boolean NOT NULL,
    PRIMARY KEY (collection_id, sequence)
);
CREATE INDEX IF NOT EXISTS dav_collection_changes_href_idx
    ON dav_collection_changes(collection_id, href, sequence DESC);
CREATE OR REPLACE FUNCTION mates_record_change(object_id uuid, is_deleted boolean)
RETURNS void LANGUAGE plpgsql AS $$
DECLARE collection uuid; next_sequence bigint; object_href text;
BEGIN
    SELECT collection_id, href INTO collection, object_href FROM dav_objects WHERE id = object_id;
    IF collection IS NULL THEN RETURN; END IF;
    UPDATE dav_collections SET change_sequence = change_sequence + 1
        WHERE id = collection RETURNING change_sequence INTO next_sequence;
    INSERT INTO dav_collection_changes VALUES (collection, next_sequence, object_href, is_deleted);
END $$;
CREATE OR REPLACE FUNCTION mates_revision_context()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
    NEW.actor_principal := nullif(current_setting('mates.principal', true), '');
    NEW.client_user_agent := nullif(current_setting('mates.user_agent', true), '');
    RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS mates_revision_context ON dav_object_revisions;
CREATE TRIGGER mates_revision_context BEFORE INSERT ON dav_object_revisions
    FOR EACH ROW EXECUTE FUNCTION mates_revision_context();
CREATE OR REPLACE FUNCTION mates_revision_change()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
    PERFORM mates_record_change(NEW.dav_object_id, NEW.operation = 'delete');
    RETURN NEW;
END $$;
DROP TRIGGER IF EXISTS mates_revision_change ON dav_object_revisions;
CREATE TRIGGER mates_revision_change AFTER INSERT ON dav_object_revisions
    FOR EACH ROW EXECUTE FUNCTION mates_revision_change();

ALTER TABLE calendar_events ADD COLUMN IF NOT EXISTS dtstart_local timestamp;
ALTER TABLE calendar_events ADD COLUMN IF NOT EXISTS dtend_local timestamp;
ALTER TABLE calendar_todos ADD COLUMN IF NOT EXISTS dtstart_local timestamp;
ALTER TABLE calendar_todos ADD COLUMN IF NOT EXISTS due_local timestamp;
ALTER TABLE calendar_participants ADD COLUMN IF NOT EXISTS participant_kind text NOT NULL DEFAULT 'attendee';
-- Upgrade the early development schema's timing checks to admit floating times.
DO $$ DECLARE constraint_row record;
BEGIN
    FOR constraint_row IN SELECT conname FROM pg_constraint WHERE conrelid = 'calendar_events'::regclass AND contype = 'c' AND (pg_get_constraintdef(oid) LIKE '%dtstart_at%' OR pg_get_constraintdef(oid) LIKE '%dtend_at%') LOOP
        EXECUTE format('ALTER TABLE calendar_events DROP CONSTRAINT %I', constraint_row.conname);
    END LOOP;
    ALTER TABLE calendar_events ADD CONSTRAINT calendar_events_start_value CHECK (num_nonnulls(dtstart_at, dtstart_date, dtstart_local) = 1);
    ALTER TABLE calendar_events ADD CONSTRAINT calendar_events_end_value CHECK (num_nonnulls(dtend_at, dtend_date, dtend_local) <= 1);
END $$;
ALTER TABLE contact_relationships ADD COLUMN IF NOT EXISTS related_uri text;
ALTER TABLE calendar_recurrence_dates ADD COLUMN IF NOT EXISTS occurs_local timestamp;
DO $$ DECLARE constraint_row record;
BEGIN
    FOR constraint_row IN SELECT conname FROM pg_constraint WHERE conrelid = 'calendar_recurrence_dates'::regclass AND contype = 'c' AND pg_get_constraintdef(oid) LIKE '%occurs_at%' LOOP
        EXECUTE format('ALTER TABLE calendar_recurrence_dates DROP CONSTRAINT %I', constraint_row.conname);
    END LOOP;
    ALTER TABLE calendar_recurrence_dates ADD CONSTRAINT recurrence_date_value CHECK (num_nonnulls(occurs_at, occurs_date, occurs_local) = 1);
END $$;
ALTER TABLE calendar_recurrence_dates ADD COLUMN IF NOT EXISTS period_end_at timestamptz;
ALTER TABLE calendar_recurrence_dates ADD COLUMN IF NOT EXISTS period_end_local timestamp;
ALTER TABLE calendar_recurrence_dates ADD COLUMN IF NOT EXISTS period_duration interval;
ALTER TABLE dav_collections ADD COLUMN IF NOT EXISTS presentation_version text;
CREATE TABLE IF NOT EXISTS mates_schema_version (
    singleton boolean PRIMARY KEY DEFAULT true CHECK (singleton),
    digest text NOT NULL
);

-- Raw revisions are immutable even when an operator accidentally issues a
-- direct UPDATE or DELETE. Projection rebuilds write their own status table.
CREATE OR REPLACE FUNCTION mates_reject_revision_change()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
    RAISE EXCEPTION 'dav_object_revisions is append-only' USING ERRCODE = '55000';
END $$;
DROP TRIGGER IF EXISTS mates_immutable_revisions ON dav_object_revisions;
CREATE TRIGGER mates_immutable_revisions BEFORE UPDATE OR DELETE ON dav_object_revisions
    FOR EACH ROW EXECUTE FUNCTION mates_reject_revision_change();
