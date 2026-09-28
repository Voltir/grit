-- grit app schema, applied idempotently at startup.
-- DBOS system tables (dbos.*) are managed separately by DBOS's own migrations.
-- No migrations yet: until a database holds real data, change this file and run
-- scripts/reset-db. IF NOT EXISTS never reshapes a
-- table, so a changed table needs the reset.

CREATE SCHEMA IF NOT EXISTS grit;

-- BM25 ranking (ADR 0005). The image preloads the library; this makes it usable here.
CREATE EXTENSION IF NOT EXISTS pg_textsearch;

-- The searchable text of a stored payload (PayloadJson): a message's user text, its
-- assistant text blocks, a tool result's content; a summary's text; a closing entry's
-- flows (ClosingJson): its prose, its outcome, the lines it added, resolved or dropped and
-- how or why. Never the balance it carries, which repeats in every closing: the closing
-- that added a line holds it in its flows. Never reasoning,
-- tool-call arguments, or any other kind of payload. IMMUTABLE, so the generated column
-- below may call it.
CREATE OR REPLACE FUNCTION grit.entry_text(payload jsonb) RETURNS text
LANGUAGE sql IMMUTABLE PARALLEL SAFE STRICT
RETURN CASE payload ->> 'kind'
    WHEN 'message' THEN concat_ws(' ',
        payload #>> '{message,text}',
        (SELECT string_agg(b ->> 'text', ' ')
           FROM jsonb_array_elements(
                  CASE WHEN jsonb_typeof(payload #> '{message,blocks}') = 'array'
                       THEN payload #> '{message,blocks}' ELSE '[]'::jsonb END) AS b
          WHERE b ->> 'type' = 'text'),
        payload #>> '{message,content}')
    WHEN 'summary' THEN payload ->> 'text'
    WHEN 'closed' THEN concat_ws(' ',
        payload #>> '{closing,flows,prose}',
        payload #>> '{closing,flows,outcome}',
        (SELECT string_agg(concat_ws(' ',
                  change #>> '{added,text}',
                  change #>> '{resolved,text}', change ->> 'how',
                  change #>> '{dropped,text}',
                  CASE WHEN change ? 'dropped' THEN change ->> 'why' END), ' ')
           FROM jsonb_array_elements(
                  CASE WHEN jsonb_typeof(payload #> '{closing,flows,changes}') = 'array'
                       THEN payload #> '{closing,flows,changes}' ELSE '[]'::jsonb END) AS change))
END;

-- Where conversations happen (grit.core.place.Place, ADR 0013): one containment tree whose
-- root is everywhere. A path runs from its namespace down: {fs,home,nick,Projects,grit},
-- {slack,acme,#grit-dev,1712.3}, {task,m0,main}; segments verbatim, never empty or NULL. A
-- place is recorded with its first conversation, from that conversation's origin
-- (Origin.place), and never changes. Which place is within which is Place.within's alone:
-- nothing here tests it.
-- Retention: ledger: deleted with its last conversation (Target.Quiet).
CREATE TABLE IF NOT EXISTS grit.places (
    id   UUID PRIMARY KEY DEFAULT uuidv7(),
    path TEXT[] NOT NULL UNIQUE
         CHECK (cardinality(path) >= 1 AND array_position(path, '') IS NULL
                AND array_position(path, NULL) IS NULL)
);

-- The engine holding this database's lock (EngineLock, ADR 0015), as it described itself.
-- The lock is the truth; this row says who holds it. It is current only while its
-- backend_pid holds the advisory lock (pg_locks): a row a dead engine left is overwritten by
-- the next holder, and never reported, since only a process refused the lock reads it.
-- Where grit runs (a directory) is an edge's, and not here.
-- Retention: kept: one row, overwritten by each holder.
CREATE TABLE IF NOT EXISTS grit.engines (
    slot         BOOLEAN PRIMARY KEY DEFAULT true CHECK (slot),
    machine      TEXT NOT NULL,
    pid          BIGINT NOT NULL,
    backend_pid  INTEGER NOT NULL,
    epoch        TEXT NOT NULL,
    started_at   TIMESTAMPTZ NOT NULL,
    heartbeat_at TIMESTAMPTZ NOT NULL
);

-- Who actions are done for (grit.core.id.PrincipalId): `local`, the one person every edge
-- acts for until principals are registered, `grit`, the engine itself, and each person an
-- edge enrolled (Principals.enroll: a Slack user as `slack:{team}/{user}`), with the name a
-- window shows on what they wrote. Other tables name a principal here, never by a free
-- string.
-- Retention: kept: the identities other rows name.
CREATE TABLE IF NOT EXISTS grit.principals (
    id   TEXT PRIMARY KEY,
    kind TEXT NOT NULL CHECK (kind IN ('person', 'grit')),
    name TEXT
);

INSERT INTO grit.principals (id, kind) VALUES ('local', 'person'), ('grit', 'grit')
    ON CONFLICT (id) DO NOTHING;

-- One row per origin; `origin` is the Origin ADT as JSON, and jsonb equality
-- ignores key order, so the unique index is on the value, not its spelling. Its place is
-- its origin's, set when it is created, and `created_by` the principal of the edge that
-- created it, kept whoever finds it later.
-- Retention: ledger: deleted whole once quiet past the ledger window (Target.Quiet).
CREATE TABLE IF NOT EXISTS grit.conversations (
    id         UUID PRIMARY KEY DEFAULT uuidv7(),
    origin     JSONB NOT NULL UNIQUE,
    place_id   UUID NOT NULL REFERENCES grit.places(id),
    created_by TEXT NOT NULL REFERENCES grit.principals(id),
    created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_conversations_place ON grit.conversations (place_id);

-- Retention: journal: a closed period's raw entries after the raw window (Target.Raw); its closing
-- entry is ledger (Target.Superseded, Target.Quiet).
CREATE TABLE IF NOT EXISTS grit.entries (
    id              TEXT PRIMARY KEY,
    conversation_id UUID NOT NULL REFERENCES grit.conversations(id) ON DELETE CASCADE,
    turn_seq        BIGINT NOT NULL,
    parent_id       TEXT,
    seq             BIGINT NOT NULL,
    payload         JSONB NOT NULL DEFAULT '{}'::jsonb,
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
    -- What EntrySearch ranks; NULL for a payload with nothing searchable.
    search_text     TEXT GENERATED ALWAYS AS (grit.entry_text(payload)) STORED,
    -- One entry per position. Writers allocate seq under the conversation's row lock
    -- (EntryStore.lockNext); this makes a writer that skipped the lock fail, not interleave.
    CONSTRAINT entries_conversation_seq UNIQUE (conversation_id, seq)
);

CREATE INDEX IF NOT EXISTS idx_entries_parent       ON grit.entries(parent_id);
CREATE INDEX IF NOT EXISTS idx_entries_conversation ON grit.entries(conversation_id);
-- The index's statistics span every row it holds, so conversations sharing it affect
-- each other's ranking (ADR 0005). Queries name it: to_bm25query(q, 'grit.idx_entries_bm25').
CREATE INDEX IF NOT EXISTS idx_entries_bm25 ON grit.entries
    USING bm25 (search_text) WITH (text_config = 'english');

-- Who wrote each inbound entry: the principal of the edge that ingested it (Inbox.ingest).
-- Every other entry is grit's own. A table beside `entries` rather than a column on it, so
-- no entry needs an author it does not have.
-- Retention: journal: with its entry (Target.Raw), by cascade.
CREATE TABLE IF NOT EXISTS grit.inbound (
    entry_id TEXT PRIMARY KEY REFERENCES grit.entries(id) ON DELETE CASCADE,
    author   TEXT NOT NULL REFERENCES grit.principals(id)
);

-- One row per model response: what it cost. Keyed by the entry that holds the response,
-- so the turn's append writes both in one transaction and a replay cannot count twice. Not a
-- foreign key: the row outlives the entry's purge, and goes with its period's closing, by
-- the turn it was made for (a close's, its period's last).
-- Retention: ledger: with its period's closing (Target.Superseded, Target.Quiet).
CREATE TABLE IF NOT EXISTS grit.usage_ledger (
    entry_id            TEXT PRIMARY KEY,
    conversation_id     UUID NOT NULL,
    turn_seq            BIGINT NOT NULL,
    workflow_id         TEXT NOT NULL,
    model               TEXT NOT NULL,
    input_tokens        BIGINT NOT NULL,
    output_tokens       BIGINT NOT NULL,
    cached_input_tokens BIGINT NOT NULL,
    -- The provider's own figure; NULL when it reports none.
    cost_usd            NUMERIC,
    -- grit's estimate of input_tokens for the same request (TokenEstimator): the two
    -- side by side are how an estimator is checked.
    estimated_input_tokens BIGINT NOT NULL,
    created_at          TIMESTAMPTZ NOT NULL DEFAULT now(),
    -- Record order, which UsageLedger.of reads in. created_at cannot say it: now() is the
    -- transaction's start, the same for every row one transaction records.
    ordinal             BIGINT GENERATED ALWAYS AS IDENTITY
);

-- A turn's costs, read by edges (UsageLedger.of).
CREATE INDEX IF NOT EXISTS idx_usage_ledger_workflow ON grit.usage_ledger (workflow_id, ordinal);
CREATE INDEX IF NOT EXISTS idx_usage_ledger_turn ON grit.usage_ledger (conversation_id, turn_seq);
-- A day's spend, read before each new message when a daily cap is set (Spending.on).
CREATE INDEX IF NOT EXISTS idx_usage_ledger_created ON grit.usage_ledger (created_at);

-- Every distinct profile a turn ran under (grit.core.model.TurnProfile): the model, budget,
-- upstream and settings each role's calls were made under. Keyed by its content hash, so
-- it is written once however many turns share it, and never changed.
-- Retention: kept: content-addressed, one per distinct profile a turn ran under.
CREATE TABLE IF NOT EXISTS grit.model_profiles (
    id         TEXT PRIMARY KEY,
    profile    JSONB NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

-- Which profile each turn ran under, set once when the turn starts.
-- Retention: ledger: with its period's closing (Target.Superseded, Target.Quiet).
CREATE TABLE IF NOT EXISTS grit.turn_model_profiles (
    workflow_id      TEXT PRIMARY KEY,
    model_profile_id TEXT NOT NULL REFERENCES grit.model_profiles(id),
    created_at       TIMESTAMPTZ NOT NULL DEFAULT now()
);

-- Every distinct system-prompt fragment a turn was sent or an edge advertised
-- (grit.core.prompt.Fragment), keyed by its content hash, so it is written once however many
-- turns and conversations share it, and never changed. Kept forever: a turn's recorded
-- first step names these ids, and its replay reads the texts back from here. Nothing deletes
-- a row, even one no turn names any more.
-- Retention: kept: content-addressed; replay reads fragments by id.
CREATE TABLE IF NOT EXISTS grit.prompt_fragments (
    id         TEXT PRIMARY KEY,
    layer      TEXT NOT NULL,
    source     TEXT NOT NULL,
    text       TEXT NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

-- The fragments each turn's system prompt was built from, in order: for the turn panel and
-- the eval. Set once when the turn starts.
-- Retention: ledger: with its period's closing (Target.Superseded, Target.Quiet).
CREATE TABLE IF NOT EXISTS grit.turn_prompts (
    workflow_id TEXT PRIMARY KEY,
    fragments   JSONB NOT NULL,
    created_at  TIMESTAMPTZ NOT NULL DEFAULT now()
);

-- Every distinct tool set a turn was offered (grit.core.tool.ToolSet): each tool's name,
-- description, schema, whether it asks first, and its retry. Keyed by its content hash, so
-- it is written once however many turns share it, and never changed; a turn's recorded
-- first step names it, and its replay reads it back from here.
-- Retention: kept: content-addressed; replay reads a set by id.
CREATE TABLE IF NOT EXISTS grit.tool_sets (
    id         TEXT PRIMARY KEY,
    tools      JSONB NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

-- A turn's reply an edge is to post outside grit (Deliveries): where it goes (the edge's own
-- address, such as a Slack channel and thread), and whether it is wholly delivered; each part
-- begun is a row of delivery_parts, `posted_as` NULL while it is being posted. What lets an
-- edge that restarts post every reply and none twice.
-- Retention: journal: with its turn's period (Target.Raw), by PeriodStore.purge.
CREATE TABLE IF NOT EXISTS grit.deliveries (
    workflow        TEXT PRIMARY KEY,
    conversation_id UUID NOT NULL REFERENCES grit.conversations(id) ON DELETE CASCADE,
    turn_seq        BIGINT NOT NULL,
    address         TEXT NOT NULL,
    delivered       BOOLEAN NOT NULL DEFAULT false,
    awaited         BIGSERIAL NOT NULL
);

CREATE INDEX IF NOT EXISTS idx_deliveries_pending ON grit.deliveries (awaited) WHERE NOT delivered;
CREATE INDEX IF NOT EXISTS idx_deliveries_turn ON grit.deliveries (conversation_id, turn_seq);

-- Retention: journal: with its delivery, by cascade.
CREATE TABLE IF NOT EXISTS grit.delivery_parts (
    workflow  TEXT NOT NULL REFERENCES grit.deliveries(workflow) ON DELETE CASCADE,
    part      INTEGER NOT NULL,
    posted_as TEXT,
    PRIMARY KEY (workflow, part)
);

-- An edge's registration (ADR 0017): the principal it acts for, the machine it runs on, and
-- the places it hosts (edge_places). A live edge holds the advisory lock (EdgeLock.Class,
-- lock_key) on its desk's connection: "live" is read from pg_locks, never from heartbeat_at,
-- which the desk writes every beat for a person to read. `session` names its current
-- incarnation, so a claim made by an earlier one is known for an orphan. A registration is
-- reused when the same principal hosts the same places on the same machine again (`key`),
-- unless it is live: then the new edge takes another slot.
-- Retention: kept: bounded by the places opened on each machine, and the edges open at once.
CREATE TABLE IF NOT EXISTS grit.edges (
    id           UUID PRIMARY KEY DEFAULT uuidv7(),
    lock_key     INTEGER GENERATED ALWAYS AS IDENTITY UNIQUE,
    key          TEXT NOT NULL,
    principal    TEXT NOT NULL REFERENCES grit.principals(id),
    machine      TEXT NOT NULL,
    pid          BIGINT NOT NULL,
    session      UUID NOT NULL,
    protocol     INTEGER NOT NULL,
    started_at   TIMESTAMPTZ NOT NULL,
    heartbeat_at TIMESTAMPTZ NOT NULL
);

CREATE INDEX IF NOT EXISTS idx_edges_key ON grit.edges (key);

-- The places an edge registered, and what it offers in each: the tool set it runs there,
-- and the instruction files it read there (fragment ids, farthest first; their texts in
-- prompt_fragments).
-- Retention: kept: with its edge.
CREATE TABLE IF NOT EXISTS grit.edge_places (
    edge_id   UUID NOT NULL REFERENCES grit.edges(id) ON DELETE CASCADE,
    place_id  UUID NOT NULL REFERENCES grit.places(id),
    tools     TEXT REFERENCES grit.tool_sets(id),
    fragments JSONB NOT NULL DEFAULT '[]'::jsonb,
    PRIMARY KEY (edge_id, place_id)
);

-- One hosted tool call (ADR 0017): written by the engine's turn, claimed and answered by an
-- edge that hosts `workspace`. Keyed by the call's slot (CallSlot.key), so a rerun of the
-- turn finds its own request. A claimed request is never run again by another claim: the
-- claim is the attempt marker; an orphan is settled by its `retry`. Each phase is one
-- conditional UPDATE: open -> claimed -> answered, or open|claimed -> expired, and nothing
-- after expired or answered.
-- Retention: journal: with its period's raw entries (Target.Raw): PeriodStore.purge deletes
-- the requests of the period's turns with their entries.
CREATE TABLE IF NOT EXISTS grit.tool_requests (
    key             TEXT PRIMARY KEY,
    protocol        INTEGER NOT NULL,
    workflow_id     TEXT NOT NULL,
    conversation_id UUID NOT NULL REFERENCES grit.conversations(id) ON DELETE CASCADE,
    turn_seq        BIGINT NOT NULL,
    workspace_id    UUID NOT NULL REFERENCES grit.places(id),
    principal       TEXT NOT NULL REFERENCES grit.principals(id),
    tool            TEXT NOT NULL,
    permit          TEXT NOT NULL CHECK (permit IN ('free', 'approved')),
    retry           TEXT NOT NULL CHECK (retry IN ('rerun', 'interrupt')),
    arguments       JSONB NOT NULL,
    repairs         JSONB NOT NULL,
    state           TEXT NOT NULL CHECK (state IN ('open', 'claimed', 'answered', 'expired')),
    claimed_by      UUID REFERENCES grit.edges(id),
    claim_session   UUID,
    outcome         JSONB,
    created_at      TIMESTAMPTZ NOT NULL DEFAULT clock_timestamp(),
    claimed_at      TIMESTAMPTZ,
    answered_at     TIMESTAMPTZ,
    CHECK ((state = 'answered') = (outcome IS NOT NULL)),
    CHECK ((claimed_by IS NULL) = (claim_session IS NULL))
);

CREATE INDEX IF NOT EXISTS idx_tool_requests_waiting ON grit.tool_requests (workspace_id)
    WHERE state IN ('open', 'claimed');
CREATE INDEX IF NOT EXISTS idx_tool_requests_turn ON grit.tool_requests (conversation_id, turn_seq);

-- Settings of model pairs learned while grit runs, each approved by a person: the
-- database's layer over the checked-in seed catalog (grit/models/resources/catalog.json).
-- Append-only; `settings` is a partial profile in the seed's form (CatalogJson.writeProfile).
-- Retention: kept: approved by a person, the catalog's runtime layer.
CREATE TABLE IF NOT EXISTS grit.model_settings (
    ordinal     BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    settings    JSONB NOT NULL,
    approved_by TEXT NOT NULL,
    approved_at TIMESTAMPTZ NOT NULL
);

-- A conversation's periods (ADR 0011): a run of its turns, from `first_turn` to the turn
-- before the next period's first. An entry's period follows from its turn_seq. A closed
-- period has its closing entry, written in the same transaction as the seal, its reason,
-- its last turn and its place in close order: all four, or none (PeriodState.Closed).
-- When it closes, and when it is asked whether anyone is waiting, are never stored or computed
-- here: Deadline is their one definition.
-- Retention: ledger: with its closing (Target.Superseded, Target.Quiet).
CREATE TABLE IF NOT EXISTS grit.periods (
    conversation_id UUID NOT NULL REFERENCES grit.conversations(id) ON DELETE CASCADE,
    seq             BIGINT NOT NULL CHECK (seq >= 1),
    first_turn      BIGINT NOT NULL,
    opened_at       TIMESTAMPTZ NOT NULL,
    last_turn       BIGINT,
    closed_at       TIMESTAMPTZ,
    reason          TEXT CHECK (reason IN ('resolved', 'lapsed')),
    -- A resolved close's probability that nobody was waiting (CloseReason.Resolved); none for a
    -- lapse.
    confidence      DOUBLE PRECISION CHECK (confidence BETWEEN 0 AND 1),
    closing_id      TEXT REFERENCES grit.entries(id),
    -- Close order across conversations, what plugin cursors count. Taken under a lock held
    -- to commit (SqlPeriodStore.seal), so no seal commits before one with a lower number.
    close_ordinal   BIGINT UNIQUE,
    purged_at       TIMESTAMPTZ,
    PRIMARY KEY (conversation_id, seq),
    CHECK ((closed_at IS NULL) = (reason IS NULL)
       AND (closed_at IS NULL) = (closing_id IS NULL)
       AND (closed_at IS NULL) = (last_turn IS NULL)
       AND (closed_at IS NULL) = (close_ordinal IS NULL)),
    CHECK (purged_at IS NULL OR closed_at IS NOT NULL),
    CHECK ((confidence IS NOT NULL) = (reason IS NOT DISTINCT FROM 'resolved'))
);

-- Close order, taken by each seal (SqlPeriodStore.seal): never the greatest kept, so an
-- ordinal whose period was deleted is never taken again.
CREATE SEQUENCE IF NOT EXISTS grit.close_ordinals;

-- At most one open period per conversation.
CREATE UNIQUE INDEX IF NOT EXISTS idx_periods_open ON grit.periods (conversation_id)
    WHERE closed_at IS NULL;

-- What the classifier made of each period once it went quiet (grit.core.period.Verdict):
-- whether anyone is waiting on anything, the tuning data for when a period closes, deleted
-- with the period's raw entries (ADR 0011). A verdict is about the period as it stood with
-- `last_turn` its newest turn; weighed, with a probability for each answer (nobody, the
-- person, something else) and the model, or unanswered, with why.
-- Retention: journal: with its period's raw entries (Target.Raw).
CREATE TABLE IF NOT EXISTS grit.verdicts (
    ordinal          BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    conversation_id  UUID NOT NULL,
    seq              BIGINT NOT NULL,
    last_turn        BIGINT NOT NULL,
    at               TIMESTAMPTZ NOT NULL,
    nobody           DOUBLE PRECISION CHECK (nobody BETWEEN 0 AND 1),
    waiting_person   DOUBLE PRECISION CHECK (waiting_person BETWEEN 0 AND 1),
    waiting_other    DOUBLE PRECISION CHECK (waiting_other BETWEEN 0 AND 1),
    model            TEXT,
    unanswered       TEXT,
    FOREIGN KEY (conversation_id, seq) REFERENCES grit.periods(conversation_id, seq)
        ON DELETE CASCADE,
    CHECK ((unanswered IS NULL) = (nobody IS NOT NULL)
       AND (nobody IS NULL) = (waiting_person IS NULL)
       AND (nobody IS NULL) = (waiting_other IS NULL)
       AND (nobody IS NULL) = (model IS NULL))
);

CREATE INDEX IF NOT EXISTS idx_verdicts_period ON grit.verdicts (conversation_id, seq, at);

-- The lifecycle's settings in force (LifecycleSettings): one row, or none for the defaults.
-- Seeded on first start, then changed by /set or by hand. Their rules are checked where
-- they are read (LifecycleSettings.of), not here, so they have one home.
-- Retention: kept: one row, changed by people.
CREATE TABLE IF NOT EXISTS grit.lifecycle_settings (
    one       BOOLEAN PRIMARY KEY DEFAULT true CHECK (one),
    idle        INTERVAL NOT NULL,
    retention   INTERVAL NOT NULL,
    ledger      INTERVAL NOT NULL,
    balance     INTEGER NOT NULL,
    settle      INTERVAL NOT NULL,
    resolve_at DOUBLE PRECISION NOT NULL,
    asks        INTEGER NOT NULL,
    -- Which places' open periods a window draws on (Place.written of each prefix; '{}' is
    -- none, '{everywhere}' the default), and how its own conversation's hits are weighted.
    scope       TEXT[] NOT NULL,
    weight      DOUBLE PRECISION NOT NULL
);

-- How grit talks to the person (Voice): one of grit's named voices, by key, or the person's
-- own words. One row, or none for the default (plain). Read by a turn's offer step into the
-- person layer of its system prompt; a key this build does not know reads as the default.
-- Retention: kept: one row, changed by people.
CREATE TABLE IF NOT EXISTS grit.voice (
    one  BOOLEAN PRIMARY KEY DEFAULT true CHECK (one),
    kind TEXT NOT NULL CHECK (kind IN ('named', 'own')),
    text TEXT NOT NULL
);

-- Each plugin's documents, under keys it chooses (grit.core.plugin): a plugin builds only from
-- closed periods, and reaches only its own rows. Each is of the generation of the plugin's
-- cursor it was written under, and is read only while that is the cursor's; `source` is the
-- close ordinal of the period it was posted from, and it is deleted with that closing.
-- Retention: cache: with the closing each was posted from, or the generation it was written in
-- (Target.Superseded, Target.Quiet, Target.Restarted, Target.Disabled).
CREATE TABLE IF NOT EXISTS grit.plugin_docs (
    plugin     TEXT NOT NULL,
    generation BIGINT NOT NULL,
    key        TEXT NOT NULL,
    doc        JSONB NOT NULL,
    source     BIGINT NOT NULL,
    PRIMARY KEY (plugin, generation, key)
);

CREATE INDEX IF NOT EXISTS idx_plugin_docs_source ON grit.plugin_docs (source);

-- How far each plugin has posted, in close order (grit.periods.close_ordinal), and at
-- which version: another version starts again at 0, in the next generation.
-- Retention: cache: with a plugin no longer enabled (Target.Disabled).
CREATE TABLE IF NOT EXISTS grit.plugin_cursors (
    plugin     TEXT PRIMARY KEY,
    version    INTEGER NOT NULL,
    ordinal    BIGINT NOT NULL,
    generation BIGINT NOT NULL
);

-- What grit has decided to delete (grit.core.store.Tombstones, ADR 0014): nothing deletes a row
-- or a workflow history no tombstone names. One row per target, `kind` and `target` as
-- grit.core.retention.Target stores them. Pending until the collector deletes the target
-- (`collected_at`) or finds it alive (`collected_at` and `spared`); `deferred_at` is when it
-- last could not be collected, which puts it behind the ones due since.
-- Retention: ledger: forgotten once collected or spared longer ago than the ledger window.
CREATE TABLE IF NOT EXISTS grit.tombstones (
    kind         TEXT NOT NULL,
    target       TEXT NOT NULL,
    written_at   TIMESTAMPTZ NOT NULL,
    deferred_at  TIMESTAMPTZ,
    collected_at TIMESTAMPTZ,
    spared       BOOLEAN NOT NULL DEFAULT false,
    PRIMARY KEY (kind, target),
    CHECK (NOT spared OR collected_at IS NOT NULL)
);

CREATE INDEX IF NOT EXISTS idx_tombstones_due ON grit.tombstones
    (kind, (coalesce(deferred_at, written_at)), target COLLATE "C") WHERE collected_at IS NULL;
