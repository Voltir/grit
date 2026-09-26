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

-- One row per origin; `origin` is the Origin ADT as JSON, and jsonb equality
-- ignores key order, so the unique index is on the value, not its spelling.
CREATE TABLE IF NOT EXISTS grit.conversations (
    id         UUID PRIMARY KEY DEFAULT uuidv7(),
    origin     JSONB NOT NULL UNIQUE,
    created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

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

-- One row per model response: what it cost. Keyed by the entry that holds the response,
-- so the turn's append writes both in one transaction and a replay cannot count twice.
CREATE TABLE IF NOT EXISTS grit.usage_ledger (
    entry_id            TEXT PRIMARY KEY REFERENCES grit.entries(id) ON DELETE CASCADE,
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

-- Every distinct profile a turn ran under (grit.core.model.TurnProfile): the model, budget,
-- upstream and settings each role's calls were made under. Keyed by its content hash, so
-- it is written once however many turns share it, and never changed.
CREATE TABLE IF NOT EXISTS grit.model_profiles (
    id         TEXT PRIMARY KEY,
    profile    JSONB NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

-- Which profile each turn ran under, set once when the turn starts.
CREATE TABLE IF NOT EXISTS grit.turn_model_profiles (
    workflow_id      TEXT PRIMARY KEY,
    model_profile_id TEXT NOT NULL REFERENCES grit.model_profiles(id),
    created_at       TIMESTAMPTZ NOT NULL DEFAULT now()
);

-- Facts about model pairs learned while grit runs, each approved by a person: the
-- database's layer over the checked-in seed catalog (grit/models/resources/catalog.json).
-- Append-only; `facts` is a partial profile in the seed's form (CatalogJson.writeProfile).
CREATE TABLE IF NOT EXISTS grit.model_facts (
    ordinal     BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    facts       JSONB NOT NULL,
    approved_by TEXT NOT NULL,
    approved_at TIMESTAMPTZ NOT NULL
);

-- A conversation's periods (ADR 0011): a run of its turns, from `first_turn` to the turn
-- before the next period's first. An entry's period follows from its turn_seq. A closed
-- period has its closing entry, written in the same transaction as the seal, its reason,
-- its last turn and its place in close order: all four, or none (PeriodState.Closed).
-- When it closes, and when it is asked whether anyone is waiting, are never stored or computed
-- here: Deadline is their one definition.
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

-- At most one open period per conversation.
CREATE UNIQUE INDEX IF NOT EXISTS idx_periods_open ON grit.periods (conversation_id)
    WHERE closed_at IS NULL;

-- What the classifier made of each period once it went quiet (grit.core.period.Verdict):
-- whether anyone is waiting on anything, the tuning data for when a period closes, kept
-- when the period's raw entries are purged. A verdict is about the period as it stood with
-- `last_turn` its newest turn; weighed, with a probability for each answer (nobody, the
-- person, something else) and the model, or unanswered, with why.
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
CREATE TABLE IF NOT EXISTS grit.lifecycle_settings (
    one       BOOLEAN PRIMARY KEY DEFAULT true CHECK (one),
    idle        INTERVAL NOT NULL,
    retention   INTERVAL NOT NULL,
    balance     INTEGER NOT NULL,
    settle      INTERVAL NOT NULL,
    resolve_at DOUBLE PRECISION NOT NULL,
    asks        INTEGER NOT NULL
);

-- Each plugin's documents, under keys it chooses (grit.core.plugin.PluginDocs): a plugin
-- builds only from closed periods, and reaches only its own rows. Removing a plugin is
-- DELETE ... WHERE plugin = its name.
CREATE TABLE IF NOT EXISTS grit.plugin_docs (
    plugin TEXT NOT NULL,
    key    TEXT NOT NULL,
    doc    JSONB NOT NULL,
    PRIMARY KEY (plugin, key)
);

-- How far each plugin has posted, in close order (grit.periods.close_ordinal), and at
-- which version: another version starts again at 0, its documents cleared.
CREATE TABLE IF NOT EXISTS grit.plugin_cursors (
    plugin  TEXT PRIMARY KEY,
    version INTEGER NOT NULL,
    ordinal BIGINT NOT NULL
);
