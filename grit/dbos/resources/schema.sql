-- grit app schema, applied idempotently at startup.
-- DBOS system tables (dbos.*) are managed separately by DBOS's own migrations.
-- No migrations yet: until a database holds real data, change this file and run
-- scripts/reset-db (.local/backlog/schema-migrations.md). IF NOT EXISTS never reshapes a
-- table, so a changed table needs the reset.

CREATE SCHEMA IF NOT EXISTS grit;

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
    -- One entry per position. Writers allocate seq under the conversation's row lock
    -- (EntryStore.lockNext); this makes a writer that skipped the lock fail, not interleave.
    CONSTRAINT entries_conversation_seq UNIQUE (conversation_id, seq)
);

CREATE INDEX IF NOT EXISTS idx_entries_parent       ON grit.entries(parent_id);

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
    created_at          TIMESTAMPTZ NOT NULL DEFAULT now()
);