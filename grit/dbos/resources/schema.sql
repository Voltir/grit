-- grit app schema, applied idempotently at startup.
-- DBOS system tables (dbos.*) are managed separately by DBOS's own migrations.
-- No migrations yet: until a database holds real data, change this file and drop the local
-- schemas (.local/backlog/schema-migrations.md). IF NOT EXISTS never reshapes a table.

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
    -- (SqlInbox.lockNext); this makes a writer that skipped the lock fail, not interleave.
    CONSTRAINT entries_conversation_seq UNIQUE (conversation_id, seq)
);

CREATE INDEX IF NOT EXISTS idx_entries_parent       ON grit.entries(parent_id);

CREATE TABLE IF NOT EXISTS grit.usage_ledger (
    id            BIGSERIAL PRIMARY KEY,
    workflow_id   TEXT NOT NULL,
    model         TEXT NOT NULL,
    input_tokens  INTEGER NOT NULL,
    output_tokens INTEGER NOT NULL,
    created_at    TIMESTAMPTZ NOT NULL DEFAULT now()
);