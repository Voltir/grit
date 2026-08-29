-- grit app schema, applied idempotently at startup.
-- DBOS system tables (dbos.*) are managed separately by DBOS's own migrations.

CREATE SCHEMA IF NOT EXISTS grit;

CREATE TABLE IF NOT EXISTS grit.entries (
    id         TEXT PRIMARY KEY,
    parent_id  TEXT,
    seq        BIGINT NOT NULL,
    payload    JSONB NOT NULL DEFAULT '{}'::jsonb,
    created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_entries_seq    ON grit.entries(seq);
CREATE INDEX IF NOT EXISTS idx_entries_parent ON grit.entries(parent_id);

CREATE TABLE IF NOT EXISTS grit.usage_ledger (
    id            BIGSERIAL PRIMARY KEY,
    workflow_id   TEXT NOT NULL,
    model         TEXT NOT NULL,
    input_tokens  INTEGER NOT NULL,
    output_tokens INTEGER NOT NULL,
    created_at    TIMESTAMPTZ NOT NULL DEFAULT now()
);