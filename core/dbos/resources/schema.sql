-- grit app schema, applied idempotently at startup.
-- DBOS system tables (dbos.*) are managed separately by DBOS's own migrations.
-- No migrations yet: until a database holds real data, change this file and run
-- scripts/reset-db. IF NOT EXISTS never reshapes a
-- table, so a changed table needs the reset.

CREATE SCHEMA IF NOT EXISTS grit;

-- BM25 ranking (ADR 0005). The image preloads the library; this makes it usable here.
CREATE EXTENSION IF NOT EXISTS pg_textsearch;

-- The searchable text of a stored payload (PayloadJson): a message's user text, its
-- assistant text blocks, a tool result's content; a heard message's text; a post's text; a summary's text; a closing entry's
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
    WHEN 'heard' THEN payload ->> 'text'
    WHEN 'posted' THEN payload ->> 'text'
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

-- Every engine start on this database (grit.dbos.engine.Build.Started): which process, under
-- which epoch, running which grit build. Written with grit.engines' row, in one statement, so
-- a start that did not take the lock leaves none. grit.engines says who holds the lock now;
-- this is the history. `commit` and `dirty` are NULL together when the build is not known
-- (Build.Unknown).
-- Retention: kept: one small row per start.
CREATE TABLE IF NOT EXISTS grit.engine_starts (
    id         UUID PRIMARY KEY DEFAULT uuidv7(),
    started_at TIMESTAMPTZ NOT NULL,
    machine    TEXT NOT NULL,
    pid        BIGINT NOT NULL,
    epoch      TEXT NOT NULL,
    commit     TEXT,
    dirty      BOOLEAN,
    CHECK ((commit IS NULL) = (dirty IS NULL))
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
-- created it, kept whoever finds it later. `next_turn` and `next_seq` are past every turn and
-- entry ever written in it, purged ones included: each entry's insert raises them, under the
-- row's lock (EntryStore.lockNext), so no position is taken twice.
-- Retention: ledger: deleted whole once quiet past the ledger window (Target.Quiet).
CREATE TABLE IF NOT EXISTS grit.conversations (
    id         UUID PRIMARY KEY DEFAULT uuidv7(),
    origin     JSONB NOT NULL UNIQUE,
    place_id   UUID NOT NULL REFERENCES grit.places(id),
    created_by TEXT NOT NULL REFERENCES grit.principals(id),
    created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    next_turn  BIGINT NOT NULL DEFAULT 0,
    next_seq   BIGINT NOT NULL DEFAULT 0
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

-- A conversation's entries, and its turns': every query that names a conversation, and a
-- period's purge, which deletes by its turns' range.
CREATE INDEX IF NOT EXISTS idx_entries_turn ON grit.entries (conversation_id, turn_seq);
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

-- A message an edge marks as being answered while the turn answering it runs
-- (Acknowledgements): the turn, the edge's own address of the message, and how far the mark
-- has got: wanted, shown, or taken down or never to be shown (cleared), with when. What lets an
-- edge that restarts take down every mark it put up, and put up none after its turn has ended.
-- Retention: journal: with its turn's period (Target.Raw), by PeriodStore.purge.
CREATE TABLE IF NOT EXISTS grit.acknowledgements (
    workflow        TEXT PRIMARY KEY,
    conversation_id UUID NOT NULL REFERENCES grit.conversations(id) ON DELETE CASCADE,
    turn_seq        BIGINT NOT NULL,
    address         TEXT NOT NULL,
    stage           TEXT NOT NULL CHECK (stage IN ('wanted', 'shown', 'cleared')),
    wanted_at       TIMESTAMPTZ NOT NULL,
    shown_at        TIMESTAMPTZ,
    cleared_at      TIMESTAMPTZ,
    wanted          BIGSERIAL NOT NULL,
    CHECK ((stage = 'cleared') = (cleared_at IS NOT NULL)
       AND (stage <> 'shown' OR shown_at IS NOT NULL))
);

CREATE INDEX IF NOT EXISTS idx_acknowledgements_standing
    ON grit.acknowledgements (wanted) WHERE stage <> 'cleared';
CREATE INDEX IF NOT EXISTS idx_acknowledgements_turn
    ON grit.acknowledgements (conversation_id, turn_seq);

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
-- database's layer over the checked-in seed catalog (extensions/models/resources/catalog.json).
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
    reason          TEXT CONSTRAINT periods_reason_check
                    CHECK (reason IN ('resolved', 'lapsed', 'unearned', 'ran')),
    -- A resolved close's probability that nobody was waiting (CloseReason.Resolved); none for a
    -- lapse.
    confidence      DOUBLE PRECISION CHECK (confidence BETWEEN 0 AND 1),
    -- UNIQUE: a period has one closing, and the index serves the check each entry's delete
    -- makes against it.
    closing_id      TEXT UNIQUE REFERENCES grit.entries(id),
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

-- What triage made of each heard message (grit.core.triage.Tags): its question set's answers,
-- each under its question's name in the order asked (grit.core.classify.AnswersJson), the
-- model and what the call consumed; or unanswered, with why. A message triaged before named
-- answers holds v1's: kind, waiting, durable and helps.
-- Retention: journal: deleted with its entry, so with its period's raw entries (Target.Raw).
CREATE TABLE IF NOT EXISTS grit.triage (
    entry_id        TEXT PRIMARY KEY REFERENCES grit.entries(id) ON DELETE CASCADE,
    at              TIMESTAMPTZ NOT NULL,
    answers         JSONB CHECK (answers IS NULL OR jsonb_typeof(answers) = 'array'),
    model           TEXT,
    input_tokens    BIGINT CHECK (input_tokens >= 0),
    output_tokens   BIGINT CHECK (output_tokens >= 0),
    cached_tokens   BIGINT CHECK (cached_tokens >= 0),
    cost_usd        NUMERIC,
    unanswered      TEXT,
    CHECK ((unanswered IS NULL) = (answers IS NOT NULL)
       AND (answers IS NULL) = (model IS NULL)
       AND (answers IS NULL) = (input_tokens IS NULL)
       AND (answers IS NULL) = (output_tokens IS NULL)
       AND (answers IS NULL) = (cached_tokens IS NULL)
       AND (answers IS NOT NULL OR cost_usd IS NULL))
);

-- What each declared shadow variant made of a heard message already triaged
-- (grit.core.triage.Shadowed), one row per message and variant: recorded, never acted on. The
-- request's digest, and either every answer (keys and probabilities, grit.core.triage.ShadowedJson),
-- what the call consumed and the models requested and answering, or which kind of failure;
-- and how long the call took. Not grit.shadows: speech's shadow mode is another thing.
-- Retention: journal: deleted with its entry, so with its period's raw entries (Target.Raw).
CREATE TABLE IF NOT EXISTS grit.triage_shadows (
    entry_id        TEXT NOT NULL REFERENCES grit.entries(id) ON DELETE CASCADE,
    name            TEXT NOT NULL CHECK (name ~ '^[a-z0-9-]+$'),
    at              TIMESTAMPTZ NOT NULL,
    request         TEXT NOT NULL,
    latency_ms      BIGINT NOT NULL CHECK (latency_ms >= 0),
    failure         TEXT CHECK (failure IN ('unavailable', 'unreadable')),
    answers         JSONB,
    requested       TEXT,
    answered        TEXT,
    input_tokens    BIGINT CHECK (input_tokens >= 0),
    output_tokens   BIGINT CHECK (output_tokens >= 0),
    cached_tokens   BIGINT CHECK (cached_tokens >= 0),
    cost_usd        NUMERIC,
    PRIMARY KEY (entry_id, name),
    CHECK ((failure IS NULL) = (answers IS NOT NULL)
       AND (answers IS NULL) = (requested IS NULL)
       AND (answers IS NULL) = (answered IS NULL)
       AND (answers IS NULL) = (input_tokens IS NULL)
       AND (answers IS NULL) = (output_tokens IS NULL)
       AND (answers IS NULL) = (cached_tokens IS NULL)
       AND (answers IS NOT NULL OR cost_usd IS NULL))
);

CREATE INDEX IF NOT EXISTS idx_triage_shadows_name_at ON grit.triage_shadows (name, at);

-- Where a reply to each heard message could go, and whom it names (grit.core.speech.Reach): its
-- edge's own address, NULL when it is never answered (a past message), and the principals it
-- names besides the assistant. Written by Inbox.hear; the first kept stands.
-- Retention: journal: deleted with its entry, so with its period's raw entries (Target.Raw).
CREATE TABLE IF NOT EXISTS grit.heard (
    entry_id        TEXT PRIMARY KEY REFERENCES grit.entries(id) ON DELETE CASCADE,
    conversation_id UUID NOT NULL,
    turn_seq        BIGINT NOT NULL,
    reply_to        TEXT,
    asked           JSONB NOT NULL DEFAULT '[]'::jsonb,
    UNIQUE (conversation_id, turn_seq)
);

-- The hosted call whose post a conversation begins with (Inbox.posted): a thread under a
-- post of grit's, its first entry that post. `request` is the call's slot key (CallSlot.key),
-- which names the turn that asked for it.
-- Retention: journal: deleted with its entry, so with its period's raw entries (Target.Raw).
CREATE TABLE IF NOT EXISTS grit.posted (
    entry_id        TEXT PRIMARY KEY REFERENCES grit.entries(id) ON DELETE CASCADE,
    conversation_id UUID NOT NULL UNIQUE REFERENCES grit.conversations(id) ON DELETE CASCADE,
    request         TEXT NOT NULL
);

-- Where each stitchable conversation's first message was placed among its room's exchanges
-- (grit.core.stitch.Placed, ADR 0023): it follows `root`'s exchange, begins something new, or
-- was not read; `placed` is its StitchJson form, with what the classifier was shown, each
-- exchange offered and why, and the tuning in force. `root` is set for a follows alone.
-- Retention: journal: deleted with its entry, so with its period's raw entries (Target.Raw),
-- or with the conversation it follows.
CREATE TABLE IF NOT EXISTS grit.stitches (
    entry_id        TEXT PRIMARY KEY REFERENCES grit.entries(id) ON DELETE CASCADE,
    conversation_id UUID NOT NULL REFERENCES grit.conversations(id) ON DELETE CASCADE,
    kind            TEXT NOT NULL CHECK (kind IN ('follows', 'begins', 'unread')),
    root            UUID REFERENCES grit.conversations(id) ON DELETE CASCADE,
    at              TIMESTAMPTZ NOT NULL,
    placed          JSONB NOT NULL,
    CHECK ((kind = 'follows') = (root IS NOT NULL))
);

CREATE INDEX IF NOT EXISTS idx_stitches_conversation ON grit.stitches (conversation_id);
CREATE INDEX IF NOT EXISTS idx_stitches_root ON grit.stitches (root);

-- grit's decision on each heard message that reached it (grit.core.speech.Decision): held,
-- with why (`silence`), or drafting in the heard message's own turn (`workflow`), `answering`
-- when the message was put to grit and its turn runs as an addressed one does (counted in no
-- rate and in no speech spend); and, once the turn settles, what became of it (`outcome`), the
-- judge's scores and model (an unprompted draft's; a named draft or a reply is not judged, its
-- `grounded` and `worth` NULL), an excerpt of the draft or reply, and a posted reply's
-- position. Triage's answers are in grit.triage. Forms in
-- SpeechJson. No foreign key: it outlives the heard entry's purge, so the rates and the
-- day's speech spend count every decision, and each post can be stated with its approval.
-- Retention: ledger: with its period's usage (SpeechStore.forget, beside UsageLedger.forget),
-- or its conversation.
CREATE TABLE IF NOT EXISTS grit.speech (
    workflow        TEXT PRIMARY KEY,
    conversation_id UUID NOT NULL REFERENCES grit.conversations(id) ON DELETE CASCADE,
    turn_seq        BIGINT NOT NULL,
    room            TEXT NOT NULL,
    decided_at      TIMESTAMPTZ NOT NULL,
    drafting        BOOLEAN NOT NULL,
    silence         JSONB,
    outcome         JSONB,
    outcome_kind    TEXT,
    grounded        DOUBLE PRECISION CHECK (grounded BETWEEN 0 AND 1),
    worth           DOUBLE PRECISION CHECK (worth BETWEEN 0 AND 1),
    post_at         DOUBLE PRECISION CHECK (post_at BETWEEN 0 AND 1),
    judge_model     TEXT,
    excerpt         TEXT,
    posted_seq      BIGINT,
    judged_at       TIMESTAMPTZ,
    answering       BOOLEAN NOT NULL DEFAULT false,
    CHECK (drafting = (silence IS NULL)
       AND (drafting OR NOT answering)
       AND (outcome IS NULL) = (outcome_kind IS NULL)
       AND (outcome IS NULL OR drafting)
       AND (outcome IS NULL) = (judged_at IS NULL)
       AND (posted_seq IS NULL OR outcome_kind = 'posted'))
);

CREATE INDEX IF NOT EXISTS idx_speech_decided ON grit.speech (decided_at);
-- A turn's decisions: what SpeechStore.forget deletes, and a conversation's removal cascades.
CREATE INDEX IF NOT EXISTS idx_speech_turn ON grit.speech (conversation_id, turn_seq);

-- Each heard message a review considered against a shadow (grit.core.review.Considered): the
-- reason the two gates gave (NULL when either could not be read), when it was considered,
-- and when it was picked, if it was; a picked message's prompt, where its edge posted it (its
-- own address form) and when; and the verdict its rater reacted with, the latest standing.
-- Every message considered is kept, picked or not, so what was picked can be weighed against
-- what was not. No text: neither the message's nor a draft's. No foreign key to the entry: a
-- label outlives the raw text it was given on (ADR 0024).
-- Retention: ledger: with its conversation (Target.Quiet), by cascade.
CREATE TABLE IF NOT EXISTS grit.reviews (
    entry_id        TEXT PRIMARY KEY,
    conversation_id UUID NOT NULL REFERENCES grit.conversations(id) ON DELETE CASCADE,
    shadow          TEXT NOT NULL CHECK (shadow ~ '^[a-z0-9-]+$'),
    reason          TEXT CHECK (reason IN ('shadow-only', 'live-only', 'both', 'neither')),
    considered_at   TIMESTAMPTZ NOT NULL,
    picked_at       TIMESTAMPTZ,
    address         TEXT UNIQUE,
    posted_at       TIMESTAMPTZ,
    verdict         TEXT CHECK (verdict IN ('welcome', 'interruption', 'cut-in')),
    rater           TEXT,
    labelled_at     TIMESTAMPTZ,
    CHECK ((picked_at IS NULL OR reason IS NOT NULL)
       AND (address IS NULL) = (posted_at IS NULL)
       AND (address IS NULL OR picked_at IS NOT NULL)
       AND (verdict IS NULL) = (rater IS NULL)
       AND (verdict IS NULL) = (labelled_at IS NULL)
       AND (verdict IS NULL OR address IS NOT NULL))
);

CREATE INDEX IF NOT EXISTS idx_reviews_considered ON grit.reviews (considered_at);
-- What a conversation's removal cascades to.
CREATE INDEX IF NOT EXISTS idx_reviews_conversation ON grit.reviews (conversation_id);

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

-- Each plugin's documents (grit.core.document, ADR 0028), one row per version: a version is
-- never edited; the next written under its key supersedes it (`superseded_at`). A version
-- with `body` NULL withdraws its key: never shelved, listed, searched or counted toward the
-- bound. `version` is numbered across every plugin, never reused, and alone names a version
-- (a window's record, a tombstone). `place` is Place's segments (not a grit.places row, which
-- goes with its last conversation). `body` is what a window shows and what DocumentSearch
-- ranks; `data` is the plugin's own, never shown or searched. `placed` and `last_placed`
-- count the windows that held it (DocumentStore.placed); `last_placed` starts at `written_at`.
-- Writes to one plugin's documents are serialized by its posting; a concurrent write to one
-- key fails on idx_documents_current.
-- Retention: ledger: a version no longer current, its plugin's declared retention after
-- (Target.Document); every version of a plugin no longer enabled (Target.Disabled).
CREATE TABLE IF NOT EXISTS grit.documents (
    version       BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    plugin        TEXT NOT NULL,
    key           TEXT NOT NULL,
    place         TEXT[] NOT NULL
                  CHECK (array_position(place, '') IS NULL AND array_position(place, NULL) IS NULL),
    body          TEXT,
    data          JSONB NOT NULL DEFAULT '{}'::jsonb,
    written_at    TIMESTAMPTZ NOT NULL,
    superseded_at TIMESTAMPTZ,
    placed        BIGINT NOT NULL DEFAULT 0,
    last_placed   TIMESTAMPTZ NOT NULL
);

CREATE UNIQUE INDEX IF NOT EXISTS idx_documents_current ON grit.documents (plugin, key)
    WHERE superseded_at IS NULL;
CREATE INDEX IF NOT EXISTS idx_documents_plugin ON grit.documents (plugin, written_at);
-- Its own statistics, apart from the entries' (ADR 0005): scores are scaled by each plugin's
-- weight before they rank beside entries. Queries name it:
-- to_bm25query(q, 'grit.idx_documents_bm25').
CREATE INDEX IF NOT EXISTS idx_documents_bm25 ON grit.documents
    USING bm25 (body) WITH (text_config = 'english');

-- The terms each plugin declared for its documents (DocumentTerms), written at every engine
-- start: `enabled` for the plugins that start enabled, false for one declared before and not
-- now; the newest start wins. Not versioned: readers use the terms in force.
-- Retention: cache: with a plugin no longer enabled (Target.Disabled).
CREATE TABLE IF NOT EXISTS grit.document_terms (
    plugin    TEXT PRIMARY KEY,
    label     TEXT NOT NULL,
    weight    DOUBLE PRECISION NOT NULL CHECK (weight > 0),
    retention INTERVAL NOT NULL,
    bound     INTEGER NOT NULL CHECK (bound >= 1),
    enabled   BOOLEAN NOT NULL
);

-- A job's schedule (grit.core.job, ADR 0029): its job by name, its slot rule (SlotRuleJson), its
-- parameters as its job wrote them, the principal it runs for, and where its runs report besides
-- their own conversations (ReportJson). `source` 'declared' (a deployment's or a plugin's, its id
-- `declared:…`, reconciled at every start) or 'asked' (written by a plugin's tool from the call
-- `asked_in`, a CallSlot.key; never reconciled). `next_at` is its next slot's nominal instant,
-- NULL when it has none left; `started_at` the nominal instant of the newest slot started, and
-- `running` the job version that slot's run was started at, NULL once it replied. `ended` says how
-- it ended (Ending), with `ended_at`; an ended schedule has no slot left.
-- Retention: journal: an ended schedule, after the raw window (Target.Schedule).
CREATE TABLE IF NOT EXISTS grit.schedules (
    id          TEXT PRIMARY KEY,
    source      TEXT NOT NULL CHECK (source IN ('declared', 'asked')),
    job         TEXT NOT NULL,
    rule        JSONB NOT NULL,
    params      JSONB NOT NULL,
    principal   TEXT NOT NULL REFERENCES grit.principals(id),
    report      JSONB NOT NULL,
    asked_in    TEXT UNIQUE,
    created_at  TIMESTAMPTZ NOT NULL,
    next_at     TIMESTAMPTZ,
    started_at  TIMESTAMPTZ,
    running     INTEGER,
    ended       TEXT CHECK (ended IN ('ran', 'missed', 'failed', 'cancelled', 'undeclared')),
    ended_at    TIMESTAMPTZ,
    CHECK ((source = 'asked') = (asked_in IS NOT NULL)),
    CHECK ((ended IS NULL) = (ended_at IS NULL)),
    CHECK (ended IS NULL OR next_at IS NULL)
);

-- What the clock edge reads each pass: a slot due, or a run not yet replied.
CREATE INDEX IF NOT EXISTS idx_schedules_waiting ON grit.schedules (next_at)
    WHERE ended IS NULL;
CREATE INDEX IF NOT EXISTS idx_schedules_running ON grit.schedules (id)
    WHERE running IS NOT NULL;
-- A person's pending asked schedules: the desk's cap, `reminders`, `cancel_reminder`.
CREATE INDEX IF NOT EXISTS idx_schedules_asker ON grit.schedules (principal, job)
    WHERE source = 'asked' AND ended IS NULL;

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
-- What Tombstones.forget deletes each sweep: the rows collected or spared before a time.
CREATE INDEX IF NOT EXISTS idx_tombstones_collected ON grit.tombstones (collected_at)
    WHERE collected_at IS NOT NULL;
