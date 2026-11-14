-- One-off, for a database written before a machine's places were keyed `machine`
-- (grit.core.place.Namespace.Machine): rewrites every stored place under the old key `worker`
-- to `machine` (`worker:desktop/home/a` becomes `machine:desktop/home/a`), and nothing else.
-- grit reads no place under `worker`, so run it once, by hand, with the deployment's serve
-- stopped, before the build that reads `machine` starts on the database.
--
-- It holds no COMMIT: run as it is, it prints what it would change and rolls back as psql
-- disconnects; with COMMIT after it, it keeps what it printed.
--
--   psql -q -v ON_ERROR_STOP=1 -d <database> -f machine-from-worker.sql            (dry run)
--   psql -q -v ON_ERROR_STOP=1 -d <database> -f machine-from-worker.sql -c COMMIT  (the run)
--
-- A place is kept as its segments, the key first, in grit.places.path (so every room and
-- conversation, by id), and in the `place` of grit.sessions, grit.documents, grit.files and
-- grit.piece_sets; and written (`worker:desktop/home/a`) in a session event's shape under
-- `worktree`, and in a visibility change under `room`.
--
-- A shape is at most 300 bytes (grit.core.session.Event.Shape.Longest) and `machine` is a byte
-- longer than `worker`. A shape the rewrite would take past 300 is kept as the code writes
-- one that does not fit: a commit's loses its subject (Event.Shape.committed leaves it out when
-- no cut fits); a commit's that still does not fit, and any other kind's, which the code
-- refuses whole, is deleted. Its session's counters keep counting it. The printed table says
-- how many of each.
--
-- It raises, changing nothing, when a shape names its worktree in a form other than the
-- compact one grit writes. A second run changes nothing. MachineFromWorkerTests runs this file.
BEGIN;

CREATE TEMP TABLE machine_from_worker (what TEXT PRIMARY KEY, n BIGINT NOT NULL) ON COMMIT DROP;

WITH changed AS (UPDATE grit.places SET path[1] = 'machine' WHERE path[1] = 'worker' RETURNING 1)
INSERT INTO machine_from_worker SELECT 'grit.places rewritten', count(*) FROM changed;

WITH changed AS (UPDATE grit.sessions SET place[1] = 'machine' WHERE place[1] = 'worker' RETURNING 1)
INSERT INTO machine_from_worker SELECT 'grit.sessions rewritten', count(*) FROM changed;

WITH changed AS (UPDATE grit.documents SET place[1] = 'machine' WHERE place[1] = 'worker' RETURNING 1)
INSERT INTO machine_from_worker SELECT 'grit.documents rewritten', count(*) FROM changed;

WITH changed AS (UPDATE grit.files SET place[1] = 'machine' WHERE place[1] = 'worker' RETURNING 1)
INSERT INTO machine_from_worker SELECT 'grit.files rewritten', count(*) FROM changed;

WITH changed AS (UPDATE grit.piece_sets SET place[1] = 'machine' WHERE place[1] = 'worker' RETURNING 1)
INSERT INTO machine_from_worker SELECT 'grit.piece_sets rewritten', count(*) FROM changed;

WITH changed AS (
       UPDATE grit.visibility_changes
          SET change = jsonb_set(change, '{room}',
                                 to_jsonb('machine:' || substr(change ->> 'room', length('worker:') + 1)))
        WHERE change ->> 'room' LIKE 'worker:%'
    RETURNING 1)
INSERT INTO machine_from_worker SELECT 'grit.visibility_changes rewritten', count(*) FROM changed;

-- Each shape naming a worktree under `worker`: its text with the one `"worktree":"worker:…"`
-- pair rewritten (the pair's value kept as written, escapes and all), then what is kept.
CREATE TEMP TABLE machine_shapes ON COMMIT DROP AS
SELECT session, seq, kind, rewritten,
       regexp_replace(rewritten, '^(.*),"subject":"(?:[^"\\]|\\.)*"\}$', '\1}') AS bare
  FROM (SELECT e.session, e.seq, e.kind, e.shape::text AS was,
               replace(e.shape::text,
                       '"worktree":' || (e.shape -> 'worktree')::text,
                       '"worktree":"machine:' ||
                         substr((e.shape -> 'worktree')::text, length('"worker:') + 1)) AS rewritten
          FROM grit.session_events e
         WHERE e.shape ->> 'worktree' LIKE 'worker:%') s
 WHERE rewritten <> was;

DO $$
DECLARE
    named   BIGINT := (SELECT count(*) FROM grit.session_events WHERE shape ->> 'worktree' LIKE 'worker:%');
    matched BIGINT := (SELECT count(*) FROM machine_shapes);
BEGIN
    IF matched <> named THEN
        RAISE EXCEPTION '% shapes name a worktree under worker, but only % in the compact form grit writes',
            named, matched;
    END IF;
END $$;

ALTER TABLE machine_shapes ADD COLUMN kept TEXT, ADD COLUMN dropped BOOLEAN NOT NULL DEFAULT false;

UPDATE machine_shapes SET kept = rewritten WHERE octet_length(rewritten) <= 300;

UPDATE machine_shapes SET kept = bare, dropped = true
 WHERE kept IS NULL AND kind = 'commit' AND bare <> rewritten AND octet_length(bare) <= 300;

WITH changed AS (
       UPDATE grit.session_events e SET shape = m.kept::json
         FROM machine_shapes m
        WHERE e.session = m.session AND e.seq = m.seq AND m.kept IS NOT NULL
    RETURNING 1)
INSERT INTO machine_from_worker SELECT 'grit.session_events rewritten', count(*) FROM changed;

INSERT INTO machine_from_worker
SELECT 'grit.session_events rewritten without their subject', count(*) FROM machine_shapes WHERE dropped;

WITH gone AS (
       DELETE FROM grit.session_events e
        USING machine_shapes m
        WHERE e.session = m.session AND e.seq = m.seq AND m.kept IS NULL
    RETURNING 1)
INSERT INTO machine_from_worker SELECT 'grit.session_events deleted, too long to rewrite', count(*) FROM gone;

SELECT what, n FROM machine_from_worker ORDER BY what;
