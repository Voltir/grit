-- One-off, for a database whose grit.claims was made before a claim named the session working
-- it: adds `session_harness` and `session_local`, with the CHECKs schema.sql now creates. Every
-- claim it finds is left with no session, for its holder's next session to start to take. Run
-- once, in one transaction, before an engine of the build that names a claim's session starts on
-- the database; running it again changes nothing.
-- ClaimSessionMigrationTests runs this file.
BEGIN;

ALTER TABLE grit.claims ADD COLUMN IF NOT EXISTS session_harness TEXT
    CHECK (session_harness ~ '^[a-z][a-z0-9-]{0,63}$');
ALTER TABLE grit.claims ADD COLUMN IF NOT EXISTS session_local TEXT
    CHECK (length(session_local) BETWEEN 1 AND 200);
ALTER TABLE grit.claims DROP CONSTRAINT IF EXISTS claims_session;
ALTER TABLE grit.claims ADD CONSTRAINT claims_session
    CHECK ((session_harness IS NULL) = (session_local IS NULL));

COMMIT;
