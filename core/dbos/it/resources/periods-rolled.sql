-- One-off, for a database whose grit.periods was made before a channel's top level rolled
-- (ADR 0060): widens periods_reason_check to admit 'rolled' (CloseReason.Rolled), the CHECK
-- schema.sql now creates. Run once, in one transaction, before an engine of the build that
-- rolls starts on the database; running it again changes nothing.
-- PeriodsRolledMigrationTests runs this file.
BEGIN;

ALTER TABLE grit.periods DROP CONSTRAINT periods_reason_check;
ALTER TABLE grit.periods ADD CONSTRAINT periods_reason_check
    CHECK (reason IN ('resolved', 'lapsed', 'unearned', 'ran', 'rolled'));

COMMIT;
