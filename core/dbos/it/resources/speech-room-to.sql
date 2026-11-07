-- One-off, for a database whose grit.heard and grit.speech were made before a heard message
-- kept a room address and a draft its venue (ADR 0059): adds `room_to` to each, and to
-- grit.heard the thread a message is in (`within`), with the CHECKs schema.sql now creates. Run once, in one transaction, before an engine of the build that
-- places drafts starts on the database; running it again changes nothing.
-- SpeechRoomToMigrationTests runs this file.
BEGIN;

ALTER TABLE grit.heard ADD COLUMN IF NOT EXISTS room_to TEXT;
ALTER TABLE grit.heard DROP CONSTRAINT IF EXISTS heard_room_to_check;
ALTER TABLE grit.heard ADD CONSTRAINT heard_room_to_check
    CHECK (room_to IS NULL OR reply_to IS NOT NULL);
ALTER TABLE grit.heard ADD COLUMN IF NOT EXISTS within TEXT;
ALTER TABLE grit.heard DROP CONSTRAINT IF EXISTS heard_within_check;
ALTER TABLE grit.heard ADD CONSTRAINT heard_within_check
    CHECK (within IS NULL OR reply_to IS NOT NULL);

ALTER TABLE grit.speech ADD COLUMN IF NOT EXISTS room_to TEXT;
ALTER TABLE grit.speech DROP CONSTRAINT IF EXISTS speech_room_to_check;
ALTER TABLE grit.speech ADD CONSTRAINT speech_room_to_check
    CHECK (room_to IS NULL OR (drafting AND NOT answering));

COMMIT;
