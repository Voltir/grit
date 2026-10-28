-- One-off, for a database that holds the projects plugin's claims as documents (`claim/<artifact>`):
-- moves each live claim into grit.claims, then deletes every claim document, with its refs,
-- tombstones and reviews. Run once, in one transaction, after an engine of the build that
-- creates grit.claims has started on the database and before any claim is made through it.
-- It raises, changing nothing, when a live claim's maker is no principal: `by` holds the
-- maker's accounts, sorted and joined by ", ", or their principal's id when they had none.
-- ClaimsMigrationTests runs this file.
BEGIN;

INSERT INTO grit.claims
       (plugin, subject, holder, role, principal_id, made_at, classification_id, room_id)
SELECT DISTINCT ON (d.key, d.classification_id)
       d.plugin, substr(d.key, length('claim/') + 1), d.data -> 'claimed' ->> 'for',
       d.data -> 'claimed' ->> 'role', w.principal_id, d.written_at, d.classification_id,
       d.room_id
  FROM grit.documents d
  JOIN LATERAL (
         SELECT l.principal_id, 0 AS rank, l.account AS via
           FROM grit.links l
          WHERE l.account = ANY (string_to_array(d.data -> 'claimed' ->> 'by', ', '))
         UNION ALL
         SELECT p.id, 1, p.id
           FROM grit.principals p
          WHERE p.id = d.data -> 'claimed' ->> 'by'
       ) w ON true
 WHERE d.plugin = 'projects' AND d.key LIKE 'claim/%'
   AND d.superseded_at IS NULL AND d.body IS NOT NULL
 ORDER BY d.key, d.classification_id, w.rank, w.via;

DO $$
DECLARE
    moved bigint := (SELECT count(*) FROM grit.claims WHERE plugin = 'projects');
    live  bigint := (SELECT count(*) FROM grit.documents
                      WHERE plugin = 'projects' AND key LIKE 'claim/%'
                        AND superseded_at IS NULL AND body IS NOT NULL);
BEGIN
    IF moved <> live THEN
        RAISE EXCEPTION 'grit.claims holds % projects claims, but % claim documents are live', moved, live;
    END IF;
END $$;

DELETE FROM grit.tombstones t
 USING grit.documents d
 WHERE t.kind = 'document' AND t.target = d.version::text
   AND d.plugin = 'projects' AND d.key LIKE 'claim/%';

DELETE FROM grit.document_reviews WHERE plugin = 'projects' AND key LIKE 'claim/%';

-- Their refs and declassifications go by cascade.
DELETE FROM grit.documents WHERE plugin = 'projects' AND key LIKE 'claim/%';

COMMIT;
