# 0005. Entries are ranked with BM25 inside Postgres, through pg_textsearch

Status: accepted (2026-09-24)

Context: Retrieval assembly ranks stored entries against a query, and code mode is meant
to put workspace chunks in the same index instead of exploring files turn by turn.
Postgres's own `ts_rank`/`ts_rank_cd` have no inverse document frequency and no
term-frequency saturation. The alternatives were Lucene in the JVM (a second store kept in
step with Postgres, outside its transactions), BM25 computed in plain SQL (portable, but it
scores every candidate and needs term counts maintained on every write), and ParadeDB's
`pg_search` (AGPL, and on no managed Postgres grit would use). Tiger Data's `pg_textsearch`
(PostgreSQL license, Postgres 17 and 18) is offered on Cloud SQL and AlloyDB, where grit
deploys, and on neither RDS nor Aurora; that trade-off is accepted. Evidence: the spike on
branch `spike/pg-textsearch` (`.local/history/pg-textsearch-FINDINGS.md`): Lucene-exact
scores, searchable within the writing transaction, single-digit milliseconds per query on
one conversation at 100k entries.

Decision: grit's Postgres is `postgres:18` plus `pg_textsearch`, preloaded
(`docker/postgres/Dockerfile`, built by docker-compose and by the live tests alike), and
entries are ranked by its BM25 index, in SQL, in the same database as everything else.

Consequences: Writes are searchable in their own transaction, and no second store has to
be kept in step. grit cannot run on a Postgres without the extension. Corpus statistics
span everything one index holds, so tenants sharing a table change each other's ranking
and can infer each other's term frequencies: the cloud mode must give each tenant its own
database or partition before one table holds two. The index file keeps its high-water
mark and needs a periodic `REINDEX CONCURRENTLY`. A lexical ranker finds nothing for a
question that shares no words with its answer (the spike: verbatim asks found 0 of 5
labelled entries, model-style queries 5 of 5), so retrieval queries are written, not
copied from the user. Enforced by `PgTextsearchTests`, which fails if the image loses the
extension.
