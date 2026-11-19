"""P1 (no Jev): the turn's message as the search query. Reruns pg_textsearch BM25 (the
`<@> to_bm25query` form grit's SqlDocuments, SqlEntrySearch and SqlPieces use) over documents
current at the turn, closing entries said before it and pieces, merging the three shelves by
negated score. The same reproduction run with the written query is the control: its recall
of the Jev-passed candidates bounds how faithful the reproduction is.

Metric: recall at 10 and at 30 of the candidates Jev passed under the written query.
Writes out/p1.json (ids and numbers, no text)."""
import json, os, sys, collections
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import db, common as c

LIMIT = 60
FINDABLE = "findable" in sys.argv  # only the passed candidates the snapshot still holds


def lit(s):
    tag = "q" + "x" * 3
    while f"${tag}$" in s:
        tag += "x"
    return f"${tag}${s}${tag}$"


def search(query, at):
    q, t = lit(query[:4000]), f"'{at}'::timestamptz"
    sql = f"""
    (select 'doc:'||version, -s from (select d.version, d.body <@> to_bm25query({q}, 'grit.idx_documents_bm25') s
       from grit.documents d where d.body is not null and d.written_at < {t}
        and (d.superseded_at is null or d.superseded_at > {t})
        and not exists (select 1 from grit.document_refs b where b.version=d.version and b.kind='brief')
       order by s limit {LIMIT}) x where s < 0)
    union all
    (select 'closed:'||conversation_id, -s from (select e.conversation_id, e.search_text <@> to_bm25query({q}, 'grit.idx_entries_bm25') s
       from grit.entries e where e.payload->>'kind'='closed' and e.created_at < {t}
       order by s limit {LIMIT}) x where s < 0)
    union all
    (select 'piece:'||id, -s from (select p.id, p.search_text <@> to_bm25query({q}, 'grit.idx_pieces_bm25') s
       from grit.pieces p order by s limit {LIMIT}) x where s < 0)
    """
    hits = [(i, float(s)) for i, s in db.rows(sql)]
    hits.sort(key=lambda x: -x[1])
    seen, out = set(), []
    for i, s in hits:
        if i not in seen:
            seen.add(i)
            out.append(i)
    return out


def recall(ranked, passed, k):
    return sum(1 for p in passed if p in ranked[:k]), len(passed)


def present():
    """The candidate ids the snapshot still holds, of the kinds the reproduction searches."""
    ids = {"doc:" + v for [v] in db.rows("select version from grit.documents where body is not null")}
    ids |= {"closed:" + v for [v] in db.rows("select distinct conversation_id from grit.entries where payload->>'kind'='closed'")}
    ids |= {"piece:" + v for [v] in db.rows("select id from grit.pieces")}
    return ids


def main():
    turns = json.load(open(os.path.join(c.DATA, "turns.json")))
    have = present()
    rows = []
    for t in turns:
        passed = [k for k, v in t["verdicts"].items() if v.get("passed")]
        if FINDABLE:
            passed = [p for p in passed if p in have]
        if not passed or not t["message"] or not t["written"]:
            continue
        r = {"wid": t["wid"], "passed": passed, "kinds": collections.Counter(p.split(":")[0] for p in passed)}
        for name, query in (("message", t["message"]), ("written", t["written"])):
            ranked = search(query, t["at"])
            r[name] = {f"r@{k}": recall(ranked, passed, k) for k in (10, 30, 60)}
            r[name + "_ranks"] = [ranked.index(p) + 1 if p in ranked else None for p in passed]
        r["recorded_ranks"] = [next(i + 1 for i, f in enumerate(t["found"]) if f["id"] == p) for p in passed]
        rows.append(r)
    json.dump(rows, open(os.path.join(c.OUT, "p1-findable.json" if FINDABLE else "p1.json"), "w"), indent=0)
    for name in ("written", "message"):
        for k in (10, 30, 60):
            h = sum(r[name][f"r@{k}"][0] for r in rows)
            n = sum(r[name][f"r@{k}"][1] for r in rows)
            per = [r[name][f"r@{k}"][0] / r[name][f"r@{k}"][1] for r in rows]
            print(f"{name:8s} recall@{k}: pooled {h}/{n}={h/n:.3f}  per-turn mean {sum(per)/len(per):.3f}"
                  f"  turns at 100% {sum(p == 1 for p in per)}/{len(per)}")
    kinds = collections.Counter()
    for r in rows:
        kinds.update(r["kinds"])
    print("turns:", len(rows), "passed by kind:", dict(kinds))
    for kind in kinds:
        for name in ("written", "message"):
            hit = sum(1 for r in rows for p, rk in zip(r["passed"], r[name + "_ranks"]) if p.startswith(kind) and rk and rk <= 30)
            print(f"   {kind:6s} {name:8s} in top30: {hit}/{kinds[kind]}")


if __name__ == "__main__":
    main()
