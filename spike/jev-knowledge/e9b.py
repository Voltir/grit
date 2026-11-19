"""E9b: recency and text search combined, (b) structural always pinned first (at most 16).
Arms, at k = 32 and 64:
  text      BM25 alone fills the rest
  newestN   the N newest (N = 8, 16), then BM25 fills the rest
  rrf w:v   score = w/(60 + text rank) + v/(60 + recency rank), w:v in 1:1, 2:1, 1:2
Recency is as of each case's run day: candidates written at or before it, newest first (a
pseudo-topic by its card's or piece set's write time; one written after the day ranks last).
Text ranking: in-memory BM25 over lead+passages (pools 69, 500, all), and the Postgres bm25
index over grit.documents restricted to the case's topic versions plus every card version
(read-only snapshot: no scratch table, so pieces are not in the index pool).
Writes out/e9b.json (numbers only)."""
import json, os, sys, random, collections, datetime
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import db, common as c, e9, p1

KS = [32, 64]
ARMS = ["text", "newest8", "newest16", "rrf1:1", "rrf2:1", "rrf1:2"]

WRITTEN = {}
for v, w in db.rows("select version, written_at::text from grit.documents where plugin='knowledge'"):
    WRITTEN["card:" + v] = e9.ts(w)
for i, w in db.rows("select p.id, s.written_at::text from grit.pieces p join grit.piece_sets s on s.id=p.set_id"):
    WRITTEN["piece:" + i] = e9.ts(w)


def recency(ids, day, real):
    far = datetime.datetime(1970, 1, 1, tzinfo=datetime.timezone.utc)
    def when(i):
        return real[i]["written"] if i in real else WRITTEN.get(i, far)
    before = sorted((i for i in ids if when(i) <= day), key=lambda i: when(i), reverse=True)
    after = sorted((i for i in ids if when(i) > day), key=lambda i: when(i))
    return before + after


def fill(k, pinned, rest):
    out = list(pinned)
    for x in rest:
        if len(out) >= k:
            break
        if x not in out:
            out.append(x)
    return out


def arm_list(arm, k, b, text, rec):
    b = b[:16]
    if arm == "text":
        return fill(k, b, text)
    if arm.startswith("newest"):
        n = int(arm[6:])
        head = [x for x in rec if x not in b][:n]
        return fill(k, b + head, text)
    w, v = (int(x) for x in arm[3:].split(":"))
    tr = {x: i for i, x in enumerate(text)}
    rr = {x: i for i, x in enumerate(rec)}
    big = len(rec) + len(text)
    fused = sorted(set(text) | set(rec), key=lambda x: -(w / (60 + tr.get(x, big)) + v / (60 + rr.get(x, big))))
    return fill(k, b, fused)


def main():
    cs = e9.cases()
    rng = random.Random(9)
    tally = collections.defaultdict(lambda: [0, 0])
    cards = {int(v): b for v, b in db.rows("select version, body from grit.documents where plugin='knowledge' and body is not null")}
    for case in cs:
        real = e9.topics_at(case["when"], case["override"])
        if case["right"] not in real:
            continue
        day = case.get("day") or case["when"]
        b = e9.rank_b(case, real)
        groups = [case["set"]] + (["ALL-but-SELF"] if case["set"] != "SELF" else [])
        for size in e9.SIZES:
            n_pseudo = len(e9.PSEUDO_IDS) if size is None else max(0, size - len(real))
            pseudo = rng.sample(e9.PSEUDO_IDS, n_pseudo) if size is not None else e9.PSEUDO_IDS
            text = e9.rank_a(case, real, pseudo, "lead+passages")
            rec = recency(list(real) + list(pseudo), day, real)
            for arm in ARMS:
                for k in KS:
                    hit = case["right"] in arm_list(arm, k, b, text, rec)
                    for g in groups:
                        t = tally[(g, f"mem pool={'all' if size is None else size}", arm, k)]
                        t[0] += hit
                        t[1] += 1
        # The index: topic versions with a body plus every card version.
        ids = {x["version"]: x["body"] for x in real.values() if x["body"]}
        if real[case["right"]]["version"] not in ids:
            continue
        key_of = {x["version"]: kk for kk, x in real.items()}
        ids.update(cards)
        q = p1.lit(case["text"][:4000])
        arr = ",".join(str(v) for v in ids)
        pg = [key_of.get(int(v), f"card:{v}") for [v] in db.rows(
            f"select version from (select version, body <@> to_bm25query({q}, 'grit.idx_documents_bm25') s from grit.documents "
            f"where version = any(array[{arr}]::bigint[]) order by s limit 200) x where s < 0 order by s")]
        mem = [key_of.get(int(v), f"card:{v}") for v, _ in c.BM25({str(v): bb for v, bb in ids.items()}).rank(case["text"])]
        pool = [key_of.get(v, f"card:{v}") for v in ids]
        rec = recency(pool, day, real)
        for name, text in (("pg index, topics+cards", pg), ("mem, topics+cards", mem)):
            for arm in ARMS:
                for k in KS:
                    hit = case["right"] in arm_list(arm, k, b, text, rec)
                    for g in groups:
                        t = tally[(g, name, arm, k)]
                        t[0] += hit
                        t[1] += 1
    out = {"|".join(map(str, key)): v for key, v in tally.items()}
    json.dump(out, open(os.path.join(c.OUT, "e9b.json"), "w"), indent=0)
    for g in ("SELF", "ALL-but-SELF"):
        print("==", g)
        pools = sorted({p for (gg, p, _, _) in tally if gg == g})
        print(f"{'pool':26s}" + "".join(f"{a+'@'+str(k):>14s}" for k in KS for a in ARMS))
        for p in pools:
            print(f"{p:26s}" + "".join(f"{tally[(g, p, a, k)][0]:>8d}/{tally[(g, p, a, k)][1]:<5d}" for k in KS for a in ARMS))


if __name__ == "__main__":
    main()
