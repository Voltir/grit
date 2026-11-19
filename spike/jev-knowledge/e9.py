"""E9: shortlist recall at scale (no model calls). Pure code picks at most k topics for Jev's
one Choice; how often is the right topic among them, as the pool grows to thousands?

Pool: the real topics (lead and passages, at the version the case saw) plus pseudo-topics: every
card version's body (grit.documents, plugin knowledge) and every piece's text (grit.pieces).
Cases:
  SELF  a discussion against the topic it opened (latest text; leaks: written from it)
  CONT  Haiku's 4 continuations with text (topic text at the version shown that night)
  HIST  from topic history: each passage a version added, its discussion as the strand said it
        before that version, against the version before (`same` strand: grit attaches it
        without asking; `new` strand: a real placement)
  L1C   cards: Haiku chose no topic for any, so none has a right topic and all are skipped.
Sources: a = BM25 of the item's text (lead / lead+passages; lead+passages+card titles equals
lead+passages, as no topic's lineage names a card); b = structural by ids (same root/thread,
then same room, newest first); c = recency (newest written real topics).
Also: the Postgres bm25 index over grit.documents restricted to a candidate set, against
in-memory BM25 over the same bodies.
Writes out/e9.json (numbers only)."""
import json, os, sys, random, collections, datetime, re
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import db, common as c, items as I, p1

KS = [16, 32, 48, 64]
SIZES = [69, 500, None]  # None: the whole pool


def ts(s):
    s = s.replace(" ", "T").replace("Z", "+00:00")
    s = re.sub(r"(\.\d{6})\d+", r"\1", s)
    s = re.sub(r"([+-]\d\d)$", r"\1:00", s)
    return datetime.datetime.fromisoformat(s)


# ---- the real topics, every version with text, with lineage ----------------------------------
VERSIONS = collections.defaultdict(list)  # key -> [(version, written, lead, passages, lineage, body)]
for v, k, w, d, body in db.rows("select version, key, written_at::text, data::text, coalesce(body,'') from grit.documents "
                                "where plugin='topics' and data::text<>'{}' order by version"):
    d = json.loads(d)
    lineage = [(f["strand"], f["room"]) for p in d.get("passages", []) for f in p.get("from", [])]
    VERSIONS[k].append({"version": int(v), "written": ts(w), "lead": c.prose_text(d.get("lead")) if hasattr(c, "prose_text") else "",
                        "passages": [p for p in d.get("passages", [])], "lineage": lineage, "body": body})


def prose(p):
    out = []
    for b in (p or {}).get("blocks", []):
        if "text" in b:
            out.append("".join(s.get("t", "") for s in b["text"] if isinstance(s, dict)))
    return "\n".join(out)


for vs in VERSIONS.values():
    for x in vs:
        x["passages_text"] = [prose(p.get("body")) for p in x["passages"]]


def text_of(x, mode):
    lead = c.cut(c.TOPICS[x["version"]]["lead"], 400) if x["version"] in c.TOPICS else ""
    if mode == "lead":
        return lead
    return lead + "\n" + "\n".join(x["passages_text"])


def topics_at(when, override=None):
    """Each real topic's latest version written at or before `when` (override: key -> version)."""
    out = {}
    for k, vs in VERSIONS.items():
        if override and k in override:
            out[k] = next(x for x in vs if x["version"] == override[k])
            continue
        before = [x for x in vs if when is None or x["written"] <= when]
        if before:
            out[k] = before[-1]
    return out


# ---- pseudo-topics ------------------------------------------------------------------------------
PSEUDO = {}
for v, body in db.rows("select version, body from grit.documents where plugin='knowledge' and body is not null"):
    PSEUDO["card:" + v] = body
for i, t in db.rows("select id, search_text from grit.pieces"):
    PSEUDO["piece:" + i] = t
PSEUDO_IDS = sorted(PSEUDO)


# ---- items ---------------------------------------------------------------------------------------
ROOM = {}


def room_of(conv):
    if conv not in ROOM:
        r = db.rows(f"select origin->>'team', origin->>'channel', coalesce(origin->>'threadTs','') from grit.conversations where id='{conv}'")
        ROOM[conv] = (f"slack:{r[0][0]}/{r[0][1]}", r[0][2]) if r else (None, None)
    return ROOM[conv]


def strand_text_at(root, when):
    """A strand's text as said before `when`: its root and the conversations following it."""
    convs = [root] + [x for [x] in db.rows(f"select distinct conversation_id from grit.stitches where root='{root}' and kind='follows'")]
    names = p1_names()
    rows = db.rows("select conversation_id::text, id, created_at::text, payload::text from grit.entries where conversation_id in ("
                   + ",".join(f"'{x}'" for x in convs) + f") and created_at < '{when.isoformat()}'::timestamptz "
                   "and payload->>'kind' in ('heard','message','posted') order by created_at")
    lines = []
    for cid, eid, at, p in rows:
        p = json.loads(p)
        if p["kind"] == "heard":
            lines.append((cid, names.get(eid, "Someone"), p["text"]))
        elif p["kind"] == "posted":
            lines.append((cid, "Assistant", p.get("text", "")))
        elif p["message"].get("role") == "user":
            lines.append((cid, names.get(eid, "Someone"), p["message"].get("text", "")))
        elif p["message"].get("role") == "assistant":
            t = "".join(b.get("text", "") for b in p["message"].get("blocks", []) if b.get("type") == "text")
            if t:
                lines.append((cid, "Assistant", t))
    if not lines:
        return None
    opening = next((l for l in lines if l[0] == root), None)
    rest = [l for l in lines if l is not opening]
    text = {"opening": [opening[1], opening[2][:300]] if opening else None,
            "rest": [[w, t[:300], ""] for _, w, t in rest]}
    return c.strand_text(text, "whole")


_N = None


def p1_names():
    global _N
    if _N is None:
        _N = dict(db.rows("select a.entry_id, n.name from grit.authors a join grit.identities n on n.account=a.account "
                          "where a.kind='person' and n.name is not null"))
    return _N


def cases():
    out = []
    for it in I.self_set():
        out.append({"set": "SELF", "root": it["id"], "when": None, "day": ts(it["slot"].split("@")[-1]), "said": it["asOf"][:10], "text": c.item_text(it), "right": it["label"],
                    "exclude_strand": it["id"], "override": None})
    for it in I.l1_strands():
        if it["label"].startswith("topic:"):
            shown = dict(it["topics"])
            # Topics exist by wall-clock time: a backfill's asOf is the day it replays, but the
            # topics it saw were written when it ran (its slot's instant).
            out.append({"set": "CONT", "root": it["id"], "when": ts(it["slot"].split("@")[-1]), "said": it["asOf"][:10],
                        "text": c.item_text(it),
                        "right": it["label"], "exclude_strand": None,
                        "override": {it["label"]: shown[it["label"]]} if it["label"] in shown else None})
    for k, vs in VERSIONS.items():
        for x0, x1 in zip(vs, vs[1:]):
            old_days = {p["day"] for p in x0["passages"]}
            old_strands = {s for s, _ in x0["lineage"]}
            for p in x1["passages"]:
                if p["day"] in old_days:
                    continue
                for f in p.get("from", []):
                    t = strand_text_at(f["strand"], x1["written"])
                    if not t:
                        continue
                    out.append({"set": "HIST-same" if f["strand"] in old_strands else "HIST-new", "root": f["strand"],
                                "when": x1["written"] - datetime.timedelta(seconds=1), "text": t, "right": k,
                                "exclude_strand": None, "override": {k: x0["version"]}})
    return out


# ---- sources ---------------------------------------------------------------------------------------
def rank_a(case, real, pseudo_ids, mode):
    docs = {k: text_of(x, mode) for k, x in real.items()}
    docs.update({i: PSEUDO[i] for i in pseudo_ids})
    return [i for i, _ in c.BM25(docs).rank(case["text"])]


def rank_b(case, real):
    room, thread = room_of(case["root"])
    same_root, same_room = [], []
    for k, x in real.items():
        strands = {s for s, _ in x["lineage"] if s != case["exclude_strand"]}
        rooms = {r for s, r in x["lineage"] if s != case["exclude_strand"]}
        if case["root"] in strands:
            same_root.append((x["written"], k))
        elif room in rooms:
            same_room.append((x["written"], k))
    return [k for _, k in sorted(same_root, reverse=True)] + [k for _, k in sorted(same_room, reverse=True)]


def rank_c(real):
    return [k for _, k in sorted(((x["written"], k) for k, x in real.items()), reverse=True)]


def union(k, a, b, c_, b_share=16, c_share=8):
    out = []
    for src, n in ((b, b_share), (c_, c_share)):
        for x in src:
            if len([y for y in out if y in src]) >= n or len(out) >= k:
                break
            if x not in out:
                out.append(x)
    for x in a:
        if len(out) >= k:
            break
        if x not in out:
            out.append(x)
    return out


def main():
    cs = cases()
    rng = random.Random(9)
    res = collections.defaultdict(lambda: collections.defaultdict(lambda: [0, 0]))
    per_case = []
    for case in cs:
        real = topics_at(case["when"], case["override"])
        if case["right"] not in real:
            continue
        b = rank_b(case, real)
        c_ = rank_c(real)
        for size in SIZES:
            n_pseudo = len(PSEUDO_IDS) if size is None else max(0, size - len(real))
            pseudo_ids = rng.sample(PSEUDO_IDS, n_pseudo) if size is not None else PSEUDO_IDS
            pool = len(real) + len(pseudo_ids)
            a_lead = rank_a(case, real, pseudo_ids, "lead")
            a_lp = rank_a(case, real, pseudo_ids, "lead+passages")
            lists = {"a lead": a_lead, "a lead+passages": a_lp, "b structural": b, "c recency": c_}
            for k in KS:
                lists_k = {name: l[:k] for name, l in lists.items()}
                lists_k["a+c (c<=8)"] = union(k, a_lp, [], c_, 0, 8)
                lists_k["a+b+c (b<=16, c<=8)"] = union(k, a_lp, b, c_, 16, 8)
                lists_k["b+c+a lead (b<=16, c<=8)"] = union(k, a_lead, b, c_, 16, 8)
                for name, l in lists_k.items():
                    for grp in (case["set"], "ALL-but-SELF" if case["set"] != "SELF" else None):
                        if grp:
                            cell = res[(grp, "all" if size is None else size, name)][k]
                            cell[0] += case["right"] in l
                            cell[1] += 1
            if size is None:
                per_case.append({"set": case["set"], "pool": pool, "rank_a_lp": a_lp.index(case["right"]) + 1,
                                 "rank_a_lead": a_lead.index(case["right"]) + 1,
                                 "rank_b": b.index(case["right"]) + 1 if case["right"] in b else None,
                                 "rank_c": c_.index(case["right"]) + 1, "real": len(real), "b_len": len(b)})
    out = {f"{g}|{s}|{n}": {str(k): v for k, v in d.items()} for (g, s, n), d in res.items()}
    pg = pg_check(cs)
    json.dump({"recall": out, "cases": per_case, "pg": pg}, open(os.path.join(c.OUT, "e9.json"), "w"), indent=0)
    print("cases:", collections.Counter(x["set"] for x in per_case), "pool sizes (all):", sorted({x["pool"] for x in per_case})[:3], "…")
    for (g, s, n), d in sorted(res.items(), key=lambda kv: (kv[0][0], str(kv[0][1]), kv[0][2])):
        print(f"{g:13s} pool={str(s):4s} {n:28s}", "  ".join(f"k{k}:{d[k][0]}/{d[k][1]}" for k in KS))
    print("pg check:", json.dumps(pg))


def pg_check(cs):
    """Recall@k over a docs-backed pool (the case's real topic versions + every card version: the
    pseudo-topics grit.documents holds; pieces live in another table), ranked by the
    grit.documents bm25 index restricted to that version set, against in-memory BM25 over the
    same bodies; and the must-show unions (b, then c) filled from each ranking."""
    cards = {int(v): b for v, b in db.rows("select version, body from grit.documents where plugin='knowledge' and body is not null")}
    tally = collections.defaultdict(lambda: collections.Counter())
    for case in cs:
        real = topics_at(case["when"], case["override"])
        if case["right"] not in real:
            continue
        ids = {x["version"]: x["body"] for x in real.values() if x["body"]}
        target = real[case["right"]]["version"]
        if target not in ids:
            tally[case["set"]]["target has no body"] += 1
            continue
        key_of = {x["version"]: k for k, x in real.items()}
        ids.update(cards)
        q = p1.lit(case["text"][:4000])
        arr = ",".join(str(v) for v in ids)
        pg = [int(v) for [v] in db.rows(
            f"select version from (select version, body <@> to_bm25query({q}, 'grit.idx_documents_bm25') s from grit.documents "
            f"where version = any(array[{arr}]::bigint[]) order by s limit 64) x where s < 0 order by s")]
        mem = [int(i) for i, _ in c.BM25({str(v): b for v, b in ids.items()}).rank(case["text"])]
        as_keys = lambda vs: [key_of.get(v, f"card:{v}") for v in vs]
        b, c_ = rank_b(case, real), rank_c(real)
        for grp in (case["set"], "ALL-but-SELF" if case["set"] != "SELF" else None):
            if not grp:
                continue
            t = tally[grp]
            t["n"] += 1
            for k in KS:
                t[f"pg@{k}"] += target in pg[:k]
                t[f"mem@{k}"] += target in mem[:k]
            for k in (32, 64):
                for name, a in (("pg", as_keys(pg)), ("mem", as_keys(mem))):
                    t[f"{name}+c8@{k}"] += case["right"] in union(k, a, [], c_, 0, 8)
                    t[f"{name}+b16@{k}"] += case["right"] in union(k, a, b, [], 16, 0)
                    t[f"{name}+b16+c8@{k}"] += case["right"] in union(k, a, b, c_, 16, 8)
                    t[f"{name}+b8+c4@{k}"] += case["right"] in union(k, a, b, c_, 8, 4)
            t["pool"] = len(ids)
    return {g: dict(t) for g, t in tally.items()}


if __name__ == "__main__":
    main()
