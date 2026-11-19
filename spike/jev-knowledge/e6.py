"""E6: grouping new items. Positives: pairs of discussions Haiku put in one `new` entry, and
pairs where Haiku put a later night's discussion into the topic an earlier one opened. Negatives,
as many: half the BM25-nearest other discussion of the same night, half random others.
 pairwise:   one YesNo "same matter?" over {first, second}
 sequential: each night's discussions in order, each placed (E2 arm a's Choice) among the topics
             shown that night plus each provisionally opened topic (described by its opening
             discussion's text, cut to 400), so a pair is joined when both land in one topic.
Writes out/e6.json (ids and numbers)."""
import json, os, sys, random, collections, itertools
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import common as c, items as I, jev, e2, e7

SAME = ("Are these two discussions about the same matter, so they belong in one topic the team "
        "keeps? Being on a related subject, or between the same people, is not enough.")


def pairs():
    S = I.l1_strands()
    by = {it["id"]: it for it in S}
    g = collections.defaultdict(list)
    for it in S:
        if it["group"]:
            g[it["group"]].append(it)
    pos = [(a, b, "same-new") for v in g.values() for a, b in itertools.combinations(v, 2)]
    pos += [(by[it["label"][len("topic:strand-"):]], it, "cross-night") for it in S
            if it["label"].startswith("topic:") and it["label"][len("topic:strand-"):] in by]
    rng = random.Random(6)
    negs, seen = [], {frozenset((a["id"], b["id"])) for a, b, _ in pos}
    runs = collections.defaultdict(list)
    for it in S:
        runs[it["run"]].append(it)
    together = {it["id"]: it["group"] for it in S}
    for a, b, _ in pos[: len(pos) // 2 + 1]:
        mates = [x for x in runs[a["run"]] if x["id"] != a["id"] and frozenset((a["id"], x["id"])) not in seen
                 and not (together.get(x["id"]) and together.get(x["id"]) == together.get(a["id"]))]
        if not mates:
            continue
        docs = {x["id"]: c.item_text(x) for x in mates}
        near = c.BM25(docs).rank(c.item_text(a))[0][0]
        negs.append((a, next(x for x in mates if x["id"] == near), "bm25-near"))
        seen.add(frozenset((a["id"], near)))
    while len(negs) < len(pos):
        a, b = rng.sample(S, 2)
        k = frozenset((a["id"], b["id"]))
        if a["id"] != b["id"] and k not in seen and not (a["group"] and a["group"] == b["group"]) \
                and a["label"] != "topic:strand-" + b["id"] and b["label"] != "topic:strand-" + a["id"]:
            negs.append((a, b, "random"))
            seen.add(k)
    return [(a, b, k, True) for a, b, k in pos] + [(a, b, k, False) for a, b, k in negs]


def pairwise(ps):
    out = []
    for a, b, kind, y in ps:
        s = {"first": {"text": c.item_text(a)}, "second": {"text": c.item_text(b)}}
        ans, u = jev.ask("E6-pairwise", s, [{"type": "noul", "instructions": SAME}])
        out.append({"a": a["id"], "b": b["id"], "kind": kind, "same": y, "p": jev.yes(ans["q1"])})
    return out


def sequential():
    o = type("O", (), {"tmode": "lead", "smode": "whole", "cmode": "title+summary"})
    runs = collections.defaultdict(list)
    for it in I.l1_strands():
        runs[it["run"]].append(it)
    placed = {}
    for run, its in runs.items():
        provisional = []  # (key, text)
        for it in its:
            keys = [k for k, _ in it["topics"]]
            crit = {f"t{j+1:02d}": I.topic_doc(k, dict(it["topics"])[k]) for j, k in enumerate(keys)}
            back = {f"t{j+1:02d}": k for j, k in enumerate(keys)}
            for j, (k, text) in enumerate(provisional):
                kk = f"t{len(keys)+j+1:02d}"
                crit[kk], back[kk] = c.cut(text, 400), k
            crit["new"], crit["aside"] = e2.NEW, e2.ASIDE
            ans, u = jev.ask("E6-sequential", {"item": {"kind": "discussion", "text": c.item_text(it)}},
                             [{"type": "choice", "instructions": "Which topic is this discussion part of?", "criteria": crit}])
            ch, p = jev.choice_probs(ans["q1"])
            where = back.get(ch, ch)
            if where == "new":
                where = "prov:" + it["id"]
                provisional.append((where, c.item_text(it)))
            placed[(run, it["id"])] = (where, p.get(ch))
    return placed


def main():
    ps = pairs()
    pw = pairwise(ps)
    seq = sequential()
    rows = []
    for r, (a, b, kind, y) in zip(pw, ps):
        same_run = a["run"] == b["run"]
        sa, sb = seq.get((a["run"], a["id"])), seq.get((b["run"], b["id"]))
        # Across nights: b joined a when b went into the topic a opened (or a's provisional one).
        if same_run:
            joined = sa and sb and sa[0] == sb[0] and sa[0] != "aside"
        else:
            joined = sb and sb[0] in ("topic:strand-" + a["id"],)
        rows.append({**r, "same_run": same_run, "seq_joined": bool(joined)})
    json.dump(rows, open(os.path.join(c.OUT, "e6.json"), "w"), indent=0)
    pos = sum(r["same"] for r in rows)
    print(f"pairs {len(rows)}: positives {pos} ({collections.Counter(r['kind'] for r in rows if r['same'])}), "
          f"negatives {len(rows)-pos} ({collections.Counter(r['kind'] for r in rows if not r['same'])})")
    print("pairwise AUROC", round(e7.auroc([(r["p"], r["same"]) for r in rows]), 3))
    for t in (0.3, 0.5, 0.7, 0.9):
        tp = sum(r["p"] >= t and r["same"] for r in rows)
        fp = sum(r["p"] >= t and not r["same"] for r in rows)
        print(f"  pairwise at {t}: precision {tp}/{tp+fp}  recall {tp}/{pos}")
    tp = sum(r["seq_joined"] and r["same"] for r in rows)
    fp = sum(r["seq_joined"] and not r["same"] for r in rows)
    print(f"  sequential: precision {tp}/{tp+fp}  recall {tp}/{pos}")
    for k in ("bm25-near", "random"):
        print(f"  false joins on {k} negatives: pairwise@0.5 {sum(r['p']>=0.5 for r in rows if r['kind']==k)}, "
              f"sequential {sum(r['seq_joined'] for r in rows if r['kind']==k)} of {sum(r['kind']==k for r in rows)}")


if __name__ == "__main__":
    main()
