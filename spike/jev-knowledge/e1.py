"""E1: BM25 shortlist recall (no Jev). Recall of the answer topic in the top k, k in 3,5,8,12.

Sets: SELF (a strand vs all 69 topics, its own topic the answer); L1S's 4 strands Haiku put in an
existing topic (vs the topics shown that night); E0's passages (a topic's newest passage vs all
69 leads)."""
import json, os, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import common as c, items as I

KS = [1, 3, 5, 8, 12]


def recall(cases, mode, smode="whole"):
    hits = {k: 0 for k in KS}
    ranks = []
    for it in cases:
        sl = I.shortlist(it, k=len(it["topics"]), mode=mode, smode=smode)
        r = sl.index(it["label"]) + 1 if it["label"] in sl else None
        ranks.append(r)
        for k in KS:
            hits[k] += 1 if r and r <= k else 0
    return {f"top{k}": round(hits[k] / len(cases), 3) for k in KS} | {"n": len(cases), "ranks": ranks}


def main():
    self_ = I.self_set()
    l1pos = [x for x in I.l1_strands() if x["label"].startswith("topic:")]
    allk = c.live_keys() + c.withdrawn_keys()
    passages = []
    for k in c.live_keys():
        t = c.latest(k)
        if t["passages"]:
            passages.append({"kind": "card", "title": "", "body": "x\n\n" + t["passages"][0]["text"],
                             "label": k, "topics": [(kk, c.latest(kk)["version"]) for kk in allk]})
    out = {}
    for mode in ("lead", "lead+passage", "firstline"):
        for name, cases in (("SELF", self_), ("L1S-existing", l1pos), ("E0-passages", passages)):
            for smode in (("whole", "cut800", "ends") if name == "SELF" else ("whole",)):
                r = recall(cases, mode, smode)
                out[f"{name}/{mode}/{smode}"] = r
                print(f"{name:13s} {mode:13s} {smode:7s}", {k: v for k, v in r.items() if k != "ranks"},
                      "ranks" if name == "L1S-existing" else "", r["ranks"] if name == "L1S-existing" else "")
    with open(os.path.join(c.OUT, "e1.json"), "w") as f:
        json.dump(out, f, indent=0)


if __name__ == "__main__":
    main()
