"""E12a: run purity of the room sheet at gaps of 5, 15, 30 and 60 minutes (code only, no model).
Writes aggregate numbers only to out/e12a.json. Run: python3 -I E12-a.py"""
import collections
import json
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import e12sheet as s

HERE = os.path.dirname(os.path.abspath(__file__))


def purity(us):
    lab_units = [u for u in us if s.labels_of(u)]
    pure = [u for u in lab_units if len(set(s.labels_of(u))) == 1]
    n_lab = sum(len(s.labels_of(u)) for u in lab_units)
    n_pure = sum(len(s.labels_of(u)) for u in pure)
    where = collections.defaultdict(set)
    for u in us:
        for l in s.labels_of(u):
            where[l].add(u["id"])
    split = {l: len(v) for l, v in where.items() if len(v) > 1}
    runs = [u for u in lab_units if u["kind"] == "run"]
    return {"units": len(us), "runs": sum(1 for u in us if u["kind"] == "run"),
            "threads": sum(1 for u in us if u["kind"] != "run"),
            "chatter-only units": len(us) - len(lab_units),
            "labelled units": len(lab_units), "pure units": len(pure),
            "pure runs": sum(1 for u in runs if len(set(s.labels_of(u))) == 1), "labelled runs": len(runs),
            "labelled messages": n_lab, "in a pure unit": n_pure,
            "labels split": len(split), "labels": len(where),
            "units per label, mean": round(sum(len(v) for v in where.values()) / len(where), 2),
            "multi-message units": sum(1 for u in us if len(u["msgs"]) > 1),
            "impure units' label counts": sorted(len(set(s.labels_of(u))) for u in lab_units if len(set(s.labels_of(u))) > 1)}


def context(msgs):
    """For each labelled top-level message: do the previous 2 top-level messages share its label;
    and the 'newest two labels' baseline for short (<150) top-level ones."""
    top = [m for m in msgs if m["where"] == "room"]
    r = collections.Counter()
    for i, m in enumerate(top):
        if m["label"] == s.CHATTER:
            continue
        prev = [p["label"] for p in top[max(0, i - 2):i]]
        r["labelled top-level"] += 1
        r["prev2 both share"] += all(p == m["label"] for p in prev) and len(prev) == 2
        r["prev2 one or more share"] += m["label"] in prev
    # newest two distinct labels seen before the message, in the whole channel (threads included)
    for scope in ("channel", "top-level"):
        seq = msgs if scope == "channel" else top
        hit = tot = 0
        for m in top:
            if m["label"] == s.CHATTER or len(m["text"]) >= 150:
                continue
            before = [p["label"] for p in seq if p["n"] < m["n"] and p["label"] != s.CHATTER]
            newest = []
            for l in reversed(before):
                if l not in newest:
                    newest.append(l)
                if len(newest) == 2:
                    break
            tot += 1
            hit += m["label"] in newest
        r[f"short top-level: newest two labels ({scope})"] = f"{hit}/{tot}"
    return dict(r)


def main():
    msgs = s.rows()
    labs = collections.Counter(m["label"] for m in msgs)
    res = {"messages": len(msgs), "labels": len([l for l in labs if l != s.CHATTER]),
           "chatter": labs[s.CHATTER], "top-level": sum(m["where"] == "room" for m in msgs),
           "short top-level labelled": sum(m["where"] == "room" and m["label"] != s.CHATTER and len(m["text"]) < 150 for m in msgs),
           "gaps": {g: purity(s.units(msgs, g)) for g in (0, 5, 15, 30, 60)},
           "context": context(msgs)}
    with open(os.path.join(HERE, "out", "e12a.json"), "w") as f:
        json.dump(res, f, indent=1)
    print(json.dumps(res, indent=1))


if __name__ == "__main__":
    main()
