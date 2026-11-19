"""E7: calibration of E2's arms (no new calls). For each arm, the confidence signals top
probability and top-minus-second margin against agreement (with Haiku on L1S/L1C, with the
answer by construction on SELF): AUROC, risk-coverage, the lowest threshold with >=95%
agreement above it and the coverage there."""
import json, os, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import common as c


def signals(arm, r):
    a = r["arms"][arm]
    if arm in ("a", "b"):
        ps = sorted(a["weights"].values(), reverse=True)
        return ps[0], ps[0] - (ps[1] if len(ps) > 1 else 0)
    if arm == "c":
        ys = sorted(a["weights"].values(), reverse=True) or [0]
        top = ys[0] if a["place"].startswith("topic:") else max(a["extra"]["aside"], 1 - a["extra"]["aside"])
        return top, (ys[0] - (ys[1] if len(ys) > 1 else 0)) if a["place"].startswith("topic:") else top
    ss = sorted((v["score"] for v in a["weights"].values()), reverse=True) or [0]
    top = ss[0] / 3 if a["place"].startswith("topic:") else max(a["extra"]["aside"], 1 - a["extra"]["aside"])
    return top, ((ss[0] - (ss[1] if len(ss) > 1 else 0)) / 3) if a["place"].startswith("topic:") else top


def auroc(pairs):
    pos = [s for s, y in pairs if y]
    neg = [s for s, y in pairs if not y]
    if not pos or not neg:
        return None
    wins = sum((p > n) + 0.5 * (p == n) for p in pos for n in neg)
    return wins / (len(pos) * len(neg))


def best_threshold(pairs, target=0.95):
    """The lowest threshold whose items at or above it agree at >= target, and its coverage."""
    for t in sorted({s for s, _ in pairs}):
        above = [y for s, y in pairs if s >= t]
        if above and sum(above) / len(above) >= target:
            return t, len(above) / len(pairs), sum(above) / len(above)
    return None


def curve(pairs):
    out = []
    for cov in (1.0, 0.8, 0.6, 0.4, 0.2):
        k = max(1, int(round(cov * len(pairs))))
        top = sorted(pairs, key=lambda x: -x[0])[:k]
        out.append((cov, round(sum(y for _, y in top) / k, 3)))
    return out


def main(path=os.path.join(c.OUT, "e2-lead-whole-title+summary.json")):
    res = json.load(open(path))
    report = {}
    for arm in "abcd":
        for sets in (("L1S",), ("L1C",), ("SELF",), ("L1S", "L1C", "SELF")):
            rs = [r for r in res.values() if r["set"] in sets and arm in r["arms"] and "error" not in r["arms"][arm]]
            for si, sname in ((0, "top"), (1, "margin")):
                pairs = [(signals(arm, r)[si], r["arms"][arm]["place"] == r["label"]) for r in rs]
                if not pairs:
                    continue
                k = f"{arm}/{'+'.join(sets)}/{sname}"
                report[k] = {"n": len(pairs), "agree": round(sum(y for _, y in pairs) / len(pairs), 3),
                             "auroc": auroc(pairs) and round(auroc(pairs), 3),
                             "risk_coverage": curve(pairs), "t95": best_threshold(pairs)}
                print(k, report[k])
    name = os.path.basename(path).replace("e2-", "e7-")
    with open(os.path.join(c.OUT, name), "w") as f:
        json.dump(report, f, indent=0)


if __name__ == "__main__":
    main(*sys.argv[1:])
