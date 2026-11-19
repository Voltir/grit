"""E13c: CP5's meeting head bar, by code alone (no model): E12c's day rule as written, on both
sheets. The bar is 0 wrong days and at least 5 of sheet 1's 9 meeting messages given their day.
Run: python3 -I E13-c.py. Aggregates to out/e13c.json; per-message results (no text) to
.local/jev-spike/e13/e13c.json."""
import datetime as dt
import importlib.util
import json
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import e13sheet as s

_spec = importlib.util.spec_from_file_location("e12c", os.path.join(HERE, "E12-c.py"))
e12c = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(e12c)  # FITTED stays False: E13 has no --fitted

TRUTH = {"meeting 11-16 team": dt.date(2026, 11, 16), "meeting 11-18 engine": dt.date(2026, 11, 18),
         "meeting 11-18 followup": dt.date(2026, 11, 18),
         "meeting 10-28 engine": dt.date(2026, 10, 28), "meeting 11-02 david nick": dt.date(2026, 11, 2)}


def main():
    res, per = {}, []
    for sh in s.SHEETS:
        msgs = s.rows(sh)
        meet = [m for m in msgs if m["label"].startswith("meeting")]
        unknown = {m["label"] for m in meet} - set(TRUTH)
        if unknown:
            raise SystemExit(f"no day for {unknown}")
        rs = []
        for m in meet:
            c = e12c.days_in(m["text"], m["when"])
            rs.append({"sheet": sh, "n": m["n"], "label": m["label"], "candidates": [str(d) for d in c],
                       "first right": bool(c) and c[0] == TRUTH[m["label"]],
                       "wrong": bool(c) and c[0] != TRUTH[m["label"]]})
        per += rs
        occ = sorted({r["label"] for r in rs})
        others = [m for m in msgs if not m["label"].startswith("meeting")]
        res[sh] = {"meeting messages": len(rs), "day found, first candidate right": sum(r["first right"] for r in rs),
                   "wrong day": sum(r["wrong"] for r in rs),
                   "occurrences with a right day": f"{sum(any(r['first right'] for r in rs if r['label'] == o) for o in occ)}/{len(occ)}",
                   "non-meeting msgs with a day word": f"{sum(1 for m in others if e12c.days_in(m['text'], m['when']))}/{len(others)}"}
    res["bar (sheet 1: 0 wrong, >= 5 of 9)"] = res["s1"]["wrong day"] == 0 and res["s1"]["day found, first candidate right"] >= 5
    os.makedirs(s.OUT, exist_ok=True)
    with open(os.path.join(s.OUT, "e13c.json"), "w") as f:
        json.dump(per, f, indent=1)
    with open(os.path.join(HERE, "out", "e13c.json"), "w") as f:
        json.dump(res, f, indent=1)
    print(json.dumps(res, indent=1))
    for r in per:
        print(r["sheet"], r["n"], r["label"], r["candidates"], r["first right"])


if __name__ == "__main__":
    main()
