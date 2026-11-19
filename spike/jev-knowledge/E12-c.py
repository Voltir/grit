"""E12c: can pure code read a meeting's tentative day from a message's text and timestamp?
No model. Run: python3 -I E12-c.py [--fitted]. Aggregates to out/e12c.json; per-message results (no text) to
.local/jev-spike/e12/e12c.json.

The rule: a weekday name (or its common short form) is the next such day on or after the message's
local day; "today" / "tonight" is that day, "tomorrow" the next; a numeric m/d is that date. Several
days in one message ("monday or tuesday?") give several candidates, the earliest first. --fitted adds a rule fitted to this
sample after seeing it: "now", "right now" or "real quick" is that day. The local
day is UTC minus 5 hours (the team's zone is assumed, not read from anywhere)."""
import datetime as dt
import json
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import e12sheet as s

TZ = dt.timedelta(hours=-5)
DAYS = {"monday": 0, "mon": 0, "tuesday": 1, "tue": 1, "tues": 1, "wednesday": 2, "wed": 2,
        "thursday": 3, "thu": 3, "thurs": 3, "friday": 4, "fri": 4, "saturday": 5, "sat": 5,
        "sunday": 6, "sun": 6}
TRUTH = {"meeting 11-16 team": dt.date(2026, 11, 16), "meeting 11-18 engine": dt.date(2026, 11, 18),
         "meeting 11-18 followup": dt.date(2026, 11, 18)}
TIME = re.compile(r"\b(\d{1,2})(?::?(\d{2}))?\s*(am|pm)\b|\b(\d{3,4})\b(?=\s*(?:am|pm|mon|tue|wed|thu|fri|today|tomorrow|$|\W))", re.I)


NOW = re.compile(r"\b(right now|real quick|now)\b")  # the fitted variant: an immediate call is today
FITTED = "--fitted" in sys.argv


def days_in(text, said):
    local = (said + TZ).date()
    out = []
    low = text.lower()
    for w in re.findall(r"[a-z]+", low):
        if w in DAYS:
            ahead = (DAYS[w] - local.weekday()) % 7
            out.append(local + dt.timedelta(days=ahead))
        elif w in ("today", "tonight"):
            out.append(local)
        elif w == "tomorrow":
            out.append(local + dt.timedelta(days=1))
    if FITTED and NOW.search(low):
        out.append(local)
    for mo, d in re.findall(r"\b(1[0-2]|0?[1-9])/([0-3]?\d)\b", low):
        try:
            out.append(dt.date(local.year, int(mo), int(d)))
        except ValueError:
            pass
    return sorted(set(out))


def has_time(text):
    return bool(TIME.search(text))


def main():
    msgs = s.rows()
    meet = [m for m in msgs if m["label"] in TRUTH]
    per, agg = [], {"meeting messages": len(meet)}
    for m in meet:
        c = days_in(m["text"], m["when"])
        per.append({"n": m["n"], "label": m["label"], "candidates": [str(d) for d in c],
                    "first right": bool(c) and c[0] == TRUTH[m["label"]],
                    "any right": TRUTH[m["label"]] in c, "time stated": has_time(m["text"])})
    agg["with a day word"] = sum(1 for p in per if p["candidates"])
    agg["first candidate right"] = sum(p["first right"] for p in per)
    agg["right day among candidates"] = sum(p["any right"] for p in per)
    agg["wrong only"] = sum(1 for p in per if p["candidates"] and not p["any right"])
    agg["with a time"] = sum(p["time stated"] for p in per)
    # by occurrence: does any of its messages yield the right day, from what had been said by then
    occ = {}
    for p in per:
        occ.setdefault(p["label"], []).append(p)
    agg["occurrences with the right day from some message"] = f"{sum(any(p['any right'] for p in v) for v in occ.values())}/{len(occ)}"
    agg["occurrences whose first day-bearing message is right first"] = (
        f"{sum(next((p['first right'] for p in v if p['candidates']), False) for v in occ.values())}/{len(occ)}")
    # false positives: non-meeting messages carrying a day word
    other = [m for m in msgs if m["label"] not in TRUTH]
    agg["non-meeting messages with a day word"] = f"{sum(1 for m in other if days_in(m['text'], m['when']))}/{len(other)}"
    os.makedirs(s.OUT, exist_ok=True)
    with open(os.path.join(s.OUT, "e12c-fitted.json" if FITTED else "e12c.json"), "w") as f:
        json.dump(per, f, indent=1)
    with open(os.path.join(HERE, "out", "e12c-fitted.json" if FITTED else "e12c.json"), "w") as f:
        json.dump(agg, f, indent=1)
    print(json.dumps(agg, indent=1))
    for p in per:
        print(p["n"], p["label"], p["candidates"], p["first right"], p["time stated"])


if __name__ == "__main__":
    main()
