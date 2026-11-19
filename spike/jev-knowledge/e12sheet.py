"""E12's shared reading of the hand-labelled room sheet (gitignored, real Slack text: never copied
into the repository) and its grouping into units. Outputs that hold text go to OUT, under .local."""
import datetime as dt
import os
import re

SHEET = "/home/nickchilders/Projects/grit/.local/jev-spike/room-sheet.md"
OUT = "/home/nickchilders/Projects/grit/.local/jev-spike/e12"
FOLD = {"enngine sync": "engine sync", "enging sync": "engine sync"}
CHATTER = "-"


def rows():
    """Messages oldest first: dicts n, when (datetime, UTC), where ('room' or 'thread N'), who, text, label."""
    out = []
    with open(SHEET) as f:
        for line in f:
            if not re.match(r"^\| \d+ \|", line):
                continue
            p = [x.strip() for x in line.strip().strip("|").split(" | ")]
            n, when, where, who, label = p[0], p[1], p[2], p[3], p[-1]
            text = " | ".join(p[4:-1])
            label = FOLD.get(label, label)
            out.append({"n": int(n), "when": dt.datetime.strptime(when, "%Y-%m-%d %H:%M"),
                        "where": where, "who": who, "text": text, "label": label})
    return out


def units(msgs, gap_min):
    """Top-level messages joined into runs while the gap to the previous top-level message is at most
    gap_min; each thread is one unit of its own (the sheet does not name a thread's root). Units are
    ordered by their first message."""
    out, cur, last = [], None, None
    threads = {}
    for m in msgs:
        if m["where"] == "room":
            if cur is not None and (m["when"] - last).total_seconds() <= gap_min * 60:
                cur["msgs"].append(m)
            else:
                cur = {"kind": "run", "msgs": [m]}
                out.append(cur)
            last = m["when"]
        else:
            u = threads.get(m["where"])
            if u is None:
                u = threads[m["where"]] = {"kind": m["where"], "msgs": []}
                out.append(u)
            u["msgs"].append(m)
    out.sort(key=lambda u: u["msgs"][0]["when"])
    for i, u in enumerate(out):
        u["id"] = i
    return out


def labels_of(u):
    return [m["label"] for m in u["msgs"] if m["label"] != CHATTER]
