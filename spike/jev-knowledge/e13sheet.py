"""E13's reading of both hand-labelled sheets (gitignored, real Slack text: never copied into the
repository) and the cut of each into units under an arm. Outputs that hold text go to OUT, under
.local.

Sheet 1 is Nick's room sheet (one room; its threads name no root, so a thread is a unit with no
opening, as in E12). Sheet 2 is wisdom's: legacy messages, every one inside a Slack thread, in two
channels (stream A, a team's room; stream B, a bot-test room). On sheet 2 a one-message thread
counts as a top-level message, and a longer thread stays whole in every arm."""
import collections
import datetime as dt
import re

SHEETS = {"s1": "/home/nickchilders/Projects/grit/.local/jev-spike/room-sheet.md",
          "s2": "/home/nickchilders/Projects/grit/.local/jev-spike/sheet-2.md"}
OUT = "/home/nickchilders/Projects/grit/.local/jev-spike/e13"
FOLD = {"enngine sync": "engine sync", "enging sync": "engine sync"}
CHATTER = "-"
BOT = "Assistant"
# Sheet 2's messages whose topic only a later message defines (picked for E13 by reading the
# sheet: no artifact lists them): 83 and 87 (defined by 88, the live token), 114 (by 115, the
# lender deck), 129 and 130 (by 131, email marketing).
FORWARD = {"s2": [83, 87, 114, 129, 130], "s1": []}


def rows(sheet):
    """Messages oldest first: n, when (UTC), stream, top (a top-level message), thread (a key, for a
    message in a thread that stays whole), who, text, label."""
    out = []
    with open(SHEETS[sheet]) as f:
        for line in f:
            if not re.match(r"^\| \d+ \|", line):
                continue
            p = [x.strip() for x in line.strip().strip("|").split(" | ")]
            if sheet == "s1":
                n, when, where, who, label, text = p[0], p[1], p[2], p[3], p[-1], " | ".join(p[4:-1])
                stream = "R"
            else:
                n, stream, when, where, who, label, text = p[0], p[1], p[2], p[3], p[4], p[-1], " | ".join(p[5:-1])
            out.append({"n": int(n), "when": dt.datetime.strptime(when, "%Y-%m-%d %H:%M"), "stream": stream,
                        "where": where, "who": who, "text": text, "label": FOLD.get(label, label)})
    if sheet == "s2":
        size = collections.Counter((m["stream"], m["where"]) for m in out)
        for m in out:
            m["top"] = size[(m["stream"], m["where"])] == 1
            m["thread"] = None if m["top"] else f"{m['stream']} {m['where']}"
    else:
        for m in out:
            m["top"] = m["where"] == "room"
            m["thread"] = None if m["top"] else m["where"]
    out.sort(key=lambda m: (m["when"], m["n"]))
    return out


# arm: gap minutes (0: each top-level message its own unit), most per run, before, after context,
# and whether grit's reply joins the person's message before it.
ARMS = {
    "R5": dict(gap=5, most=64, before=0, after=0, pair=False),
    "M22": dict(gap=0, most=64, before=2, after=2, pair=False),
    "R5C": dict(gap=5, most=64, before=2, after=2, pair=False),
    "M20": dict(gap=0, most=64, before=2, after=0, pair=False),
    "PR": dict(gap=0, most=64, before=2, after=2, pair=True),
}


def units(msgs, arm):
    """The units an arm cuts, ordered by their first message. A thread is whole and has no context.
    Top-level messages less than gap minutes apart join one run of at most most (gap 0: each its
    own, or with pair, a person's message and grit's reply after it); each run is shown up to before
    and after top-level neighbours of its own channel as context."""
    c = ARMS[arm]
    out, threads = [], {}
    for m in msgs:
        if not m["top"]:
            u = threads.get(m["thread"])
            if u is None:
                u = threads[m["thread"]] = {"kind": "thread", "msgs": [], "before": [], "after": []}
                out.append(u)
            u["msgs"].append(m)
    by_stream = collections.defaultdict(list)
    for m in msgs:
        if m["top"]:
            by_stream[m["stream"]].append(m)
    for top in by_stream.values():
        runs = []
        for i, m in enumerate(top):
            prev = top[i - 1] if i else None
            if c["gap"] > 0:
                join = (prev is not None and (m["when"] - prev["when"]).total_seconds() < c["gap"] * 60
                        and len(runs[-1]) < c["most"])
            else:
                join = (c["pair"] and prev is not None and m["who"] == BOT and prev["who"] != BOT
                        and len(runs[-1]) == 1)
            if join:
                runs[-1].append(i)
            else:
                runs.append([i])
        for r in runs:
            lo, hi = r[0], r[-1]
            out.append({"kind": "run", "msgs": [top[i] for i in r],
                        "before": top[max(0, lo - c["before"]):lo],
                        "after": top[hi + 1:hi + 1 + c["after"]]})
    out.sort(key=lambda u: (u["msgs"][0]["when"], u["msgs"][0]["n"]))
    for i, u in enumerate(out):
        u["id"] = i
    return out


def labels_of(u):
    return [m["label"] for m in u["msgs"] if m["label"] != CHATTER]
