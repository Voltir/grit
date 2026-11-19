"""Writes the sheets for Nick into data/ (untracked: they quote Slack text).

 data/disagreements.md  E2 arm a vs Haiku on L1S (<=40), with arms b-d's placements beside
 data/topics-keep-junk.md  the 69 topics ever opened, to mark keep or junk
"""
import json, os, sys, collections
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import common as c, items as I


def show(it, key):
    if key in ("new", "aside"):
        return f"**{key}**"
    t = c.topic_at(key, dict(it["topics"]).get(key, 10 ** 9)) or c.latest(key)
    return f"`…{key[-6:]}` {c.first_line(t['lead'])[:160]}"


def disagreements():
    res = json.load(open(os.path.join(c.OUT, "e2-lead-whole-title+summary.json")))
    byrid = {f"L1S:{it['run'][:13]}:{it['id']}": it for it in I.l1_strands()}
    rows = [(k, r) for k, r in res.items() if r["set"] == "L1S" and r["arms"]["a"]["place"] != r["label"]]
    rows.sort(key=lambda kr: -kr[1]["arms"]["a"]["top"])
    out = ["# E2 disagreements: Jev (arm a, Choice over every topic shown) vs Haiku, discussions",
           "",
           f"{len(rows)} of {sum(1 for r in res.values() if r['set']=='L1S')} discussions disagree; the first 40 by Jev's top probability.",
           "Mark **who is right** in the last line of each: H (Haiku), J (Jev), both (either is fine), neither.",
           "Arms: b = Choice over the BM25 top 8, c = YesNo per top-8 topic, d = Score per top-8 topic.", ""]
    for n, (k, r) in enumerate(rows[:40], 1):
        it = byrid[k]
        a = r["arms"]["a"]
        text = c.strand_text(it["text"], "whole").replace("\n", " ⏎ ")
        out += [f"## {n}. {it['day']} ({len(it['topics'])} topics shown)",
                f"- discussion: {c.cut(text, 420)}",
                f"- Haiku: {show(it, r['label'])}",
                f"- Jev (a, p={a['top']:.2f}): {show(it, a['place'])}",
                "- b / c / d: " + " / ".join(I.norm(r["arms"][x]["place"]) +
                                            ("(same as a)" if r["arms"][x]["place"] == a["place"] else "")
                                            for x in "bcd"),
                "- right: ", ""]
    p = os.path.join(c.DATA, "disagreements.md")
    open(p, "w").write("\n".join(out))
    print(p, len(rows))


def keep_junk():
    placed = collections.Counter(it["label"] for it in c.L1 if it["label"].startswith("topic:"))
    keys = c.live_keys() + c.withdrawn_keys()
    out = ["# The 69 topics ever opened: keep or junk?", "",
           "Mark each `keep` (the team will come back to it) or `junk` (chatter, a test, a one-off). "
           "live = current; withdrawn = since withdrawn (its last text shown). "
           "passages = how many it holds; placed = how often Haiku's attach put an item into it.", "",
           "| # | key | state | passages | placed | lead's first line | keep/junk |", "|---|---|---|---|---|---|---|"]
    for n, k in enumerate(keys, 1):
        t = c.latest(k)
        state = "live" if k in c.live_keys() else "withdrawn"
        out.append(f"| {n} | `…{k[-12:]}` | {state} | {len(t['passages'])} | {placed.get(k, 0)} | "
                   f"{c.first_line(t['lead'])[:200].replace('|', '/')} | |")
    p = os.path.join(c.DATA, "topics-keep-junk.md")
    open(p, "w").write("\n".join(out))
    print(p, len(keys))


if __name__ == "__main__":
    disagreements()
    keep_junk()
