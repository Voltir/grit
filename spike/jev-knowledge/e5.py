"""E5: is "worth keeping" its own question?
 (i)  the Choice with new/aside (E2 arm a, read from its results)
 (ii) one request: YesNo "Will the team come back to this?" and a Choice over the topics shown +
      `none`; code derives aside (keep < 0.5), new (keep and none) or the topic.
On the discussions of L1S and on SELF, whose items each opened one of the 69 topics: once Nick marks
data/topics-keep-junk.md, each SELF item's keep/junk is its own topic's mark, and
`python3 -I e5.py score <marked sheet>` scores both arms. Writes out/e5.json (keys, numbers)."""
import json, os, sys, re
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import common as c, items as I, jev, e2

KEEP = ("Will the team come back to this discussion: is it about a matter they will discuss, "
        "decide or look up again, rather than chatter, a greeting or a test?")


def opts():
    class O:
        tmode, smode, cmode = "lead", "whole", "title+summary"
    return O


def request(it):
    o = opts()
    keys = [k for k, _ in it["topics"]]
    crit = {f"t{j+1:02d}": I.topic_doc(k, dict(it["topics"])[k], o.tmode) for j, k in enumerate(keys)}
    back = {f"t{j+1:02d}": k for j, k in enumerate(keys)}
    crit["none"] = "None of these topics: the discussion is about a different matter."
    return ({"item": {"kind": "discussion", "text": c.item_text(it, o.smode)}},
            [{"type": "noul", "instructions": KEEP},
             {"type": "choice", "instructions": "Which topic is this discussion part of?", "criteria": crit}], back)


def run():
    e2res = json.load(open(os.path.join(c.OUT, "e2-lead-whole-title+summary.json")))
    out = {}
    for it in I.l1_strands() + I.self_set():
        if not it["topics"]:
            continue
        s, q, back = request(it)
        a, u = jev.ask("E5-ii", s, q)
        if "_error" in a:
            continue
        keep = jev.yes(a["q1"])
        ch, p = jev.choice_probs(a["q2"])
        ch = back.get(ch, ch)
        place = "aside" if keep < 0.5 else ("new" if ch == "none" else ch)
        rid = e2.rid(it)
        i_ = e2res.get(rid, {}).get("arms", {}).get("a", {})
        out[rid] = {"set": it["set"], "label": it["label"], "opened": "topic:strand-" + it["id"],
                    "ii": {"keep": keep, "choice": ch, "p_top": max(p.values()) if p else None,
                           "place": place, "tokens": u.get("input_tokens", 0)},
                    "i": {"place": i_.get("place"), "p_aside": (i_.get("weights") or {}).get("aside")}}
    json.dump(out, open(os.path.join(c.OUT, "e5.json"), "w"), indent=0)
    report(out)


def report(out, marks=None):
    import collections
    for s in ("L1S", "SELF"):
        rs = [r for r in out.values() if r["set"] == s]
        both = collections.Counter((I.norm(r["i"]["place"] or "?"), I.norm(r["ii"]["place"])) for r in rs)
        agree_h = sum(r["ii"]["place"] == r["label"] for r in rs)
        agree_i = sum(r["ii"]["place"] == r["i"]["place"] for r in rs)
        print(f"{s}: n={len(rs)} (ii) agrees with Haiku {agree_h}, with (i) {agree_i}; set aside by (i) "
              f"{sum(r['i']['place']=='aside' for r in rs)}, by (ii) {sum(r['ii']['place']=='aside' for r in rs)}")
        print("   (i, ii):", dict(sorted(both.items())))
        if marks and s == "SELF":
            scored = [(r, marks[r["opened"][-12:]]) for r in rs if r["opened"][-12:] in marks]
            for arm in ("i", "ii"):
                ok = sum((r[arm]["place"] != "aside") == (m == "keep") for r, m in scored)
                print(f"   vs Nick's keep/junk: arm ({arm}) {ok}/{len(scored)}")


if __name__ == "__main__":
    if sys.argv[1] == "run":
        run()
    elif sys.argv[1] == "score":
        marks = {}
        for line in open(sys.argv[2]):
            m = re.match(r"\|\s*\d+\s*\|\s*`…([0-9a-f-]{12})`.*\|\s*(keep|junk)\s*\|\s*$", line.strip())
            if m:
                marks[m.group(1)] = m.group(2)
        report(json.load(open(os.path.join(c.OUT, "e5.json"))), marks)
    else:
        report(json.load(open(os.path.join(c.OUT, "e5.json"))))
