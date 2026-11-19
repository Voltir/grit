"""E2: question shape. Arms over each item:
 a  Choice over every topic shown that night + new + aside
 b  Choice over the BM25 top 8 + new + aside
 c  YesNo "does this continue topic X?" for each of the top 8, plus one aside YesNo, in one request
 d  Score (4 levels) for each of the top 8, plus one aside YesNo, in one request
Cards are not offered `new` (a new topic opens from a discussion only, as the attach ask says).

python3 -I e2.py dry [arm]       # render 3 states of an arm
python3 -I e2.py run [arms] [--tmode lead] [--smode whole] [--cmode title+summary] [--sets L1S,L1C,SELF]
Writes out/e2-<tmode>-<smode>-<cmode>.json (keys and probabilities only, no text).
"""
import json, os, sys, argparse, collections
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import common as c, items as I, jev

NEW = "A matter none of these topics covers, that the team is likely to come back to."
ASIDE = "Too slight to keep: chatter, a greeting, a test, or nothing the team will come back to."
ASIDE_Q = ("Is this {what} too slight to keep: chatter, a greeting, a test, or nothing the team "
           "will come back to?")
LEVELS = ["unrelated", "the same area, but a different matter", "the same matter, adding nothing new",
          "continues the same matter with something new"]


def what(it):
    return "discussion" if it["kind"] == "strand" else "document"


def state(it, o):
    return {"item": {"kind": what(it), "text": c.item_text(it, o.smode, o.cmode)}}


def keyed(it, keys, o):
    crit = {f"t{j+1:02d}": I.topic_doc(k, dict(it["topics"])[k], o.tmode) for j, k in enumerate(keys)}
    back = {f"t{j+1:02d}": k for j, k in enumerate(keys)}
    if it["kind"] == "strand":
        crit["new"] = NEW
    crit["aside"] = ASIDE if it["kind"] == "strand" else "None of these topics: the document is about none of them."
    return crit, back


def request(arm, it, o):
    keys = [k for k, _ in it["topics"]] if arm == "a" else I.shortlist(it, 8, o.tmode, o.smode)
    if arm in ("a", "b"):
        crit, back = keyed(it, keys, o)
        instr = ("Which topic is this discussion part of?" if it["kind"] == "strand"
                 else "Which topic is this document about?")
        return [{"type": "choice", "instructions": instr, "criteria": crit}], back
    qs = []
    for k in keys:
        lead = I.topic_doc(k, dict(it["topics"])[k], o.tmode)
        if arm == "c":
            instr = (f"Does this discussion continue the topic: {lead}" if it["kind"] == "strand"
                     else f"Is this document about the topic: {lead}")
            qs.append({"type": "noul", "instructions": instr})
        else:
            qs.append({"type": "score", "instructions": f"How does this {what(it)} relate to the topic: {lead}",
                       "criteria": LEVELS})
    qs.append({"type": "noul", "instructions": ASIDE_Q.format(what=what(it))})
    return qs, {f"q{j+1}": k for j, k in enumerate(keys)}


def read(arm, it, a, back):
    """{topic key | new | aside: weight} and the raw per-key numbers."""
    if arm in ("a", "b"):
        ch, p = jev.choice_probs(a["q1"])
        return {back.get(k, k): v for k, v in p.items()}, {"confidence": a["q1"].get("confidence")}
    n = len(back)
    raw = {}
    for j in range(n):
        x = a[f"q{j+1}"]
        if arm == "c":
            raw[back[f"q{j+1}"]] = jev.yes(x)
        else:
            raw[back[f"q{j+1}"]] = {"score": x["score"], "p": [x["probabilities"].get(str(l), 0) for l in range(4)]}
    aside = jev.yes(a[f"q{n+1}"])
    return raw, {"aside": aside}


def decide(arm, it, weights, extra, theta=None):
    """The placement (topic key, new or aside), and its top and margin."""
    if arm in ("a", "b"):
        ranked = sorted(weights.items(), key=lambda kv: -kv[1])
        top = ranked[0]
        second = ranked[1][1] if len(ranked) > 1 else 0
        return top[0], top[1], top[1] - second
    if arm == "c":
        theta = 0.5 if theta is None else theta
        vals = {k: v for k, v in weights.items()}
    else:
        theta = 2.0 if theta is None else theta
        vals = {k: v["score"] for k, v in weights.items()}
    ranked = sorted(vals.items(), key=lambda kv: -kv[1])
    if ranked and ranked[0][1] >= theta:
        second = ranked[1][1] if len(ranked) > 1 else 0
        return ranked[0][0], ranked[0][1], ranked[0][1] - second
    fallback = "aside" if extra["aside"] >= 0.5 or it["kind"] == "card" else "new"
    return fallback, (extra["aside"] if fallback == "aside" else 1 - extra["aside"]), None


def sets(names):
    out = []
    for n in names:
        out += {"L1S": I.l1_strands, "L1C": I.l1_cards, "SELF": I.self_set}[n]()
    return out


def rid(it):
    return f"{it['set']}:{it['run'][:13] if it['set'] != 'SELF' else ''}:{it['id']}"


def run(o):
    path = os.path.join(c.OUT, f"e2-{o.tmode}-{o.smode}-{o.cmode}.json")
    res = json.load(open(path)) if os.path.exists(path) else {}
    for arm in o.arms:
        tag = f"E2-{arm}" if (o.tmode, o.smode, o.cmode) == ("lead", "whole", "title+summary") else f"E34-{arm}-{o.tmode}-{o.smode}-{o.cmode}"
        for it in sets(o.sets):
            qs, back = request(arm, it, o)
            if not back:
                continue
            a, u = jev.ask(tag, state(it, o), qs)
            r = res.setdefault(rid(it), {"set": it["set"], "kind": it["kind"], "label": it["label"],
                                          "shown": len(it["topics"]), "arms": {}})
            if "_error" in a:
                r["arms"][arm] = {"error": a["_error"], "tokens": 0}
                continue
            w, extra = read(arm, it, a, back)
            place, top, margin = decide(arm, it, w, extra)
            r["arms"][arm] = {"weights": w, "extra": extra, "place": place, "top": top, "margin": margin,
                              "keys": list(back.values()), "tokens": u.get("input_tokens", 0)}
    with open(path, "w") as f:
        json.dump(res, f, indent=0)
    report(res, o.arms)


def mrr(r, arm):
    w = r["arms"][arm]["weights"]
    if arm == "d":
        w = {k: v["score"] for k, v in w.items()}
    ranked = [k for k, _ in sorted(w.items(), key=lambda kv: -kv[1])]
    return 1 / (ranked.index(r["label"]) + 1) if r["label"] in ranked else 0.0


def report(res, arms):
    for arm in arms:
        print(f"== arm {arm}")
        for s in ("L1S", "L1C", "SELF"):
            rs = [r for r in res.values() if r["set"] == s and arm in r["arms"] and "error" not in r["arms"][arm]]
            errs = sum(1 for r in res.values() if r["set"] == s and "error" in r["arms"].get(arm, {}))
            if not rs:
                continue
            agree = sum(r["arms"][arm]["place"] == r["label"] for r in rs)
            conf = collections.Counter((I.norm(r["label"]), I.norm(r["arms"][arm]["place"]) +
                                        ("" if not r["label"].startswith("topic:") or r["arms"][arm]["place"] in ("new", "aside")
                                         else ("=" if r["arms"][arm]["place"] == r["label"] else "≠")))
                                       for r in rs)
            pos = [r for r in rs if r["label"].startswith("topic:")]
            m = sum(mrr(r, arm) for r in pos) / len(pos) if pos else None
            tok = sum(r["arms"][arm]["tokens"] for r in rs) / len(rs)
            print(f"  {s:5s} n={len(rs)} errors={errs} agree={agree}/{len(rs)}={agree/len(rs):.3f}"
                  f" MRR(on existing-topic labels)={m if m is None else round(m, 3)} tokens/item={tok:.0f}")
            print("     (haiku, jev):", dict(sorted(conf.items())))


if __name__ == "__main__":
    p = argparse.ArgumentParser()
    p.add_argument("cmd")
    p.add_argument("arms", nargs="?", default="abcd")
    p.add_argument("--tmode", default="lead")
    p.add_argument("--smode", default="whole")
    p.add_argument("--cmode", default="title+summary")
    p.add_argument("--sets", default="L1S,L1C,SELF")
    o = p.parse_args()
    o.arms = list(o.arms)
    o.sets = o.sets.split(",")
    if o.cmd == "dry":
        ss = sets(o.sets)
        for it in (ss[0], ss[85] if len(ss) > 85 else ss[-1], ss[-1]):
            for arm in o.arms:
                qs, back = request(arm, it, o)
                print(f"--- {it['set']} label={I.norm(it['label'])} arm={arm}")
                print(json.dumps(state(it, o), indent=1))
                print(json.dumps(qs, indent=1)[:1800])
    elif o.cmd == "report":
        res = json.load(open(os.path.join(c.OUT, f"e2-{o.tmode}-{o.smode}-{o.cmode}.json")))
        report(res, o.arms)
    else:
        run(o)
