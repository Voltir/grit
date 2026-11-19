"""E11: do many options or short, vague items make Jev miscategorise?

1. (cached, no calls) E0, E2 and E3/E4 accuracy by the item's text length (<150, 150-400,
   400-800, >800 chars) and by the number of keys shown.
2. SELF, CONT and HIST-new with arm (a) (topics as lead + newest passage head, E10 view i) over
   a shortlist of k = 16, 32, 64 by E9's rule: structural (<= 16) first, then the 8 newest, then
   BM25 over lead + passages fills; and the candidate rule for short items: structural + the
   text top 16, a pick under p 0.86 set aside.
python3 -I e11.py cached | run
Writes out/e11.json (numbers only)."""
import json, os, sys, collections
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import common as c, items as I, jev, e2, e9, e9b, e10

BUCKETS = [(0, 150, "<150"), (150, 400, "150-400"), (400, 800, "400-800"), (800, 10 ** 9, ">800")]
MARK = 0.86


def bucket(n):
    return next(name for lo, hi, name in BUCKETS if lo <= n < hi)


def kbucket(n):
    return "<=10" if n <= 10 else "11-20" if n <= 20 else "21-42" if n <= 42 else ">42"


def cached():
    out = {}
    # E0: passages cut at 600 against N keys.
    e0 = json.load(open(os.path.join(c.OUT, "e0.json")))["choice"]
    t = collections.defaultdict(lambda: [0, 0])
    for x in e0:
        n = len(c.cut(c.latest(x["key"])["passages"][0]["text"], 600))
        for key in (("E0", "len", bucket(n)), ("E0", "keys", str(x["n"]))):
            t[key][0] += x["choice"] == x["want"]
            t[key][1] += 1
    # E2 / E3-E4: by item text as each variant showed it.
    items = {e2.rid(it): it for it in I.l1_strands() + I.l1_cards() + I.self_set()}
    for fname in sorted(os.listdir(c.OUT)):
        if not fname.startswith("e2-"):
            continue
        variant = fname[3:-5]
        tmode, smode, cmode = variant.split("-", 2) if variant.count("-") >= 2 else (variant, "whole", "")
        res = json.load(open(os.path.join(c.OUT, fname)))
        for rid, r in res.items():
            it = items.get(rid)
            if not it:
                continue
            n = len(c.item_text(it, smode if smode in ("whole", "cut800", "ends") else "whole",
                                cmode if cmode in ("title", "title+summary") else "title+summary"))
            for arm, a in r["arms"].items():
                if "error" in a or arm != "a":
                    continue
                ok = a["place"] == r["label"]
                for key in ((f"{variant} arm a {r['set']}", "len", bucket(n)),
                            (f"{variant} arm a {r['set']}", "keys", kbucket(r["shown"]))):
                    t[key][0] += ok
                    t[key][1] += 1
                if r["set"] == "SELF" and not ok:
                    t[(f"{variant} arm a SELF wrong", "confident", "p>=0.86" if a["top"] >= MARK else "p<0.86")][0] += 1
                    t[(f"{variant} arm a SELF wrong", "confident", "p>=0.86" if a["top"] >= MARK else "p<0.86")][1] += 1
    for key in sorted(t):
        print(" | ".join(key), f"{t[key][0]}/{t[key][1]}")
    return {" | ".join(k): v for k, v in t.items()}


def shortlist(case, real, k, rule):
    b = e9.rank_b(case, real)[:16]
    text = e9.rank_a(case, real, [], "lead+passages")
    if rule == "short":
        return e9b.fill(min(len(real), len(b) + 16), b, text)
    rec = e9b.recency(list(real), case.get("day") or case["when"], real)
    head = [x for x in rec if x not in b][:8]
    return e9b.fill(k, b + head, text)


def run():
    rows = []
    stats = collections.Counter()
    for cs in e10.cases():
        real = e9.topics_at(cs["when"], cs["override"])
        if cs["right"] not in real:
            continue
        for k, rule in ((16, "e9"), (32, "e9"), (64, "e9"), (None, "short")):
            keys = shortlist(cs, real, k, rule)
            crit = {f"t{j+1:02d}": e10.view(real[kk], "i", cs["when"], None, stats) for j, kk in enumerate(keys)}
            back = {f"t{j+1:02d}": kk for j, kk in enumerate(keys)}
            crit["new"], crit["aside"] = e2.NEW, e2.ASIDE
            state = {"item": {"kind": "discussion", "said": e10.item_said(cs), "text": cs["text"]}}
            a, u = jev.ask(f"E11-{rule}-{k}", state,
                           [{"type": "choice", "instructions": "Which topic is this discussion part of?", "criteria": crit}])
            if "_error" in a:
                continue
            ch, p = jev.choice_probs(a["q1"])
            rows.append({"set": cs["set"], "len": len(cs["text"]), "k": k, "rule": rule, "keys": len(keys),
                         "in_list": cs["right"] in keys, "got_right": back.get(ch) == cs["right"],
                         "got": "topic" if ch in back else ch, "p": p.get(ch), "tokens": u.get("input_tokens", 0)})
    json.dump(rows, open(os.path.join(c.OUT, "e11.json"), "w"), indent=0)
    report(rows)


def report(rows):
    for size in ("short", "long"):
        rs = [r for r in rows if (r["len"] < 400) == (size == "short")]
        print(f"== {size} items (<400 chars: {size == 'short'})")
        for (k, rule) in ((16, "e9"), (32, "e9"), (64, "e9"), (None, "short")):
            for s in ("SELF", "CONT+HIST-new"):
                xs = [r for r in rs if r["k"] == k and r["rule"] == rule and (r["set"] == s or (s != "SELF" and r["set"] != "SELF"))]
                if not xs:
                    continue
                ok = sum(r["got_right"] for r in xs)
                wrong = [r for r in xs if not r["got_right"]]
                conf = sum(r["p"] >= MARK for r in wrong)
                # The candidate rule: a pick under the mark is set aside, not placed or opened.
                placed_right = sum(r["got_right"] and r["p"] >= MARK for r in xs)
                placed_wrong = sum(not r["got_right"] and r["p"] >= MARK and r["got"] == "topic" for r in xs)
                opened = sum(r["got"] == "new" and r["p"] >= MARK for r in xs)
                aside = len(xs) - placed_right - placed_wrong - opened
                label = f"k={k}" if rule == "e9" else "short rule (b + text top 16)"
                print(f"  {label:28s} {s:14s} n={len(xs):2d} in list {sum(r['in_list'] for r in xs)}  right {ok}  "
                      f"wrong {len(wrong)} (p>=0.86: {conf}, p<0.86: {len(wrong) - conf})  | under the mark-asides rule: "
                      f"right {placed_right}, wrong topic {placed_wrong}, new {opened}, aside {aside}  "
                      f"keys~{sum(r['keys'] for r in xs) / len(xs):.0f} tokens~{sum(r['tokens'] for r in xs) / len(xs):.0f}")


if __name__ == "__main__":
    if sys.argv[1] == "cached":
        json.dump(cached(), open(os.path.join(c.OUT, "e11-cached.json"), "w"), indent=0)
    elif sys.argv[1] == "report":
        report(json.load(open(os.path.join(c.OUT, "e11.json"))))
    else:
        run()
