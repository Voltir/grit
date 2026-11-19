"""E0: the all-topics Choice's key-count limit and position bias, answer known by construction;
and the questions-per-request limit for YesNo.

python3 -I e0.py dry     # render 3 states
python3 -I e0.py run
"""
import json, random, sys, os, collections, time
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import common as c, jev

INSTR = "Which topic is this text part of?"
NEW = "A topic none of the others is: the text is about a different matter."
ASIDE = "Too slight to keep: chatter, a greeting or a test, about no matter the team will come back to."
NS = [5, 10, 20, 42, 69]


def targets():
    live = [c.latest(k) for k in c.live_keys()]
    live = [t for t in live if t["passages"]]
    live.sort(key=lambda t: -len(t["passages"][0]["text"]))
    return live[:20]


def state_for(t, n, shuffle, i, rng):
    others_live = [k for k in c.live_keys() if k != t["key"]]
    others_w = c.withdrawn_keys()
    rng.shuffle(others_live)
    rng.shuffle(others_w)
    pool = (others_live + others_w)[: n - 1]
    pos = (i * n) // 20 if shuffle == 0 else n - 1 - (i * n) // 20
    keys = pool[:pos] + [t["key"]] + pool[pos:]
    crit = {f"t{j+1:02d}": c.topic_text(c.latest(k)) for j, k in enumerate(keys)}
    crit["new"], crit["aside"] = NEW, ASIDE
    state = {"item": {"text": c.cut(t["passages"][0]["text"], 600)}}
    return state, [{"type": "choice", "instructions": INSTR, "criteria": crit}], f"t{pos+1:02d}", pos


def cases():
    ts = targets()
    for n in NS:
        for shuffle in (0, 1):
            for i, t in enumerate(ts):
                rng = random.Random(f"{n}/{shuffle}/{i}")
                s, q, want, pos = state_for(t, n, shuffle, i, rng)
                yield {"n": n, "shuffle": shuffle, "i": i, "key": t["key"], "want": want, "pos": pos}, s, q


def yesno_cases():
    ts = targets()
    for m in (5, 10, 20, 40):
        for i, t in enumerate(ts[:5]):
            rng = random.Random(f"yn/{m}/{i}")
            others = [k for k in c.live_keys() + c.withdrawn_keys() if k != t["key"]]
            rng.shuffle(others)
            keys = others[: m - 1]
            pos = rng.randrange(m)
            keys = keys[:pos] + [t["key"]] + keys[pos:]
            qs = [{"type": "noul", "instructions": "Is this text part of the topic: " + c.topic_text(c.latest(k)) + "?"}
                  for k in keys]
            yield {"m": m, "i": i, "pos": pos}, {"item": {"text": c.cut(t["passages"][0]["text"], 600)}}, qs


def run():
    rows = []
    for meta, s, q in cases():
        t0 = time.time()
        a, u = jev.ask("E0-choice", s, q)
        meta["secs"] = round(time.time() - t0, 2)
        meta["tokens"] = u.get("input_tokens", 0)
        if "_error" in a:
            meta["error"] = a["_error"]
        else:
            ch, p = jev.choice_probs(a["q1"])
            meta.update(choice=ch, p_sum=round(sum(p.values()), 4), p_want=p.get(meta["want"], 0),
                        p_top=max(p.values()) if p else None, confidence=a["q1"].get("confidence"),
                        nkeys_returned=len(p))
        rows.append(meta)
    yn = []
    for meta, s, q in yesno_cases():
        t0 = time.time()
        a, u = jev.ask("E0-yesno", s, q)
        meta["secs"] = round(time.time() - t0, 2)
        meta["tokens"] = u.get("input_tokens", 0)
        if "_error" in a:
            meta["error"] = a["_error"]
        else:
            ys = [jev.yes(a.get(f"q{j+1}", {"noul": float("nan")})) for j in range(meta["m"])]
            meta.update(answered=sum(1 for y in ys if y == y), top=max(range(len(ys)), key=lambda j: ys[j]),
                        y_want=ys[meta["pos"]], y_others_max=max(y for j, y in enumerate(ys) if j != meta["pos"]))
        yn.append(meta)
    with open(os.path.join(c.OUT, "e0.json"), "w") as f:
        json.dump({"choice": rows, "yesno": yn}, f, indent=0)
    report(rows, yn)


def report(rows, yn):
    print("N  calls errors top1  p_sum_range  tokens/call  secs/call")
    for n in NS:
        r = [x for x in rows if x["n"] == n]
        ok = [x for x in r if "error" not in x]
        acc = sum(x["choice"] == x["want"] for x in ok) / max(1, len(ok))
        ps = [x["p_sum"] for x in ok]
        print(n, len(r), len(r) - len(ok), f"{acc:.3f}", (min(ps), max(ps)) if ps else None,
              round(sum(x["tokens"] for x in r) / len(r)), round(sum(x["secs"] for x in r) / len(r), 2))
        first = [x for x in ok if x["pos"] < n / 3]
        last = [x for x in ok if x["pos"] >= 2 * n / 3]
        f_ = sum(x["choice"] == x["want"] for x in first) / max(1, len(first))
        l_ = sum(x["choice"] == x["want"] for x in last) / max(1, len(last))
        wrong = collections.Counter(x["choice"] if x["choice"] in ("new", "aside") else "other topic"
                                    for x in ok if x["choice"] != x["want"])
        print("   first third", len(first), f"{f_:.3f}", " last third", len(last), f"{l_:.3f}", " wrong:", dict(wrong))
    print("YesNo m calls errors answered_all top=true tokens secs")
    for m in (5, 10, 20, 40):
        r = [x for x in yn if x["m"] == m]
        ok = [x for x in r if "error" not in x]
        print(m, len(r), len(r) - len(ok), sum(x["answered"] == m for x in ok),
              sum(x["top"] == x["pos"] for x in ok), round(sum(x["tokens"] for x in r) / len(r)),
              round(sum(x["secs"] for x in r) / len(r), 2), [x.get("error", {}).get("detail", "")[:120] for x in r if "error" in x][:1])


if __name__ == "__main__":
    if sys.argv[1] == "dry":
        for k, (meta, s, q) in enumerate(cases()):
            if k in (0, 45, 170):
                print(meta, json.dumps(s, indent=1), json.dumps(q, indent=1)[:2500], sep="\n")
    elif sys.argv[1] == "report":
        d = json.load(open(os.path.join(c.OUT, "e0.json")))
        report(d["choice"], d["yesno"])
    else:
        run()
