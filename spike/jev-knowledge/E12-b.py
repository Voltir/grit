"""E12b: a kind-aware incremental sort of the room sheet's units, one Jev Choice per unit, measured
against Nick's labels.

Run: python3 -I E12-b.py run [arm ...]   (arms below; default all), then python3 -I E12-b.py report.
Aggregates go to out/e12b.json; per-unit placements (they quote text) to .local/jev-spike/e12/.
E12's own spend is capped at 15 cents, counted from spend.json's e12 tags."""
import collections
import json
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import e12sheet as s
import jev

CAP_CENTS = 15.0
ITEM_CHARS = 1500
FIRST_CHARS, NEWEST_CHARS = 360, 300  # with the header, a view is about 700 chars

# Instruction wordings tried. W1 is plain; W2 says what each kind is; W3 adds the follow-up rule.
WORDINGS = {
    "W1": "Which document does this conversation from a team's chat room belong to?",
    "W2": ("Which document does this conversation from a team's chat room belong to? A topic is an "
           "ongoing subject of work. A meeting is one planned occurrence: a call or get-together on a "
           "particular day. Talk that plans, moves, or reports on a specific meeting belongs to that "
           "meeting, not to the topic it will discuss."),
    "W3": ("Which document does this conversation from a team's chat room belong to? A topic is an "
           "ongoing subject of work. A meeting is one planned occurrence: a call or get-together on a "
           "particular day. Talk that plans, moves, or reports on a specific meeting belongs to that "
           "meeting, not to the topic it will discuss. A short reply usually continues what was "
           "being discussed just before it."),
}
OPT_KIND = {
    "new_topic": "A subject of work none of these documents covers, that the team is likely to come back to.",
    "new_meeting": "A planned meeting none of these documents is: a call or get-together on a particular day.",
    "aside": "Too slight to keep: chatter, a greeting, a joke, or nothing the team will come back to.",
}
OPT_PLAIN = {
    "new": "A matter none of these documents covers, that the team is likely to come back to.",
    "aside": OPT_KIND["aside"],
}

# arm: (gap minutes, wording, kinds?, mark, below-mark rule)
ARMS = {
    "g5-W1": (5, "W1", True, None, None),
    "g5-W2": (5, "W2", True, None, None),
    "g5-W3": (5, "W3", True, None, None),
    "g15-W2": (15, "W2", True, None, None),
    "g5-W2-nokind": (5, "W2", False, None, None),
    "g5-W2-m50-retry": (5, "W2", True, 0.5, "retry"),
    "g5-W2-m70-retry": (5, "W2", True, 0.7, "retry"),
    "g5-W2-m70-drop": (5, "W2", True, 0.7, "drop"),
}


def e12_cents():
    return sum(t.get("cents", 0) for k, t in jev.spent()["by_tag"].items() if k.startswith("e12"))


def cut(t, n):
    return t if len(t) <= n else t[: n - 1] + "…"


def render(msgs, n):
    return cut("\n".join(f"{m['when']:%m-%d %H:%M} {m['who']}: {m['text']}" for m in msgs), n)


def view(doc, kinds):
    days = sorted({m["when"].strftime("%m-%d") for u in doc["units"] for m in u})
    head = (f"{doc['kind']}; " if kinds else "") + "mentioned on " + ", ".join(days)
    first = doc["units"][0]
    v = head + "\nFirst: " + render(first, FIRST_CHARS)
    if len(doc["units"]) > 1:
        v += "\nNewest: " + render(doc["units"][-1], NEWEST_CHARS)
    return v


def ask(arm, docs, msgs):
    gap, wording, kinds, _, _ = ARMS[arm]
    crit = {f"d{i+1:02d}": view(d, kinds) for i, d in enumerate(docs)}
    crit.update(OPT_KIND if kinds else OPT_PLAIN)
    state = {"item": {"kind": "conversation", "said": msgs[0]["when"].strftime("%Y-%m-%d"),
                      "text": render(msgs, ITEM_CHARS)}}
    if e12_cents() >= CAP_CENTS:
        raise SystemExit(f"E12 cap reached: {e12_cents():.3f} cents")
    a, usage = jev.ask("e12b", state, [{"type": "choice", "instructions": WORDINGS[wording], "criteria": crit}])
    if "_error" in a:
        raise SystemExit(f"Jev error: {a['_error']}")
    pick, probs = jev.choice_probs(a["q1"])
    return pick, probs.get(pick, 0.0), usage.get("input_tokens", 0)


def replay(arm):
    gap, _, kinds, mark, rule = ARMS[arm]
    us = s.units(s.rows(), gap)
    docs, placed, log = [], {}, []  # placed: message n -> doc index or None (aside)
    tokens = calls = 0
    pending = None  # (msgs, unit ids) held below the mark, joined to the next unit
    for u in us:
        msgs, ids, retried = list(u["msgs"]), [u["id"]], False
        if pending is not None:
            msgs, ids, retried = pending[0] + msgs, pending[1] + ids, True
            msgs.sort(key=lambda m: m["n"])
            pending = None
        pick, p, tok = ask(arm, docs, msgs)
        tokens, calls = tokens + tok, calls + 1
        if mark is not None and p < mark and not retried:
            if rule == "retry":
                pending = (msgs, ids)
                log.append({"units": ids, "pick": pick, "p": p, "held": True})
                continue
            pick = "aside"  # drop
        if pick in ("new_topic", "new_meeting", "new"):
            docs.append({"kind": "meeting" if pick == "new_meeting" else "topic", "units": [msgs]})
            d = len(docs) - 1
        elif pick == "aside":
            d = None
        else:
            d = int(pick[1:]) - 1
            docs[d]["units"].append(msgs)
        for m in msgs:
            placed[m["n"]] = d
        log.append({"units": ids, "pick": pick, "p": round(p, 3), "doc": d, "retry": retried,
                    "msgs": [m["n"] for m in msgs]})
    if pending is not None:  # the last unit held: nothing follows, so it stays set aside
        for m in pending[0]:
            placed[m["n"]] = None
    return {"docs": [{"kind": d["kind"], "msgs": [m["n"] for u in d["units"] for m in u]} for d in docs],
            "placed": placed, "log": log, "tokens": tokens, "calls": calls}


def ari(a, b):
    from math import comb
    n = len(a)
    cont = collections.Counter(zip(a, b))
    sa, sb = collections.Counter(a), collections.Counter(b)
    idx = sum(comb(v, 2) for v in cont.values())
    ea, eb = sum(comb(v, 2) for v in sa.values()), sum(comb(v, 2) for v in sb.values())
    exp = ea * eb / comb(n, 2)
    mx = (ea + eb) / 2
    return (idx - exp) / (mx - exp) if mx != exp else 1.0


def measure(r):
    msgs = s.rows()
    lab = {m["n"]: m["label"] for m in msgs}
    placed = {int(k): v for k, v in r["placed"].items()}
    labelled = [m for m in msgs if m["label"] != s.CHATTER]
    clus = [placed[m["n"]] if placed[m["n"]] is not None else f"aside{m['n']}" for m in labelled]
    truth = [m["label"] for m in labelled]
    n = len(labelled)
    by_c = collections.defaultdict(list)
    for c, t in zip(clus, truth):
        by_c[c].append(t)
    by_t = collections.defaultdict(list)
    for c, t in zip(clus, truth):
        by_t[t].append(c)
    pur = sum(collections.Counter(v).most_common(1)[0][1] for v in by_c.values()) / n
    inv = sum(collections.Counter(v).most_common(1)[0][1] for v in by_t.values()) / n
    major = {}
    for i, d in enumerate(r["docs"]):
        ls = [lab[k] for k in d["msgs"] if lab[k] != s.CHATTER]
        major[i] = collections.Counter(ls).most_common(1)[0][0] if ls else s.CHATTER
    meet = [m for m in msgs if m["label"].startswith("meeting")]
    meet_docs = collections.defaultdict(set)
    in_topic = in_aside = 0
    for m in meet:
        d = placed[m["n"]]
        if d is None:
            in_aside += 1
        elif r["docs"][d]["kind"] == "meeting":
            meet_docs[m["label"]].add(d)
        else:
            in_topic += 1
    # a meeting document per occurrence: occurrences whose messages in meeting documents share one
    # document no other occurrence uses
    own = {o: ds for o, ds in meet_docs.items()}
    clean = sum(1 for o, ds in own.items() if len(ds) == 1 and all(ds.isdisjoint(v) for p, v in own.items() if p != o))
    meeting_docs = [i for i, d in enumerate(r["docs"]) if d["kind"] == "meeting"]
    meeting_doc_nonmeeting_msgs = sum(1 for i in meeting_docs for k in r["docs"][i]["msgs"]
                                      if not lab[k].startswith("meeting") and lab[k] != s.CHATTER)
    chatter = [m for m in msgs if m["label"] == s.CHATTER]
    short = [m for m in msgs if m["where"] == "room" and m["label"] != s.CHATTER and len(m["text"]) < 150]
    short_ok = sum(1 for m in short if placed[m["n"]] is not None and major[placed[m["n"]]] == m["label"])
    all_ok = sum(1 for m in labelled if placed[m["n"]] is not None and major[placed[m["n"]]] == m["label"])
    return {"docs opened": len(r["docs"]), "meeting docs": len(meeting_docs),
            "ARI": round(ari(clus, truth), 3), "purity": round(pur, 3), "inverse purity": round(inv, 3),
            "labelled placed under their label's majority doc": f"{all_ok}/{n}",
            "labelled set aside": sum(1 for m in labelled if placed[m["n"]] is None),
            "meeting msgs in a meeting doc": f"{len(meet) - in_topic - in_aside}/{len(meet)}",
            "meeting msgs in a topic doc": in_topic, "meeting msgs set aside": in_aside,
            "meeting occurrences with a meeting doc of their own": f"{clean}/3",
            "meeting occurrences -> meeting docs": {o: sorted(ds) for o, ds in sorted(own.items())},
            "non-meeting labelled msgs in meeting docs": meeting_doc_nonmeeting_msgs,
            "chatter set aside": f"{sum(1 for m in chatter if placed[m['n']] is None)}/{len(chatter)}",
            "short top-level right": f"{short_ok}/{len(short)}",
            "calls": r["calls"], "tokens": r["tokens"],
            "held below mark": sum(1 for e in r["log"] if e.get("held")),
            "low p picks (<0.7)": sum(1 for e in r["log"] if not e.get("held") and e["p"] < 0.7)}


def main():
    cmd = sys.argv[1] if len(sys.argv) > 1 else "report"
    os.makedirs(s.OUT, exist_ok=True)
    if cmd == "run":
        for arm in sys.argv[2:] or list(ARMS):
            r = replay(arm)
            with open(os.path.join(s.OUT, f"e12b-{arm}.json"), "w") as f:
                json.dump(r, f, indent=1, default=str)
            print(arm, json.dumps(measure(r)), f"e12 spend {e12_cents():.3f}c", flush=True)
    res = {}
    for arm in ARMS:
        p = os.path.join(s.OUT, f"e12b-{arm}.json")
        if os.path.exists(p):
            with open(p) as f:
                res[arm] = measure(json.load(f))
    res["_e12 cents"] = round(e12_cents(), 4)
    with open(os.path.join(HERE, "out", "e12b.json"), "w") as f:
        json.dump(res, f, indent=1)
    if cmd == "report":
        print(json.dumps(res, indent=1))


if __name__ == "__main__":
    main()
