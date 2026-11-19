"""E13b: CP5's stage 0. Each arm's units (e13sheet.ARMS) replayed oldest first through one Jev Choice
under E12's W2 wording, over every document opened so far plus new_topic, new_meeting and aside,
on both sheets, measured against their labels.

Run: python3 -I E13-b.py units            (code only: unit counts and the oracle per arm)
     python3 -I E13-b.py run SHEET ARM ... (Jev), then python3 -I E13-b.py report.
Aggregates go to out/e13b.json; per-unit placements (they quote text) to .local/jev-spike/e13/.
E13's spend is capped at 10 cents, counted from spend.json's e13 tags; a call that could pass the
cap is not made.

A unit without context is asked exactly as E12 asked it, so E12's cached answers serve it. A unit
with context carries it in the state as `before` and `after`, each marked as context and not part
of the conversation; the question's wording stays W2's."""
import collections
import json
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import e13sheet as s
import jev

CAP_CENTS = 10.0
ITEM_CHARS = 1500
CONTEXT_CHARS = 600
FIRST_CHARS, NEWEST_CHARS = 360, 300
W2 = ("Which document does this conversation from a team's chat room belong to? A topic is an "
      "ongoing subject of work. A meeting is one planned occurrence: a call or get-together on a "
      "particular day. Talk that plans, moves, or reports on a specific meeting belongs to that "
      "meeting, not to the topic it will discuss.")
OPT_KIND = {
    "new_topic": "A subject of work none of these documents covers, that the team is likely to come back to.",
    "new_meeting": "A planned meeting none of these documents is: a call or get-together on a particular day.",
    "aside": "Too slight to keep: chatter, a greeting, a joke, or nothing the team will come back to.",
}
CONTEXT_NOTE = {"before": "Messages said in the room just before the conversation, shown only to help read it; not part of it.",
                "after": "Messages said in the room just after the conversation, shown only to help read it; not part of it."}


def e13_cents():
    return sum(t.get("cents", 0) for k, t in jev.spent()["by_tag"].items() if k.startswith("e13"))


def cut(t, n):
    return t if len(t) <= n else t[: n - 1] + "…"


def render(msgs, n):
    return cut("\n".join(f"{m['when']:%m-%d %H:%M} {m['who']}: {m['text']}" for m in msgs), n)


def view(doc):
    days = sorted({m["when"].strftime("%m-%d") for u in doc["units"] for m in u})
    v = f"{doc['kind']}; mentioned on " + ", ".join(days) + "\nFirst: " + render(doc["units"][0], FIRST_CHARS)
    if len(doc["units"]) > 1:
        v += "\nNewest: " + render(doc["units"][-1], NEWEST_CHARS)
    return v


def ask(docs, u):
    crit = {f"d{i+1:02d}": view(d) for i, d in enumerate(docs)}
    crit.update(OPT_KIND)
    item = {"kind": "conversation", "said": u["msgs"][0]["when"].strftime("%Y-%m-%d"),
            "text": render(u["msgs"], ITEM_CHARS)}
    if u["before"] or u["after"]:
        state = {}
        if u["before"]:
            state["before"] = {"kind": "context", "note": CONTEXT_NOTE["before"], "text": render(u["before"], CONTEXT_CHARS)}
        state["item"] = item
        if u["after"]:
            state["after"] = {"kind": "context", "note": CONTEXT_NOTE["after"], "text": render(u["after"], CONTEXT_CHARS)}
    else:
        state = {"item": item}
    q = {"type": "choice", "instructions": W2, "criteria": crit}
    est = len(json.dumps(state)) + len(json.dumps(q))
    if e13_cents() + est / 3.0 * jev.USD_PER_M / 1e4 >= CAP_CENTS:
        raise SystemExit(f"E13 cap would be passed: {e13_cents():.3f} cents spent")
    # e12b is the tag E12 asked under; a context-free request is E12's own and is served from the cache.
    a, usage = jev.ask("e13b", state, [q])
    if "_error" in a:
        raise SystemExit(f"Jev error: {a['_error']}")
    pick, probs = jev.choice_probs(a["q1"])
    return pick, probs.get(pick, 0.0), usage.get("input_tokens", 0)


def replay(sheet, arm):
    msgs = s.rows(sheet)
    us = s.units(msgs, arm)
    docs, placed, log = [], {}, []
    tokens = calls = 0
    for u in us:
        pick, p, tok = ask(docs, u)
        tokens, calls = tokens + tok, calls + 1
        if pick in ("new_topic", "new_meeting"):
            docs.append({"kind": "meeting" if pick == "new_meeting" else "topic", "units": [u["msgs"]], "unit_ids": [u["id"]]})
            d = len(docs) - 1
        elif pick == "aside":
            d = None
        else:
            d = int(pick[1:]) - 1
            docs[d]["units"].append(u["msgs"])
            docs[d]["unit_ids"].append(u["id"])
        for m in u["msgs"]:
            placed[m["n"]] = d
        log.append({"unit": u["id"], "pick": pick, "p": round(p, 3), "doc": d, "msgs": [m["n"] for m in u["msgs"]],
                    "context": [m["n"] for m in u["before"] + u["after"]]})
    return {"docs": [{"kind": d["kind"], "units": len(d["units"]), "msgs": [m["n"] for x in d["units"] for m in x]} for d in docs],
            "placed": placed, "log": log, "tokens": tokens, "calls": calls}


def frac(a, b):
    return f"{a}/{b}"


def score(msgs, r, keep):
    """The scores over the messages keep admits."""
    lab = {m["n"]: m["label"] for m in msgs}
    placed = {int(k): v for k, v in r["placed"].items()}
    major = {}
    for i, d in enumerate(r["docs"]):
        ls = [lab[k] for k in d["msgs"] if lab[k] != s.CHATTER]
        major[i] = collections.Counter(ls).most_common(1)[0][0] if ls else s.CHATTER
    ms = [m for m in msgs if keep(m)]

    def right(m):
        return placed[m["n"]] is not None and major[placed[m["n"]]] == m["label"]
    labelled = [m for m in ms if m["label"] != s.CHATTER]
    top = [m for m in labelled if m["top"]]
    short = [m for m in top if len(m["text"]) < 150]
    chatter = [m for m in ms if m["label"] == s.CHATTER]
    meet = [m for m in ms if m["label"].startswith("meeting")]
    occ = collections.defaultdict(set)
    for m in meet:
        d = placed[m["n"]]
        if d is not None and r["docs"][d]["kind"] == "meeting":
            occ[m["label"]].add(d)
    occs = sorted({m["label"] for m in meet})
    own = sum(1 for o in occs if len(occ[o]) == 1 and all(occ[o].isdisjoint(occ[p]) for p in occs if p != o))
    docs = {placed[m["n"]] for m in ms if placed[m["n"]] is not None}
    return {"labelled right": frac(sum(map(right, labelled)), len(labelled)),
            "top-level labelled right": frac(sum(map(right, top)), len(top)),
            "short right": frac(sum(map(right, short)), len(short)),
            "meeting msgs in a meeting doc": frac(sum(1 for m in meet if placed[m["n"]] is not None and r["docs"][placed[m["n"]]]["kind"] == "meeting"), len(meet)),
            "occurrences in their own meeting doc": frac(own, len(occs)),
            "chatter set aside": frac(sum(1 for m in chatter if placed[m["n"]] is None), len(chatter)),
            "docs holding these msgs": len(docs),
            "one-unit docs among them": sum(1 for d in docs if r["docs"][d]["units"] == 1),
            "forward-defined right": frac(sum(1 for m in ms if m["n"] in s.FORWARD.get(r["sheet"], []) and right(m)),
                                          sum(1 for m in ms if m["n"] in s.FORWARD.get(r["sheet"], [])))}


def measure(sheet, r):
    msgs = s.rows(sheet)
    r["sheet"] = sheet
    res = {"docs opened": len(r["docs"]), "meeting docs": sum(d["kind"] == "meeting" for d in r["docs"]),
           "one-unit docs": sum(1 for d in r["docs"] if d["units"] == 1),
           "units": len(r["log"]), "sorts per message": round(r["calls"] / len(msgs), 3),
           "calls": r["calls"], "tokens": r["tokens"],
           "all": score(msgs, r, lambda m: True)}
    if sheet == "s2":
        res["stream A (team room)"] = score(msgs, r, lambda m: m["stream"] == "A")
        res["stream B (bot tests)"] = score(msgs, r, lambda m: m["stream"] == "B")
    return res


def oracle(sheet, arm):
    """Code only: units, and labelled messages under their unit's majority label (a perfect sort)."""
    msgs = s.rows(sheet)
    us = s.units(msgs, arm)
    ok = n = 0
    for u in us:
        ls = s.labels_of(u)
        if ls:
            ok += collections.Counter(ls).most_common(1)[0][1]
            n += len(ls)
    top = [m for m in msgs if m["top"]]
    return {"units": len(us), "runs": sum(u["kind"] == "run" for u in us), "threads": sum(u["kind"] == "thread" for u in us),
            "largest run": max(len(u["msgs"]) for u in us if u["kind"] == "run"),
            "top-level msgs": len(top), "oracle labelled right": frac(ok, n),
            "pure labelled units": frac(sum(1 for u in us if s.labels_of(u) and len(set(s.labels_of(u))) == 1),
                                        sum(1 for u in us if s.labels_of(u)))}


def differ(a, b):
    """Units whose answer differs: aside, or the document (named by its first message) joined or opened."""
    def key(r):
        first = {i: d["msgs"][0] for i, d in enumerate(r["docs"])}
        return {e["unit"]: ("aside" if e["doc"] is None else first[e["doc"]]) for e in r["log"]}
    ka, kb = key(a), key(b)
    return sum(1 for u in ka if ka[u] != kb.get(u))


def followed(sheet, arm):
    """Of the units shown context that joined an existing document: how many joined one holding a
    context message, and how many of those joined wrong (the unit's majority label is not the
    document's)."""
    msgs = s.rows(sheet)
    lab = {m["n"]: m["label"] for m in msgs}
    with open(os.path.join(s.OUT, f"e13b-{sheet}-{arm}.json")) as f:
        r = json.load(f)
    placed = {int(k): v for k, v in r["placed"].items()}
    major = {}
    for i, d in enumerate(r["docs"]):
        ls = [lab[k] for k in d["msgs"] if lab[k] != s.CHATTER]
        major[i] = collections.Counter(ls).most_common(1)[0][0] if ls else s.CHATTER
    joined = with_ctx = wrong = wrong_other = 0
    for e in r["log"]:
        if not e["context"] or e["doc"] is None or e["pick"].startswith("new"):
            continue
        joined += 1
        ls = [lab[k] for k in e["msgs"] if lab[k] != s.CHATTER]
        bad = bool(ls) and collections.Counter(ls).most_common(1)[0][0] != major[e["doc"]]
        if any(placed.get(c) == e["doc"] for c in e["context"]):
            with_ctx += 1
            wrong += bad
        else:
            wrong_other += bad
    return {"joined": joined, "joined a doc holding a context msg": with_ctx, "of those, wrong": wrong,
            "joined another doc, wrong": wrong_other}


def main():
    cmd = sys.argv[1] if len(sys.argv) > 1 else "report"
    os.makedirs(s.OUT, exist_ok=True)
    if cmd == "units":
        print(json.dumps({sh: {a: oracle(sh, a) for a in s.ARMS} for sh in s.SHEETS}, indent=1))
        return
    if cmd == "repeat":
        # Asks again, past the cache, and writes the fresh answers to a separate cache, so E12's
        # and the first run's answers stay what later runs are served.
        jev._CACHED, jev.CACHE = {}, os.path.join(s.OUT, "repeat-cache.jsonl")
        sheet = sys.argv[2]
        for arm in sys.argv[3:]:
            r = replay(sheet, arm)
            with open(os.path.join(s.OUT, f"e13b-{sheet}-{arm}-repeat.json"), "w") as f:
                json.dump(r, f, indent=1, default=str)
            print(sheet, arm, "repeat", json.dumps(measure(sheet, r)["all"]), f"e13 spend {e13_cents():.3f}c", flush=True)
        return
    if cmd == "run":
        sheet = sys.argv[2]
        for arm in sys.argv[3:] or list(s.ARMS):
            r = replay(sheet, arm)
            with open(os.path.join(s.OUT, f"e13b-{sheet}-{arm}.json"), "w") as f:
                json.dump(r, f, indent=1, default=str)
            print(sheet, arm, json.dumps(measure(sheet, r)["all"]), f"e13 spend {e13_cents():.3f}c", flush=True)
    res = {}
    for sh in s.SHEETS:
        for arm in s.ARMS:
            p = os.path.join(s.OUT, f"e13b-{sh}-{arm}.json")
            if os.path.exists(p):
                with open(p) as f:
                    res[f"{sh} {arm}"] = measure(sh, json.load(f))
                res[f"{sh} {arm}"]["cut"] = oracle(sh, arm)
    for sh in s.SHEETS:
        for arm in s.ARMS:
            p = os.path.join(s.OUT, f"e13b-{sh}-{arm}-repeat.json")
            if os.path.exists(p):
                with open(p) as f:
                    rr = json.load(f)
                with open(os.path.join(s.OUT, f"e13b-{sh}-{arm}.json")) as f:
                    r0 = json.load(f)
                res[f"{sh} {arm} repeat"] = measure(sh, rr)
                res[f"{sh} {arm} repeat"]["picks differing from the first run"] = differ(r0, rr)
    res["context followed"] = {f"{sh} {arm}": followed(sh, arm) for sh in s.SHEETS for arm in s.ARMS
                               if s.ARMS[arm]["before"] + s.ARMS[arm]["after"]
                               and os.path.exists(os.path.join(s.OUT, f"e13b-{sh}-{arm}.json"))}
    res["_e13 cents"] = round(e13_cents(), 4)
    with open(os.path.join(HERE, "out", "e13b.json"), "w") as f:
        json.dump(res, f, indent=1)
    if cmd == "report":
        print(json.dumps(res, indent=1))


if __name__ == "__main__":
    main()
