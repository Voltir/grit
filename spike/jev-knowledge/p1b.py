"""P1b: can Jev tell when the message alone is a good enough query?

Label (from P1, findable candidates): "suffices" when message-as-query recall@30 of the
Jev-passed candidates is >= 0.9, else "needs a written query". Jev is asked, in the ask:needs
state shape {"message", "thread"} (TurnNeeds.State: the message under its speaker's name, its
first 4,000 chars; the thread before it, its last 4,000), one YesNo, in two phrasings. Free
baselines: message length, and the share of words that are pronouns or deictic.

python3 -I p1b.py dry | run"""
import json, os, sys, re
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import db, common as c, jev, p2, e7

PHRASINGS = {
    "sketch": ("Does the newest message say on its own what it is about, so a search for its own "
               "words would find what it needs? No when it leans on the conversation, with words "
               "like 'that', 'it', 'the thing above' or 'same as before'."),
    "short": ("Read message. Would a search using only the words of message find the documents and "
              "past discussions a reply needs, without the thread to say what it refers to?"),
}
DEICTIC = set("it its this that these those they them their there here he she him her his above "
              "before same earlier previous last one ones such which what so".split())


def thread_of(t):
    rows = db.rows(f"select id, payload::text from grit.entries where conversation_id='{t['conv']}' "
                   f"and turn_seq < {t['seq']} order by seq")
    out = []
    for eid, p in rows:
        p = json.loads(p)
        if p["kind"] == "heard":
            out.append(f"{p2.names().get(eid, 'Someone')}: {p['text']}")
        elif p["kind"] == "message" and p["message"].get("role") == "user":
            out.append(f"{p2.names().get(eid, 'Someone')}: {p['message'].get('text', '')}")
        elif p["kind"] == "message" and p["message"].get("role") == "assistant":
            x = "".join(b.get("text", "") for b in p["message"].get("blocks", []) if b.get("type") == "text")
            if x:
                out.append(f"Assistant: {x}")
    return "\n".join(out)[-4000:]


def cases():
    rows = json.load(open(os.path.join(c.OUT, "p1-findable.json")))
    turns = {t["wid"]: t for t in json.load(open(os.path.join(c.DATA, "turns.json")))}
    for r in rows:
        t = turns[r["wid"]]
        h, n = r["message"]["r@30"]
        yield r["wid"], t, h / n >= 0.9


def baseline(msg):
    words = re.findall(r"[a-z']+", msg.split(":", 1)[-1].lower())
    return len(msg), (sum(w in DEICTIC for w in words) / len(words)) if words else 1.0


def main(dry):
    out = []
    for k, (wid, t, suffices) in enumerate(cases()):
        state = {"message": t["message"][:4000], "thread": thread_of(t)}
        if dry:
            if k < 3:
                print(json.dumps(state, indent=1)[:1500], "suffices:", suffices)
            continue
        row = {"wid": wid, "suffices": suffices, "len": baseline(t["message"])[0],
               "deictic": baseline(t["message"])[1]}
        for name, q in PHRASINGS.items():
            a, u = jev.ask(f"P1b-{name}", state, [{"type": "noul", "instructions": q}])
            row[name] = None if "_error" in a else jev.yes(a["q1"])
        out.append(row)
    if dry:
        return
    json.dump(out, open(os.path.join(c.OUT, "p1b.json"), "w"), indent=0)
    n_s = sum(r["suffices"] for r in out)
    print(f"turns {len(out)}: suffices {n_s}, needs a written query {len(out) - n_s}")
    for sig, f in (("jev sketch", lambda r: r["sketch"]), ("jev short", lambda r: r["short"]),
                   ("length (longer = stands alone)", lambda r: r["len"]),
                   ("deictic share (lower = stands alone)", lambda r: -r["deictic"])):
        pairs = [(f(r), r["suffices"]) for r in out if f(r) is not None]
        au = e7.auroc(pairs)
        # The threshold keeping the written query for >= 90% of the needs turns: the call is
        # skipped only on a score strictly above the 90th-percentile needs turn's.
        ranked_needs = sorted((s for s, y in pairs if not y))
        t90 = ranked_needs[-(-9 * len(ranked_needs) // 10) - 1] if ranked_needs else None
        skip = [s for s, _ in pairs if s > t90]
        kept_needs = sum(1 for s, y in pairs if not y and s <= t90)
        print(f"  {sig:38s} AUROC {au:.3f}  threshold {t90}  keeps the written query for "
              f"{kept_needs}/{len(ranked_needs)} needs turns, skips the call on {len(skip)}/{len(pairs)} turns")


if __name__ == "__main__":
    main(sys.argv[1] == "dry")
