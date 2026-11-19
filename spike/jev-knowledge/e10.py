"""E10: should each topic in the Choice show its last two messages and when they were said?

1. limit: one request of about 30k tokens and one of about 40k (card text as the state), to see
   what Jev does past its reported 32k context.
2. views, arm (a) Choice over every topic existing at the case's time + new + aside:
   i   lead + newest passage head (300)
   ii  lead + the last 2 messages of the topic's newest strand, each "yyyy-mm-dd Speaker: text"
   iii lead + passage head + the last 2 dated messages
   A topic whose messages are purged (or are all the discussion's own, on SELF) falls back to i.
   The discussion carries its date too ({"item": {"kind", "said", "text"}}).
python3 -I e10.py limit | dry | run
Writes out/e10.json (numbers only)."""
import json, os, sys, collections, datetime
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import db, common as c, jev, e9, e2, items as I

VIEWS = ["i", "ii", "iii"]
# `leaky`: SELF's topics show their last messages even when they are the discussion's own.
LEAKY = "leaky" in sys.argv
if LEAKY:
    VIEWS = ["ii", "iii"]


def limit():
    bodies = [b for [b] in db.rows("select body from grit.documents where plugin='knowledge' and body is not null order by version")]
    bodies += [t for [t] in db.rows("select search_text from grit.pieces order by id")]
    big = "\n\n".join(bodies)
    out = {}
    for name, chars in (("~30k", 128_000), ("~40k", 168_000), ("~60k", 250_000)):
        text = big[:chars]
        a, u = jev.ask(f"E10-limit-{name}", {"item": {"text": text}},
                       [{"type": "noul", "instructions": "Does this text mention a README?"}])
        out[name] = {"chars": len(text), "answer": a if "_error" in a else {"noul": a["q1"].get("noul")},
                     "input_tokens": u.get("input_tokens")}
        print(name, json.dumps(out[name])[:600])
    return out


_LINES = {}


def lines_before(root, when):
    """(day, speaker, text) of a strand's messages said before `when`, oldest first."""
    key = (root, when)
    if key in _LINES:
        return _LINES[key]
    convs = [root] + [x for [x] in db.rows(f"select distinct conversation_id from grit.stitches where root='{root}' and kind='follows'")]
    names = e9.p1_names()
    rows = db.rows("select id, created_at::text, payload::text from grit.entries where conversation_id in ("
                   + ",".join(f"'{x}'" for x in convs) + f") and created_at < '{when.isoformat()}'::timestamptz "
                   "and payload->>'kind' in ('heard','message','posted') order by created_at")
    out = []
    for eid, at, p in rows:
        p = json.loads(p)
        if p["kind"] == "heard":
            out.append((at[:10], names.get(eid, "Someone"), p["text"], eid))
        elif p["kind"] == "posted":
            out.append((at[:10], "Assistant", p.get("text", ""), eid))
        elif p["message"].get("role") == "user":
            out.append((at[:10], names.get(eid, "Someone"), p["message"].get("text", ""), eid))
        elif p["message"].get("role") == "assistant":
            t = "".join(b.get("text", "") for b in p["message"].get("blocks", []) if b.get("type") == "text")
            if t:
                out.append((at[:10], "Assistant", t, eid))
    _LINES[key] = out
    return out


def view(x, mode, when, exclude_root, stats):
    lead = c.cut(c.TOPICS[x["version"]]["lead"], 400) if x["version"] in c.TOPICS else ""
    head = ("\nLatest: " + c.cut(x["passages_text"][0], 300)) if x["passages_text"] else ""
    if mode == "i":
        return lead + head
    newest = [f["strand"] for f in x["passages"][0].get("from", [])] if x["passages"] else []
    msgs = []
    for root in newest:
        if root == exclude_root:
            continue
        msgs += lines_before(root, when or x["written"] + datetime.timedelta(days=1))
    msgs = sorted(msgs)[-2:]
    if not msgs:
        stats["fallback"] += 1
        return lead + head
    stats["with messages"] += 1
    last = "\nLast messages:\n" + "\n".join(f"{d} {w}: {c.cut(t, 300)}" for d, w, t, _ in msgs)
    return lead + (head if mode == "iii" else "") + last


def cases():
    out = []
    for cs in e9.cases():
        if cs["set"] not in ("SELF", "CONT", "HIST-new"):
            continue
        out.append(cs)
    return out


def item_said(cs):
    return cs.get("said") or cs["when"].date().isoformat()


def request(cs, mode, stats):
    real = e9.topics_at(cs["when"], cs["override"])
    keys = sorted(real, key=lambda k: real[k]["version"])
    exclude = cs["root"] if cs["set"] == "SELF" and not LEAKY else None
    crit = {f"t{j+1:02d}": view(real[k], mode, cs["when"], exclude, stats) for j, k in enumerate(keys)}
    back = {f"t{j+1:02d}": k for j, k in enumerate(keys)}
    crit["new"], crit["aside"] = e2.NEW, e2.ASIDE
    state = {"item": {"kind": "discussion", "said": item_said(cs), "text": cs["text"]}}
    return state, [{"type": "choice", "instructions": "Which topic is this discussion part of?", "criteria": crit}], back


def run(dry=False):
    res = {}
    for mode in VIEWS:
        stats = collections.Counter()
        rows = []
        for n, cs in enumerate(cases()):
            s, q, back = request(cs, mode, stats)
            if dry:
                if n in (0, 47) and mode != "i":
                    print(f"--- {cs['set']} view {mode}", json.dumps(s)[:400])
                    print(json.dumps(q[0]["criteria"], indent=1)[:1500])
                continue
            a, u = jev.ask(f"E10-{mode}", s, q)
            if "_error" in a:
                rows.append({"set": cs["set"], "error": a["_error"]})
                continue
            ch, p = jev.choice_probs(a["q1"])
            rows.append({"set": cs["set"], "right": cs["right"], "got": back.get(ch, ch), "p": p.get(ch),
                         "keys": len(back), "tokens": u.get("input_tokens", 0),
                         "chars": len(json.dumps(q)) + len(json.dumps(s))})
        res[mode] = {"rows": rows, "stats": dict(stats)}
        if not dry:
            report(mode, res[mode])
    if not dry:
        json.dump(res, open(os.path.join(c.OUT, "e10-leaky.json" if LEAKY else "e10.json"), "w"), indent=0)


def report(mode, r):
    rows = [x for x in r["rows"] if "error" not in x]
    print(f"== view {mode}: errors {len(r['rows']) - len(rows)}, topic views {r['stats']}")
    for s in ("SELF", "CONT", "HIST-new"):
        xs = [x for x in rows if x["set"] == s]
        if xs:
            ok = sum(x["got"] == x["right"] for x in xs)
            wrong = collections.Counter(x["got"] if x["got"] in ("new", "aside") else "other topic" for x in xs if x["got"] != x["right"])
            print(f"   {s:9s} {ok}/{len(xs)}  wrong: {dict(wrong)}  tokens/request {sum(x['tokens'] for x in xs)/len(xs):.0f}"
                  f" (keys {min(x['keys'] for x in xs)}-{max(x['keys'] for x in xs)}), chars/token {sum(x['chars'] for x in xs)/sum(x['tokens'] for x in xs):.2f}")


if __name__ == "__main__":
    cmd = sys.argv[1]
    if cmd == "limit":
        r = limit()
        json.dump(r, open(os.path.join(c.OUT, "e10-limit.json"), "w"), indent=0)
    else:
        run(dry=(cmd == "dry"))
