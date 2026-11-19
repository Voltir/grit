"""P2: judge the recorded ranks 31-60 with the relevance steps' question and state shape
(TurnRelevance: state {"message", "candidates"}, one YesNo per candidate, ten to a call, each
text cut to 1,200 chars, passing at 0.5).

The recorded steps keep only each verdict's probability, not the state, so each candidate is
re-rendered here as RetrievalAssembler.shownOf shows it (Shown.document / recorded / piece /
nearby), as nearly as the snapshot allows. A fidelity pass first re-judges recorded ranks 1-30
of some turns and compares with the recorded probabilities.

python3 -I p2.py dry | fidelity [n] | run | report
Writes out/p2.json (ids and probabilities only)."""
import json, os, sys, collections, datetime
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import db, common as c, jev, turns as TU

CHARS = 1200
Q = ("Would candidates[{n}] help answer the message? Yes only when it holds facts, decisions or "
     "context a reply to the message needs; no when it is only on a related subject.")
NAMES = None


def names():
    global NAMES
    if NAMES is None:
        NAMES = dict(db.rows("select a.entry_id, n.name from grit.authors a join grit.identities n on "
                             "n.account=a.account where a.kind='person' and n.name is not null"))
    return NAMES


def place_written(path):
    """Place.written: `ns:` then the rest joined by `/` (`fs:/…` for files)."""
    segs = path.strip("{}").split(",") if isinstance(path, str) else path
    if not segs or segs == [""]:
        return "everywhere"
    return f"{segs[0]}:{'/' if segs[0] == 'fs' else ''}{'/'.join(segs[1:])}"


def closing_body(cl):
    f, b = cl.get("flows", {}), cl.get("balance", {})
    parts = [f.get("prose", "")]
    if f.get("outcome"):
        parts.append("Outcome: " + f["outcome"])
    def lst(title, ls):
        if ls:
            parts.append(title + ":\n" + "\n".join("- " + l for l in ls))
    def texts(xs):
        return [x.get("text", "") if isinstance(x, dict) else str(x) for x in xs or []]
    lst("Still open", texts(b.get("open")))
    lst("Standing", texts(b.get("standing")))
    if b.get("topics"):
        parts.append("Topics so far: " + "; ".join(texts(b.get("topics"))))
    return "\n".join(p for p in parts if p)


def entry_lines(conv, seq=None):
    cond = f"conversation_id='{conv}'" + (f" and turn_seq={int(seq)}" if seq is not None else "")
    out = []
    for eid, p, at in db.rows(f"select id, payload::text, created_at::text from grit.entries where {cond} order by seq"):
        p = json.loads(p)
        day = at[:16] + " UTC"
        if p["kind"] == "heard":
            out.append(f"{names().get(eid, 'Someone')}, {day}: {p['text']}")
        elif p["kind"] == "message" and p["message"].get("role") == "user":
            out.append(f"{names().get(eid, 'Someone')}, {day}: {p['message'].get('text', '')}")
        elif p["kind"] == "message" and p["message"].get("role") == "assistant":
            t = "".join(b.get("text", "") for b in p["message"].get("blocks", []) if b.get("type") == "text")
            if t:
                out.append(f"Assistant, {day}: {t}")
    return out


def render(f, t):
    """A found candidate as the judge saw it, or None when the snapshot no longer holds it."""
    k, raw = f["kind"], f["raw"]
    if k == "doc":
        r = db.rows(f"select place::text, written_at::text, body, coalesce(pointer::text,'') from grit.documents where version={int(raw['doc'])}")
        if not r:
            return None
        place, at, body, pointer = r[0]
        return f"[doc] internal, at {place_written(place)}, {at[:10]}:\n{body}"
    if k == "closed":
        r = db.rows(f"select e.payload::text, e.created_at::text, p.path::text from grit.entries e join grit.conversations c "
                    f"on c.id=e.conversation_id join grit.places p on p.id=c.place_id where e.conversation_id='{raw['closed']}' "
                    f"and e.payload->>'kind'='closed' and e.created_at < '{t['at']}'::timestamptz order by e.created_at desc limit 1")
        if not r:
            return None
        p, at, path = r[0]
        return (f"[afar] another conversation's record, written by grit when it closed on {at[:10]}, at "
                f"{place_written(path)}: " + closing_body(json.loads(p)["closing"]))
    if k == "piece":
        r = db.rows(f"select s.name, p.heading::text, s.place::text, s.written_at::text, p.search_text from grit.pieces p "
                    f"join grit.piece_sets s on s.id=p.set_id where p.id={int(raw['piece'])}")
        if not r:
            return None
        name, heading, place, at, text = r[0]
        hs = [h for h in heading.strip("{}").split(",") if h]
        path = (", under " + " > ".join(h.strip('"') for h in hs)) if hs else ""
        more = raw.get("more", 0)
        return (f"[doc] internal, part of the file {name}{path}, at {place_written(place)}, {at[:10]}:\n{text}"
                + (f"\n{more} more parts of this file matched the search." if more else ""))
    if k in ("near", "own"):
        conv = raw["near"] if k == "near" else t["conv"]
        seq = raw.get("turn") if k == "near" else raw["own"]
        ls = entry_lines(conv, seq)
        if not ls:
            return None
        if k == "near":
            r = db.rows(f"select p.path::text from grit.conversations c join grit.places p on p.id=c.place_id where c.id='{conv}'")
            return f"[afar] another conversation, shown by grit, still open, at {place_written(r[0][0]) if r else ''}:\n" + "\n".join(ls)
        return "\n".join(ls)
    return None


def judge(tag, t, batch):
    """p(yes) for each of batch (found entries), None for one gone, asked ten to a call."""
    shown = [(f, render(f, t)) for f in batch]
    live = [(f, x) for f, x in shown if x]
    if not live:
        return {f["id"]: None for f in batch}, 0
    state = {"message": t["message"][:CHARS], "candidates": [x[:CHARS] for _, x in live]}
    qs = [{"type": "noul", "instructions": Q.format(n=i)} for i in range(len(live))]
    a, u = jev.ask(tag, state, qs)
    if "_error" in a:
        return {f["id"]: None for f in batch}, 0
    out = {f["id"]: None for f in batch}
    for i, (f, _) in enumerate(live):
        out[f["id"]] = jev.yes(a[f"q{i+1}"])
    return out, u.get("input_tokens", 0)


def eligible():
    ts = json.load(open(os.path.join(c.DATA, "turns.json")))
    return [t for t in ts if t["message"] and len(t["found"]) > 30]


def save(res):
    json.dump(res, open(os.path.join(c.OUT, "p2.json"), "w"), indent=0)


def load():
    p = os.path.join(c.OUT, "p2.json")
    return json.load(open(p)) if os.path.exists(p) else {"fidelity": {}, "new": {}}


def fidelity(n):
    res = load()
    for t in eligible()[:n]:
        for b in range(3):
            batch = t["found"][b * 10:(b + 1) * 10]
            ps, tok = judge("P2-fidelity", t, batch)
            for f in batch:
                rec = t["verdicts"].get(f["id"], {})
                res["fidelity"][t["wid"] + "|" + f["id"]] = {"rank": t["found"].index(f) + 1, "recorded": rec.get("p"),
                                                            "now": ps[f["id"]]}
    save(res)
    report(res)


def run():
    res = load()
    for t in eligible():
        for b in range(3, 6):
            batch = t["found"][b * 10:(b + 1) * 10]
            if not batch:
                continue
            ps, tok = judge("P2-31-60", t, batch)
            for f in batch:
                res["new"][t["wid"] + "|" + f["id"]] = {"rank": t["found"].index(f) + 1, "kind": f["kind"], "now": ps[f["id"]]}
    save(res)
    report(res)


def report(res):
    fid = [v for v in res["fidelity"].values() if v["recorded"] is not None and v["now"] is not None]
    if fid:
        agree = sum((v["recorded"] >= 0.5) == (v["now"] >= 0.5) for v in fid)
        rp = sum(v["recorded"] >= 0.5 for v in fid)
        np_ = sum(v["now"] >= 0.5 for v in fid)
        both = sum(v["recorded"] >= 0.5 and v["now"] >= 0.5 for v in fid)
        xs, ys = [v["recorded"] for v in fid], [v["now"] for v in fid]
        mx, my = sum(xs) / len(xs), sum(ys) / len(ys)
        cov = sum((x - mx) * (y - my) for x, y in zip(xs, ys))
        r = cov / ((sum((x - mx) ** 2 for x in xs) * sum((y - my) ** 2 for y in ys)) ** 0.5)
        gone = sum(1 for v in res["fidelity"].values() if v["now"] is None)
        print(f"fidelity: n={len(fid)} gone={gone} verdict agreement {agree}/{len(fid)}={agree/len(fid):.3f} "
              f"pearson r={r:.3f} recorded passes {rp}, re-rendered passes {np_}, both {both}")
    ts = eligible()
    bands = collections.defaultdict(lambda: [0, 0, 0])  # passed, judged, gone
    for t in ts:
        for i, f in enumerate(t["found"][:30]):
            v = t["verdicts"].get(f["id"], {})
            b = bands[(i // 10) * 10 + 1]
            if "p" in v:
                b[1] += 1
                b[0] += v["p"] >= 0.5
    for v in res["new"].values():
        b = bands[((v["rank"] - 1) // 10) * 10 + 1]
        if v["now"] is None:
            b[2] += 1
        else:
            b[1] += 1
            b[0] += v["now"] >= 0.5
    for k in sorted(bands):
        p, n, g = bands[k]
        print(f"ranks {k}-{k+9}: passed {p}/{n} = {p/max(1,n):.3f}  (gone {g})  {'recorded' if k <= 21 else 'new, re-rendered'}")


if __name__ == "__main__":
    cmd = sys.argv[1]
    if cmd == "dry":
        t = eligible()[3]
        batch = t["found"][30:33]
        for f in batch:
            print("----", f["kind"], f["score"])
            print((render(f, t) or "GONE")[:CHARS])
        print("MESSAGE", t["message"][:400])
    elif cmd == "fidelity":
        fidelity(int(sys.argv[2]) if len(sys.argv) > 2 else 15)
    elif cmd == "run":
        run()
    else:
        report(load())
