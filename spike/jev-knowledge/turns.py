"""Extracts the turns that ran a written-query search (ask:query, search, ask:relevance-0..2) into
data/turns.json: per turn its message, the needs gate's p, the written query, the 60 found with
kinds and scores, each judged one's p(yes) and verdict, and the turn's time. No model calls."""
import json, os, sys, collections
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import db, common as c


def kind_of(f):
    return next(k for k in f if k != "score")


def ident(f):
    k = kind_of(f)
    return f"{k}:{f[k] if not isinstance(f[k], dict) else json.dumps(f[k], sort_keys=True)}"


def main():
    ws = [w for [w] in db.rows("select distinct o.workflow_uuid from dbos.operation_outputs o join dbos.workflow_status s "
                               "on s.workflow_uuid=o.workflow_uuid where s.name='turn' and o.function_name='search'")]
    names = dict(db.rows("select a.entry_id, n.name from grit.authors a join grit.identities n on n.account=a.account "
                         "where a.kind='person' and n.name is not null"))
    turns, stats = [], collections.Counter()
    for w in ws:
        steps = {n: db.decode(o) for n, o in db.rows(
            f"select function_name, output from dbos.operation_outputs where workflow_uuid='{w}'")}
        q = steps.get("ask:query", {})
        made = db.decode(q.get("made")) if isinstance(q, dict) else None
        written = (made or {}).get("ok", {}).get("written") if isinstance(made, dict) else None
        found = (steps["search"].get("ok") or {}).get("ranked", [])
        verdicts = {}
        for n in range(3):
            s = steps.get(f"ask:relevance-{n}", {})
            for v in db.decode(s.get("made")) or [] if isinstance(s, dict) else []:
                key = ident({k: x for k, x in v.items() if k not in ("passed", "rejected", "unjudged")} | {"score": 0})
                verdicts[key] = ({"passed": True, "p": v["passed"]} if "passed" in v else
                                 {"passed": False, "p": v["rejected"]} if "rejected" in v else {"unjudged": v.get("unjudged")})
        needs = db.decode((steps.get("ask:needs") or {}).get("made"))
        conv, seq = w.rsplit(":", 1)
        ents = db.rows(f"select id, payload::text, created_at::text from grit.entries where conversation_id='{conv}' "
                       f"and turn_seq={int(seq)} order by seq")
        msg, at = [], None
        for eid, p, cat in ents:
            p = json.loads(p)
            at = at or cat
            if p["kind"] == "heard":
                msg.append(f"{names.get(eid, 'Someone')}: {p['text']}")
            elif p["kind"] == "message" and p["message"].get("role") == "user":
                msg.append(f"{names.get(eid, 'Someone')}: {p['message'].get('text', '')}")
            elif p["kind"] == "continuation":
                msg.append(f"{names.get(eid, 'Someone')}: {p.get('text', '')}")
        origin = db.rows(f"select origin::text, c.created_at::text from grit.conversations c where id='{conv}'")
        if not msg:
            stats["turn: no message text found"] += 1
        stats["turns"] += 1
        turns.append({"wid": w, "conv": conv, "seq": int(seq), "at": at, "message": "\n".join(msg),
                      "origin": json.loads(origin[0][0]) if origin else None,
                      "needs": needs, "written": written,
                      "found": [{"id": ident(f), "kind": kind_of(f), "score": f["score"], "raw": f} for f in found],
                      "verdicts": verdicts})
    for t in turns:
        stats["found entries"] += len(t["found"])
        for f in t["found"]:
            stats[f"found kind {f['kind']}"] += 1
        stats["judged"] += len(t["verdicts"])
        stats["passed"] += sum(1 for v in t["verdicts"].values() if v.get("passed"))
        stats["turns with >=1 passed"] += 1 if any(v.get("passed") for v in t["verdicts"].values()) else 0
        stats["turns with 60 found"] += 1 if len(t["found"]) >= 60 else 0
        stats["turns with >30 found"] += 1 if len(t["found"]) > 30 else 0
    json.dump(turns, open(os.path.join(c.DATA, "turns.json"), "w"))
    print(json.dumps(stats, indent=1))


if __name__ == "__main__":
    main()
