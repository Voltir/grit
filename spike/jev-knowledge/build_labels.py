"""Builds L1 (Haiku's attach placements, joined to their scans) and the topic and card texts.

Writes data/l1.json, data/topics.json, data/cards.json, data/strands.json. No model calls.
Run: python3 -I build_labels.py
"""
import json, os, sys, collections
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import db

HERE = os.path.dirname(os.path.abspath(__file__))
DATA = os.path.join(HERE, "data")
MESSAGE_CHARS = 300  # Stitching.MessageChars


def prose_text(p):
    """Plain text of a prose JSON value ({v, blocks:[{text:[{t}]}]})."""
    if not isinstance(p, dict):
        return ""
    out = []
    for b in p.get("blocks", []):
        if "text" in b:
            out.append("".join(s.get("t", "") for s in b["text"] if isinstance(s, dict)))
        elif "items" in b:
            for it in b["items"]:
                out.append(prose_text({"blocks": it if isinstance(it, list) else [it]}))
    return "\n".join(x for x in out if x)


def ts_(s):
    """A timestamp's instant, from psql's or Java's text form."""
    import datetime, re
    s = s.replace(" ", "T").replace("Z", "+00:00")
    s = re.sub(r"(\.\d{6})\d+", r"\1", s)
    s = re.sub(r"([+-]\d\d)$", r"\1:00", s)
    return datetime.datetime.fromisoformat(s)


def runs():
    """Each topics-dream run holding an attach ask: (workflow, slot, steps by name)."""
    ws = db.rows("select workflow_uuid, output from dbos.operation_outputs where function_name='read-slot' "
                 "and workflow_uuid in (select workflow_uuid from dbos.operation_outputs where function_name='ask:attach')")
    for w, slot in ws:
        steps = {n: db.decode(o) for n, o in db.rows(
            f"select function_name, output from dbos.operation_outputs where workflow_uuid='{w}'")}
        yield w, db.decode(slot), steps


def main():
    topics = {}
    for version, key, sup, written, data, body in db.rows(
            "select version, key, coalesce(superseded_at::text,''), written_at::text, data::text, coalesce(body,'') "
            "from grit.documents where plugin='topics' order by version"):
        d = json.loads(data)
        topics[int(version)] = {
            "version": int(version), "key": key, "superseded": sup, "written": written,
            "tombstone": d == {},
            "lead": prose_text(d.get("lead")),
            "passages": [{"day": p.get("day"), "text": prose_text(p.get("body"))} for p in d.get("passages", [])],
            "units": d.get("units", []),
            "review": d.get("review"),
        }
    cards = {}
    for version, key, sup, body, data in db.rows(
            "select version, key, coalesce(superseded_at::text,''), coalesce(body,''), data::text "
            "from grit.documents where plugin='knowledge' order by version"):
        cards[int(version)] = {"version": int(version), "key": key, "superseded": sup, "body": body,
                               "data": json.loads(data)}
    names = dict((e, n) for e, n in db.rows(
        "select a.entry_id, n.name from grit.authors a join grit.identities n on n.account=a.account "
        "where a.kind='person' and n.name is not null"))

    items, strands, stats = [], {}, collections.Counter()
    for w, slot, steps in runs():
        scan = db.decode(steps["keep:scan"]["kept"])
        final = steps.get("ask:attach-again") or steps["ask:attach"]
        shaped = final.get("shaped") if isinstance(final, dict) else None
        if not shaped or not isinstance(shaped.get("reply"), dict):
            stats["run: no shaped reply"] += 1
            continue
        reply, shown = shaped["reply"], shaped.get("shown", [])
        lead_versions = {l["version"]: l["key"] for l in scan.get("leads", [])}
        shown_topics = [(lead_versions[v], v) for v in shown if v in lead_versions]
        shown_cards = [c for c in scan["cards"] if c["version"] in set(shown)]
        sts = scan["strands"]
        place = collections.defaultdict(list)  # ("d"|"c", index from 1) -> [targets]
        for gi, g in enumerate(reply.get("topics", [])):
            t = str(g.get("topic")).strip().lower()
            if t == "new":
                target = ("new", gi)
            else:
                try:
                    n = int(t)
                except ValueError:
                    stats["group: unreadable topic"] += 1
                    continue
                target = ("topic", shown_topics[n - 1][0]) if 1 <= n <= len(shown_topics) else ("unshown", n)
            for i in g.get("discussions", []) or []:
                place[("d", int(i))].append(target)
            for j in g.get("cards", []) or []:
                place[("c", int(j))].append(target)
        aside = reply.get("aside", {}) or {}
        for i in aside.get("discussions", []) or []:
            place[("d", int(i))].append(("aside", None))
        for j in aside.get("cards", []) or []:
            place[("c", int(j))].append(("aside", None))
        anything = any(t[0] in ("new", "topic") for ts in place.values() for t in ts)
        if not anything and not any(t[0] == "aside" for ts in place.values() for t in ts):
            stats["run: placed nothing"] += 1
            continue
        stats["run: used"] += 1
        day = slot.get("slot", "").split("@")[-1][:10]
        common = {"run": w, "slot": slot.get("slot"), "day": day, "asOf": None,
                  "shown_topics": [[k, v] for k, v in shown_topics],
                  "model": shaped.get("calls", [{}])[0].get("model")}
        for i, st in enumerate(sts, 1):
            ts = place.get(("d", i), [])
            strands[f"{w}#{i}"] = st
            if len(ts) != 1 or ts[0][0] == "unshown":
                stats["strand: dropped (placed %s)" % ("nowhere" if not ts else "twice/unshown")] += 1
                continue
            kind, arg = ts[0]
            items.append({**common, "kind": "strand", "id": st["root"], "ref": f"{w}#{i}",
                          "asOf": st["asOf"], "members": st["members"], "label": kind if kind != "topic" else arg,
                          "group": f"{w}/{arg}" if kind == "new" else None})
        for j, c in enumerate(shown_cards, 1):
            ts = place.get(("c", j), [])
            if len(ts) > 1 or (ts and ts[0][0] == "unshown"):
                stats["card: dropped (twice/unshown)"] += 1
                continue
            if not ts:
                if not anything:
                    stats["card: dropped (nothing placed)"] += 1
                    continue
                ts = [("aside", "omitted")]
            kind, arg = ts[0]
            items.append({**common, "kind": "card", "id": c["version"], "title": c["title"], "cardKey": c["key"],
                          "cardKind": c.get("kind"),
                          "label": kind if kind != "topic" else arg,
                          "omitted": arg == "omitted",
                          "group": f"{w}/{arg}" if kind == "new" else None})

    # Strand texts, from their members' entries said before the run.
    need = {it["ref"]: strands[it["ref"]] for it in items if it["kind"] == "strand"}
    convs = sorted({m for st in need.values() for m in st["members"]})
    said = collections.defaultdict(list)
    if convs:
        inlist = ",".join(f"'{c}'" for c in convs)
        for cid, eid, at, payload in db.rows(
                f"select conversation_id::text, id, created_at::text, payload::text from grit.entries "
                f"where conversation_id in ({inlist}) and payload->>'kind' in ('heard','message','posted') order by created_at"):
            p = json.loads(payload)
            text, who = None, None
            if p["kind"] == "heard":
                text, who = p.get("text"), names.get(eid, "Someone")
            elif p["kind"] == "posted":
                text, who = p.get("text"), "Assistant"
            else:
                m = p.get("message", {})
                if m.get("role") == "user":
                    text, who = m.get("text"), names.get(eid, "Someone")
                elif m.get("role") == "assistant":
                    text = "".join(b.get("text", "") for b in m.get("blocks", []) if b.get("type") == "text")
                    who = "Assistant"
            if text:
                said[cid].append((at, who, text))
    texts = {}
    for ref, st in need.items():
        cut = ts_(st["asOf"])
        lines = sorted((x for m in st["members"] for x in said[m] if ts_(x[0]) < cut), key=lambda x: ts_(x[0]))
        # The root's first message is the opening.
        opening = next((x for x in said[st["root"]] if x in lines), None)
        rest = [x for x in lines if x is not opening]
        texts[ref] = {"opening": opening and [opening[1], opening[2][:MESSAGE_CHARS]],
                      "rest": [[w_, t[:MESSAGE_CHARS], at] for at, w_, t in rest],
                      "said": st.get("said"), "chars": st.get("chars")}
    for it in items:
        if it["kind"] == "strand":
            it["text"] = texts[it["ref"]]
            if not it["text"]["opening"] and not it["text"]["rest"]:
                stats["strand: no text found"] += 1
        else:
            it["body"] = cards.get(it["id"], {}).get("body", "")
            if not it["body"]:
                same = [c for c in cards.values() if c["key"] == it["cardKey"] and c["body"]]
                if same:
                    it["body"] = min(same, key=lambda c: abs(c["version"] - it["id"]))["body"]
                    stats["card: body from another version of its key"] += 1
                else:
                    stats["card: no body"] += 1
    missing = {(k, v) for it in items for k, v in it["shown_topics"] if v not in topics}
    stats["topic versions shown but missing"] = len(missing)
    stats["... of which no version of the key has a lead"] = len(
        {(k, v) for k, v in missing if not any(t["key"] == k and t["lead"] for t in topics.values())})
    for name, obj in (("l1", items), ("topics", topics), ("cards", cards)):
        with open(os.path.join(DATA, name + ".json"), "w") as f:
            json.dump(obj, f)
    by = collections.Counter((it["kind"], "topic" if it["label"].startswith("topic:") else it["label"]) for it in items)
    print(json.dumps({"stats": stats, "by_kind_label": {f"{k}/{l}": n for (k, l), n in sorted(by.items())},
                      "items": len(items)}, indent=1))


if __name__ == "__main__":
    main()
