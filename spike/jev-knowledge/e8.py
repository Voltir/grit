"""E8 prep: the room era's messages (room and thread conversations), counted, and written in time
order to data/room-sheet.md (untracked: Slack text) for Nick to cluster into matters."""
import json, os, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import db, common as c, p2

rows = db.rows("""select c.id, c.origin->>'kind', coalesce(c.origin->>'threadTs',''), e.id, e.created_at::text, e.payload::text
  from grit.conversations c join grit.entries e on e.conversation_id=c.id
 where c.origin->>'kind' in ('room','thread')
   and (e.payload->>'kind' in ('heard','posted') or (e.payload->>'kind'='message' and e.payload->'message'->>'role' in ('user','assistant')))
 order by e.created_at""")
threads = {}
out = ["# Room-era messages, oldest first", "",
       "Mark each line's matter in the last column (any short name; same name = same matter; `-` for chatter).",
       "`room` = said at the top level; `thread N` = in a Slack thread.", "",
       "| # | when (UTC) | where | who | text | matter |", "|---|---|---|---|---|---|"]
n_room = n_thread = 0
for i, (cid, kind, ts, eid, at, p) in enumerate(rows, 1):
    p = json.loads(p)
    if p["kind"] in ("heard", "posted"):
        text, who = p.get("text", ""), (p2.names().get(eid, "Someone") if p["kind"] == "heard" else "Assistant")
    elif p["message"]["role"] == "user":
        text, who = p["message"].get("text", ""), p2.names().get(eid, "Someone")
    else:
        text, who = "".join(b.get("text", "") for b in p["message"].get("blocks", []) if b.get("type") == "text"), "Assistant"
    if kind == "thread":
        threads.setdefault(cid, len(threads) + 1)
        n_thread += 1
    else:
        n_room += 1
    where = "room" if kind == "room" else f"thread {threads[cid]}"
    out.append(f"| {i} | {at[:16]} | {where} | {who} | {c.cut(text, 300).replace('|', '/').replace(chr(10), ' ⏎ ')} | |")
path = os.path.join(c.DATA, "room-sheet.md")
open(path, "w").write("\n".join(out))
print(path, {"messages": len(rows), "room": n_room, "in threads": n_thread, "threads": len(threads),
             "share in threads": round(n_thread / max(1, len(rows)), 3)})
