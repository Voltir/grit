"""Freeze the hand labels: write labels.jsonl beside this file, one root per line.

  python3 -I build_corpus.py

Each line: id, genre, kind, hard, author, text, thread (the thread before the root, rendered as
`author: text` lines, as live triage's state carries it; "" when none), and the two subsets:
`pilot` (the hard-phrasing pilot: indices 3, 11, ..., 43 of each genre, and 47 of investigation and
coding; 50 in all) and `reask` (the re-asked subset: indices 1, 5, ..., 45 of each genre, and 49 of
four genres; 100 in all). Deterministic; no randomness.
"""
import json, os, sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
from corpus_a import CHAT, LOOKUP, INVESTIGATION, WRITING  # noqa: E402
from corpus_b import CODING, DECISION, MEETING, ACKS  # noqa: E402

GENRES = [("chat", CHAT), ("lookup", LOOKUP), ("investigation", INVESTIGATION),
          ("writing", WRITING), ("coding", CODING), ("decision", DECISION),
          ("meeting", MEETING), ("ack", ACKS)]
KINDS = {"chat", "lookup", "investigation", "writing", "coding", "decision"}
AUTHORS = ["dana", "eli", "fern", "gus", "hana", "ivo", "jun"]


def main():
    rows = []
    for g, roots in GENRES:
        assert len(roots) == 50, (g, len(roots))
        for i, r in enumerate(roots):
            assert r.kind in KINDS and isinstance(r.hard, bool), r
            thread = "\n".join(f"{a}: {t}" for a, t in (r.thread or []))
            rows.append({
                "id": f"{g}-{i:02d}", "genre": g, "kind": r.kind, "hard": r.hard,
                "author": AUTHORS[(len(rows)) % len(AUTHORS)], "text": r.text, "thread": thread,
                "pilot": i % 8 == 3 or (i == 47 and g in ("investigation", "coding")),
                "reask": (i % 4 == 1 and i < 48) or (i == 49 and g in ("chat", "lookup", "investigation", "writing")),
            })
    texts = [r["text"] + "\x00" + r["thread"] for r in rows]
    assert len(set(texts)) == len(texts), "a root repeats with the same thread"
    print("roots", len(rows), "with thread", sum(1 for r in rows if r["thread"]),
          "hard", sum(r["hard"] for r in rows), "pilot", sum(r["pilot"] for r in rows),
          "reask", sum(r["reask"] for r in rows))
    for k in sorted(KINDS):
        ks = [r for r in rows if r["kind"] == k]
        print(f"  kind {k:14s} {len(ks):3d}  hard {sum(r['hard'] for r in ks)}")
    with open(os.path.join(HERE, "labels.jsonl"), "w") as f:
        for r in rows:
            f.write(json.dumps(r, ensure_ascii=False) + "\n")


if __name__ == "__main__":
    main()
