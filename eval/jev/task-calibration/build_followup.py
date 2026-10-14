"""Freeze the follow-up labels: write followup_labels.jsonl beside this file.

  python3 -I build_followup.py

Each line: id, genre (border, analysis, mixed), kind (the primary), kinds (every acceptable kind),
hard, sure, author, text, thread (rendered as in build_corpus.py). Deterministic.
"""
import json, os, sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
from corpus_c import BORDER, ANALYSIS, MIXED  # noqa: E402

KINDS = {"chat", "lookup", "investigation", "analysis", "writing", "coding", "decision"}
AUTHORS = ["dana", "eli", "fern", "gus", "hana", "ivo", "jun"]
SIZES = {"border": 50, "analysis": 50, "mixed": 15}


def main():
    rows = []
    for g, roots in [("border", BORDER), ("analysis", ANALYSIS), ("mixed", MIXED)]:
        assert len(roots) == SIZES[g], (g, len(roots))
        for i, r in enumerate(roots):
            assert r.kinds and set(r.kinds) <= KINDS and isinstance(r.hard, bool), r
            rows.append({"id": f"{g}-{i:02d}", "genre": g, "kind": r.kinds[0], "kinds": r.kinds,
                         "hard": r.hard, "sure": r.sure, "author": AUTHORS[len(rows) % len(AUTHORS)],
                         "text": r.text, "thread": "\n".join(f"{a}: {t}" for a, t in (r.thread or []))})
    keys = [r["text"] + "\x00" + r["thread"] for r in rows]
    assert len(set(keys)) == len(keys), "a root repeats with the same thread"
    for g in SIZES:
        gs = [r for r in rows if r["genre"] == g]
        print(f"{g:9s} {len(gs)}  hard {sum(r['hard'] for r in gs)}  unsure {sum(not r['sure'] for r in gs)}  "
              f"thread {sum(1 for r in gs if r['thread'])}  terse<=40ch {sum(1 for r in gs if len(r['text']) <= 40)}")
    with open(os.path.join(HERE, "followup_labels.jsonl"), "w") as f:
        for r in rows:
            f.write(json.dumps(r, ensure_ascii=False) + "\n")


if __name__ == "__main__":
    main()
