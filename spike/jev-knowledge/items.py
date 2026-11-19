"""The item sets E1/E2/E3-E7 read.

L1S: L1's strands that still have text (the legacy Slack messages behind most September and
early-October strands are purged). Topics: those shown that night, at their shown versions.
L1C: a sample of L1's cards from runs that showed topics. Labels: Haiku's.
SELF: each strand of L1S that opened a topic (Haiku said `new` and the topic keyed by its root
exists), against every topic ever opened (69, latest text): its own topic is the answer by
construction (the lead was written from it, so this is easier than a continuation).
"""
import random, collections, os, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import common as c


def norm(label):
    return "topic" if label.startswith("topic:") else label


def l1_strands():
    out = []
    for it in c.L1:
        if it["kind"] == "strand" and c.has_text(it):
            out.append({**it, "set": "L1S", "topics": [(k, v) for k, v in it["shown_topics"]
                                                       if c.topic_at(k, v)]})
    return out


def l1_cards(n=80, seed=7):
    cs = [it for it in c.L1 if it["kind"] == "card" and it["shown_topics"] and it.get("body")]
    rng = random.Random(seed)
    new = [x for x in cs if x["label"] == "new"]
    rest = [x for x in cs if x["label"] != "new"]
    rng.shuffle(rest)
    pick = new + rest[: max(0, n - len(new))]
    return [{**it, "set": "L1C", "topics": [(k, v) for k, v in it["shown_topics"] if c.topic_at(k, v)]}
            for it in pick]


def self_set():
    allk = c.live_keys() + c.withdrawn_keys()
    seen, out = set(), []
    for it in l1_strands():
        k = "topic:strand-" + it["id"]
        if it["label"] == "new" and k in allk and it["id"] not in seen:
            seen.add(it["id"])
            out.append({**it, "set": "SELF", "label": k,
                        "topics": [(kk, c.latest(kk)["version"]) for kk in allk]})
    return out


def topic_doc(key, version, mode="lead"):
    return c.topic_text(c.topic_at(key, version), mode)


def shortlist(it, k=8, mode="lead", smode="whole"):
    """BM25 top-k of the item's topics, queried by its text."""
    docs = {kk: topic_doc(kk, v, mode) for kk, v in it["topics"]}
    if not docs:
        return []
    return [kk for kk, _ in c.BM25(docs).rank(c.item_text(it, smode))[:k]]


if __name__ == "__main__":
    for name, s in (("L1S", l1_strands()), ("L1C", l1_cards()), ("SELF", self_set())):
        print(name, len(s), dict(collections.Counter(norm(x["label"]) for x in s)),
              "topics shown: median", sorted(len(x["topics"]) for x in s)[len(s) // 2] if s else None)
