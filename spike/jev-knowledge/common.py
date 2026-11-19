"""Shared reads and renderings: topics at a version, strands and cards as text, BM25."""
import json, os, math, re, collections

HERE = os.path.dirname(os.path.abspath(__file__))
DATA = os.path.join(HERE, "data")
OUT = os.path.join(HERE, "out")
os.makedirs(OUT, exist_ok=True)


def load(name):
    with open(os.path.join(DATA, name + ".json")) as f:
        return json.load(f)


TOPICS = {int(k): v for k, v in load("topics").items()}
L1 = load("l1")
BY_KEY = collections.defaultdict(list)
for _v, _t in sorted(TOPICS.items()):
    BY_KEY[_t["key"]].append(_t)


def topic_at(key, version):
    """The topic `key` as written at `version`, or its nearest earlier written version with a
    lead (a later one when none is earlier); None when the key never had a lead."""
    t = TOPICS.get(version)
    if t and t["lead"]:
        return t
    vs = [x for x in BY_KEY.get(key, []) if x["lead"]]
    earlier = [x for x in vs if x["version"] <= version]
    return (earlier[-1] if earlier else (vs[0] if vs else None))


def latest(key):
    """The key's last version that has a lead (a withdrawn topic's last live text)."""
    vs = [x for x in BY_KEY.get(key, []) if x["lead"]]
    return vs[-1] if vs else None


def live_keys():
    return [k for k, vs in BY_KEY.items() if not vs[-1]["tombstone"] and vs[-1]["lead"]]


def withdrawn_keys():
    return [k for k, vs in BY_KEY.items() if vs[-1]["tombstone"] and latest(k)]


def cut(s, n):
    s = s or ""
    return s if len(s) <= n else s[: max(0, n - 1)].rstrip() + "…"


def first_line(s):
    s = (s or "").strip()
    m = re.match(r"(.+?[.!?])(\s|$)", s)
    return m.group(1) if m else s.split("\n")[0]


def topic_text(t, mode="lead"):
    """A topic's description as a key: lead (400), lead + newest passage head (300), lead's first
    sentence, or lead + its cards' titles."""
    lead = cut(t["lead"], 400)
    if mode == "lead":
        return lead
    if mode == "lead+passage":
        p = t["passages"][0]["text"] if t["passages"] else ""
        return lead + ("\nLatest: " + cut(p, 300) if p else "")
    if mode == "firstline":
        return first_line(t["lead"])
    if mode == "lead+cards":
        return lead  # only one topic in the store holds a unit (a Drive file, no title): see FINDINGS
    raise ValueError(mode)


def lines_of(text):
    """A strand's lines, oldest first, as (speaker, text)."""
    out = []
    if text["opening"]:
        out.append(tuple(text["opening"]))
    out += [(w, t) for w, t, _ in text["rest"]]
    return out


def strand_text(text, mode="whole"):
    """A strand as `speaker: text` lines. whole: the opening then the latest that fit 1,500
    chars (Stitching.Excerpt.within); cut800: the same within 800; ends: the opening and the
    latest two messages."""
    op = [tuple(text["opening"])] if text["opening"] else []
    rest = [(w, t) for w, t, _ in text["rest"]]
    def render(ls, skipped):
        rows = [f"{w}: {t}" for w, t in ls[:len(op)]]
        if skipped:
            rows.append(f"[{skipped} earlier messages not shown]")
        rows += [f"{w}: {t}" for w, t in ls[len(op):]]
        return "\n".join(rows)
    if mode == "ends":
        keep = rest[-2:]
        return render(op + keep, len(rest) - len(keep))
    chars = {"whole": 1500, "cut800": 800}[mode]
    for k in range(len(rest), -1, -1):
        r = render(op + rest[len(rest) - k:], len(rest) - k)
        if len(r) <= chars:
            return r
    return cut(render(op, len(rest)), chars)


def has_text(it):
    return it["kind"] == "card" or bool(it["text"]["opening"] or it["text"]["rest"])


def card_text(it, mode="title+summary"):
    if mode == "title":
        return it["title"]
    body = it.get("body") or ""
    # A card's body is its title line, a blank line, then its summary.
    summary = body.split("\n\n", 1)[1] if "\n\n" in body else body
    return it["title"] + "\n" + cut(summary, 500)


def item_text(it, smode="whole", cmode="title+summary"):
    return strand_text(it["text"], smode) if it["kind"] == "strand" else card_text(it, cmode)


# ---- BM25 -----------------------------------------------------------------------------------
STOP = set("""a an the and or of to in on for with at by from is are was were be been it its this that
these those as but not no we i you he she they them our your their his her has have had do does did so
if then than there here what which who whom when where why how all any each can could will would should
may might just also about into over after before up down out more most some such only own same very s t
""".split())


def tokens(s):
    return [w for w in re.findall(r"[a-z0-9]+", (s or "").lower()) if w not in STOP and len(w) > 1]


class BM25:
    def __init__(self, docs, k1=1.2, b=0.75):
        self.ids = list(docs)
        self.tf = [collections.Counter(tokens(docs[i])) for i in self.ids]
        self.len = [sum(c.values()) for c in self.tf]
        self.avg = (sum(self.len) / len(self.len)) if self.len else 1
        df = collections.Counter(w for c in self.tf for w in c)
        n = len(self.ids)
        self.idf = {w: math.log(1 + (n - d + 0.5) / (d + 0.5)) for w, d in df.items()}
        self.k1, self.b = k1, b

    def rank(self, query):
        q = tokens(query)
        scores = []
        for i, (c, l) in enumerate(zip(self.tf, self.len)):
            s = 0.0
            for w in q:
                f = c.get(w)
                if f:
                    s += self.idf[w] * f * (self.k1 + 1) / (f + self.k1 * (1 - self.b + self.b * l / self.avg))
            scores.append((s, self.ids[i]))
        scores.sort(key=lambda x: -x[0])
        return [(i, s) for s, i in scores]
