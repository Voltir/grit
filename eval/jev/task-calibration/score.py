"""Score Jev's task answers against the frozen labels.

  python3 -I score.py LABELS.jsonl RESULTS.jsonl pilot          # AUC of each hard phrasing
  python3 -I score.py LABELS.jsonl RESULTS.jsonl main [OUT.md]   # the full report

Only answered calls count. AUC is Mann-Whitney's (ties count a half). Cost is Jev's reported
input tokens x $0.042/M, summed per plan.
"""
import json, math, sys
from collections import Counter, defaultdict

KINDS = ["chat", "lookup", "investigation", "writing", "coding", "decision"]
THRESHOLDS = [0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]


def auc(pos, neg):
    if not pos or not neg:
        return float("nan")
    s = 0.0
    for p in pos:
        for n in neg:
            s += 1.0 if p > n else 0.5 if p == n else 0.0
    return s / (len(pos) * len(neg))


def auc_ci(pos, neg, B=2000):
    """AUC with a 95% bootstrap interval (stratified, fixed seed)."""
    import random
    rnd = random.Random(7)
    a = auc(pos, neg)
    bs = sorted(auc([rnd.choice(pos) for _ in pos], [rnd.choice(neg) for _ in neg]) for _ in range(B))
    return a, bs[int(0.025 * B)], bs[int(0.975 * B) - 1]


def pct(xs, q):
    xs = sorted(xs)
    if not xs:
        return float("nan")
    i = min(len(xs) - 1, max(0, math.ceil(q * len(xs)) - 1))
    return xs[i]


def load(labels, results):
    roots = {r["id"]: r for r in (json.loads(l) for l in open(labels))}
    ans, recs = {}, []
    for l in open(results):
        r = json.loads(l)
        recs.append(r)
        if "answers" in r:
            ans[(r["id"], r["form"], r["rep"])] = r["answers"]
    return roots, ans, recs


def cost(recs, pred=lambda r: True):
    rs = [r for r in recs if pred(r)]
    return len([r for r in rs if "answers" in r]), sum(r.get("input_tokens", 0) for r in rs), \
        sum(r.get("usd", 0.0) for r in rs)


def pilot(roots, ans, recs):
    forms = sorted({f for (_, f, _) in ans if f.startswith("pilot-")})
    print("phrasing  n   AUC [95% CI]           mean p hard / easy")
    for f in forms:
        pos = [ans[(i, f, 0)]["hard"] for i, r in roots.items() if (i, f, 0) in ans and r["hard"]]
        neg = [ans[(i, f, 0)]["hard"] for i, r in roots.items() if (i, f, 0) in ans and not r["hard"]]
        a, lo, hi = auc_ci(pos, neg)
        print(f"{f:9s} {len(pos) + len(neg):3d} {a:.3f} [{lo:.3f}, {hi:.3f}]   "
              f"{sum(pos) / len(pos):.3f} / {sum(neg) / len(neg):.3f}")
    n, tok, usd = cost(recs, lambda r: r["form"].startswith("pilot-"))
    print(f"pilot cost: {n} calls, {tok} input tokens, {usd * 100:.3f}¢")


def main_report(roots, ans, recs):
    L = []
    w = L.append
    ids = list(roots)
    p_hard = {f: {i: ans[(i, f, 0)]["hard"] for i in ids if (i, f, 0) in ans} for f in ("together", "hard")}
    p_kind = {f: {i: ans[(i, f, 0)]["kind"] for i in ids if (i, f, 0) in ans} for f in ("together", "kind")}
    w(f"answered: together {len(p_hard['together'])}, kind alone {len(p_kind['kind'])}, "
      f"hard alone {len(p_hard['hard'])}")

    # --- kind
    for f in ("together", "kind"):
        k = p_kind[f]
        w(f"\n## kind ({f})\n")
        w("| kind | n | AUC [95% CI] | precision of argmax | recall of argmax |")
        w("|---|---|---|---|---|")
        for kk in KINDS:
            pos = [k[i]["probabilities"][kk] for i in k if roots[i]["kind"] == kk]
            neg = [k[i]["probabilities"][kk] for i in k if roots[i]["kind"] != kk]
            a, lo, hi = auc_ci(pos, neg)
            chosen = [i for i in k if k[i]["choice"] == kk]
            tp = sum(1 for i in chosen if roots[i]["kind"] == kk)
            w(f"| {kk} | {len(pos)} | {a:.3f} [{lo:.3f}, {hi:.3f}] | "
              f"{tp}/{len(chosen)} | {tp}/{len(pos)} |")
        acc = sum(1 for i in k if k[i]["choice"] == roots[i]["kind"])
        w(f"\naccuracy of argmax: {acc}/{len(k)} = {acc / len(k):.3f}\n")
        w("confusion (rows: label, columns: Jev's choice)\n")
        w("| label \\ choice | " + " | ".join(KINDS) + " |")
        w("|---|" + "---|" * len(KINDS))
        for lk in KINDS:
            c = Counter(k[i]["choice"] for i in k if roots[i]["kind"] == lk)
            w(f"| {lk} | " + " | ".join(str(c[x]) for x in KINDS) + " |")
        w("\nby genre: argmax accuracy\n")
        for g in dict.fromkeys(roots[i]["genre"] for i in ids):
            gi = [i for i in k if roots[i]["genre"] == g]
            w(f"- {g}: {sum(1 for i in gi if k[i]['choice'] == roots[i]['kind'])}/{len(gi)}; "
              f"chosen {dict(Counter(k[i]['choice'] for i in gi).most_common())}")

    # --- hard
    for f in ("together", "hard"):
        h = p_hard[f]
        w(f"\n## hard ({f})\n")
        pos = [h[i] for i in h if roots[i]["hard"]]
        neg = [h[i] for i in h if not roots[i]["hard"]]
        a, lo, hi = auc_ci(pos, neg)
        w(f"AUC {a:.3f} [{lo:.3f}, {hi:.3f}] (n {len(pos)} hard / {len(neg)} not)")
        posn = [h[i] for i in h if roots[i]["hard"] and not roots[i]["pilot"]]
        negn = [h[i] for i in h if not roots[i]["hard"] and not roots[i]["pilot"]]
        w(f"AUC without the pilot's 50 roots: {auc(posn, negn):.3f} (n {len(posn)} / {len(negn)})")
        nonchat = [i for i in h if roots[i]["kind"] != "chat"]
        w(f"AUC among roots not labelled chat: "
          f"{auc([h[i] for i in nonchat if roots[i]['hard']], [h[i] for i in nonchat if not roots[i]['hard']]):.3f}")
        w("AUC within each labelled kind (hard vs not):")
        for kk in KINDS:
            kp = [h[i] for i in h if roots[i]["kind"] == kk and roots[i]["hard"]]
            kn = [h[i] for i in h if roots[i]["kind"] == kk and not roots[i]["hard"]]
            if kp and kn:
                w(f"  - {kk}: {auc(kp, kn):.3f} ({len(kp)}/{len(kn)})")
        w("\n| threshold | routed strong | precision (hard among strong) | recall (strong among hard) | easy routed strong |")
        w("|---|---|---|---|---|")
        for t in THRESHOLDS:
            s = [i for i in h if h[i] >= t]
            tp = sum(1 for i in s if roots[i]["hard"])
            w(f"| {t:.1f} | {len(s)} | {tp}/{len(s)} = {tp / len(s) if s else float('nan'):.3f} | "
              f"{tp}/{len(pos)} = {tp / len(pos):.3f} | {len(s) - tp}/{len(neg)} |")
        w("\nmean p(hard) by genre, split by label:")
        for g in dict.fromkeys(roots[i]["genre"] for i in ids):
            gp = [h[i] for i in h if roots[i]["genre"] == g and roots[i]["hard"]]
            gn = [h[i] for i in h if roots[i]["genre"] == g and not roots[i]["hard"]]
            m = lambda xs: f"{sum(xs) / len(xs):.3f} (n {len(xs)})" if xs else "-"
            w(f"- {g}: hard {m(gp)}, not {m(gn)}")
        ah = [h[i] for i in h if roots[i]["genre"] == "ack"]
        # acknowledgement after hard work vs after easy news (corpus_b: even index -> hard thread)
        hard_t = [h[i] for i in h if roots[i]["genre"] == "ack" and int(i.split("-")[1]) % 2 == 0]
        easy_t = [h[i] for i in h if roots[i]["genre"] == "ack" and int(i.split("-")[1]) % 2 == 1]
        w(f"\nacknowledgements: mean p(hard) after a hard thread {sum(hard_t) / len(hard_t):.3f}, "
          f"after easy news {sum(easy_t) / len(easy_t):.3f}; max {max(ah):.3f}")
        thr = [h[i] for i in h if roots[i]["thread"] and roots[i]["genre"] != "ack"]
        w(f"roots with a thread (not acks): AUC "
          f"{auc([h[i] for i in h if roots[i]['thread'] and roots[i]['genre'] != 'ack' and roots[i]['hard']], [h[i] for i in h if roots[i]['thread'] and roots[i]['genre'] != 'ack' and not roots[i]['hard']]):.3f} (n {len(thr)})")
        w("\nmisses at 0.5: hard with p < 0.5, and easy with p >= 0.5 (id, p, text start)")
        for i in sorted(h, key=lambda i: h[i]):
            r = roots[i]
            if (r["hard"] and h[i] < 0.5) or (not r["hard"] and h[i] >= 0.5):
                w(f"  - {i} {'HARD' if r['hard'] else 'easy'} {h[i]:.3f} {r['text'][-90:]!r}")

    # --- together vs alone
    w("\n## together vs alone (rep 0)\n")
    both = [i for i in ids if i in p_hard["together"] and i in p_hard["hard"]]
    d = [p_hard["together"][i] - p_hard["hard"][i] for i in both]
    ad = [abs(x) for x in d]
    w(f"hard: n {len(d)}, mean signed (together - alone) {sum(d) / len(d):+.4f}, mean |d| "
      f"{sum(ad) / len(ad):.4f}, p95 |d| {pct(ad, 0.95):.4f}, max |d| {max(ad):.4f}")
    for t in (0.3, 0.4, 0.5, 0.6, 0.7):
        flips = sum(1 for i in both if (p_hard["together"][i] >= t) != (p_hard["hard"][i] >= t))
        w(f"  - flips at {t:.1f}: {flips}")
    kb = [i for i in ids if i in p_kind["together"] and i in p_kind["kind"]]
    agree = sum(1 for i in kb if p_kind["together"][i]["choice"] == p_kind["kind"][i]["choice"])
    w(f"kind: argmax agrees {agree}/{len(kb)}")
    for kk in KINDS:
        dk = [p_kind["together"][i]["probabilities"][kk] - p_kind["kind"][i]["probabilities"][kk] for i in kb]
        adk = [abs(x) for x in dk]
        w(f"  - p({kk}): mean signed {sum(dk) / len(dk):+.4f}, p95 |d| {pct(adk, 0.95):.4f}, max |d| {max(adk):.4f}")

    # --- noise on re-asking
    w("\n## noise on re-asking (together, reps 1-3 against rep 0, 100 roots)\n")
    rs = [i for i in ids if roots[i]["reask"]]
    dh, rng, dk = [], [], defaultdict(list)
    kflip = 0
    nk = 0
    for i in rs:
        ps = [ans[(i, "together", rep)] for rep in range(4) if (i, "together", rep) in ans]
        if len(ps) < 2:
            continue
        rng.append(max(p["hard"] for p in ps) - min(p["hard"] for p in ps))
        for p in ps[1:]:
            dh.append(abs(p["hard"] - ps[0]["hard"]))
            nk += 1
            kflip += p["kind"]["choice"] != ps[0]["kind"]["choice"]
            for kk in KINDS:
                dk[kk].append(abs(p["kind"]["probabilities"][kk] - ps[0]["kind"]["probabilities"][kk]))
    w(f"hard: {len(dh)} re-asks, moved {sum(1 for x in dh if x > 0)}, mean |d| {sum(dh) / len(dh):.4f}, "
      f"p95 |d| {pct(dh, 0.95):.4f}, max |d| {max(dh):.4f}; per-root range (max - min over 4 asks) "
      f"p95 {pct(rng, 0.95):.4f}, max {max(rng):.4f}")
    for t in (0.3, 0.4, 0.5, 0.6, 0.7):
        fl = 0
        for i in rs:
            ps = [ans[(i, "together", rep)]["hard"] for rep in range(4) if (i, "together", rep) in ans]
            fl += len({p >= t for p in ps}) > 1
        w(f"  - roots whose side of {t:.1f} changed across the 4 asks: {fl}/{len(rs)}")
    w(f"kind: argmax changed on {kflip}/{nk} re-asks")
    for kk in KINDS:
        w(f"  - p({kk}): p95 |d| {pct(dk[kk], 0.95):.4f}, max |d| {max(dk[kk]):.4f}")

    # --- operations and cost
    w("\n## operations and cost\n")
    st = Counter(r["status"] for r in recs)
    lat = [r["latency"] for r in recs if "answers" in r]
    w(f"calls {len(recs)}, statuses {dict(st)}, retries {sum(1 for r in recs if r['attempt'] > 0)}; "
      f"latency median {pct(lat, 0.5):.3f} s, p95 {pct(lat, 0.95):.3f} s")
    for name, pred in [("pilot", lambda r: r["form"].startswith("pilot-")),
                       ("together", lambda r: r["form"] == "together"),
                       ("kind alone", lambda r: r["form"] == "kind"),
                       ("hard alone", lambda r: r["form"] == "hard"),
                       ("all", lambda r: True)]:
        n, tok, usd = cost(recs, pred)
        w(f"- {name}: {n} answered calls, {tok} input tokens, {usd * 100:.3f}¢")
    return "\n".join(L)


def baselines(roots, ans):
    """Free checks, no calls: how far surface length alone gets on `hard`, and how many roots
    sit within the re-ask noise of each candidate threshold (together, rep 0)."""
    ids = list(roots)
    hard = lambda f: ([f(i) for i in ids if roots[i]["hard"]], [f(i) for i in ids if not roots[i]["hard"]])
    out = []
    out.append(f"length of new_message alone: AUC {auc(*hard(lambda i: len(roots[i]['text']))):.3f}")
    out.append(f"length of new_message + thread: AUC {auc(*hard(lambda i: len(roots[i]['text']) + len(roots[i]['thread']))):.3f}")
    out.append(f"word count of new_message: AUC {auc(*hard(lambda i: len(roots[i]['text'].split()))):.3f}")
    out.append(f"labelled kind is investigation (0/1): AUC {auc(*hard(lambda i: roots[i]['kind'] == 'investigation')):.3f}")
    p = {i: ans[(i, "together", 0)]["hard"] for i in ids}
    no_inv = [i for i in ids if roots[i]["kind"] != "investigation"]
    out.append("length AUC excluding investigation: "
               f"{auc([len(roots[i]['text']) for i in no_inv if roots[i]['hard']], [len(roots[i]['text']) for i in no_inv if not roots[i]['hard']]):.3f}; "
               f"Jev: {auc([p[i] for i in no_inv if roots[i]['hard']], [p[i] for i in no_inv if not roots[i]['hard']]):.3f}")
    short = [i for i in ids if len(roots[i]["text"]) <= 60]
    out.append(f"new_message of 60 chars or fewer (n {len(short)}, hard {sum(roots[i]['hard'] for i in short)}): Jev AUC "
               f"{auc([p[i] for i in short if roots[i]['hard']], [p[i] for i in short if not roots[i]['hard']]):.3f}, "
               f"length AUC {auc([len(roots[i]['text']) for i in short if roots[i]['hard']], [len(roots[i]['text']) for i in short if not roots[i]['hard']]):.3f}")
    for t in (0.5, 0.55, 0.6, 0.65, 0.7):
        near = [i for i in ids if abs(p[i] - t) <= 0.05]
        out.append(f"roots within 0.05 of {t:.2f}: {len(near)} (hard {sum(roots[i]['hard'] for i in near)})")
    return "\n".join(out)



if __name__ == "__main__":
    labels, results, which = sys.argv[1:4]
    roots, ans, recs = load(labels, results)
    if which == "pilot":
        pilot(roots, ans, recs)
    elif which == "main":
        out = main_report(roots, ans, recs) + "\n\n## baselines (no calls)\n\n" + baselines(roots, ans)
        print(out)
        if len(sys.argv) > 4:
            open(sys.argv[4], "w").write(out + "\n")
