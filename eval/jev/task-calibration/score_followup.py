"""Score the follow-up: seven kinds, arms S (short definitions) and R (rich), the borderline set.

  python3 -I score_followup.py LABELS.jsonl FOLLOWUP_LABELS.jsonl MAIN_RESULTS.jsonl FOLLOWUP_RESULTS.jsonl [OUT.md]

S is form `together7` (3 asks on the follow-up roots, 1 on the original 400); R is `together7r`
(1 ask on every follow-up root and on the original 100 `reask` roots, a second on the follow-up
roots the budget reached). A kind answer is right when it is one of the root's acceptable kinds;
"clear" roots have exactly one.
"""
import json, sys
from collections import Counter, defaultdict

import os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from score import auc, auc_ci, pct  # noqa: E402

K7 = ["chat", "lookup", "investigation", "analysis", "writing", "coding", "decision"]
ARMS = {"S": "together7", "R": "together7r"}


def load(path):
    out = defaultdict(dict)  # (id, form) -> {rep: answers}
    recs = []
    for l in open(path):
        r = json.loads(l)
        recs.append(r)
        if "answers" in r:
            out[(r["id"], r["form"])][r["rep"]] = r["answers"]
    return out, recs


def report(labels, flabels, main_results, fresults):
    old = {r["id"]: r for r in (json.loads(l) for l in open(labels))}
    for r in old.values():
        r["kinds"] = [r["kind"]]
    new = {r["id"]: r for r in (json.loads(l) for l in open(flabels))}
    roots = {**old, **new}
    fa, frecs = load(fresults)
    ma, _ = load(main_results)
    L = []
    w = L.append

    def ask(i, arm, rep=0):
        return fa.get((i, ARMS[arm]), {}).get(rep)

    def hard_block(arm, ids, title):
        ps = {i: ask(i, arm)["hard"] for i in ids if ask(i, arm)}
        pos = [ps[i] for i in ps if roots[i]["hard"]]
        neg = [ps[i] for i in ps if not roots[i]["hard"]]
        a, lo, hi = auc_ci(pos, neg) if pos and neg else (float("nan"),) * 3
        w(f"- {title}: AUC {a:.3f} [{lo:.3f}, {hi:.3f}] (n {len(pos)} hard / {len(neg)} not)")
        return ps

    # ---- hard on the follow-up roots
    w("## hard on the follow-up roots (rep 0)\n")
    border = [i for i in new if new[i]["genre"] == "border"]
    for arm in ARMS:
        w(f"\n### arm {arm}\n")
        ps = hard_block(arm, border, "borderline, all 50")
        hard_block(arm, [i for i in border if new[i]["sure"]], "borderline, labels sure")
        hard_block(arm, [i for i in border if not new[i]["sure"]], "borderline, labels unsure")
        hard_block(arm, [i for i in border if new[i]["thread"]], "borderline with a thread")
        hard_block(arm, [i for i in new if new[i]["genre"] == "analysis"], "analysis roots")
        hard_block(arm, list(new), "all 115 follow-up roots")
        w("\nborderline: threshold sweep\n")
        w("| threshold | strong | precision | recall | easy routed strong |")
        w("|---|---|---|---|---|")
        npos = sum(1 for i in ps if roots[i]["hard"])
        nneg = len(ps) - npos
        for t in (0.3, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7):
            s = [i for i in ps if ps[i] >= t]
            tp = sum(1 for i in s if roots[i]["hard"])
            w(f"| {t:.2f} | {len(s)} | {tp}/{len(s)} = {tp / len(s) if s else float('nan'):.3f} | "
              f"{tp}/{npos} = {tp / npos:.3f} | {len(s) - tp}/{nneg} |")
        w("\nborderline roots, sorted by p(hard) (label, sure?, p, text):")
        for i in sorted(ps, key=lambda i: ps[i]):
            r = roots[i]
            t = r["text"] if len(r["text"]) < 70 else r["text"][:67] + "..."
            w(f"  - {ps[i]:.2f} {'HARD' if r['hard'] else 'easy'} {'sure' if r['sure'] else 'UNSURE'} "
              f"{i} {t!r}{' +thread' if r['thread'] else ''}")

    # ---- noise
    w("\n## noise on re-asking, follow-up roots\n")
    for arm in ARMS:
        d, rng, near_d, cross = [], [], [], defaultdict(int)
        n = 0
        for i in new:
            reps = fa.get((i, ARMS[arm]), {})
            if len(reps) < 2:
                continue
            n += 1
            hs = [reps[k]["hard"] for k in sorted(reps)]
            rng.append(max(hs) - min(hs))
            for h in hs[1:]:
                d.append(abs(h - hs[0]))
                if 0.4 <= hs[0] <= 0.8:
                    near_d.append(abs(h - hs[0]))
            for t in (0.5, 0.55, 0.6, 0.65):
                cross[t] += len({h >= t for h in hs}) > 1
        w(f"- arm {arm}: {n} roots with 2+ asks, {len(d)} re-asks; |d| mean {sum(d) / len(d):.4f}, "
          f"p95 {pct(d, 0.95):.3f}, max {max(d):.3f}; per-root range p95 {pct(rng, 0.95):.3f}, max "
          f"{max(rng):.3f}; re-asks of roots first scored 0.4-0.8: n {len(near_d)}, p95 "
          f"{pct(near_d, 0.95):.3f}, max {max(near_d) if near_d else float('nan'):.3f}; roots whose side "
          f"changed: " + ", ".join(f"{t:.2f}: {c}/{n}" for t, c in cross.items()))
        flips = sum(1 for i in new if len(fa.get((i, ARMS[arm]), {})) > 1 and
                    len({fa[(i, ARMS[arm])][k]["kind"]["choice"] for k in fa[(i, ARMS[arm])]}) > 1)
        w(f"  kind argmax changed across asks on {flips}/{n} roots")

    # ---- kind
    w("\n## kind, seven choices\n")

    def kind_acc(arm, ids, title):
        got = [(i, ask(i, arm)["kind"]["choice"]) for i in ids if ask(i, arm)]
        ok = sum(1 for i, c in got if c in roots[i]["kinds"])
        w(f"- arm {arm}, {title}: {ok}/{len(got)} = {ok / len(got):.3f}")
        return got

    clear_new = [i for i in new if len(new[i]["kinds"]) == 1]
    sub = [i for i in old if old[i]["reask"]]
    for arm in ARMS:
        kind_acc(arm, clear_new, f"clear follow-up roots")
        kind_acc(arm, sub, "original 100 reask roots (original labels; analysis never right)")
    kind_acc("S", list(old), "original 400 (original labels)")
    w(f"- six-kind together, original 100 reask roots, for reference: "
      f"{sum(1 for i in sub if ma[(i, 'together')][0]['kind']['choice'] == old[i]['kind'])}/100")

    for arm in ARMS:
        w(f"\n### arm {arm}: analysis vs investigation, clear roots (rows: label; columns: choice)\n")
        ids = [i for i in roots if len(roots[i]["kinds"]) == 1 and roots[i]["kind"] in ("analysis", "investigation")
               and ask(i, arm)]
        w("| label \\ choice | " + " | ".join(K7) + " |")
        w("|---|" + "---|" * len(K7))
        for lk in ("analysis", "investigation"):
            c = Counter(ask(i, arm)["kind"]["choice"] for i in ids if roots[i]["kind"] == lk)
            w(f"| {lk} ({sum(c.values())}) | " + " | ".join(str(c[x]) for x in K7) + " |")
        for lk in ("analysis", "investigation"):
            pos = [ask(i, arm)["kind"]["probabilities"][lk] for i in ids if roots[i]["kind"] == lk]
            neg = [ask(i, arm)["kind"]["probabilities"][lk] for i in ids if roots[i]["kind"] != lk]
            w(f"  - p({lk}) separates {lk} from the other of the pair: AUC {auc(pos, neg):.3f}")
        mixed = [i for i in new if new[i]["genre"] == "mixed"]
        pa = [ask(i, arm)["kind"]["probabilities"]["analysis"] for i in mixed]
        pi = [ask(i, arm)["kind"]["probabilities"]["investigation"] for i in mixed]
        w(f"\nmixed roots (15): choice {dict(Counter(ask(i, arm)['kind']['choice'] for i in mixed))}; "
          f"mean p(analysis) {sum(pa) / 15:.3f}, mean p(investigation) {sum(pi) / 15:.3f}, mean sum "
          f"{sum(a + b for a, b in zip(pa, pi)) / 15:.3f}; roots where both >= 0.2: "
          f"{sum(1 for a, b in zip(pa, pi) if a >= 0.2 and b >= 0.2)}")
        for i, a, b in zip(mixed, pa, pi):
            w(f"  - {i} analysis {a:.2f} investigation {b:.2f} {new[i]['text'][:70]!r}")
        w("\nother kind errors on clear follow-up roots:")
        for i in clear_new:
            c = ask(i, arm)["kind"]["choice"]
            if c not in new[i]["kinds"]:
                w(f"  - {i} {new[i]['kind']} -> {c} {new[i]['text'][:70]!r}")
        olds = [i for i in (old if arm == "S" else sub) if ask(i, arm)]
        moved = [(i, old[i]["kind"]) for i in olds if ask(i, arm)["kind"]["choice"] == "analysis"]
        w(f"\noriginal roots now chosen as analysis ({len(moved)}/{len(olds)}):")
        for i, k in moved:
            w(f"  - {i} (labelled {k}) {old[i]['text'][:80]!r}")

    # ---- hard and chat on the original 400, six kinds vs seven
    w("\n## hard and the chat veto on the original roots: six kinds vs seven\n")
    for arm, ids in (("S", list(old)), ("R", sub)):
        d = [ask(i, arm)["hard"] - ma[(i, "together")][0]["hard"] for i in ids]
        ad = [abs(x) for x in d]
        p7 = {i: ask(i, arm)["hard"] for i in ids}
        p6 = {i: ma[(i, "together")][0]["hard"] for i in ids}
        a7 = auc([p7[i] for i in ids if old[i]["hard"]], [p7[i] for i in ids if not old[i]["hard"]])
        a6 = auc([p6[i] for i in ids if old[i]["hard"]], [p6[i] for i in ids if not old[i]["hard"]])
        fl = sum(1 for i in ids if (p7[i] >= 0.6) != (p6[i] >= 0.6))
        s7 = [i for i in ids if p7[i] >= 0.6]
        tp = sum(1 for i in s7 if old[i]["hard"])
        nh = sum(1 for i in ids if old[i]["hard"])
        w(f"- arm {arm} on {len(ids)} roots: hard shift (7 - 6 kinds) mean {sum(d) / len(d):+.4f}, p95 |d| "
          f"{pct(ad, 0.95):.3f}, max |d| {max(ad):.3f}; AUC six kinds {a6:.3f}, seven {a7:.3f}; flips at 0.6: "
          f"{fl}; at 0.6 with seven: precision {tp}/{len(s7)}, recall {tp}/{nh}")
        chat = [i for i in ids if old[i]["kind"] == "chat"]
        notchat = [i for i in ids if old[i]["kind"] != "chat"]
        cc = sum(1 for i in chat if ask(i, arm)["kind"]["choice"] == "chat")
        fp = [i for i in notchat if ask(i, arm)["kind"]["choice"] == "chat"]
        mh = max(ask(i, arm)["hard"] for i in chat)
        w(f"  chat veto: labelled chat chosen chat {cc}/{len(chat)}; other roots chosen chat {len(fp)} "
          f"{fp}; max p(hard) on chat roots {mh:.3f}")

    # ---- cost and tokens
    w("\n## cost and tokens\n")
    st = Counter(r["status"] for r in frecs)
    for arm, f in ARMS.items():
        rs = [r for r in frecs if r["form"] == f and "answers" in r]
        tok = sum(r["input_tokens"] for r in rs)
        w(f"- arm {arm}: {len(rs)} calls, {tok} input tokens, {tok / len(rs):.1f} per call, "
          f"{sum(r['usd'] for r in rs) * 100:.3f}¢")
        for g, ids in (("follow-up roots", new), ("original roots", old)):
            x = [r["input_tokens"] for r in rs if r["id"] in ids]
            if x:
                w(f"  - {g}: {sum(x) / len(x):.1f} per call (n {len(x)})")
    allusd = sum(r.get("usd", 0.0) for r in frecs)
    w(f"- follow-up total: {len(frecs)} calls, statuses {dict(st)}, "
      f"{sum(r.get('input_tokens', 0) for r in frecs)} input tokens, {allusd * 100:.3f}¢")
    return "\n".join(L)


if __name__ == "__main__":
    out = report(*sys.argv[1:5])
    print(out)
    if len(sys.argv) > 5:
        open(sys.argv[5], "w").write(out + "\n")
