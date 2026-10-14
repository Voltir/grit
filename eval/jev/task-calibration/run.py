"""Ask Jev the task questions (`kind`, a choice; `hard`, yes/no) about each labelled root.

Usage (the key is read from JEV_API_KEY alone; nothing here prints or logs it):
  python3 -I run.py LABELS.jsonl RESULTS.jsonl PLAN [--budget-cents 7]

PLAN is one of:
  pilot  -- each `hard` phrasing (H1, H2, H3) alone, on the 50 pilot roots (150 calls)
  followup-new  -- `together7` (seven kinds and hard) three times on each follow-up root (345)
  followup-orig -- `together7` once on each original root (400)
  followup-r-new  -- arm R (`together7r`, rich definitions) once on each follow-up root (115)
  followup-r-noise -- arm R's second ask on the follow-up roots, borderline first (up to 115)
  followup-r-orig -- arm R once on the original 100 `reask` roots (100)
  main   -- `together` (kind and hard in one request), `kind` alone and `hard` alone on every
            root (1,200 calls), then `together` three more times on the 100 `reask` roots (300)
The `hard` phrasing the main run asks is HARD_PICK, set from the pilot.

Request shape: grit's JevJson (questions keyed q1.., `choice` with criteria {key: description},
`noul` yes/no), state as live triage's TriageQuestion.State: {new_message, author, thread}.
Resumable: an (id, form, rep) already answered in RESULTS is skipped. Stops itself before a call
that could cross the budget; the spend counts every answered call in RESULTS, pilot included.
call() and the budget are the integrity experiment's (../integrity-features/run.py).
"""
import json, os, sys, time, urllib.error, urllib.request

ENDPOINT = "https://api.typesafe.ai/v1/systemone"
MODEL = "jev-latest"
USD_PER_M_INPUT = 0.042  # JevConfig.UsdPerMillionInput; output is not billed

LEAD = "Read new_message, which author sent to an AI assistant; thread is what came before it. "

KIND = {
    "type": "choice",
    "instructions": LEAD + "What does new_message ask the assistant to do?",
    "criteria": {
        "chat": "nothing to do: a greeting, thanks, an acknowledgement or small talk",
        "lookup": "find or recall a fact, a link, a name or a document",
        "investigation": "find out why something happened or what is going on, across several sources",
        "writing": "draft, rewrite, translate or summarise text",
        "coding": "write, fix, review or explain code",
        "decision": "weigh options and recommend or choose one",
    },
}
KINDS = list(KIND["criteria"])

# The follow-up's seven-kind question: `analysis` split from `investigation`, each kind defined in
# its criterion, and one line in the instructions telling the two apart.
KIND7 = {
    "type": "choice",
    "instructions": LEAD + "What does new_message ask the assistant to do? Analysis computes over "
                           "structured data; investigation reads and reconciles prose and history.",
    "criteria": {
        "chat": "nothing to do: a greeting, thanks, an acknowledgement or small talk",
        "lookup": "find or recall one fact, link, name or document",
        "investigation": "find out why something happened or what is going on by reading threads, docs, "
                         "notes, logs or tickets",
        "analysis": "compute over structured data such as tables, metrics, a database or a spreadsheet: "
                    "query, count, compare or chart",
        "writing": "draft, rewrite, translate or summarise text",
        "coding": "write, fix, review or explain code",
        "decision": "weigh options and recommend or choose one",
    },
}
KINDS7 = list(KIND7["criteria"])

# Arm R: the same seven keys and instructions, each criterion saying what the kind works over, what
# its answer looks like, and one example worded unlike any corpus root.
KIND7R = {
    "type": "choice",
    "instructions": KIND7["instructions"],
    "criteria": {
        "chat": "Nothing to do. Works over: the conversation itself. Answer looks like: a short social "
                "reply. e.g. 'nice one, have a good evening'",
        "lookup": "Find or recall one thing. Works over: docs, records or memory. Answer looks like: a "
                  "single fact, link, name or document. e.g. 'what's the phone number for the front desk?'",
        "investigation": "Find out why something happened or what is going on. Works over: prose and "
                         "history (threads, docs, notes, logs, tickets); find, read and reconcile what "
                         "people said or did. Answer looks like: a finding, with where it came from. "
                         "e.g. 'why did we stop the Contoso integration, and who decided?'",
        "analysis": "Compute over structured data. Works over: tables, metrics, a database or a "
                    "spreadsheet; query, count, compute and compare. Answer looks like: numbers, a table "
                    "or a chart, and what they show. e.g. 'average basket size per region last quarter, "
                    "as a table'",
        "writing": "Produce or rework text. Works over: the material and notes given. Answer looks like: "
                   "a draft, rewrite, translation or summary. e.g. 'turn my bullet points into a friendly "
                   "note for the parents' group'",
        "coding": "Write, fix, review or explain code. Works over: source code, errors and tests. Answer "
                  "looks like: code, a patch, or an explanation of code. e.g. 'make this shell script "
                  "exit as soon as any command fails'",
        "decision": "Choose between options. Works over: the options and what matters about them. Answer "
                    "looks like: a recommendation with its reasons. e.g. 'should our team try a four-day "
                    "week or flexible hours?'",
    },
}

HARD = {
    # the plan's wording (m11 §1.4)
    "H1": {"type": "noul", "instructions": LEAD + "Would a careful expert need several steps or sources "
                                                  "to do what new_message asks?"},
    # the plan's wording, with what does not count
    "H2": {"type": "noul", "instructions": LEAD + "Is what new_message asks hard: work a careful expert "
                                                  "would do in several steps, or by reading several sources, "
                                                  "rather than with one quick answer? A greeting, thanks or a "
                                                  "single fact is not hard."},
    # criteria on each side (JevJson's YesNo yes/no descriptions)
    "H3": {"type": "noul", "instructions": LEAD + "Does doing what new_message asks, done well, take a lot "
                                                  "of work?",
           "criteria": {"true": "several steps, several sources, or a long careful piece of work",
                        "false": "one short reply, a single fact or link, or nothing to do"}},
}
HARD_PICK = "H2"  # from the pilot (score.py pilot, 50 roots): AUC H1 0.983, H2 0.996, H3 0.988; H2 lowest on easy (0.104)


def state(root):
    return {"new_message": root["text"], "author": root["author"], "thread": root["thread"]}


def questions(form):
    """{qid: (name, question)} for a form."""
    if form == "together":
        return {"q1": ("kind", KIND), "q2": ("hard", HARD[HARD_PICK])}
    if form == "together7":
        return {"q1": ("kind", KIND7), "q2": ("hard", HARD[HARD_PICK])}
    if form == "together7r":
        return {"q1": ("kind", KIND7R), "q2": ("hard", HARD[HARD_PICK])}
    if form == "kind":
        return {"q1": ("kind", KIND)}
    if form == "hard":
        return {"q1": ("hard", HARD[HARD_PICK])}
    if form.startswith("pilot-"):
        return {"q1": ("hard", HARD[form[len("pilot-"):]])}
    raise ValueError(form)


def payload(root, form):
    return {"model": MODEL, "state": state(root),
            "questions": {q: spec for q, (_, spec) in questions(form).items()}}


def call(key, body):
    req = urllib.request.Request(ENDPOINT, data=json.dumps(body).encode(), method="POST",
                                 headers={"Authorization": "Bearer " + key, "Content-Type": "application/json",
                                          "User-Agent": "grit-eval"})
    t = time.time()
    try:
        with urllib.request.urlopen(req, timeout=60) as r:
            status, raw = r.status, r.read().decode()
    except urllib.error.HTTPError as e:
        status, raw = e.code, e.read().decode(errors="replace")[:600]
    except Exception as e:  # network; the message never holds the request
        status, raw = None, f"{type(e).__name__}: {e}"[:300]
    return status, raw, time.time() - t


def read(raw, form):
    """({name: answer}, input tokens, model, usage) or an error string. A choice's answer is
    {choice, probabilities, confidence}; a yes/no's is its probability of yes."""
    try:
        d = json.loads(raw)
        ans = d["answers"]
        out = {}
        for q, (name, spec) in questions(form).items():
            a = ans[q]
            if spec["type"] == "choice":
                keys = list(spec["criteria"])
                assert a["choice"] in keys, a["choice"]
                out[name] = {"choice": a["choice"], "confidence": float(a["confidence"]),
                             "probabilities": {k: float(a["probabilities"].get(k, 0.0)) for k in keys}}
            else:
                out[name] = float(a["noul"])
        return out, int(d.get("usage", {}).get("input_tokens", 0)), d.get("model"), d.get("usage")
    except Exception as e:
        return f"unreadable: {type(e).__name__}: {e}"


def plan(roots, which):
    if which == "pilot":
        return [(r, f"pilot-{h}", 0) for h in HARD for r in roots if r["pilot"]]
    if which == "main":
        assert HARD_PICK in HARD, "run the pilot and set HARD_PICK first"
        once = [(r, f, 0) for f in ("together", "kind", "hard") for r in roots]
        again = [(r, "together", rep) for rep in (1, 2, 3) for r in roots if r["reask"]]
        return once + again
    if which == "followup-new":  # the follow-up's 115 roots, three asks each
        return [(r, "together7", rep) for rep in (0, 1, 2) for r in roots]
    if which == "followup-orig":  # the original 400, once, with seven kinds
        return [(r, "together7", 0) for r in roots]
    if which == "followup-r-new":  # arm R once on each follow-up root
        return [(r, "together7r", 0) for r in roots]
    if which == "followup-r-noise":  # arm R's second ask, borderline roots first; the budget cuts it
        return [(r, "together7r", 1) for r in sorted(roots, key=lambda r: r["genre"] != "border")]
    if which == "followup-r-orig":  # arm R once on the original 100-root reask subset (budget)
        return [(r, "together7r", 0) for r in roots if r["reask"]]
    raise ValueError(which)


def main(labels, results, which, budget_cents):
    key = os.environ.get("JEV_API_KEY", "").strip()
    if not key:
        sys.exit("JEV_API_KEY is not set")
    roots = [json.loads(l) for l in open(labels)]
    done, spent = set(), 0.0
    if os.path.exists(results):
        for l in open(results):
            r = json.loads(l)
            spent += r.get("usd", 0.0)
            if "answers" in r:
                done.add((r["id"], r["form"], r["rep"]))
    todo = [(r, f, rep) for r, f, rep in plan(roots, which) if (r["id"], f, rep) not in done]
    print(f"{len(todo)} to ask; spent so far {spent * 100:.3f}¢", flush=True)
    with open(results, "a") as out:
        for n, (r, form, rep) in enumerate(todo):
            body = payload(r, form)
            worst = len(json.dumps(body)) * USD_PER_M_INPUT / 1e6  # every character a token
            if (spent + worst) * 100 > budget_cents:
                print(f"budget stop at {spent * 100:.3f}¢", flush=True)
                return
            for attempt in range(4):
                status, raw, latency = call(key, body)
                rec = {"id": r["id"], "form": form, "rep": rep, "status": status,
                       "latency": round(latency, 3), "attempt": attempt, "t": time.time()}
                if status == 200:
                    got = read(raw, form)
                    if isinstance(got, tuple):
                        answers, tokens, model, usage = got
                        rec.update(answers=answers, input_tokens=tokens, model=model, usage=usage,
                                   usd=tokens * USD_PER_M_INPUT / 1e6)
                        spent += rec["usd"]
                    else:
                        rec.update(error=got, raw=raw[:600])
                else:
                    rec.update(error=f"HTTP {status}", raw=raw[:600])
                out.write(json.dumps(rec) + "\n"); out.flush()
                if "answers" in rec:
                    break
                time.sleep(2 ** attempt * (5 if status in (429, 529) else 1))
            if n % 100 == 99:
                print(f"{n + 1} asked; {spent * 100:.3f}¢", flush=True)
    print(f"done; spent {spent * 100:.3f}¢", flush=True)


if __name__ == "__main__":
    a = sys.argv[1:]
    bud = float(a[a.index("--budget-cents") + 1]) if "--budget-cents" in a else 7.0
    main(a[0], a[1], a[2], bud)
