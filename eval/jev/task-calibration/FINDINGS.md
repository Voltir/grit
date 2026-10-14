# Jev task calibration, kind and hard (2026-10-13)

Labels, harness and scorers are in this folder, run with `python3 -I`; results are not kept in
the repository. Cost **3.31¢** (1,650 calls, 787,815 Jev input tokens × $0.042/M). Model
`jev-1.13.0`.

## Answer
1. **`hard` separates, well above the 0.75 bar: AUC 0.998 [0.995, 0.999]** together, 0.998 alone
   (115 hard / 285 not). It is 0.998 on the 350 roots outside the pilot, and 0.98–1.00 within
   each labelled kind, so it is not just reading the kind.
2. **Threshold: route strong at p(hard) ≥ 0.6.** Precision 109/110 (0.991), recall 109/115
   (0.948), and 1 of 285 easy roots routed strong. Re-asking moves p(hard) by at most 0.05
   (p95 0.02), and asking together rather than alone by at most 0.04. Only 5 roots fall within
   0.05 of 0.6, and none of the 100 re-asked roots crossed it in four asks. 0.5 costs 10 easy roots
   for 1 more hard; 0.7 is perfectly precise but drops recall to 0.90.
3. **Ask the questions together.** The together-vs-alone shift is noise-sized: hard's mean signed
   shift is +0.0002, p95 |Δ| 0.02, with 0 flips at 0.5 and 1 at 0.6; kind's argmax agrees on
   398/400. One call averages 556 input tokens, against 488 + 373 = 861 for two.
4. **Kind is near-perfect: argmax accuracy 391/400 together** (393 alone), AUC ≥ 0.990 for every
   kind. Chat is 100/100, both ways. Of the 9 errors (together), the largest group is 3
   coding-knowledge questions chosen as lookup (`let` vs `const`, Big-O of binary search, grep's
   `-n`). Those are defensibly lookups, so they are as much a labelling call as an error. The
   others:
   - 2 investigations chosen as lookup: "is the status page showing an incident", and "which
     actions are done? check the tickets";
   - 1 naming ask chosen as decision;
   - 3 meeting-notes questions.

   Meeting-notes questions take the kind of what they ask (46/50).
5. **What `Shipped` should read:** `hard` for the grade. Kind adds nothing to the grade here,
   because `hard` already separates within every kind. Read **`chat` as a veto** (chat ⇒ standard).
   It is free, and it guards the thread-leak case. Measured: acknowledgements after a hard thread
   score mean p(hard) 0.051, after easy news 0.036, max 0.19, so nothing leaked, but the veto is
   the belt. Keep all six as recorded features. Don't route on lookup, coding or the other kinds.
6. **Surprise, what it misses:** all 5 hard roots under 0.5 put their difficulty in the thread,
   not in the new message. Examples are "what would you choose?" after a three-option sharding
   debate, "turn the whole thread into a decision record" and "what's going on here?". At 0.6 these
   go standard, which fails safe. The easy roots Jev rates highest are one-log "why did the
   deploy/backup fail" asks (0.59–0.69), which are borderline even for a person.

## Data
- **Corpus:** 400 synthetic roots, all written by the agent, with no web fetch and no deployment
  data. There are 50 in each of eight genres (chat, lookup, investigation, writing, coding,
  decision, meeting-notes question, acknowledgement).
- **Labels:** labelled per message for kind (chat 100, lookup 79, investigation 56, writing 61,
  coding 50, decision 54) and hard (115). The labels were frozen in commit `b777bf7b` before any
  call.
- **Threads:** 139 roots have a thread. Every acknowledgement does, and each of the 25 ack texts
  appears once after a hard thread and once after easy news.
- **State and request:** the state is live triage's `{new_message, author, thread}`. The request
  is JevJson's shape: kind as a `choice` with a description per key, hard as a `noul`.
- **Pilot:** 50 roots, hard alone. The AUCs were H1 (the plan's words) 0.983, H2 (the plan's words
  plus "a greeting, thanks or a single fact is not hard") 0.996, and H3 (criteria on both sides)
  0.988. H2 was kept, with the lowest mean on easy roots (0.10). Wording is in `run.py`.
- **Main run:**
  - asked together once (400 calls);
  - each question alone once (800);
  - together three more times on 100 roots (300).

  All 1,650 calls returned 200, with no retries. Median latency 0.128 s, p95 0.162 s.
- **Noise:** p(hard) moved on 137 of 300 re-asks, with mean |Δ| 0.006, p95 0.02 and max 0.05.
  Kind's argmax changed on 5/300 re-asks, and a kind probability moved by at most 0.12.

## Limits (read before trusting the AUC)
- **One author wrote both the texts and the labels**, so the corpus is cleaner than real
  traffic. Length alone gets AUC 0.82 on `hard`. But on the 246 roots of 60 characters or fewer
  (21 hard), Jev holds 0.993 where length gets 0.59, so Jev is not reading length. It may still be
  reading cues such as "check the logs, the alerts and the deploy history" that a real person
  leaves out.
- **Real asks are terser and lean on the thread more.** The thread-borne hard roots are exactly
  where Jev weakens: AUC 0.98 on roots with a thread, and all 5 of its misses are of this kind.
  Expect lower recall on real traffic, not lower precision.
- English only, and no Slack-shaped markup or mentions. n gives ±3 points on precision at 0.6.

## Recommendation
Ship `TaskQuestions` as one call: kind (the six keys, the descriptions in `run.py`) and hard
(H2's words). Set `Shipped` to strong when hard ≥ 0.6 and kind's argmax is not chat, and
standard otherwise. Re-check on real roots once the route step records its features: recall on
thread-borne asks is the number to watch.

## Parked
- A thread-aware hard question ("counting what the thread asks for"), aimed at the 5 misses.
- Calibrating the threshold on recorded live features.
- Whether the router should read the kind confidence.

## Follow-up (2026-10-13)

**What was asked:**
- **New roots:** 115, committed in `a50b88d2` before any call:
  - 50 terse borderline roots: 25 hard, 26 labelled *unsure*, 41 with a thread, and no listed sources;
  - 50 `analysis` roots, 20 of them hard;
  - 15 roots that are both analysis and investigation.
- **Kind question:** seven kinds now, `analysis` split from `investigation`.
- **Two arms:**
  - **S:** one-line definitions;
  - **R:** each definition says what it works over, what the answer looks like, and gives one example. Its examples are worded unlike any root.
- **Asks:**
  - S on the new roots ×3 and on the original 400 ×1;
  - R on the new roots ×1, a second ask on 87 of them (all 50 borderline, 37 analysis), and the original 100-root re-ask subset ×1.

**R was cut to fit the 3¢ stop.** The full design measured at 3.3¢. Wording is in `run.py` (`KIND7`, `KIND7R`).

**Answer:**
1. **`hard` holds on terse, borderline text: AUC 0.978 [0.933, 1.000] in S** (R 0.976).
   - It is 1.000 on the 24 *sure* labels and 0.944 on the 26 *unsure* ones. Every ranking error falls among roots I was unsure of myself.
   - The analysis roots score 0.996. All 115 new roots together score 0.971.
2. **At 0.6 on the borderline set: precision 17/17, recall 17/25 (0.68).** Every borderline easy root scored ≤ 0.41, and a threshold of 0.45 reaches recall 22/25 with no easy root strong. The price of going lower shows on the original set: 8 easy roots score 0.52–0.57 there. They are one-log investigations, "add tests for the edge cases mentioned", and two-option choices.
   - Pooled over all 515 roots in S, sweeping the threshold down from 0.6 to 0.5 trades precision for recall:

     | threshold | precision | recall |
     |---|---|---|
     | 0.6 | 0.981 | 0.902 |
     | 0.55 | 0.964 | 0.919 |
     | 0.5 | 0.916 | 0.948 |
   - **Recommendation: keep 0.6.** A miss falls to standard, which fails safe. The alternative is 0.5 if a missed hard turn is priced above roughly a 9% waste of strong calls. That is a cost call, not a calibration one.
3. **Noise near the threshold is a little higher on terse text.**
   - S: |Δ| p95 0.04, max 0.07, against 0.05 on the original corpus. Roots first scored 0.4–0.8 show the same p95 0.04 and max 0.07.
   - Roots that changed side over 3 asks: 3 of 115 at 0.6, and 6 at 0.55.
   - The margin at 0.6 still holds: the nearest borderline roots sit at 0.59 and 0.61, both hard, so a flip there costs nothing.
   - Kind's top choice never changed across asks (0/115 in S, 0/87 in R).
4. **Analysis vs investigation separates on clear roots.** In S, analysis is chosen 49/51 and investigation 64/69, with 4 investigations taken as analysis. AUC is 0.995 / 0.996.
   - **The mixed roots do not split; they go to investigation.** That is 14/15 in both arms: S mean p(analysis) 0.10 against p(investigation) 0.90, and only 3 of 15 put ≥ 0.2 on both.
   - Jev reads "why did signups drop in March?" as investigation. Only "did the pricing change hurt conversion?" went to analysis.
5. **Adding `analysis` did not move `hard` or the chat veto.**
   - **`hard`:** on the original 400 (S), the shift from six kinds to seven has mean −0.0001 and max |Δ| 0.03. AUC stays 0.998, with 1 flip at 0.6.
   - **Chat:** chosen for 99/100 chat roots. "Tell me a joke" went to writing, with p(hard) 0.04, so the veto lost nothing. No other root was chosen chat, and the max p(hard) on chat roots is 0.20.
   - **Six-kind accuracy fell 391 → 380** because 11 original roots now choose analysis. Most are defensible: monthly revenue from the finance sheets, the AWS bill broken down by service, the investor update's numbers.
6. **Arm S over arm R.** R matches S on `hard` (borderline AUC 0.976 against 0.978) and on analysis/investigation. It is worse on kind for the clear new roots, 82/93 against 87/93. The examples pulled terse asks toward surface matches:
   - "did we ever settle…" and "what did legal say…" went to investigation;
   - "fix?" went to writing;
   - "how bad is it?" went to chat.

   On the original 100 subset, R scores 93/100 against S's 94/100. R costs **852 input tokens per call against S's 607 (+40%)**.

**Recommendation:**
- **Threshold:** keep 0.6.
- **Kind question:** ship arm S's seven-kind question.
- **`analysis`:** keep it as a recorded kind, used as a **tool hint only** (offer the structured-data tools). Don't merge it into investigation, because the clear cases separate. Don't route the grade on it either: `hard` already carries the grade, and a metrics "why" question lands in investigation.
- **The hint should read "analysis or investigation":** any data tool offered on analysis should also be offered on investigation, or the mixed asks miss it.

**Cost: 2.98¢** (1,047 calls, 709,659 input tokens; S 1.90¢, R 1.08¢), all calls 200. The total is 6.29¢ with the main run.

**Limits:**
- The same author wrote and labelled the text again. The *unsure* labels are where the ranking errs, so real disagreement between labellers is likely about that size.
- R's noise rests on 87 roots with two asks, and its original-corpus check on 100 roots.
- The four S investigation→analysis cases are among the original roots, which name tables.

**Parked:**
- Reading p(analysis) beside the top choice for the tool hint.
- A threshold set from the real price ratio of strong to standard.
