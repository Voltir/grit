# Jev placement for the knowledge plugin: findings

The spike ran on a snapshot of a deployment's database, taken 2026-11-19. Jev's spend was 38.80¢ in total
(20.52¢ through P2, E9 none, E10 11.35¢, E11 4.46¢, E12 2.47¢), counted from the API's reported input tokens
(`spend.json`). No other model was called.
`out/` holds ids and probabilities only. The text-bearing case files and label sheets are in the
untracked `data/`.

## Label sets

**L1** comes from the 63 usable runs of the topics job's attach ask. It holds 498 items:

| Item | New | Aside | An existing topic |
|---|---|---|---|
| Discussion | 143 | 9 | 6 |
| Card | 62 | 278 | 0 |

Retention purged the text of 78 of the 158 discussions.

| Set | What it holds |
|---|---|
| L1S | 80 discussions with text |
| L1C | 80 cards, from nights that showed topics |
| SELF | 46 discussions, each against all 69 topics ever opened; the answer is the topic it opened |
| L2 | the 393 recorded stitch placements |

SELF is easier than a real continuation, because the topic's lead was written from the discussion. L2 has no label independent of Jev.

61 of the 69 topics ever opened hold one discussion, and one topic holds a unit.

## Results

| | Finding | Cost |
|---|---|---|
| E0 key count | Top-1 by keys: 5 → 40/40; 10 → 39/40; 20 → 39/40; 42 → 39/40; 69 → 38/40. No position bias (first and last thirds both 13/13 at 42 and at 69). Probabilities sum to 1; no refusals; under 0.2 s a call. Requests of 5 to 40 YesNo questions all answered. Every miss was one near-duplicate pair of topics. | 2.21¢ |
| E1 BM25 shortlist | SELF recall@8: 0.848 over leads alone, 0.957–0.978 over lead plus passage (leaky). The 4 real continuations rank 13, 2, 12 and 2. | free |
| E2 question shape | (a) all-topics Choice: SELF 0.891, MRR 0.925. (b) top-8 Choice: 0.826. (c) top-8 YesNo: 0.739. (d) Score: 0.761. Agreement with Haiku on new versus existing: 0.21–0.33. On cards: (c) 0.887, (a) 0.762. | 4.72¢ |
| E7 calibration | Against Haiku on L1S, AUROC 0.48–0.59 for every arm. SELF with arm (a): 0.898, 95.1% correct at p ≥ 0.86 and 89% coverage. L1C with arm (c): 0.997, 95.9% at yes ≥ 0.72 and 92.5% coverage. The margin adds nothing over the top probability. | free |
| E3/E4 representation | Topic as lead plus newest-passage head: SELF 0.957 (3.9k tokens, against 2.2k for the lead). First line of the lead: 0.891. Discussion cut to 800 characters: 0.891; opening plus the latest two: 0.870. Card as title alone: 0.600, against 0.762 with its summary. | 9.23¢ |
| E5 keep as its own question | The separate yes/no sets aside more of SELF's real topics (10, against 3). It waits on keep/junk labels. | 1.68¢ |
| E6 grouping new items | Pairwise "same matter?": AUROC 0.506, precision 4/7, recall 4/20. Sequential placement does no better. | 0.20¢ |
| P1 message as query | On the 97 passed candidates the snapshot still holds: recall@30 0.526 with the message, 0.887 with the written query. | free |
| P1b query gate | AUROC: Jev 0.535 and 0.590; message length 0.699. | 0.24¢ |
| P2 ranks 31–60 | Re-rendering matched the recorded verdicts on 96.2% (r 0.885). Pass rate by rank: 13.9% (1–10), 6.0% (11–20), 3.1% (21–30), 3.6% (31–40), 3.6% (41–50), 2.2% (51–60). Ranks 31–60 add 28% more passes. | 2.23¢ |
| P3 edges | 142 strand refs, 122 card pointers, 1,859 pieces in 24 sets, 77 passage sources, 0 topic units. | free |
| E8 room era | 31.1% of messages since the rooms switch are in threads. | free |

## E9: a shortlist of at most 64 topics, picked by code (no model calls)

This checks the design where code picks the shortlist and Jev's one Choice runs over it. Run with
`SPIKE_DB=<snapshot> python3 -I e9.py`, then `e9b.py`. Results are in `out/e9.json` and
`out/e9b.json`.

**The pool.** The real topics are shown as lead plus passages, at the version each case saw. The
distractors are pseudo-topics: every card version's body (341) and every piece's text (1,859).
Each case is run at three pool sizes: the real topics alone (about 69), 500, and the whole pool
(about 2,200).

**The cases.**

| Set | Cases | What it is |
|---|---|---|
| SELF | 46 | Each discussion against the topic it opened, at that topic's latest text. Leaky. |
| CONT | 4 | Haiku's continuations that still have text, against the topic as shown that night. |
| HIST-new | 4 | From topic history: a passage that a different discussion added, against the version before. These are the real placements. |
| HIST-same | 21 | The same as HIST-new, but the discussion was already in the topic's lineage. grit attaches these without asking. |
| L1C | 0 | Haiku chose no topic for any card, so no card has a right answer. |

The lead + passages + card-titles variant equals lead + passages: no topic's lineage names a
card, and all topics have 0 units. For the same reason, structural source (b) has only its strand
side: topics whose lineage names a discussion from the same thread or the same room. The
card-link side has no edges to follow.

**Where the right topic ranks**, for each source, in the whole pool (about 2,200):

| Set | BM25, lead + passages | BM25, lead only | (b) structural | (c) recency |
|---|---|---|---|---|
| CONT + HIST-new (8) | 1, 2, 2, 3, 15, 15, 22, 48 | 2, 2, 24, 24, 190, 221, 840, 1033 | 2, 3, 4, 5, 5, 5, 8, 9 | 2, 3, 4, 6, 6, 6, 8, 9 |
| HIST-same (21) | 18 at rank 1; then 2, 8, 23 | as bad as rank 1,368 | all at rank 1 | at most 27 |
| SELF (46) | 45 at rank 1; then 23 | as bad as rank 134 | 8 of 46 in its list (own discussion excluded) | spread 2–70 |

Recall at k:

- **BM25 over lead + passages** finds every case within 64 at every pool size, for every set.
  Within 32 it misses one: a continuation at rank 48 in the whole pool (CONT 3/4 at 32).
  HIST-new is 4/4, HIST-same 21/21 and SELF 46/46 at 32. At k=16 it gets 3/4, 3/4, 20/21 and
  45/46.
- **Correction.** A first run counted which topics existed at a case's time using the backfill's
  replayed day, so three CONT cases saw a single real topic. Counting by the run's wall-clock
  instant (its slot) gives 3, 15, 15 and 67 real topics. The numbers here are from the corrected
  run.
- **BM25 over the lead alone** degrades as the pool grows. At k=64, CONT and HIST-new each fall
  from 4/4 (pool 69) to 2/4 (all), and SELF falls from 46/46 to 41/46.
- **The share of cases whose right topic falls outside 64 (the false-`new` risk)** is 0 of 75
  with lead + passages, whatever the union.
- **Union with b ≤ 16 and c ≤ 8 pinned first**: every set reaches 100% at k=32 and k=64. At k=16
  it starves the text source: SELF 9/46.

**Recency and text combined** (`e9b.py`). Structural (b ≤ 16) is always pinned first. Recency is
counted as of each case's run day; a pseudo-topic's age is its card's or piece set's write time.
The table gives SELF's recall at k=32 (whole pool / pool 500).

| Arm | SELF @32 | SELF @64 |
|---|---|---|
| Text alone | 45/46 / 46/46 | 46/46 |
| 8 newest, then text | 45 / 45 | 46/46 |
| 16 newest, then text | **15 / 22** | 46/46 |
| RRF 1:1 | 45 / 44 | 46/46 |
| RRF 2:1 (text weighted) | 45 / 45 | 46/46 |
| RRF 1:2 (recency weighted) | 29 / 27 | 39/46 at the whole pool |

- The 29 cases other than SELF reach 29/29 in every arm, at every pool size and at both k. Each
  has its right topic within structural's first 9.
- Sixteen newest, plus structural's 16, leaves text nothing at k=32. Weighting recency over text
  in RRF loses up to 7 SELF cases even at 64.

**The Postgres index against in-memory BM25.** The snapshot is read-only, so I made no scratch
table. Instead, the existing `grit.documents` bm25 index was queried in `SqlDocuments`'
`<@> to_bm25query` form, restricted with `version = any(...)` to each case's topic versions plus
every card version (about 340–410 rows). Pieces are not in `grit.documents`, so they are not in
this pool.

| Measure | Postgres index | In-memory BM25 |
|---|---|---|
| Recall, the 29 cases other than SELF, at 16 / 32 / 64 | 27 / 27 / 28 of 29 | 29 / 29 / 29 |
| Recall, SELF at 64 | 46/46 | 46/46 |
| Rank of the 8 real placements | 1, 1, 1, 4, 8, 8, 36, **76** | 1, 1, 2, 4, 7, 16, 16, 16 |

The index misses where the item's text is short (139–489 characters). Its IDF comes from the
whole table, not from the candidate set.

Once structural is pinned, the index's misses are recovered: the rank-76 case is structural's.
Index ranking + b ≤ 16 + c ≤ 8 is 29/29 at k=32 and 64, as is in-memory BM25.

**What E9 decides.**

- **The text source.**
  - Rank by BM25 over lead plus passages, never the lead alone, which collapses at pool size
    2,200.
  - The index can do the ranking, with no candidate's text read, provided structural is pinned
    first. Alone it misses 1 of 8 real placements at 64.
- **Shares of the 64.**
  - Structural: up to 16, pinned first.
  - Recency: the 8 newest, or none, since it adds nothing measurable here.
  - Text fills the rest.
  - If recency and text are fused instead, weight text at 2:1 or higher.
  - Never give recency 16 slots at k=32.
- **What the sample cannot show.**
  - Only 8 real placements exist, plus 21 auto-attached ones and SELF's leaky 46, and every
    non-SELF case is caught by structural within its first 9. The spike cannot show what
    structural or recency is worth when the right topic sits in neither.
  - The store has about 70 real topics, so recency's ranks (all ≤ 9 for real placements) say
    nothing about a store of thousands.
  - The scale test grows the distractors, not the real topics: a pool of 2,000 real topics, with
    their near-duplicates, would be harder than 2,000 cards and pieces.

## E10: should each topic show its last two messages, and when they were said?

Run with `SPIKE_DB=<snapshot> python3 -I e10.py limit | run | run leaky`. Results are in
`out/e10-limit.json`, `out/e10.json` and `out/e10-leaky.json`. It cost 11.35¢.

**Jev's context limit.** The state was card and piece text, sent with one YesNo question.

| Request | Result |
|---|---|
| 128,000 chars | Answered; the API reported **30,292** input tokens |
| 168,000 chars (about 40k tokens) | HTTP 400 `{"detail":{"error_type":"max_tokens_exceeded"}}` |
| 250,000 chars | the same HTTP 400 |

An oversized request is refused, not truncated, and reports no tokens. The limit lies between
30.3k and about 40k tokens, which fits the reported 32k.

**Topic views.** The arm (a) Choice ran over every topic existing at the case's time, plus `new`
and `aside`. The discussion carries its date (`said`).

| View | SELF (46) | CONT (4) | HIST-new (4) | Tokens per request, SELF (70 keys) |
|---|---|---|---|---|
| (i) lead + passage head | 44/46 | 1/4 | 1/4 | 10,028 |
| (ii) lead + last 2 dated messages | 44/46 | 1/4 | 1/4 | 10,338 |
| (iii) lead + passage head + last 2 dated messages | 44/46 | 1/4 | 1/4 | 13,620 |

- **The views make no difference.** Every case gets the same pick under all three views. Only the
  probability moves: in one CONT error it is 0.88, 0.79 and 0.86.
- **Purged messages.** 1,150 of the 3,426 topic views (34%) have no messages left, because they
  were purged or, on SELF, were the discussion's own. Those views fall back to (i).
- **Leakage.** Showing SELF's own messages in its topic's view lifts SELF from 44/46 to 45/46
  (views ii and iii): 41 of the 46 topics gain their discussion's own lines. Excluding them costs
  1 case.
- **The real continuations are the hard part.** CONT and HIST-new are 2/8 under every view, and
  the two sets overlap in their placements. The wrong picks go to a near-duplicate topic, and two
  of them come with p ≥ 0.88.
- **Budget at 64 topics.**
  - Measured chars per token: 3.4–3.9, the lowest with messages shown.
  - A 64-topic request with a 1,500-char discussion stays under 32k tokens at up to about **1,650
    chars per topic** at 3.4 chars per token. About 1,500 leaves a 10% margin.
  - View (iii) uses about 195 tokens, or 730 chars, per topic. At 64 topics that is about 12.5k
    tokens.

**What E10 decides.** Keep view (i). The dated last messages cost 3–35% more tokens and changed
no pick.

## E11: do many options or short, vague items cause miscategorising?

Run with `SPIKE_DB=<snapshot> python3 -I e11.py cached | run`. Results are in
`out/e11-cached.json` and `out/e11.json`. The new calls cost 4.46¢.

**From the cached runs** (arm a; topics as lead + passage head unless noted):

| Measure | Result |
|---|---|
| E0 accuracy by key count | 5: 40/40, 10: 39/40, 20: 39/40, 42: 39/40, 69: 38/40. All items 400–800 chars. |
| SELF by length: <150 / 150–400 / 400–800 / >800 | **12/14** / 13/13 / 12/12 / 7/7. With lead only: 10/14 / 13/13 / 11/12 / 7/7. |
| SELF's wrong picks | 2 of 2 at p ≥ 0.86 (lead + passage). With lead only: 2 at p ≥ 0.86 and 3 under. |
| L1S agreement with Haiku by length: <150 / 150–400 / 400–800 / >800 | 3/23 / 5/16 / 8/19 / 8/18. Jev sets aside the short chatter Haiku opened topics for. |
| L1S agreement with Haiku by keys: ≤10 / 11–20 / 21–42 | 10/17 / 7/23 / 7/36 |

The fall-off with key count on L1S is confounded: the nights with more topics are the nights with
more existing topics to attach to. On SELF, where the answer is known, nothing falls off with key
count.

**The shortlist rule at k = 16, 32, 64**, on SELF, CONT and HIST-new. The rule is structural (≤ 16)
first, then the 8 newest, then BM25 over lead + passages to fill. Short means under 400 chars.

| Shortlist | Short SELF (27) | Short real placements (5) | Long SELF (19) | Long real placements (3) | Tokens |
|---|---|---|---|---|---|
| k=32 | 25 right; 2 wrong, both p ≥ 0.86 | 2 right; 3 wrong, 2 of them p ≥ 0.86 | 19/19 | 0/3 | about 4.8k |
| k=64 | 25 right; 2 wrong, both p ≥ 0.86 | 2/5 | 19/19 | 0/3 | about 9.3k |
| Candidate rule: structural + text top 16, about 31 keys | 25 right; 2 wrong, both p ≥ 0.86 | 2/5 | 19/19 | 0/3 | about 4.6k |

- **Accuracy on short items does not drop as the options grow** from 32 to 64.
- **k=16 is not usable under this rule.** Structural's 16 plus the 8 newest already fill it (about
  23 keys), so text gets no slot. The right topic was in the list for only 9/27 short and 7/19
  long SELF items.
- **Under the candidate rule's mark** (a pick under 0.86 is set aside):
  - Short SELF items: 25 placed right, 1 placed wrong, 1 set aside.
  - Short real placements: 2 right, 2 wrong, 1 set aside.
  - The mark cannot catch confident errors. Every SELF error is at p ≥ 0.86, and so are half of
    the errors on the real placements.

**What E11 decides.**

- **Where the errors are.** On SELF they sit under 150 chars: 12/14 there against 100% above it.
  So "short" is better set at **under 150 chars** than at 400.
- **Shortlist size.** For short items, structural plus the text top 16 (about 31 keys, about 4.6k
  tokens) is as accurate as 64 at half the tokens. It can be adopted.
- **The mark.** Setting aside picks under it, rather than opening a new topic, costs one right
  placement in 27. It does not protect against confident wrong picks. Those come mostly from
  near-duplicate topics, which merging duplicates would address, not a threshold.

## E12: the room sheet replayed through a kind-aware sort

**The data.** Nick's hand-labelled sheet of the room era: 103 messages, oldest first, in one
room with six threads.
- After two typos are folded into `engine sync` it holds **17 labels**. The brief counted 18.
- It has 89 labelled messages, 14 chatter, and 3 meeting occurrences across 9 messages.
- The sheet does not name a thread's root, so each thread is one unit.

**Running it.** `python3 -I E12-a.py`, then `E12-b.py run | report`, then `E12-c.py [--fitted]`.
Aggregates are in `out/e12a.json`, `out/e12b.json` and `out/e12c*.json`. Per-unit placements quote
text, so they are in `.local/jev-spike/e12/`. E12 cost **2.47¢**: 323 calls and 588,607 input
tokens, about 1,820 tokens a call.

### E12a: run purity (code only)

A unit is pure when its labelled messages share one label. The sheet's times have minute
resolution, so gap 0 means the same minute.

| Gap | Units | Pure / labelled units | Labelled msgs in a pure unit | Chatter-only units | Oracle: msgs under their unit's majority label |
|---|---|---|---|---|---|
| same minute | 62 | 57/59 | 76/89 | 3 | — |
| 5 min | 44 | 38/43 | 62/89 | 1 | 82/89 |
| 15 min | 37 | 30/37 | 54/89 | 0 | 80/89 |
| 30 min | 30 | 23/30 | 48/89 | 0 | — |
| 60 min | 22 | 15/22 | 42/89 | 0 | — |

- **Labels recur.** At every gap from 5 to 60 minutes, 11 of the 17 labels span more than one
  unit. Joining them across days is the sort's job.
- **Where the impurity is.** At 5 minutes, 27 labelled messages sit in a mixed unit:
  - 10 are in thread 5 (9 `engine sync` and 1 meeting message), which is mixed at every gap;
  - 17 are in four runs where the subject changes within 5 minutes.
- **Context from the previous two top-level messages.** Of the 64 labelled top-level messages,
  23 have both previous messages sharing their label, and 38 have at least one.
- **The newest-two-labels baseline** gets **28/43** short (<150 chars) labelled top-level
  messages right. That holds whether it counts the whole channel or the top level alone. The
  brief counted 42 such messages; no message sits near the 150-char line to explain the gap.

### E12b: the incremental sort (Jev)

**How it runs.**
- Units are replayed oldest first.
- A document's view is its kind, the days it was mentioned, its first unit (360 chars) and its
  newest unit's head (300).
- The item is the unit, dated, up to 1,500 chars.
- One Choice covers every document so far, plus `new_topic`, `new_meeting` and `aside`. The control
  offers only `new` and `aside`.

**The wordings** are in `E12-b.py`:
- W1 is plain;
- W2 defines the two kinds;
- W3 adds "a short reply continues what came before".

A message counts as right when it sits in a document whose majority label is its own.

| Arm | Docs | ARI | Purity / inverse | Labelled right | Meeting msgs in a meeting doc | Occurrences with their own meeting doc | Short top-level right |
|---|---|---|---|---|---|---|---|
| 5 min, W1 | 14 | 0.517 | 0.820 / 0.775 | 72/89 | 6/9 | 2/3 | 33/43 |
| **5 min, W2** | 18 | **0.554** | 0.876 / 0.697 | **77/89** | 7/9 | **3/3** | **38/43** |
| 5 min, W3 | 17 | 0.525 | 0.843 / 0.708 | 74/89 | 7/9 | 3/3 | 35/43 |
| 15 min, W2 | 15 | 0.502 | 0.798 / 0.685 | 70/89 | 7/9 | 3/3 | 33/43 |
| 5 min, W2, no kinds | 18 | 0.508 | 0.843 / 0.685 | 75/89 | 0/9 (all in topics) | 0/3 | 36/43 |
| W2, mark 0.5, retry joined | 16 | 0.547 | 0.854 / 0.719 | 74/89 | 8/9 | 3/3 | 35/43 |
| W2, mark 0.7, retry joined | 13 | 0.388 | 0.719 / 0.663 | 62/89 | 5/9 | 2/3 | 28/43 |
| W2, mark 0.7, drop | 9 | 0.242 | 0.876 / 0.472 | 30/89 | 6/9 | 2/3 | 13/43 |

- **Kinds work on meetings.**
  - Under W2, each of the 3 occurrences gets a meeting document of its own.
  - Of the 2 meeting messages it misses, 1 rides in the `engine sync` thread and 1 was set aside
    with the chatter after it.
  - The no-kind control puts all 9 in topic documents. One of those topics it shares with
    `hysa widget`.
- **Meeting documents take in other messages.** W2 opened 4 meeting documents, holding 6
  labelled messages that are not about a meeting:
  - `cms`'s request for ten minutes to chat (2), arguably a meeting;
  - 3 that ride in mixed runs;
  - 1 `engine sync` reply that follows the meeting talk.
- **Short messages.** Under W2, 38/43 are right, against 28/43 for the newest-two-labels rule.
  The best possible placement of 5-minute units gets 39/43, so most of the gain comes from runs
  carrying short replies with their neighbours.
- **The units cap the score.**
  - W2 gets 77 labelled messages right, against the oracle's 82.
  - Its largest loss is in inverse purity: `engine sync` is split in two. One document holds the
    first days (13). The other (15) was opened on 11-18 by a mixed run, at p 0.34.
- **Chatter cannot judge unit placement.** W2 sets aside 2/14. At 5 minutes the units allow at
  most 1/14, because 13 of the 14 chatter messages ride in labelled units.
- **The hindsight retry does not help.**
  - At a mark of 0.5 it holds 7 units, and against no mark it loses 3 labelled messages and 3
    short ones.
  - At 0.7 it holds 13 units and loses 15 labelled messages.
  - Joining the next unit welds two matters together.
  - Dropping a unit below the mark, instead of retrying it, is far worse.
- **The wording mattered more than the gap.** Going from W1 to W2 changed 5 labelled messages.
  Of the two gaps, 5 minutes beat 15.

### E12c: a meeting's day by code (no model)

The rule reads weekday names as the next such day, and reads today, tomorrow and m/d dates. The
zone is assumed to be UTC−5.

| Rule | Meeting msgs with a day | First candidate right | Wrong | Occurrences with the right day | Non-meeting msgs with a day word |
|---|---|---|---|---|---|
| as written | 5/9 | 5/9 | 0 | 2/3 | 3/94 |
| fitted to this sample ("now", "real quick" mean today) | 7/9 | 6/9 | 1 | 3/3 | 8/94 |

- One message states a time ("930").
- The rule as written is never wrong. It finds no day for the impromptu call, whose messages
  name none.

### What E12 says for `grit/note-knowledge-kinds`

- **Measured.**
  - `new_meeting` beside `new_topic` and `aside` gives each of the 3 occurrences a document of
    its own. The control gives none of them one.
  - It raises ARI from 0.508 to 0.554.
  - Five-minute runs plus threads are the better unit.
  - Short items get 38/43 right, against the baseline's 28/43.
- **Inferred.**
  - **Set-aside.** The note's rule (set aside, offered again joined with the room's next run)
    should not ship as written. Here it only lost placements. Setting aside should come from
    Jev's `aside` answer, not from a confidence mark.
  - **The meeting opening gate.** The note opens a meeting only when a time or attendees parse.
    With the parser as written, it would have kept 2 of the 3 occurrences. The impromptu call
    needs "now".
  - **The split `engine sync`** is what the topic merge is for.
- **Not measured.**
  - Run-to-run variance: each request ran once, and identical requests are served from the cache.
  - Any room but this one.

### Next

- Measure whether merging, at the end of the night, documents of the same kind rejoins
  `engine sync`'s two halves.
- Score the meeting opening gate together with the sort.
- Replay a second hand-labelled room before W2 is adopted as the wording.

## What it decides

- **Discussions.** Use one Choice over every live topic plus `new` and `aside`, each topic shown as its lead plus the head of its newest passage, and the discussion whole up to 1,500 characters. Expect about 96% where the answer is a topic; about 0.5¢ a night.
- **Cards.** Use a YesNo per BM25 top-8 topic, the card shown as title plus summary, placing at yes ≥ 0.72.
- **Not Jev.** Neither grouping new discussions (one discussion per new topic is the safer default), nor deciding when a turn's written query can be skipped.
- **Turns.** Keep the written query. Judging down to rank 60 is worth considering.
- **Open.** Haiku and Jev disagree on new versus existing as a policy, not as noise: Haiku put 143 of 158 discussions in a new topic and 0 of 340 cards in an existing one. A person's adjudication of 40 disagreements decides which is right, and sets any threshold for routing to Haiku.
