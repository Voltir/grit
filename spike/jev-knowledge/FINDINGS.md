# Jev placement for the knowledge plugin: findings

The spike ran on a snapshot of a deployment's database, taken 2026-11-19. Jev's spend was 20.52¢ over
2,580 calls, counted from the API's reported input tokens (`spend.json`). No other model was called.
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
| CONT + HIST-new (8) | 1, 1, 2, 3, 15, 15, 19, 22 | 1, 2, 24, 24, 188, 221, 827, 1033 | 1, 1, 1, 4, 5, 5, 5, 9 | 1, 1, 1, 4, 6, 6, 6, 9 |
| HIST-same (21) | 18 at rank 1; then 2, 8, 23 | as bad as rank 1,368 | all at rank 1 | at most 27 |
| SELF (46) | 45 at rank 1; then 23 | as bad as rank 134 | 8 of 46 in its list (own discussion excluded) | spread 2–70 |

Recall at k:

- **BM25 over lead + passages** finds every case within 32 at every pool size, for every set:
  CONT 4/4, HIST-new 4/4, HIST-same 21/21, SELF 46/46. At k=16 it gets 3/4, 3/4, 20/21 and 45/46.
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

## What it decides

- **Discussions.** Use one Choice over every live topic plus `new` and `aside`, each topic shown as its lead plus the head of its newest passage, and the discussion whole up to 1,500 characters. Expect about 96% where the answer is a topic; about 0.5¢ a night.
- **Cards.** Use a YesNo per BM25 top-8 topic, the card shown as title plus summary, placing at yes ≥ 0.72.
- **Not Jev.** Neither grouping new discussions (one discussion per new topic is the safer default), nor deciding when a turn's written query can be skipped.
- **Turns.** Keep the written query. Judging down to rank 60 is worth considering.
- **Open.** Haiku and Jev disagree on new versus existing as a policy, not as noise: Haiku put 143 of 158 discussions in a new topic and 0 of 340 cards in an existing one. A person's adjudication of 40 disagreements decides which is right, and sets any threshold for routing to Haiku.
