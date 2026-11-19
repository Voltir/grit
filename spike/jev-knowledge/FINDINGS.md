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

## What it decides

- **Discussions.** Use one Choice over every live topic plus `new` and `aside`, each topic shown as its lead plus the head of its newest passage, and the discussion whole up to 1,500 characters. Expect about 96% where the answer is a topic; about 0.5¢ a night.
- **Cards.** Use a YesNo per BM25 top-8 topic, the card shown as title plus summary, placing at yes ≥ 0.72.
- **Not Jev.** Neither grouping new discussions (one discussion per new topic is the safer default), nor deciding when a turn's written query can be skipped.
- **Turns.** Keep the written query. Judging down to rank 60 is worth considering.
- **Open.** Haiku and Jev disagree on new versus existing as a policy, not as noise: Haiku put 143 of 158 discussions in a new topic and 0 of 340 cards in an existing one. A person's adjudication of 40 disagreements decides which is right, and sets any threshold for routing to Haiku.
