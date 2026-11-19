# 0063. A topic outlives the messages it was written from and forgets by its own age; a room relabelled still withdraws it

Status: proposed

Context: a topic (`grit.knowledge.topic`) is a lead and its passages, each written on a day from
the strands and cards its lineage names. Its nightly run used to withdraw a passage whenever the
strand shelf no longer read one of its strands at the run's floor. That happened for two causes
the code did not tell apart: the strand's room is at another label now, or the strand's messages
have purged under raw retention. The second is routine: where a room's messages are kept 30
days, every topic written from it was withdrawn a month after its last passage, though nothing
about what it says had become wrong or more secret. A closing already outlives the raw entries
it summarised (`Strand.Read.gone` names them); a topic is the same kind of record.

Turned down: no horizon at all, which leaves topics unbounded; copying the strand's text into
the topic so it can be re-read later, when the passage is that copy already; and asking the
strand shelf for each room's label, when the lineage holds each room and the transaction answers
for its label in force.

Decision: a topics run withdraws a passage in two cases only, and the topic goes with its last
passage:

- a strand its lineage names is of a room whose label in force (`Tx.roomLabel`) is not the run's
  floor: the label lattice decides what a topic at that floor may hold, as before;
- its day is more than `keepFor` before the run's day. `keepFor` is a run's parameter,
  `TopicParams.KeepFor` (90 days) unless a schedule says.

A strand's messages purging withdraws nothing. Knowledge still forgets by design: a topic is
bounded by `keepFor` in age and by `Topic.Passages` in length, and its oldest passage goes first.

Consequences: a topic is remembered for as long as its own clock says, whatever the raw
retention of the rooms it was written from, and the run reads no strands to prune. A room moved
to another label still takes its passages out at the next run. A message a person deletes is not
seen by the pruning: it goes when its passage ages out or its room is relabelled. Changing what
withdraws a passage changes the job, so `TopicDream.version` moves with it (ADR 0029). Enforced
by `TopicDreamTests`.
