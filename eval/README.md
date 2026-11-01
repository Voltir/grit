# grit.eval

The eval's labelled material, written by hand and read by pure code: conversations whose
windows must hold (or must not hold) certain entries, and synthetic Slack rooms whose openings
have a known placement. It depends on core alone, makes no call and reads no database; what
runs it is elsewhere in this folder.

One idea, so one package, `grit.eval`:

- **`Case`**, **`Cases`**: one eval conversation and the text format it is written in, with
  the hand-written cases themselves; relabelling a case is editing its line in `Cases`.
- **`Filler`**: seeded chatter that buries a case's turns, so recency cannot find its labelled
  entries by accident.
- **`Layout`**: a case laid out as rows to write into a database, under its own root place,
  with the entries a window must and must not hold.
- **`Room`**: a synthetic room read from `resources/rooms/`, and the placement each of its
  openings should get.
- **`TopicScore`**: a topics run's placement of a room's openings scored against its truth.

The rest of `eval/`:

- `it/`: the assembly eval, which writes each case into a database and scores every
  assembler's windows (`scripts/it grit.eval.it`). It reports and gates nothing.
- `harness/`: the eval harness, `grit.eval.harness`, run only through `scripts/eval`
  ([`harness/README.md`](harness/README.md)).
- `bench/`: the coder bench's frozen tasks ([`bench/README.md`](bench/README.md)).
- `jev/`: experiments that ask the classifier real questions before grit relies on their
  wording ([`jev/README.md`](jev/README.md)).

What the code cannot say: a case's labels are a person's judgement, not grit's output, so a
change that moves a score is read against the labels, never the labels against the score.
