# 0006. Turn output reaches edges as a DBOS stream, written from the step, at least once

Status: accepted (2026-09-24)

Context: an edge should show a reply as the model writes it. ADR 0002 lets an edge reach a
turn only through Postgres, so the words have to cross a table. DBOS already provides one:
`writeStream` appends rows to `dbos.streams`, and `DBOSClient.readStream` is a blocking
iterator over them that ends when the workflow does. The writing executor notifies in
batches, and a client wakes on them only when built to listen, which grit's are not, so a
reader polls each second. Three alternatives were turned down:

- a table and `NOTIFY` of grit's own, which would duplicate this;
- writing from the workflow body, which is exactly once but makes every write a recorded
  operation (hundreds per turn in the journal, and a new step sequence);
- an in-process channel from provider to screen, which ADR 0002 rules out, and which a
  Slack edge could never use.

Written from inside a step, a piece takes the step's own function id, so nothing is recorded
and the step sequence is unchanged. The cost is that it is at least once: a step cut short by
a crash leaves its rows, and the rerun appends after them.

Decision: a turn tells edges what its model is writing through the DBOS stream `reply` of
its workflow (`Durable.stream`, taken before a step and written inside it; never outside a
step). Every piece carries the attempt at the `ask:n` step that wrote it
(`TurnStream.Piece`), fresh each time the step runs, and readers keep only the latest
attempt's pieces (`TurnStream.Heard`). The stream is a view: the recorded reply entry, not
the stream, is the turn's answer, and an edge shows the entry once it exists. Pieces are
gathered (`TurnStream.MaxMs`, 100 ms; `MaxChars`, 200 characters) to keep the rows to tens
per reply.

Consequences: an edge in any process can follow a reply live (`Engine.stream`), and a restart
mid-reply shows the rerun's text rather than a splice of two runs. Replay is unaffected,
because the `ask:n` step still records the whole message. The rows stay in `dbos.streams` for
as long as DBOS keeps the workflow. Streaming tool calls' arguments, and cancelling a turn
mid-stream, are not covered. Enforced by `TurnStreamTests` and the live mid-stream crash test
in `TurnRecordLiveTests`.
