# grit.turn

The durable turn: one workflow per user-visible turn — weigh its root by the answers triage
kept, or, for a message said to grit whose answers its recipe reads, by live triage's set
asked of it (`TurnWeighing`), route it (the pairs cleared for what it reads, under the day's
ceiling; its root's task asked when those span grades; the grade, reasoning and calls allotted
from that, raised to its room's baseline; ADR 0037), decide what it offers (its tools and its system prompt as fragments, among them its own room's label and, in brief, who may see what and how grit keeps it; ADRs 0016, 0017),
shaped by its deployment's recipe (ADR 0025), place its message among the
conversation's topics (ADR 0008), assemble, call the model (each call gated by what the turn has read, its tools offered only when what it read is believed enough for them; ADR 0036), send its hosted tool calls to
the edge serving its directory and wait for their answers, append, summarise.
Written against `grit.core`'s seams and `Durable`, never DBOS, so its tests run it over
core's in-memory fakes (`InMemoryDurable`, `InMemoryEntryStore`).

The turn is one flow (`grit.core.durable.Flow`), its files in this order, each reading those
before it and, of `Turn`, only its names and pins:

- **`TurnShown`**: what the model is shown: the tools each call offers, the lines a gate
  adds, the request built from the window and the turn's own entries, the items it shows.
- **`TurnRound`**: the tool loop, as a flow, and what its steps act through (`Env`); what a
  loop runs under (`Looping`) and where it stands (`Progress`), on the wire.
- **`TurnRounding`**: the loop's `Env` over a turn's env and tools; a call bound again inside
  a step's body goes through its one rebinding.
- **`TurnSteps`**: every other step as a value, with its body; what they act through (`Env`)
  and its implementation, `Turning`; what crosses between them.
- **`TurnFlow`**: the description: the steps in order, which run when, and what follows the
  reply. Which steps a turn records, and in what order, is read from it alone.
- **`Turn`**: the body that runs the flow and the readers of a recorded turn, each reading it
  over the flow (`running`, `rebuilt`), beside the step names and pins.

The other files are the helpers these steps call: an offer (`TurnOffer`), a route
(`TurnRoute`), topics, a verdict, a summary, a judge, a gate (`TurnGate`), hosted calls
(`TurnHosted`), the tool calls (`TurnTools`), the loop's pure decisions (`TurnLoop`), and the
recorded codecs (`TurnJournal`).
