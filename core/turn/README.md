# grit.turn

The durable turn: one workflow per user-visible turn — weigh its root by the answers triage
kept, or, for a message said to grit whose answers its recipe reads, by live triage's set
asked of it (`TurnWeighing`), route it (the pairs cleared for what it reads, under the day's
ceiling; its root's task asked when those span grades; the grade, reasoning and calls allotted
from that, raised to its room's baseline; ADR 0031), decide what it offers (its tools and its system prompt as fragments, among them its own room's label and, in brief, who may see what and how grit keeps it; ADRs 0016, 0017),
shaped by its deployment's recipe (ADR 0025), place its message among the
conversation's agenda (ADR 0008), draw its window (its search query written, what that finds judged, only what passed packed; ADR 0040), appended after the window its conversation carries when the turn can send that unchanged, else afresh (ADR 0052), call the model (each call gated by what the turn has read, its tools refused when what it read is not believed enough for them; ADR 0036), send its hosted tool calls to
the edge serving its directory and wait for their answers, append, summarise.
Written against `grit.core`'s seams and `Durable`, never DBOS, so its tests run it over
core's in-memory fakes (`InMemoryDurable`, `InMemoryEntryStore`).

The turn is one flow (`grit.core.durable.Flow`), its files in this order, each reading those
before it and, of `Turn`, only its names and pins:

- **`TurnShown`**: what the model is shown: the tools each call offers (its tool set's,
  whatever its gates decide), the notes a gate adds, the request built part by part from the
  window carried and drawn and the turn's own entries, the items it shows.
- **`TurnRound`**: the tool loop, as a flow, and what its steps act through (`Env`); what a
  loop runs under (`Looping`) and where it stands (`Progress`), on the wire.
- **`TurnRounding`**: the loop's `Env` over a turn's env and tools; a call bound again inside
  a step's body goes through its one rebinding.
- **`TurnSteps`**: every other step as a value, with its body; what they act through (`Env`)
  and its implementation, `Turning`; what crosses between them.
- **`TurnAssembly`**: the steps the window is drawn in, from the placed message: the window
  carried, the query, the search, each batch judged, the window packed.
- **`TurnFlow`**: the description: the steps in order, which run when, and what follows the
  reply. Which steps a turn records, and in what order, is read from it alone.
- **`Turn`**: the body that runs the flow and the readers of a recorded turn, each reading it
  over the flow (`running`, `rebuilt`), beside the step names and pins.

The other files are the helpers these steps call: an offer (`TurnOffer`), a route
(`TurnRoute`), the agenda, a verdict, a summary, a judge, what a search found judged (`TurnRelevance`), a gate (`TurnGate`), hosted calls
(`TurnHosted`), the tool calls (`TurnTools`), the loop's pure decisions (`TurnLoop`), and the
recorded codecs (`TurnJournal`).

## What a turn is offered

`TurnOffer.decide` offers a turn its tools in this order: the hosted tools of its `TurnTooling`
that the live edge serving its workspace advertises, then the others it advertises that
`Hosted.advertised` offers, less any whose name one of the tooling's has; then, on an addressed
turn, the tools advertised at each service `reaches` links its place to that `Hosted.advertised`
offers, less any whose name an earlier tool has; then the tooling's `conversing` tools unless its
recipe briefs the turn for one task (`Briefing.Tasked`); then its own `tools`; then its `operator`
tools when its origin is the operator's (`Audience.operator`); then its `administering` tools when
its root is `Addressed` or `ByName` and its origin is colleagues' (`Audience.Colleagues`).

Of its own tools a turn is offered, in that order, by four questions: what runs its conversation
(the recipe's briefing: `conversing`), every turn (`tools`), who reads it (its origin's audience:
`operator`), and what it answers in which room (its root and audience: `administering`). The coding
tools, `hosted`, the deployment's `Offered` chose.

Its prompt, in order: the base (`TurnPrompt.TaskBase` when briefed for one task),
`TurnPrompt.Candour`, `TurnPrompt.Answering`, `TurnPrompt.Security` (not when so briefed),
`TurnPrompt.Briefed` when it is briefed, its edge's fragment, what its edge says its reply shows
(`renders`, `TurnPrompt.rendered`), where its reply goes (`TurnPrompt.destination`), what it is
called (`persona`, `TurnPrompt.called`), `TurnPrompt.unprompted` when its root is heard
(`TurnPrompt.unpromptedAtTop` when its venue is its room's top level), how its persona sounds
(`TurnPrompt.voiced`), its own room's label (the transaction's floor, `TurnPrompt.room`), what it
may reach there, what it reaches besides (`TurnPrompt.reached`, for each reached service offering
tools), the voice set with `/set voice` at a terminal alone (`Audience.Operator`; none for plain),
the instruction files the edge read there, and, after them, the instructions each reached
service's edge declared there, for each reached service offering tools.
