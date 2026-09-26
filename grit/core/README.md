# grit.core

The domain and the seams everything else is written against. No DBOS, no JDBC driver on its
classpath; implementations live in the modules that need the libraries (CLAUDE.md, rule 8).

In dependency order:

- **`clock`** — what a function cannot compute: `Clock` (the time) and `Fresh` (values
  no one made before). Imports nothing in core.
- **`id`** — the opaque ids (`ConversationId`, `EntryId`, `TurnSeq`, `WorkflowId`,
  `SourceId`, `ToolCallId`) and `TurnRef`. Imports nothing in core.
- **`model`** — what grit knows about models, as data: `ModelRef` (a model snapshot at an
  upstream), each setting's `Known` value and its `Source`, the switches a `Profile` picks
  among and the `Settings` a call is made under, the `Policy` of which pair does each job,
  the `Catalog` of both, and the `TurnProfile` a turn pins from it; `CatalogJson`, their
  stored form. Imports nothing in core.
- **`message`** — the model's vocabulary: `Message`, `AssistantBlock`, `StopReason`,
  `Tokens`, `Usage`. ← `id`
- **`topic`** — a conversation's topics as recorded events: `TopicId`, `TopicEvent` (a
  topic opened, a message placed with its `Weights` over topics, a topic described), the
  `Placement` that says who placed it, `Band`, `Verdict`, and `Topics`, the pure fold over
  the events; `TopicJson`, their stored form. ← `id`
- **`store`** — what is kept and the transaction it is kept under: `Tx`, `Db` (reads),
  `Jot` (short writes from inside a step), `Entry`,
  its `Payload` and their codec `PayloadJson`, `EntryStore`, `EntrySearch`, `Conversation`, `Origin`,
  `ConversationStore`, `UsageLedger`, `StoreError`. ← `id`, `message`, `topic`
- **`durable`** — `Durable` and `Journaled`: steps that survive a crash, and waits for a
  message (`recv`). ← `id`, `store`
- **`approval`** — `Approval`, a person's answer to a gated tool call, and the message
  that carries it to the turn waiting on its topic. ← `id`
- **`context`**, **`provider`**, **`inbox`**, **`classify`** — the seams the engine plugs
  into: `ContextAssembler` (and the `Window` it builds), `Provider`, `Inbox` (which also
  answers a turn's gated call), and `Classifier` (closed questions about a state, answered
  with a probability per option; Jev's shape). Each names only the packages above, never
  another of the four.
- **`host`** — what a tool may do to the machine, as capabilities: `Workspace` (read, list,
  search), `Edits` (write, edit) and `Shell` (run), implemented in `grit.host`; and the pure
  rules they share: `RelPath` (a path that stays inside the checkout and names no secrets
  file), `Clipped` (output cut to what the model is shown), `LineNumbers` (how `read`
  numbers a file's lines), `Replace.onto` (an edit's
  matching), and their errors. Imports nothing in core.
- **`tool`** — tools as typed data: `Field`, `Args` (read into a named tuple), `ArgsError`,
  `ToolName` and `ToolSpec`, from which come the schema the model is shown (a
  `provider.ToolSchema`) and the reader of its calls; `Tool` (a spec, a `Gate`, how a call is
  shown in one line, and what it does, capture-tracked), `Toolbox` (the tools offered on one call, which `bind` a
  call to a `Bound` or a `CallError`: `Bound.Free` runs, `Bound.Gated` runs only given an
  `Approval`) and `Outcome` (what a call came to, as the model reads it). ← `id`,
  `message`, `provider`, `approval`

No source file sits at core's root, and no two packages import each other in a circle:
`scripts/enola-law.sh` fails on a new import cycle.

The test tree mirrors it: the in-memory fakes other modules' tests use are
`store.InMemoryEntryStore`, `store.InMemoryUsageLedger` and `durable.InMemoryDurable`.
`TestTx` lives in package `grit.dbos.sql`, because the `null` it holds is legal only inside
the DBOS quarantine (rule 6).
