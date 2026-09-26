# grit.core

The domain and the seams everything else is written against. No DBOS, no JDBC driver on its
classpath; implementations live in the modules that need the libraries (CLAUDE.md, rule 8).

In dependency order:

- **`clock`** — what a function cannot compute: `Clock` (the time) and `Fresh` (values
  no one made before). Imports nothing in core.
- **`id`** — the opaque ids (`ConversationId`, `EntryId`, `TurnSeq`, `WorkflowId`,
  `SourceId`, `ToolCallId`, `PeriodSeq`, `LineId`), `TurnRef`, `PeriodRef`, `CloseRef` (one
  attempt to close a period on its deadline, and its workflow id) and `SettleRef` (the one
  question whether a quiet period is finished, and its workflow id). Imports nothing in core.
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
- **`period`** — a conversation's time as periods that close (ADRs 0011, 0012): `Period`
  and its `PeriodState`, `CloseReason`, the `Windows` and `LifecycleSettings` in force,
  `Deadline` (when a period closes, and when it is asked whether it is finished: the one
  definition of each), a classifier's `Verdict` and its `Judgement`, `Probability`, an open
  period's `Activity`, the `Closing` a
  closed one leaves (its `Flows`, and the conversation's `Balance` after it: `Line`s in
  `Section`s, changed by `Edit`s into `Change`s and held to a cap) and its stored form
  `ClosingJson`, a `CloseOrdinal` (close order across
  conversations), and a `Purgeable` period's workflows. ← `id`
- **`store`** — what is kept and the transaction it is kept under: `Tx`, `Db` (reads),
  `Jot` (short writes from inside a step), `Entry`,
  its `Payload` and their codec `PayloadJson`, `EntryStore`, `EntrySearch`, `Conversation`, `Origin`,
  `ConversationStore`, `UsageLedger`, `ModelProfileStore` (which profile each turn ran
  under), `ModelFactStore` (facts about pairs approved at runtime), `PeriodStore` (which
  period is open, sealing one with its closing entry, purging one), `LifecycleStore` (the
  settings in force), `Opening` and `ClosingEntry` (the closing a period opens from), `StoreError`. ← `id`, `message`, `topic`, `model`, `period`
- **`plugin`** — features a deployment turns on, built from closed periods alone: `Plugin`
  (a name, a version, and `post`, which keeps what it wants of one `ClosedPeriod`),
  `PluginName`, `PluginDocs` (one plugin's documents, and no other's), `PluginCursors` (how
  far each has posted, in close order; a new version starts again) and `PostRef` (one
  posting run, and its workflow id). ← `id`, `period`, `store`
- **`durable`** — `Durable` and `Journaled`: steps that survive a crash, and waits for a
  message (`recv`). ← `id`, `store`
- **`approval`** — `Approval`, a person's answer to a gated tool call, and the message
  that carries it to the turn waiting on its topic. ← `id`
- **`context`**, **`provider`**, **`inbox`**, **`classify`** — the seams the engine plugs
  into: `ContextAssembler` (and the `Window` it builds, and `Shown`: what the model is
  shown of an entry a window names), `Provider` and `Models` (the
  catalog in force, and a provider per role's pin; ← `model`), `Inbox` (which also
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
  `Approval`), `Repairs` (what of a call is repaired before it is read, as the pair's
  settings say) and `Outcome` (what a call came to, as the model reads it). ← `id`,
  `message`, `model`, `provider`, `approval`

No source file sits at core's root, and no two packages import each other in a circle:
`scripts/enola-law.sh` fails on a new import cycle.

The test tree mirrors it: the in-memory fakes other modules' tests use are
`store.InMemoryEntryStore`, `store.InMemoryUsageLedger`, `store.InMemoryModelProfileStore`,
`store.InMemoryPeriodStore`, `store.InMemoryLifecycleStore`, `plugin.InMemoryPlugins` and
`durable.InMemoryDurable`; and `period.TestClosings` builds closings and balance lines.
`TestTx` lives in package `grit.dbos.sql`, because the `null` it holds is legal only inside
the DBOS quarantine (rule 6).
