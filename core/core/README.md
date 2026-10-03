# grit.core

The domain and the seams everything else is written against. No DBOS, no JDBC driver on its
classpath; implementations live in the modules that need the libraries (CLAUDE.md, rule 8).

In dependency order:

- **`clock`** — what a function cannot compute: `Clock` (the time) and `Fresh` (values
  no one made before). Imports nothing in core.
- **`id`** — the opaque ids (`ConversationId`, `EntryId`, `TurnSeq`, `EntrySeq`, `WorkflowId`,
  `SourceId`, `ToolCallId`, `PeriodSeq`, `LineId`, `PluginName`, `ShadowName`, `QuestionName` (a question's in a question set, or its per-source form), `KnowledgeSourceName`, `EdgeName`, `PrincipalId`: who an
  action is done for, `EdgeId`), `CallSlot` (a tool call's place in its turn, and its
  request's key), the one short content hash every content-addressed id uses, `TurnRef` (and its reply entry's id), `PeriodRef`, `CloseRef` (one
  attempt to close a period on its deadline, and its workflow id), `SettleRef` (the one
  question whether anyone is waiting on a quiet period, and its workflow id), `TriageRef`
  (the one triage of a heard message, and its workflow id) and `ShadowRef` (one shadow
  variant's run on a heard message, and its workflow id). Imports nothing in core.
- **`place`** — where conversations happen (ADR 0013): `Place`, a path in one
  containment tree under the root, everywhere, one `Namespace` per source (`fs`, `slack`,
  `task`, `service`), and `within` (a prefix, defined once); `Directory`, an absolute normalized
  path; `Service`, an outside service's place, and `WorksIn`, a deployment's link from
  conversations with no directory to the service they work in; `Scope` (the `Prefix`es a window may draw on beside its own conversation: places, or its own room),
  `Weight` (how far its own search hits outweigh those elsewhere) and `Locality`, both
  together. Imports nothing in core.
- **`prompt`** — a turn's system prompt as ordered fragments (ADR 0016): `Layer` (base,
  edge, person, reach, place: the most stable first, for a provider's prompt cache),
  `Fragment` (its id a content hash), `SystemPrompt` (fragments by layer, rendered to the
  same bytes every time) and `Voice` (how grit talks to the person: a named voice, plain
  by default, or their own words; the person layer's one fragment). ← `id`
- **`model`** — what grit knows about models, as data: `ModelRef` (a model snapshot at an
  upstream), each setting's `Known` value and its `Source`, the switches a `Profile` picks
  among and the `Settings` a call is made under, the `Policy` of which pair does each job,
  the `Catalog` of both, and the `TurnProfile` a turn pins from it; `CatalogJson`, their
  stored form. Imports nothing in core.
- **`message`** — the model's vocabulary: `Message`, `AssistantBlock`, `StopReason`,
  `Tokens`, `Usage`, and `Cost` (what calls cost together, and how grit writes it). ← `id`
- **`classify`** — the classifier seam: `Classifier` (closed questions about a state,
  answered with a probability per option; Jev's shape; each call one `Request`, which
  `around` hands to a function before the classifier it wraps, for caching or recording),
  and `ClassifierError` with its `Kind`, a failure without its words; `AnswersJson`, the
  stored form of its answers, each under its question's name or not. Above the packages that
  record what a classifier answered. ← `id`, `message`
- **`topic`** — a conversation's topics as recorded events: `TopicId`, `TopicEvent` (a
  topic opened, a message placed with its `Weights` over topics, a topic described), the
  `Placement` that says who placed it, `Band`, `Verdict`, and `Topics`, the pure fold over
  the events and the topics a close carried (`Topics.Carried`); `TopicJson`, their stored
  form. ← `id`
- **`period`** — a conversation's time as periods that close (ADRs 0011, 0012): `Period`
  and its `PeriodState`, `CloseReason`, the `Windows` and `LifecycleSettings` in force,
  `Deadline` (when a period closes, and when it is asked whether anyone is waiting: the one
  definition of each), a classifier's `Verdict` and its `Judgement`, `Probability`, an open
  period's `Activity`, the `Closing` a
  closed one leaves (its `Flows`, and the conversation's `Balance` after it: `Line`s in
  `Section`s, a Standing line with the `Ground` that established it (ADR 0018), changed by
  `Edit`s into `Change`s and held to a cap) and its stored form
  `ClosingJson`, a `CloseOrdinal` (close order across
  conversations), and a `Purgeable` period's workflows. ← `id`
- **`retention`** — what grit deletes, and when (ADR 0014): a `Target` (a period's raw
  entries, a superseded closing, a quiet conversation, a plugin's posting runs, its documents
  from before a restart, a plugin no longer enabled), its stored form, which window each kind
  of target is kept for, and a `Tombstone`, the decision to delete one. ← `id`, `period`
- **`store`** — what is kept and the transaction it is kept under: `Tx`, `Db` (reads),
  `Jot` (short writes from inside a step), `Entry`,
  its `Payload` and their codec `PayloadJson`, `EntryStore`, `EntrySearch`, `Conversation`, `Origin` (and its `Audience`: who its messages are for; its `Focus` at a message's `Position`: how many topics interleave where it is said),
  `ConversationStore` (each conversation's origin and who began it), `PromptStore` (each
  turn's system prompt, its fragments kept by id), `UsageLedger`, `ModelProfileStore` (which profile each turn ran
  under), `ModelSettingStore` (settings of pairs approved at runtime), `PeriodStore` (which
  period is open, sealing one with its closing entry, purging one), `LifecycleStore` (the
  settings in force), `VoiceStore` (the voice in force), `Principals` (the people an edge enrolled, and each workspace's assistant, by name; `Origin.assistant` says which principal the assistant is where a conversation happens) and `Speakers` (whose names a window shows on the inbound entries they wrote), `Tombstones` (what is to be deleted, until the collector has), `Opening` and `ClosingEntry` (the closing a period opens from), `EntryTopics` (a
  conversation's topics one period at a time: carried by its closing, then its own events), `StoreError`. ← `id`, `message`, `topic`, `model`, `period`, `retention`, `prompt`
- **`spend`** — what grit spends on model calls, read back: `Spend` (some recorded calls: how
  many, and their `Cost`), `Spending` (a day's, or a conversation's, from the ledger; what the
  ledger misses is in its doc), `Day` (a calendar day in a zone, as instants), `DailyCap`
  and `Budget` (the zone days begin in, the cap, and whether a new message is taken), and
  `Budget.Refusal`, the one line a person is told when it is not. ← `id`, `message`, `store`
- **`triage`** — what grit makes of a message it heard (ADR 0020): `Kind`, `Tags` (triage's
  answers, each under its question's name, or none; `Tags.V1`, the names and gate of the set
  triage first asked) and `TriageStore`, where they are kept beside their entry and deleted
  with it; `Earning`, whether a period earns a written closing, by its heard messages'
  `durable` answers; `Shadowing` (a shadow variant as the sweep enqueues it, within its daily cap),
  `Shadowed` (what one variant made of one message: its `ShadowAnswers`, a wording's in
  order or a question set's under their names) and `TriageShadows`, where those are
  kept beside the entry and deleted with it; `KnowledgeSources`, a deployment's catalog of
  what could supply what a message asks for, each covering a place; and `Gate`, the bounds
  on a question set's answers a draft is derived from, each on a `Reading` of one answer.
  ← `id`, `message`,
  `place`, `period`, `store`, `spend`, `classify`
- **`speech`** — whether grit speaks where it was not addressed (ADR 0022): `Speaking` (off,
  shadow, or within `Limits`, whose windows are `Rate`s), a heard message as it is weighed
  (`Heard`, its `Reach`), the ledger it is weighed against (`Ledger`, each `Spoken` turn at its
  `Stage`), `Speech.decide` (drafted, or held for a `Silence`), and `Speech.post` (what becomes
  of a draft the judge scored, `Judged`, as an `Outcome`), `Speech.spoken` (the one hold: the
  assistant already replied after the heard message), `SpeechStore` (each heard
  message's `Reach`, and grit's decisions, kept with its period's usage) and `SpeechJson`
  (their stored form). ← `id`, `message`, `period`, `place`, `spend`, `store`, `triage`
- **`review`** — a person's verdict on grit's speech decision, asked of a few heard messages
  a day where a declared shadow's gate and live triage's compare: `Settled` (live's decision,
  settled), `Review.gated` (whether live's gate passed), a `Candidate` and the `Reason` it is
  considered for, `Reviewing` (a deployment's review of one shadow) and `Review.pick` (what a
  round makes of the candidates, `Considered`, under per-reason shares of the day); `Reviews`
  (an edge's part: post a `Prompt`, keep a reacted `Verdict` as a `Label`) and `ReviewStore`
  (also which messages are considered, and every one `Reviewed`), kept with their
  conversation after the message's entry is gone (ADR 0024). ← `id`, `classify`, `store`,
  `speech`
- **`plugin`** — features a deployment turns on, built from closed periods alone: `Plugin`
  (a name, a version, and `post`, which keeps what it wants of one `ClosedPeriod`),
  `CacheDocs` (where it keeps what it makes of one closed period, deleted with that period's
  closing), `PluginDocs` (one plugin's documents as its surfaces read them), `PluginCursors` (how
  far each has posted, in close order; a new version starts again, in a new generation) and `PostRef` (one
  posting run, and its workflow id). ← `id`, `period`, `store`
- **`durable`** — `Durable` and `Journaled`: steps that survive a crash, and waits for a
  message (`recv`); `StepRecord`, a step as a reader after the fact sees it. ← `id`, `store`
- **`approval`** — `Approval`, a person's answer to a gated tool call, and the message
  that carries it to the turn waiting on its topic. ← `id`
- **`context`**, **`provider`**, **`inbox`** — the seams the engine plugs
  into: `ContextAssembler` (and the `Window` it builds, as wide as its request's `Width`
  asks: as deployed, or within a budget the eval harness names; and `Shown`: what the model is
  shown of a window, each line grit writes into it under its `Label`: the record, a
  section from afar, a gap where turns were left out; and a grit label that starts a line
  in text grit did not write, shown as a quoted paste), `Provider` and `Models` (the
  catalog in force, and a provider per role's pin; ← `model`), and `Inbox` (which also
  records a message heard where grit listens, not said to it, at the time it was said, says which of a thread's messages it has recorded, answers a turn's gated call, and says how far a turn has got: its `Progress`; the id its entry is kept under is
  `InboundId`'s). Each names only the packages above, never
  another of the three.
- **`stitch`** — a Slack thread's first message joined to an exchange in its room (ADR 0023):
  a `Link` from a conversation to the root it follows, a `Strand` (a root and its direct
  followers, never a chain), `Stitching` (which `Exchange`s a first message is offered, its
  `Offer`, where the classifier places it, as a `Placed` keeping what it was `Seen` (its exchanges read back as `Seen.Exchange`s), under a `Tuning`, and the
  one excerpt rule readers cut a strand by), `Opening` (the one message of a conversation
  that is placed, and its placement's `StitchRef`), `Placements` (an opening's placement
  waited for, in its room's order), `StitchStore` (placements kept beside their entry
  and deleted with it; what a room said), `StitchJson`, and `Along`, the one read of a strand,
  in scope. ← `id`, `message`, `place`, `period`, `store`, `classify`
- **`recipe`** — what a call's input shows beyond its own thread: a `Pool` of `Source`s (the
  room's recent messages, its author's, the exchanges stitching offered) within a budget in
  characters, each kept in the `Section` its source names, and what a pool shows of what it
  found, in its sources' order; `Pool.read`, what a pool shows for a heard message, from what
  its room held when it was said, read through `RoomReads` and the stitch kept for its
  conversation. Nothing in core imports it. ← `id`, `place`, `store`, `stitch`
- **`host`** — what a tool may do to the machine, as capabilities: `Workspace` (read, list,
  search), `Edits` (write, edit) and `Shell` (run), implemented in `grit.host`; and the pure
  rules they share: `RelPath` (a path that stays inside the checkout and names no secrets
  file), `Clipped` (output cut to what the model is shown), `LineNumbers` (how `read`
  numbers a file's lines), `Replace.onto` (an edit's
  matching), and their errors; and `Instructions`, the instruction files (`AGENTS.md`, else
  `CLAUDE.md`) around a directory; `ProcessIdentity`, which machine and process something is. ← `place`
- **`tool`** — tools as typed data: `Field`, `Args` (read into a named tuple), `ArgsError`,
  `ToolName` and `ToolSpec`, from which come the schema the model is shown (a
  `provider.ToolSchema`) and the reader of its calls; `Tool` (a spec, a `Gate`, how a call is
  shown in one line, and what it does, capture-tracked), `Hosted` (a tool's description without its run: offered by the engine, run by an edge),
  `Retry` (whether a call cut short is run again or answered `Interrupted`), `ToolSet`
  (a turn's tools as recorded, by content id) and `ToolSets` (where each is kept), `Toolbox` (the tools offered on one call, which `bind` a
  call to a `Bound` or a `CallError`: `Bound.Free` runs, `Bound.Gated` runs only given an
  `Approval`), `Repairs` (what of a call is repaired before it is read, as the pair's
  settings say) and `Outcome` (what a call came to, as the model reads it). ← `id`,
  `message`, `model`, `store`, `provider`, `approval`

- **`edge`** — what an edge and the engine share: `ServedEdge`, an edge a deployment serves
  beside its engine as it posts to a plugin (ADR 0021), opened over `EdgeStores` (what an
  edge reaches the engine through, ADR 0002) with the `Variable`s it `needs`, refused as an
  `EdgeRefusal`; `CatchUp`, what an edge hears once before serving, as `Unheard` per source;
  `Deliveries` (the replies an edge has yet
  to post outside grit, each part `Posting` or `Posted`), and the tool calls an edge runs
  (ADR 0017): `ToolRequest` (one call, addressed to a
  workspace, with its `Permit` and retry), `OutcomeJson` (its answer's stored form),
  `Edges.authorize` (the one routing decision: an edge serves only the places it registered,
  and a `Route` is the directory it may run over), `Registration`, `Desk` (an
  edge's side), `ToolRequests` (the engine's side: dispatch, settle, abandon, and each
  request's `RequestState`) and `EdgeDirectory` (which live edge serves a place, and its
  `Advert`). Named for the same idea as the `grit.edge` module: this package is the types
  every side agrees on, that module the loop an edge runs over them. ← `id`, `place`,
  `prompt`, `store`, `model`, `message`, `approval`, `tool`

No source file sits at core's root, and no two packages import each other in a circle:
`scripts/enola-law.sh` fails on a new import cycle.

The test tree mirrors it: the in-memory fakes other modules' tests use are
`store.InMemoryEntryStore`, `store.InMemoryUsageLedger`, `store.InMemoryModelProfileStore`,
`store.InMemoryPeriodStore`, `store.InMemoryLifecycleStore`, `store.InMemoryVoiceStore`, `recipe.InMemoryRoomReads`, `plugin.InMemoryPlugins` and
`durable.InMemoryDurable`; and `period.TestClosings` builds closings and balance lines.
`TestTx` lives in package `grit.dbos.sql`, because the `null` it holds is legal only inside
the DBOS quarantine (rule 6).
