# grit.core

The domain and the traits everything else is written against. No DBOS, no JDBC driver on its
classpath; implementations live in the modules that need the libraries (CLAUDE.md, rule 8).

Each package's types and their docs are the code's: `scripts/outline area <name>` maps a part of
grit, and `scripts/outline show <Type>` reads one. This file holds what the code does not say:
the packages' order, each one's idea, and the rules enforced outside the compiler.

In dependency order, each with the core packages it imports:

- **`clock`**: what a function cannot compute: the time, values no one made before, and an
  instant as grit writes it. Imports nothing in core.
- **`id`**: the opaque ids, and the one short content hash every content-addressed id uses. A
  `PrincipalId` is made from text only by `grit.dbos`, through `PrincipalIds`
  (enola-intent.yaml). Imports nothing in core.
- **`persona`**: who grit presents as; a deployment declares one (ADR 0026). Imports nothing in
  core.
- **`place`**: where conversations happen (ADR 0013): one containment tree, one namespace per
  source, and the scope and weight a window draws on beside its own conversation. Imports
  nothing in core.
- **`identity`**: who someone is across sources (ADR 0032): accounts, the realms that name them,
  what a trusted realm attests, and the person the store resolves them to. An account is never
  an email address. ← `id`
- **`visibility`**: who may see what (ADR 0030): labels, compartments, clearances, and what a
  transaction is opened for. `Label` is opaque, never taken apart; `LabelParts`, its stored
  form, may be named only here and in `grit.dbos` (enola-intent.yaml). ← `id`, `place`,
  `identity`
- **`prompt`**: a turn's system prompt as ordered fragments (ADR 0016), the most stable layer
  first, for a provider's prompt cache. ← `id`
- **`model`**: what grit knows about models, as data: the catalog, the settings a call is made
  under, and the policy of which pair does each job. Imports nothing in core.
- **`message`**: the model's vocabulary: messages, usage and cost. ← `id`
- **`schema`**: the JSON Schemas a reply is held to, in the subset every provider's strict mode
  accepts. ← `model`
- **`classify`**: the `Classifier` trait (Jev's shape: closed questions answered with a
  probability per option) and the pure readers of its answers. Above the packages that record
  what a classifier answered. ← `id`, `message`
- **`topic`**: a conversation's topics as recorded events and the pure fold over them. ← `id`
- **`period`**: a conversation's time as periods that close (ADRs 0011, 0012, 0018): the one
  definition of each deadline, and the closing a closed period leaves. ← `id`
- **`retention`**: what grit deletes, and when (ADR 0014). ← `id`, `period`
- **`store`**: what is kept, and the transaction it is kept under: `Tx` is opened for a subject,
  at the clearance it resolves to (ADRs 0030, 0031); the stores, each a trait `grit.dbos`
  implements. ← `id`, `place`, `identity`, `visibility`, `message`, `topic`, `model`, `period`,
  `retention`, `prompt`
- **`spend`**: what grit spends on model calls, read back, and the daily budget. ← `id`,
  `message`, `store`
- **`triage`**: what grit makes of a message it heard (ADR 0020): its question sets, the gates
  over their answers, shadow variants, and the corpora that could supply what a message asks
  for. ← `id`, `message`, `place`, `period`, `store`, `spend`, `classify`
- **`speech`**: whether grit speaks where it was not addressed (ADR 0022). ← `id`, `message`,
  `period`, `place`, `spend`, `store`, `triage`
- **`review`**: a person's verdict on grit's speech decision, asked of a few heard messages a
  day; kept after the message's entry is gone (ADR 0024). ← `id`, `classify`, `store`, `speech`
- **`durable`**: steps that survive a crash, and waits for a message. ← `id`, `store`
- **`approval`**: a person's answer to a gated tool call. ← `id`
- **`provider`**: the `Provider` trait the engine plugs a model into. ← `model`, `message`
- **`stitch`**: a Slack thread's first message joined to an exchange in its room (ADR 0023).
  ← `id`, `message`, `place`, `period`, `store`, `classify`
- **`host`**: what a tool may do to the machine, as capabilities `grit.host` implements, and the
  pure rules they share. ← `place`
- **`tool`**: tools as typed data: the spec the model is shown and the reader of its calls,
  hosted descriptions the engine offers and an edge runs (ADR 0017), and the toolbox that binds
  a call. ← `id`, `place`, `message`, `model`, `schema`, `store`, `provider`, `approval`
- **`document`**: plugins' documents (ADR 0028): a version is current until the next under its
  key, or a withdrawal, supersedes it. ← `id`, `place`, `visibility`, `store`, `tool`
- **`context`**: the `ContextAssembler` trait, and what the model is shown of a window. ← `id`,
  `place`, `message`, `period`, `store`, `document`
- **`recipe`**: what a call's input shows beyond its own thread, and how a turn is shaped.
  Nothing in core imports it. ← `id`, `place`, `message`, `classify`, `period`, `store`,
  `triage`, `context`, `stitch`
- **`act`**: what an actor may do, and under what (ADR 0034): who acts, its moves within their
  limits, and what they come to. ← `id`, `place`, `visibility`, `message`, `classify`, `store`,
  `document`, `spend`, `durable`, `provider`, `tool`, `schema`
- **`job`**: jobs and their schedules (ADRs 0021, 0029). ← `id`, `clock`, `visibility`,
  `message`, `store`
- **`inbox`**: the `Inbox` trait the engine takes messages through. ← `id`, `identity`,
  `message`, `store`, `spend`, `triage`, `speech`, `approval`, `job`
- **`plugin`**: features a deployment turns on (ADR 0027), each a pure bundle of contributions
  to core's points; one plugin reads another's documents only through what it exports. ← `id`,
  `period`, `store`, `document`, `job`, `tool`
- **`admin`**: changes to who may see what that people make through grit, each kept with its
  audit row, under one rule over what a change raises, lowers and removes. ← `place`,
  `identity`, `visibility`, `store`
- **`edge`**: what an edge and the engine share (ADRs 0002, 0017, 0021, 0032): served edges,
  attesting, joins, deliveries and tool requests. Named for the same idea as the `grit.edge`
  module: this package is the types every side agrees on, that module the loop an edge runs
  over them. ← `clock`, `id`, `place`, `identity`, `visibility`, `prompt`, `store`, `model`,
  `message`, `approval`, `tool`, `admin`

No source file sits at core's root, and no two packages import each other in a circle:
`scripts/enola-law.sh` fails on a new import cycle.

The test tree mirrors it: the in-memory fakes other modules' tests use are
`store.InMemoryEntryStore`, `store.InMemoryUsageLedger`, `store.InMemoryModelProfileStore`,
`store.InMemoryPeriodStore`, `store.InMemoryLifecycleStore`, `store.InMemoryVoiceStore`,
`recipe.InMemoryRoomReads`, `plugin.InMemoryPlugins`, `job.InMemorySchedules` (with each plugin's
desk), `document.InMemoryDocuments` (which owns the `store.InMemoryTombstones` its keepers mark) and
`durable.InMemoryDurable`; `period.TestClosings` builds closings and balance lines,
`id.TestCallSlots` the tool calls a test's tool is told, `clock.SetClock` is a clock a test moves,
and `durable.Probes` compiles the probe sources of core's capture and separation suites. `TestTx`
lives in package `grit.dbos.sql`, because the `null` it holds is legal only inside the DBOS
quarantine (rule 6).
