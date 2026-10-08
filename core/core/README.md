# grit.core

The domain and the seams everything else is written against. No DBOS, no JDBC driver on its
classpath; implementations live in the modules that need the libraries (CLAUDE.md, rule 8).

In dependency order:

- **`clock`** — what a function cannot compute: `Clock` (the time) and `Fresh` (values
  no one made before); and `Utc`, an instant as grit writes it for a model or a person.
  Imports nothing in core.
- **`id`** — the opaque ids (`ConversationId`, `EntryId`, `TurnSeq`, `EntrySeq`, `WorkflowId`,
  `SourceId`, `ToolCallId`, `PeriodSeq`, `LineId`, `PluginName`, `DocKey` (a plugin's document's key), `DocumentVersion` (one version of a document, numbered across every plugin), `ShadowName`, `QuestionName` (a question's in a question set, or its per-source form), `CorpusName`, `EdgeName`, `AttesterName` (a source a deployment trusts to say who a realm's accounts are), `PrincipalId`: who an
  action is done for, made from text only by `grit.dbos`, through `PrincipalIds` (enola-intent.yaml), `EdgeId`, `JobName`, `ScheduleKey` (a schedule's key among its
  declarer's), `ScheduleId` (an asked schedule's, or a declared one's, by its `Declarer`: the
  deployment or a plugin)), `CallSlot` (a tool call's place in its turn, and its
  request's key), the one short content hash every content-addressed id uses, `TurnRef` (and its reply entry's id), `PeriodRef`, `CloseRef` (one
  attempt to close a period on its deadline, and its workflow id), `SettleRef` (the one
  question whether anyone is waiting on a quiet period, and its workflow id), `TriageRef`
  (the one triage of a heard message, and its workflow id) and `ShadowRef` (one shadow
  variant's run on a heard message, and its workflow id). Imports nothing in core.
- **`persona`** — who grit presents as to the people it talks with: `Persona`, the name it
  goes by (`Persona.Grit`, grit itself). A deployment declares one (ADR 0026). Imports
  nothing in core.
- **`place`** — where conversations happen (ADR 0013): `Place`, a path in one
  containment tree under the root, everywhere, one `Namespace` per source (`fs`, `slack`,
  `task`, `service`, `direct`), and `within` (a prefix, defined once) and `direct` (a direct
  message's room or thread); `Directory`, an absolute normalized
  path; `Service`, an outside service's place, and `WorksIn`, a deployment's link from
  conversations with no directory to the service they work in; `Scope` (the `Prefix`es a window may draw on beside its own conversation: places, or its own room),
  `Weight` (how far its own search hits outweigh those elsewhere) and `Locality`, both
  together. Imports nothing in core.
- **`identity`** — who someone is across sources (ADR 0032): `Account` (how one source names
  someone, `{namespace}:{name}` (an `Account.Sourced`), or `local` or `grit`; never an email
  address), `Email` (an
  address, which a realm attests of an account) and its `Domain`, `Realm` (the accounts one
  source names, as one Slack workspace's); `Vouching` and `Identities`, the attester a
  deployment trusts for each realm and the email domains it claims, refused as an
  `IdentityRefusal`; `Evidence`, `Held` and `Principal`, a person as the store resolves them;
  `Standing` and `Vouched`, what a realm's source says of one of its accounts. ← `id`
- **`visibility`** — who may see what (ADR 0030): `Level` (core's fixed scale), `Compartment`
  (a name a deployment declares), `Label` (opaque: a level and compartments, compared, joined,
  met and keyed by, never taken apart), and `LabelParts`, the level and compartments it is
  stored as, which only this package and `grit.dbos` may name (enola-intent.yaml);
  `Compartments`, a deployment's closed set; `Labelled` and `Labeller`, how a source labels what it brings in, and `RoomLabels`, a
  deployment's declared labels for rooms; `GroupName`, `Group`, `Grant` and `Memberships`, who
  is cleared for what; `Trust`, what a deployment trusts an outside service with; `Visibility`, all of it as a
  deployment injects it, refused as a `VisibilityRefusal`; `Recorded`, what is recorded beside
  it (rooms' set labels, access and quiet, and people added to groups through grit), as a
  transaction read it when it opened; `Item`, a labelled row as reading it is decided;
  `Clearance`, what a
  transaction reads and the least label it writes at, what is said in a direct message read
  only in its own room; `Subject`, whom it is opened for; `Explanation`, what a person is told
  of their clearance, naming nothing above the room they ask in; and
  `Maintenance`, `grit.dbos`'s own clearance. ← `id`, `place`, `identity`
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
  from before a restart, a plugin no longer enabled, a version of a plugin's document no longer
  current, an ended schedule), its stored form, how long each kind of target is kept (`Retention`: a window, or as
  its plugin declares), and a `Tombstone`, the decision to delete one. ← `id`, `period`
- **`store`** — what is kept and the transaction it is kept under: `Tx` (opened for a
  `Subject`, at the clearance it resolves to, ADR 0030, under the labels in force, the
  deployment's and what is recorded beside them: each room's label and each person's
  clearance, where it may write outside grit, what it may read from a source, and
  which services it may send a call's arguments to, ADR 0031), `Db` (reads), `Reads` (reads fixed to
  one subject, for code that must not choose), `Jot` (short writes from inside a step), `Entry`,
  its `Payload` and their codec `PayloadJson`, `EntryStore`, `EntrySearch`, `Conversation`, `Origin` (a TUI session, a Slack thread, a task's run, or a direct message with one person, `Origin.Direct`, its room spelled by their account; and its `Audience`: who its messages are for; its `Focus` at a message's `Position`: how many topics interleave where it is said),
  `ConversationStore` (each conversation's origin and who began it), `PromptStore` (each
  turn's system prompt, its fragments kept by id), `UsageLedger`, `ModelProfileStore` (which profile each turn ran
  under), `ModelSettingStore` (settings of pairs approved at runtime), `PeriodStore` (which
  period is open, sealing one with its closing entry, purging one), `LifecycleStore` (the
  settings in force), `VoiceStore` (the voice in force), `Principals` (the accounts an edge named, by name), `Voucher` (the right to record what a trusted realm's source says of its accounts, ADR 0032), each change it makes, or why it made none, a `Linking`, `Askers` (who a turn answers, by the one asker rule its transactions' clearance is resolved by; held only by what a deployment's kit builds), and `Speakers` (whose names a window shows on the inbound entries they wrote), `Tombstones` (what is to be deleted, until the collector has), `Opening` and `ClosingEntry` (the closing a period opens from), `EntryTopics` (a
  conversation's topics one period at a time: carried by its closing, then its own events), `StoreError`. ← `id`, `place`, `identity`, `visibility`, `message`, `topic`, `model`, `period`, `retention`, `prompt`
- **`document`** — plugins' documents (ADR 0028): a `Document` is one version of a plugin's
  document under a key, kept at a place, its `DocText` shown and searched, its data the
  plugin's own, with its `Placement` (how many windows held it); a version is current until
  the next under its key, or a withdrawal, supersedes it. `DocumentTerms` (a `DocLabel`, a
  `DocWeight`, a retention and a bound) are what a plugin declares; `DocumentShelf` and
  `DocumentKeeper`, one plugin's documents as it reads and writes them (`Written`, what a
  write did); `DocumentSearch`, every enabled plugin's documents as windows draw on them, as
  of an instant (`Shelved`, a place holding some); `DocumentStore`, what the engine writes
  and deletes of them (terms declared at each start, placements, a version or a plugin's
  every document collected). ← `id`, `place`, `store`
- **`job`** — jobs and their schedules (ADRs 0021, 0029): a `SlotRule` (once, with its `Grace`,
  or a recurrence at a local time in a zone), and the instants its slots fall at; `Due`, what a
  schedule has due at an instant (its latest slot, or a once slot missed); `Resume`, what one
  whose run is `InFlight` gets (left, enqueued again, failed, or superseded at the current
  version); `Slot`, one slot of a schedule: the run name of its run's `Origin.Task`
  conversation, and the opening grit writes there; `Report`, where a schedule's runs report
  beyond their own conversations; `SlotRuleJson` and `ReportJson`, their stored forms. A `Job`
  (versioned code a slot's run replies with, from its `JobRun`), a deployment's `Jobs` by name,
  a `Declared` schedule (a job and its typed parameters, under a key), and how a schedule
  `Ending`s; `ScheduleStore`, the stored schedules (declared ones reconciled at start, those
  `due` and those `inFlight`, a run `replied`, one read as a `Schedule`); `OwnJobs`, a plugin's jobs as its tools
  book them (a `Booking`, or `NotOwn`); and `ScheduleDesk`, one plugin's capability to write,
  list and cancel a person's once slots from a tool call, asked for `When` (an `Asked`, their
  `Pending`, or a `DeskRefusal`). ← `id`, `message`, `store`
- **`spend`** — what grit spends on model calls, read back: `Spend` (some recorded calls: how
  many, and their `Cost`), `Spending` (a day's, or a conversation's, from the ledger; what the
  ledger misses is in its doc), `Day` (a calendar day in a zone, as instants), `DailyCap`
  and `Budget` (the zone days begin in, the cap, and whether a new message is taken), and
  `Budget.Refusal`, the one line a person is told when it is not. ← `id`, `message`, `store`
- **`triage`** — what grit makes of a message it heard (ADR 0020): `Kind`, `Tags` (triage's
  answers, each under its question's name, or none; `Tags.V1`, the names and gate of the set
  triage first asked; `Tags.V2`, the names of the set it asked next, the gates over them a
  deployment composes, and its draft gate; `Tags.V3`, v2's and `to-grit`, whether a message is
  directed at grit, with v3's draft gate and its `directed` branch, which `Tags.directed` reads kept tags by; `Tags.V4`, v3's and
  `anchor-record`, whether what a message asks for is something no stored record could supply
  whoever it is asked of, with v4's draft gate) and `TriageStore`, where they are kept beside their entry and deleted
  with it; `Earning`, whether a period earns a written closing, by its heard messages'
  `durable` answers; `Shadowing` (a shadow variant as the sweep enqueues it, within its daily cap),
  `Shadowed` (what one variant made of one message: its `ShadowAnswers`, a wording's in
  order or a question set's under their names) and `TriageShadows`, where those are
  kept beside the entry and deleted with it; `Corpora`, a deployment's catalog of
  what could supply what a message asks for, each covering a place, and the service whose
  tools reach it, if any (`supplied`, each service's sources); and `Gate`, a test of a
  question set's answers that explains every failure (a `Bound` on a `Reading` of one answer,
  every one of some gates, or any one of them), which a draft is derived from; `TagsJson` and
  `GateJson`, the recorded forms of tags and of what a gate read and found; `Weighing`, live
  triage's set put to a message said to grit that roots a turn (lifecycle's `Mentions`).
  ← `id`, `message`,
  `place`, `period`, `store`, `spend`, `classify`
- **`speech`** — whether grit speaks where it was not addressed (ADR 0022): `Speaking` (off,
  shadow, or within `Limits`, whose windows are `Rate`s), a heard message as it is weighed
  (`Heard`, its `Reach`), the ledger it is weighed against (`Ledger`, each `Spoken` turn at its
  `Stage`), `Speech.decide` (drafted; answered as said to grit, at its reply address, when
  triage read it as put to grit by name; or held for a `Silence`), and `Speech.post` (what
  becomes of an unprompted draft the judge scored, `Judged`, as an `Outcome`;
  `Speech.postNamed`, of a named draft resumed from before named messages were answered as
  said to grit; a posted draft's `Cleared` says which), `Speech.spoken` (the one hold: the
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
- **`durable`** — `Durable` and `Journaled`: steps that survive a crash, and waits for a
  message (`recv`); `StepRecord`, a step as a reader after the fact sees it. ← `id`, `store`
- **`approval`** — `Approval`, a person's answer to a gated tool call, and the message
  that carries it to the turn waiting on its topic. ← `id`
- **`context`**, **`provider`**, **`inbox`** — the seams the engine plugs
  into: `ContextAssembler` (and the `Window` it builds, as wide as its request's `Width`
  asks: as deployed, or within a budget the eval harness names; and `Shown`: what the model is
  shown of a window, each line grit writes into it under its `SectionTag`: the record, a
  section from afar, a document a plugin keeps, a gap where turns were left out; and a grit
  label that starts a line in text grit did not write, shown as a quoted paste), `Provider`
  and `Models` (the
  catalog in force, and a provider per role's pin; ← `model`), and `Inbox` (which also
  records a message heard where grit listens, not said to it, at the time it was said, says which of a thread's messages it has recorded, takes a direct message only from its own person and labels it at their clearance, refusing a new message in a direct thread begun when they were cleared for more (`InboxError.Sealed`), answers a turn's gated call, says how far a turn has got: its `Progress`, and starts what a schedule has waiting, `Slotted`; the id its entry is kept under is
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
- **`recipe`** — what a call's input shows beyond its own thread, and how a turn is shaped:
  a `Pool` of `Source`s (the
  room's recent messages, its author's, the exchanges stitching offered) within a budget in
  characters, each kept in the `Section` its source names, and what a pool shows of what it
  found, in its sources' order; `Pool.read`, what a pool shows for a heard message, from what
  its room held when it was said, read through `RoomReads` and the stitch kept for its
  conversation. `TurnRecipe`, each turn's `Shaping` by what it is `Rooted` on (a heard
  message `ByFocus`, or one said to grit): its window's `Width`, and its `Offering` of the
  services its conversation links; `Offering.decide`, each service as a `ServiceOffer` with
  its supplying sources and its verdict, withheld only where its gate over the root's
  per-source answers fails; and `TurnRecipe.Shipped`, which shapes nothing. Nothing in core
  imports it. ← `id`, `place`, `message`, `classify`, `period`, `store`, `triage`, `context`,
  `stitch`
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
  `Writing` (a hosted tool that writes outside grit: a call is bound to the place its `to`
  names, or refused `CallError.Unwritable`; the edge's tool, `over`, is told the destination
  its request holds, through `Toolbox.requested`),
  `Retry` (whether a call cut short is run again or answered `Interrupted`), `Writes` (where a
  hosted tool's calls write outside grit: the names a call may give in its `to` argument, each
  an edge's own destination and the place core labels it by), `ToolSet`
  (a turn's tools as recorded, by content id, each entry with the `Writes` it declares) and `ToolSets` (where each is kept), `Toolbox` (the tools offered on one call, which `bind` a
  call to a `Bound` or a `CallError`: `Bound.Free` runs, `Bound.Gated` runs only given an
  `Approval`, each told the `CallSlot` it runs as, which a tool made by `Hosted.calling` reads), `Repairs` (what of a call is repaired before it is read, as the pair's
  settings say) and `Outcome` (what a call came to, as the model reads it). ← `id`,
  `place`, `message`, `model`, `store`, `provider`, `approval`

- **`plugin`** — features a deployment turns on (ADR 0027), each a pure bundle of
  contributions to core's points: `Plugin` (a name, a version, the plugins it `needs`, and
  optionally a `CachePosting`, which keeps what it wants of one `ClosedPeriod`, its
  `Documents`, the terms they are kept under and its posting to them, and
  `PluginTool`s, each a `Hosted` description bound at start to a `PluginRun` over its own
  documents, `PluginReads`, its needs' services, `Needs`, and its own jobs, `OwnJobs`, run told
  its call, handed `Reads` for the turn that made it and a `ScheduleDesk`; and its `Job`s and `Declared` schedules); `Exports`, a plugin another may
  need, exporting a pure service over its own documents, the only way one plugin reads
  another's; `CacheDocs` (where it keeps what it makes of one closed period, deleted with that
  period's closing), `PluginDocs` (one plugin's documents as its surfaces read them),
  `PluginCursors` (how far each has posted, in close order; a new version starts again, in a
  new generation) and `PostRef` (one posting run, and its workflow id). ← `id`, `period`,
  `store`, `document`, `job`, `tool`

- **`admin`** — changes to who may see what that people make through grit: `Command`, what
  a person asks by command, read from the words after its name; `Change`, what one changes
  (relabel, quiet, clear, remove), with what it replaces, and `ChangeJson`, its audit row's
  form; `Authority`, the one rule over what a change lowers and removes, whose `Allowed` is
  what applying a change takes, or a `Refusal`; `Answer`, what a command did, in the
  words its asker reads; and `Administration`, which runs a command in one transaction,
  reading who administers and stewards from the deployment's declared groups, and keeps
  each change allowed with its audit row. ← `place`, `identity`, `visibility`, `store`

- **`edge`** — what an edge and the engine share: `ServedEdge`, an edge a deployment serves
  beside its engine as it posts to a plugin (ADR 0021), opened over `EdgeStores` (what an
  edge reaches the engine through, ADR 0002, its `Administration` among it) with the `Variable`s it `needs`, refused as an
  `EdgeRefusal`, and the attester it also is, if any (ADR 0032); `CatchUp`, what an edge hears once before serving, as `Unheard` per source;
  a trusted realm's source (`RealmSource`, which answers `ask` and `all` as `Asked`, deciding
  nothing) and `Attesting`, core's rules for asking it: before each message unless answered
  within `Fresh`, on the source's word of a change, and in a look every `Every` at accounts
  answered `Due` ago, an unreachable source keeping the last word, each answer recorded in a
  transaction of its own;
  `Joins`, the rooms an edge's bot is a member of (`Membership`), each join and leave ordered by
  when it happened, a join's backfill skipped when its inviter is no full member a trusted realm
  vouches, and rooms left a day (`KeptLeft`) forgotten unless a person labelled or quieted them;
  `Deliveries` (the replies an edge has yet
  to post outside grit, each part `Posting` or `Posted`), `Acknowledgements` (the messages an
  edge marks as being answered while their turns run, each `Acknowledgement` shown or not),
  and the tool calls an edge runs
  (ADR 0017): `ToolRequest` (one call, addressed to a
  workspace, with its `Permit` and retry), `OutcomeJson` (its answer's stored form),
  `Edges.authorize` (the one routing decision: an edge serves only the places it registered,
  and a `Route` is the directory it may run over), `Registration`, `Desk` (an
  edge's side), `ToolRequests` (the engine's side: dispatch, which
  writes a request its transaction may not send already answered with its `refusal` (ADR
  0031), settle, abandon, and each request's `RequestState`) and `EdgeDirectory` (which live edge serves a place, and its
  `Advert`). Named for the same idea as the `grit.edge` module: this package is the types
  every side agrees on, that module the loop an edge runs over them. ← `clock`, `id`, `place`,
  `identity`, `visibility`, `prompt`, `store`, `model`, `message`, `approval`, `tool`, `admin`

No source file sits at core's root, and no two packages import each other in a circle:
`scripts/enola-law.sh` fails on a new import cycle.

The test tree mirrors it: the in-memory fakes other modules' tests use are
`store.InMemoryEntryStore`, `store.InMemoryUsageLedger`, `store.InMemoryModelProfileStore`,
`store.InMemoryPeriodStore`, `store.InMemoryLifecycleStore`, `store.InMemoryVoiceStore`, `recipe.InMemoryRoomReads`, `plugin.InMemoryPlugins`, `job.InMemorySchedules` (with each plugin's desk), `document.InMemoryDocuments` (which owns the `store.InMemoryTombstones` its keepers mark) and
`durable.InMemoryDurable`; `period.TestClosings` builds closings and balance lines,
`id.TestCallSlots` the tool calls a test's tool is told, `clock.SetClock` is a clock a test
moves, and `durable.Probes` compiles the probe sources of core's capture and separation suites.
`TestTx` lives in package `grit.dbos.sql`, because the `null` it holds is legal only inside
the DBOS quarantine (rule 6).
