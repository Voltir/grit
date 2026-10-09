# 0034. Turns and jobs act through three moves, ask, call and keep, made under an acting value

Status: accepted (2026-10-08); amended (2026-10-09): what a move is, and one ask in shapes

Context: The turn called models through `Provider` and edges' tools through `ToolRequest`, with
spend, ADR 0031's label checks and approvals woven into its own steps, and its requests recorded
a placeholder principal. A job's run (ADR 0029) only replied text; ADR 0021 promised it named
moves. Giving jobs moves of their own would have defined spend, attribution, label checks and
gates a second time, and smart jobs and subagents a third. Handing plugins `Durable` steps was
turned down: a plugin could name, order and repeat steps freely and reach effects past every
check. Moving the turn onto the new moves' step names was turned down too: it needs a new
epoch, which strands the turns in flight, or a patch that keeps the whole old loop for the epoch.

Decision:

- Three moves, a closed set. `ask`: one model response, admitted by its actor's allowance and
  recorded in the usage ledger once, at the estimate of the request the model was sent. `call`:
  one hosted tool request (ADR 0017), checked as ADR 0031 says where its tool is offered, where
  the call is bound and where its request is written, and gated as its actor's gates say.
  `keep`: one transaction over the acting plugin's own documents. A new kind of effect is a
  tool an edge hosts, reached by `call`; no plugin is given `Durable`. An ask's shapes (below)
  are not moves.
- (Amended 2026-10-09.) A move is what touches its acting's own accounts, which no edge can do
  for it: `ask` spends its allowance on a model chosen where its pin is made, where trust can
  route it; `keep` writes grit's own store in the transaction that records its step; a future
  `spawn` derives an acting from it. Everything that acts on the world is a `call` of a hosted
  tool, those grit hosts itself included: reading memory, taking notes, asking a person.
- (Amended 2026-10-09.) An ask poses one of a closed set of shapes, and its reply's type is its
  shape's: text from the model; JSON the model must give as one required tool's arguments, held
  to a schema that is a value (it may be defined at run time), checked by grit in the subset
  every provider's strict mode accepts, and repaired once; or a judgment, questions about a
  state answered by the deployment's classifier with a probability per option or level
  (choices, yes/no, scores). Every shape is named, made at most once, counted against its run's
  limits (judgments as judgments, the rest as asks), admitted by its allowance, recorded in the
  usage ledger per model call, labelled, and digested, a diverged rerun refused. What a shape's
  model or classifier replied is recorded, and the typed reply is read from that record on
  every run. A judgment is always a classifier's, never a model's structured reply posing as
  one.
- Every move is made under an acting value: its turn, for whom every transaction is opened, so
  clearance is resolved as each opens (ADRs 0030, 0032) and never carried; whom its requests
  record, read where each is written; its allowance; its gates. It is data, not a capability. A
  turn acts for its asker, admitted when its message was taken, its asker approving its gated
  calls. A job's run acts at its schedule's clearance and for its schedule's principal, each ask
  admitted against the daily cap, and no gated call is sent. A request whose principal cannot be
  read is not sent.
- An actor derived from another, such as a subagent, must be no more than it: its clearance met
  with its parent's and its budget carved from its parent's. How is not yet decided; a child
  conversation grit begins does not give it.
- The rules are written once: the acting value and the planner's moves in core, and their
  durable steps in one module the turn and jobs share, each step sequence taking its caller's
  step names and recorded forms. The turn keeps its names and forms; a planner's moves are
  named `move:{name}`.
- A planner (a job, later a smart job) makes its moves through `Moves`: each named, at most once
  per run, its asks and calls within its job's limits, with a digest of each ask's and call's
  input in a form the moves own; a rerun whose input differs is refused. A keep records what its
  body returned, in that value's own journaled form held as a string (`{"kept": …}`), or why
  nothing was kept (`{"store": …}`), and no digest: its input is its code, which its job's
  version names. There is no reuse across versions: a run resumed under another version before
  any move is superseded at its reply; one resumed after its moves ends in error at its reply,
  and is superseded.
- A keep is one transaction step whose body runs under a savepoint (core's `Savepoints`): when
  it returns a `Left`, none of its writes are kept, a failed statement included, and the step
  still commits, recording the refusal. A transaction step commits whatever its body returns,
  and a failed statement (a raced key) aborts the transaction the step's output is recorded in,
  so the step would throw, and DBOS keeps a thrown step as the run's permanent result. Keeping
  in a plain step through `Jot` was turned down: its writes would not commit with the step's
  record.
- Whose a job is says what it may do: the deployment's or a plugin's plain job asks and calls;
  a plugin's keeping job also keeps, under the terms of the documents its plugin declares. A
  plugin's documents are their terms and, optionally, its posting to them; with no posting, only
  its keeping jobs write them, and the plugin is posted closed periods only when it has a cache
  or a posting. A deployment is refused a keeping job whose plugin declares no documents.
- Every result carries a bound on its content's label (its content is at most that label): for
  now, its actor's floor. A label below it is a declassification, decided as one.

Consequences: spend, attribution, label checks and gates are the same for every actor, and a
model call or tool request made outside a move is a review finding. The step sequences are
generic in their caller's failure type and recorded forms, which costs some indirection. A
call's answer is read in one place, in a transaction for its actor, where labels per item can
filter it; a model is chosen where a turn's or ask's pin is made, where trust can route it.
Lifecycle's model calls and classifier calls are not moves yet. Every store behind a keep must
give savepoints. Enforced by the types (a job is pure and is handed only moves; a keeping job
exists only with its plugin's terms; a keep's body captures nothing and returns a pure value),
capture probes, one contract run against the moves and their fake, the moves' recorded forms
pinned as written, the turn's and jobs' replay tests, and the law (the shared module names core
alone; jobs name no turn).

Consequences of the 2026-10-09 amendment: a classifier's calls made by a move are in the ledger
and count against the daily cap; Settle's, the close gate's and triage's still are not. A
planner's classifier is the deployment's, chosen at launch, not a catalog pin. JSON replies rely
on `tool_choice: required`; `response_format` waits for a measured setting. The classifier, like the model's provider, is trusted with
everything the deployment sends it; nothing checks what an ask sends either, until trust routes
models and classifiers.
