# 0034. Turns and jobs act through three moves, ask, call and keep, made under an acting value

Status: accepted (2026-10-08); revised (2026-10-16)

Context: the turn called models through `Provider` and edges' tools through `ToolRequest`, with
spend, ADR 0031's label checks and approvals woven into its own steps. A job's run (ADR 0029)
only replied text. Giving jobs moves of their own would have defined spend, attribution, label
checks and gates a second time, and smart jobs and subagents a third. Turned down: handing
plugins `Durable` steps (a plugin could name, order and repeat steps freely and reach effects
past every check); keeping in a plain step through `Jot` (its writes would not commit with the
step's record). Moving the turn onto the moves' step names was first turned down, since it
needed a new epoch; the 2026-10-15 epoch made that move.

Decision:

- **Three moves, a closed set.** A move is what touches its acting's own accounts, which no
  edge can do for it. `ask`: one model response, admitted by its actor's allowance, routed to a
  cleared pair or not made (ADR 0031), and recorded in the usage ledger at the estimate of the
  request sent. `call`: one hosted tool request (ADR 0017), checked as ADR 0031 says and gated
  as its actor's gates say. `keep`: one transaction over the acting plugin's own documents. A
  future `spawn` derives an acting from it. Everything that acts on the world is a `call` of a
  hosted tool, those grit hosts itself included; no plugin is given `Durable`.
- **An ask poses one of a closed set of shapes**, its reply typed by its shape: text; JSON the
  model gives as one required tool's arguments, held to a schema that is a value, checked in the
  subset every provider's strict mode accepts and repaired once; or a judgment, questions about
  a state answered by the deployment's classifier with a probability per option or level. Every
  shape is named, made at most once, counted against its run's limits, admitted, recorded per
  model call, labelled and digested. The typed reply is read from the recorded reply on every
  run. A judgment is always a classifier's, never a model's structured reply posing as one.
- **Every move is made under an acting value** (`Acting`): its turn, for whom every transaction
  is opened, so clearance is resolved as each opens (ADRs 0030, 0032) and never carried; whom
  its requests record; its allowance; its gates. It is data, not a capability. A turn acts for
  its asker, who approves its gated calls. A job's run acts at its schedule's clearance and for
  its schedule's principal, each ask admitted against the daily cap, no gated call sent. A
  request whose principal cannot be read is not sent.
- **A derived actor is no more than its parent**: its clearance met with its parent's and its
  budget carved from its parent's. How is not yet decided.
- **The rules are written once**: the acting value and the planner's moves in core, their
  durable steps in `grit.act`, which the turn, jobs and the lifecycle share. One step grammar
  (ADR 0038): a planner's moves are `ask:{name}`, `call:{name}` and `keep:{name}`, the turn's
  `ask:n` and `call:n:j`. The lifecycle's classifier calls and the close's summary call are
  moves (`Judging`), each charged to its turn under its own workflow.
- **A planner makes its moves through `Moves`**: each named, at most once per run, within its
  job's limits, with a digest of each ask's and call's input; a rerun whose input differs is
  refused. A keep records what its body returned (`{"kept": …}`) or why nothing was kept
  (`{"store": …}`), and no digest: its input is its code, which its job's version names.
- **A keep's body runs under a savepoint** (`Savepoints`): on a `Left`, none of its writes are
  kept, and the step still commits, recording the refusal; otherwise a failed statement would
  abort the transaction its output is recorded in, and DBOS keeps a thrown step as the run's
  permanent result.
- **Whose a job is says what it may do**: a plain job asks and calls; a plugin's keeping job
  also keeps, under its plugin's document terms. A deployment is refused a keeping job whose
  plugin declares no documents.
- **Every result carries a bound on its content's label**: for now, its actor's floor. A label
  below it is a declassification (ADR 0036's `Downgrade`).

Consequences: spend, attribution, label checks and gates are the same for every actor, and a
model call or tool request made outside a move is a review finding. A classifier's calls are in
the ledger and count against the daily cap. A planner's classifier is the deployment's, chosen
at launch, not a catalog pin; JSON replies rely on `tool_choice: required`. Every store behind
a keep must give savepoints. Enforced by the types (a job is pure and is handed only moves; a
keep's body captures nothing), capture probes, `MovesContract` against the moves and their
fake, the replay gate, the law (`act-names-core-alone`), and Mill (`grit.job` names no
`grit.turn`).
