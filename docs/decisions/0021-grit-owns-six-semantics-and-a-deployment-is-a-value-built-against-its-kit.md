# 0021. grit owns six semantics over two guarantees, and a deployment is a value built against its kit

Status: accepted (2026-09-30); amended (2026-10-06): a run's workflow is distinct per slot
and version, and a version's change supersedes pending runs inside their own body; amended
(2026-10-08): named moves are built (ADR 0034), without reuse across versions

Context: grit is to run more than one deployment, each with several personas, scheduled
work and code of its own. Turned down: grit as the deployment, configured by environment
(it cannot hold a second persona or an organisation's code); deployment as data (a job's
plan and an organisation's tools are code); a general workflow engine or agent framework
inside grit (a market of its own); and letting a host system's workflow engine set the
boundary (it exists for the host's reasons, not grit's).

Decision:

- Two guarantees, durable work (ADRs 0004, 0009) and capability typing (ADR 0003), carry six
  semantics grit owns because the thesis fails without each: memory (entries never
  rewritten, periods closing into grounded records), the window (each model call's context
  assembled from memory), the turn (named moves chosen by a planner: the model's loop, or a
  deployment's Scala job), place (where a conversation happens and what is hosted there),
  identity (principals with a kind and a sponsor), budget (spend bounded per sponsor).
- grit never owns a thing with a market of its own, a wire format or an organisation's
  name: not orchestration frameworks, agent protocols (MCP and A2A are extensions), identity
  proof, domain storage, cron, other workflow engines (they are tool hosts), model hosting,
  UI, persona evals, hosting, secrets or deploy; and never truth: it records ground.
- Code is placed by three questions in order: the thesis fails without it: core; it names a
  generic protocol, tool or service and no organisation: a shipped extension; it names an
  organisation's systems, data or people: the deployment's own code, outside grit.
- A deployment is a `Deployment` value declared in code, in a module of its own, against
  grit's kit, the one grit module besides a deployment that names an extension. Edges and
  plugins contribute to it through core traits. The kit owns the DBOS application version:
  a deployment registers no workflow, and its jobs run under grit's epoch. Secrets and the
  database come from the environment, never the value.
- A job is a turn with a Scala planner, and it has a version. A trigger is an edge (ADR
  0002) starting one run per slot at a task place; a run is a conversation there, closed
  like any other, and its workflow is distinct per slot and version: the slot names its
  conversation, and the version names its opening, so each (slot, version) is its own turn
  there (amended 2026-10-06; ADR 0029). The engine recovers every workflow of its epoch, so a
  deploy does not by itself end a run: a run started under another version than its job's
  current one ends itself superseded, writing no reply, and its slot is started again at the
  current version unless a later slot is due (amended 2026-10-06: nothing is cancelled out
  from under DBOS, where the kit had cancelled the pending runs of any other version). A
  job's moves are kept in memory under names the plan gives them, each with a digest of what
  it does; a new version's run reuses a kept outcome, or a kept answer to an ask, only where
  the digest matches, and a move whose request was claimed and never answered is
  interrupted, never run again (ADR 0009). Jobs are never patched. (Amended 2026-10-08.) Named
  moves are built (ADR 0034), each ask's and call's digest recorded; reuse across versions is
  not yet: a run resumed under another version after its moves ends in error and is
  superseded.
- A principal has a kind (person, org, assistant, agent) asserted by its edge, never a trust
  bit; an assistant or agent has a sponsor, and every chain ends at a person or an org. Only
  a person's word grounds as a colleague's; a gate names who may answer; budgets attach to
  sponsors.
- Personas meet only at places: a persona is configuration (an assistant principal, its
  words, its model policy, its jobs, its budget, its places), not an agent grit calls, and
  grit never calls, routes or plans between personas.
- A fact lives where its readers are, and grit keeps what was said and decided about it.
  Drop grit's database and the host loses nothing it reads; drop the host's and grit still
  knows what was decided. Never both as data.

Consequences: a second deployment is a module and a dependency, not a fork; grit's own use is
the reference deployment. Harder: the kit, `ServedEdge` and each extension's entry object
become an API other repositories compile against. Enforced by enola rules (core names no
extension, each extension names core alone, the kit names no edge extension, terminal UI or
deployment), Mill's module dependencies, and the seats' types as they are built.
