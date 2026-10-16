# 0021. grit owns six semantics, compiled in, and a deployment is a value built against its kit's surface

Status: accepted (2026-09-30); revised (2026-10-16)

Context: grit is to run more than one deployment, each with several personas, scheduled
work and code of its own. Turned down: grit as the deployment, configured by environment
(it cannot hold a second persona or an organisation's code); deployment as data (a job's
plan and an organisation's tools are code); a general workflow engine or agent framework
inside grit (a market of its own); letting a host system's workflow engine set the boundary
(it exists for the host's reasons, not grit's); and steps a deployment inserts into the
turn, versioned by joining them into the deployment's version. That last was built and
pulled: adding one stranded every workflow in flight, and the version missed an extension's
own changes.

Decision:

- Two guarantees, durable work (ADR 0004) and capability typing (ADR 0003), carry six
  semantics grit owns because the thesis fails without each: memory (entries never
  rewritten, periods closing into grounded records), the window (each model call's context
  assembled from memory), the turn (moves chosen by a planner: the model's loop, or a job's
  Scala plan, ADR 0034), place (where a conversation happens and what is hosted there),
  identity (principals, ADR 0032), budget (spend bounded per sponsor).
- **Semantics are compiled; reach is declared.** What changes what a turn does (plugins,
  jobs, the turn's steps) is code compiled into the trusted base. What extends where grit
  reaches (an edge, an MCP server, a knowledge source) is declared, labelled at its boundary
  and checked in core (ADR 0030).
- grit never owns a thing with a market of its own, a wire format or an organisation's
  name: not orchestration frameworks, agent protocols (MCP and A2A are extensions), identity
  proof, domain storage, cron, other workflow engines (they are tool hosts), model hosting,
  UI, persona evals, hosting, secrets or deploy; and never truth: it records ground.
- Code is placed by three questions in order: the thesis fails without it: core; it names a
  generic protocol, tool or service and no organisation: a shipped extension; it names an
  organisation's systems, data or people: the deployment's own code, outside grit.
- **A deployment is a `Deployment` value** declared in code, in a module of its own, against
  grit's kit, the one grit module besides a deployment that names an extension. Edges and
  plugins contribute to it through core traits. Secrets and the database come from the
  environment, never the value. Its jobs are ADR 0029's.
- **The deployment surface** is what a deployment names: core, the kit (which exports the few
  types its signatures take from past it) and each extension's entry folders, the
  `deployment-surface` component in `enola-intent.yaml`. What lies past it is
  `private[grit]`, the engine's openers and `DurableWorkflow` among it.
- **The kit owns the epoch; a deployment joins its version.** A deployment registers no
  workflow and owns no table; its engines open under grit's epoch joined with its
  `DeploymentVersion` (`Deployment.runsUnder`, ADR 0004).
- **Persona.** Who grit is, is a persona the deployment declares (`Deployment.of` takes a
  required `Persona`), not an edge's identity: an edge's bot is known only once it opens,
  after declaration, differs by edge, and would tie triage's words to another product's
  setting. A persona is one name, trimmed, one line, at most `Persona.MaxChars` characters,
  refused when built otherwise; `Persona.Grit` is grit itself. The kit hands it to every turn,
  a Slack turn is told "In this workspace you are called {name}.", `about` opens with it, and
  triage words its question about grit with it; a message put to the persona by name is
  answered as said to grit (ADR 0022). Edges name no assistant. Personas meet only at places:
  a persona is configuration, not an agent grit calls, and grit never routes between them.
- A fact lives where its readers are, and grit keeps what was said and decided about it.
  Drop grit's database and the host loses nothing it reads; drop the host's and grit still
  knows what was decided. Never both as data.

Consequences: a second deployment is a module and a dependency, not a fork; grit's own use is
the reference deployment. Harder: the kit, `ServedEdge` and each extension's entry object
are an API other repositories compile against. A Slack bot whose display name differs from
the persona is not recognised by triage when typed in plain text. Enforced by law rules
(`core-names-no-extension`, `kit-names-no-edge-or-deployment`), the compiler outside `grit`
(the kit's `BoundaryTests` probe; a deployment building a DBOS of its own is not held, since
DBOS stays on its classpath), and the compiler for the persona.
