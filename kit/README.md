# grit.kit

The kit a deployment is built against (ADR 0021): a deployment declares itself as a
`Deployment` value in a module of its own, and runs it with `Kit.serve` or `Kit.catchUp`.
The kit owns the DBOS application version: it opens every engine under the deployment's
`runsUnder`, grit's epoch joined with the deployment's own `DeploymentVersion` (ADR 0004), and
a deployment registers no workflow of its own. It reaches DBOS only through
`grit.dbos.engine.Engine`, whose openers are `private[grit]`: code outside `grit` cannot open
an engine but through the kit (`kit/probes`' `BoundaryTests`).

A deployment names the deployment surface (the `deployment-surface` component in
[`enola-intent.yaml`](../enola-intent.yaml)): core, this kit and each extension's entry
folders. `deployment` exports the few types a declaration takes from past the kit (`Rounds`,
`ShadowVariant`, `TriageQuestions`, `Seed`), and `Placer.JevService` names Jev's party.

Where a module goes (core, extension, kit or deployment), what a deployment can supply, and
how it is built outside grit: [`docs/extending.md`](../docs/extending.md). An edge reaches
the kit as a `grit.core.edge.ServedEdge` value its extension's entry object makes
(`grit.slack`'s `SlackEdge.serving`), as a plugin reaches it as a `Plugin`: the kit names no
plugin, and offers every plugin's tools the same way, each bound when the engine starts over
the plugin's own documents and the services of the plugins it needs (ADR 0027).

Its types are the code's (`scripts/scout area deployment`); this file holds the package
order and what the signatures cannot say.

In dependency order:

- **`deployment`**: what a deployment declares, built by `Deployment.of`, whose doc lists each
  refusal: its plugins and its own jobs (ADR 0029), the tools every turn is offered, how a
  window is assembled and messages placed on the agenda, triage's shadows and the corpora they
  ask about, a review, the turn recipe (ADR 0025), the persona (ADR 0021), its visibility (ADR
  0030) and its identities (ADR 0032), and by core's links which conversations work in or reach
  a service an edge hosts. Imports nothing in kit.
- **`environment`**: what the process environment supplies, never declared: a `.env` file under
  the real environment, the database, OpenRouter's key and Jev's settings. An edge's
  credentials are its own `needs`. ← `deployment`
- **`run`**: running a deployment. `Kit.serve` runs the engine and every edge until the process
  is stopped; `Kit.catchUp` hears an edge's catch-up once, estimated, each source's label shown
  and agreed to first, then sweeps until nothing is left to close. Those two alone end what the
  deployment's identities no longer trust as they start: a process running its own chat may be
  declared with fewer edges, and so trust fewer realms. Every way grit runs launches the same
  workflows, telling the time by the engine's clock, with declared schedules reconciled at
  start and grit's own tools (`about`, and `clearance` over the engine's askers, which the kit
  alone holds) offered beside every plugin's. ← `deployment`, `environment`
- **`rehearse`**: seeing what a shipped workflow body would do, without running it on the engine
  (ADR 0041).
  `Rehearsal` runs a body once and tells each step as it goes; nothing replays, and it is never
  handed to a workflow that must survive a crash. `RehearsalRunner` runs the workflows one
  message starts, in the session that recorded it, and answers their placements from its own.
  `Taps` are a dry rehearsal's models and classifier: every call is kept on its `Tape`,
  answered by `StandIns`, and the turn's own call stops the rehearsal (`Stop`). `Rehearse.dry`
  runs a `Situation` so over a database as it stands, and shows each call's input by part as
  `Rehearsed`.

No source file sits at the module's root, and the test tree mirrors it.
