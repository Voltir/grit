# grit.kit

The kit a deployment is built against (ADR 0021): a deployment declares itself as a
`Deployment` value in a module of its own, and runs it with `Kit.serve` or `Kit.catchUp`.
The kit owns the DBOS application version: it opens every engine under `Turn.Epoch`, and a
deployment registers no workflow of its own. It reaches DBOS only through
`grit.dbos.engine.Engine`.

Where a module goes (core, extension, kit or deployment), what a deployment can supply, and
how it is built outside grit: [`docs/extending.md`](../docs/extending.md). An edge reaches
the kit as a `grit.core.edge.ServedEdge` value its extension's entry object makes
(`grit.slack`'s `SlackEdge.serving`), as a plugin reaches it as a `Plugin`: the kit names no
plugin, and offers every plugin's tools the same way, each bound when the engine starts over
the plugin's own documents and the services of the plugins it needs (ADR 0027).

In dependency order:

- **`deployment`** — what a deployment declares: `Deployment` (built by `Deployment.of`,
  called with named arguments, whose doc lists each `DeploymentRefusal`), its own jobs and
  declared schedules beside its plugins' (ADR 0029), `PluginBinding` (each plugin's tools
  bound over its documents, its needs' services and a desk of its own jobs), `Offer` and
  `Offered`
  (the tools every turn's model is offered), `Assembly` (how a window is assembled),
  `Topics` (how a message is placed among topics), the shadows of triage's question it
  records (`grit.lifecycle.shadow.ShadowVariant`), the corpora their question
  sets ask about (`grit.core.triage.Corpora`), and the review of one of them it picks
  heard messages for (`ShadowReview`: `grit.core.review.Reviewing` with that shadow's gate,
  and the place the one edge that answers it posts its prompts, `ServedEdge.reviewsAt`),
  the recipe that shapes each turn by what it answers (`grit.core.recipe.TurnRecipe`,
  ADR 0025), the persona grit presents as (`grit.core.persona.Persona`, ADR 0026), its
  visibility (`grit.core.visibility.Visibility`, ADR 0030): who may see what, injected
  into core, with `Requirer` naming what else in it names a compartment (a plugin, an edge,
  a declared schedule's clearance), and its identities (`grit.core.identity.Identities`,
  ADR 0032): who it says is whom, and which edge it trusts to vouch for each realm. Beside
  its edges it declares, by core's links, which conversations work in a service an edge hosts (`WorksIn`) and which
  conversations' addressed turns also reach one (`Reaches`). Imports nothing in kit.
- **`environment`** — what the process environment supplies, never declared: `DotEnv` (a
  `.env` file under the real environment) and `Secrets` (the database, OpenRouter's key, and
  Jev's settings for `Topics.Jev`). An edge's credentials are its own `needs`. ← `deployment`
- **`run`** — running a deployment: `Kit.serve` (the engine and every edge, delivered to
  every `Kit.DeliverEvery` until the process is stopped, and a declared review's messages
  picked every `Kit.PickEvery`), `Kit.catchUp` (an edge's
  `CatchUp` heard once, estimated and agreed to first, then swept until nothing is left to
  close) and `Kit.launch` (the engine's workflows, for grit's own chat), each failing as a
  `KitFailure`; `Launch`, the workflows launched the same way by every way grit runs, with the
  deployment's declared schedules reconciled at its start and, where it sweeps, grit's clock
  edge (`grit.job.clock.ClockEdge`) starting what they have waiting (`Launch.asking`, the
  same with its models, classifiers and the jobs' clock given, is how a live test counts
  calls and sets the time);
  `Serving`, the edges opened, delivered to and closed; `Picking`, a review's pick round,
  of the messages its place may receive;
  `CatchingUp` and `Estimate`, a
  catch-up's flow and its bound; `KeptModelSettings`. ← `deployment`, `environment`

No source file sits at the module's root, and the test tree mirrors it.
