# grit.kit

The kit a deployment is built against (ADR 0021): a deployment declares itself as a
`Deployment` value in a module of its own, and runs it with `Kit.serve` or `Kit.catchUp`.
The kit owns the DBOS application version: it opens every engine under `Turn.Epoch`, and a
deployment registers no workflow of its own. It reaches DBOS only through
`grit.dbos.engine.Engine`.

**Where code goes.** Three questions, in order, place every module:

| Kind | Modules | The question |
|---|---|---|
| core | `grit.core`, `grit.dbos`, `grit.turn`, `grit.lifecycle`, `grit.assembly`, `grit.prose`, `grit.edge` | the thesis fails without it |
| shipped extension | `grit.models`, `grit.host`, `grit.tools`, `grit.digest`, `grit.slack`, `grit.mcp`, `grit.tui` | it names a generic protocol, tool or service, and no organisation |
| kit | `grit.kit` | a deployment is built against it |
| deployment | `grit.app` (the reference: grit's own chat, `grit serve`, `grit backfill`), and a deployment's own module outside grit | it names an organisation's systems, data or people, or is one deployment's wiring |

`grit.edge` is core: it is the edge half of ADR 0017's routing and names no protocol.
`grit.prose` is core: ADR 0007's edge-neutral form. `grit.eval` is unruled: an `it`-only
harness. Only the kit and a deployment name an extension module; each extension names core
alone; the kit names no edge extension, no terminal UI and no deployment
(`enola-intent.yaml`). An edge reaches the kit as a `grit.core.edge.ServedEdge` value its
extension's entry object makes (`grit.slack`'s `SlackEdge.serving`), as a plugin reaches it
as a `Plugin`.

In dependency order:

- **`deployment`** — what a deployment declares: `Deployment` (built by `Deployment.of`,
  called with named arguments, which refuses tools that ask first beside an edge that cannot
  answer an ask, two edges of one name, and a sweep under a second), `Offer` and `Offered`
  (the tools every turn's model is offered), `Assembly` (how a window is assembled),
  `Topics` (how a message is placed among topics). Imports nothing in kit.
- **`environment`** — what the process environment supplies, never declared: `DotEnv` (a
  `.env` file under the real environment) and `Secrets` (the database, OpenRouter's key, and
  Jev's settings for `Topics.Jev`). An edge's credentials are its own `needs`. ← `deployment`
- **`run`** — running a deployment: `Kit.serve` (the engine and every edge, delivered to
  every `Kit.DeliverEvery` until the process is stopped), `Kit.catchUp` (an edge's
  `CatchUp` heard once, estimated and agreed to first, then swept until nothing is left to
  close) and `Kit.launch` (the engine's workflows, for grit's own chat), each failing as a
  `KitFailure`; `Launch`, the workflows launched the same way by every way grit runs;
  `Serving`, the edges opened, delivered to and closed; `CatchingUp` and `Estimate`, a
  catch-up's flow and its bound; `KeptModelSettings`. ← `deployment`, `environment`

A deployment outside grit compiles against the published artifacts (`./mill
__.publishLocal`); it names `grit.kit.*`, core's types and each extension's entry object.

No source file sits at the module's root, and the test tree mirrors it.
