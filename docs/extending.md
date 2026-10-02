# Extending grit

Where code goes, what a deployment can add to grit, and how a module is added.

## Where code goes

[ADR 0021](decisions/0021-grit-owns-six-semantics-and-a-deployment-is-a-value-built-against-its-kit.md)
places code by three questions, asked in order: does the thesis fail without it; does it
name a generic protocol, tool or service and no organisation; does it name an
organisation's systems, data or people. The repository's top-level folders are the answers.

| Folder | Kind | The question |
|---|---|---|
| `core/` | core | the thesis fails without it |
| `extensions/` | shipped extension | it names a generic protocol, tool or service, and no organisation |
| `kit/` | kit | a deployment is built against it |
| `deployments/` | deployment | it names an organisation's systems, data or people, or is one deployment's wiring: `deployments/app` is the reference, grit's own chat, `grit serve` and `grit backfill`; a deployment's own module lives outside grit |
| `eval/` | none | it measures grit: `eval/it`, the assembly eval, an `it`-only harness the law leaves unruled; `eval/harness`, the eval harness, which names core and the models extension alone |

`core/edge` is core: it is the edge's half of ADR 0017's routing and names no protocol.
`core/prose` is core: ADR 0007's edge-neutral form.

A module's folder is `<group>/<name>`, or the group alone for `kit/` and `grit.eval`
(`eval/harness`, inside it, is `grit.eval.harness`). Its Mill module, its package and its published artifact are all
`grit.<name>` (`grit-<name>_3`): `extensions/slack` is `./mill grit.slack`, package
`grit.slack`, artifact `grit-slack_3`. `build.mill`'s `Placed` sets the folder.

## Extension points

A shipped extension implements core's traits. A deployment's own code, outside grit, can
supply two of them as values in `Deployment.of` today: a `ServedEdge` or `CatchUp`, and a
`Plugin`. The rest it chooses among grit's shipped implementations, by the kit's enums.

| Trait (package) | Shipped | A deployment's own? |
|---|---|---|
| `ServedEdge`, `CatchUp` (`grit.core.edge`) | `SlackEdge.serving`, `SlackEdge.backfill`, `McpEdge.serving` | yes: `Deployment.of(edges = …)`, `Kit.catchUp` |
| `Plugin` (`grit.core.plugin`) | `Digest` | yes: `Deployment.of(plugins = …)` |
| `Tool`, `Hosted` (`grit.core.tool`); `Tools` (`grit.edge`, what an edge's `Server` runs) | `Coding`, `Tuning`, `Probes`, `About`; Slack's `slack_post`; an MCP server's tools | through an edge that serves them at a place (ADR 0017); the tools every turn is offered are the kit's `Offered`, chosen, not supplied |
| `Provider` (`grit.core.provider`) | `OpenRouterProvider`, `StubProvider` | no: the kit builds one from `Secrets` |
| `Classifier` (`grit.core.classify`) | `JevClassifier`, `StubClassifier` | no: the kit's `Topics` chooses |
| `ContextAssembler` (`grit.core.context`), `TokenEstimator` (`grit.core.provider`) | `LinearAssembler`, `RetrievalAssembler`, `CharEstimate` | no: the kit's `Assembly` chooses; the assemblers are core (`core/assembly`), so a new one is a change to core |
| `Workspace`, `Edits`, `Shell`, `Instructions` (`grit.core.host`) | `grit.host`'s `Local*` | no: the kit and `grit.app` use `grit.host` |

So a generic provider, classifier or host is a shipped extension and a case in the kit's
choice. A deployment supplying its own `Provider`, `Classifier`, `ContextAssembler` or host
capabilities as values, as it does edges and plugins, is parked.

**A worked example: `Digest`.** `extensions/digest` is a shipped extension that names core
alone. `Digest` implements `Plugin`: the engine posts it every closed period, and its `post`
keeps one line per period in its documents, in the transaction that moves its cursor. A
deployment turns it on as a value, `Deployment.of(plugins = Vector(new Digest(name)), …)`;
the reference deployment does so for `GRIT_PLUGINS=digest`. Reading those lines back is the
kit's part: with a `Digest` among the plugins, every turn is offered its `recent_activity`
tool. A deployment's own plugin is posted the same way; the kit offers no tool over its
documents.

## What an extension may import

An extension names core alone; core includes `grit.prose` and `grit.edge`, which is how
`grit.slack` and `grit.mcp` name them. A Java library lives only in the extension whose job
needs it ([STYLE.md](../STYLE.md), rule 8). Only the kit and a deployment name an
extension, and the kit names no edge extension, no terminal UI and no deployment: an edge
reaches it as a `ServedEdge` value. Mill's `moduleDeps` hold each of these on the
classpath, and the law ([`enola-intent.yaml`](../enola-intent.yaml), its rules 1 and 4)
holds them on imports.

## Adding a module to grit

1. Its folder, by the questions above.
2. In `build.mill`, its object `with Placed`, with `folder` set; also `GritPublished` if a
   deployment outside grit compiles against it.
3. A README opening with the traits it implements, as this page's table names them.
4. In `enola-intent.yaml`: its folder in every `outside-*` component but the one for the
   library it quarantines, if any; its source tree as a component; and a
   `<name>-names-core-alone` rule. Then `scripts/enola-plant`, which must report every rule
   caught. It plants each quarantine breach in core alone, so it cannot see a folder
   missing from an `outside-*` component: check those lists by eye.
5. What reaches it: a case in the kit's choice, or a value a deployment passes.

## Outside grit

`./mill __.publishLocal` publishes every `GritPublished` module to the local ivy repository
as `dev.grit::grit-<name>:0.1.0-SNAPSHOT`; `grit.tui`, `grit.app` and `grit.eval` are never
published. A deployment is a module of its own, on Scala 3.9.0 or later (to read grit's
TASTy), depending on `grit-kit` and the extensions it names. Code that only calls grit
compiles without capture or separation checking.

```scala
package build
import mill.*, scalalib.*

object `package` extends ScalaModule {
  def scalaVersion = "3.9.0"
  def mvnDeps = Seq(
    mvn"dev.grit::grit-kit:0.1.0-SNAPSHOT",
    mvn"dev.grit::grit-slack:0.1.0-SNAPSHOT"
  )
}
```

It names `grit.kit.*`, core's types and each extension's entry object (`SlackEdge`,
`McpEdge`, `Digest`), and runs its `Deployment` with `Kit.serve` or `Kit.catchUp`
([`kit/README.md`](../kit/README.md)).
