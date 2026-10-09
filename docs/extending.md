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
supply these as values in `Deployment.of` today: a `ServedEdge` or `CatchUp` (with, inside an
edge that attests, the `RealmSource` of the realm it speaks for), a `Plugin`, a `PlainJob` with the
schedules that run it, and the `Labeller` of its rooms; beside them it declares, as data, its
`Visibility` and its `Identities` (below). The rest it chooses among grit's
shipped implementations, by the kit's enums.

| Trait (package) | Shipped | A deployment's own? |
|---|---|---|
| `ServedEdge`, `CatchUp` (`grit.core.edge`) | `SlackEdge.serving`, `SlackEdge.backfill`, `McpEdge.serving` | yes: `Deployment.of(edges = …)`, `Kit.catchUp` |
| `Plugin` (`grit.core.plugin`), with its `Documents` (or `CachePosting`), `PluginTool`s and an `Exports` service | `Digest` | yes: `Deployment.of(plugins = …)` |
| `PlainJob`, `Declared` (`grit.core.job`) | `Reminders`' `remind` | yes: a plugin's `jobs` and `schedules`, or `Deployment.of(jobs = …, schedules = …)` |
| `KeepingJob` (`grit.core.job`), a plugin's job that keeps its documents | — | yes: a plugin's `jobs`, its plugin declaring `documents` |
| `Labeller` (`grit.core.visibility`) | `RoomLabels` | yes: its `Visibility`'s `rooms` (below) |
| `RealmSource` (`grit.core.edge`) | the Slack edge's, of its own workspace | yes: inside an edge that names its `attester`, which its `Identities` trusts (below) |
| `Tool`, `Hosted` (`grit.core.tool`); `Tools` (`grit.edge`, what an edge's `Server` runs) | `Coding`, `Tuning`, `Probes`, `About`, `Cleared`; Slack's `slack_post`; an MCP server's tools | through an edge that serves them at a place (ADR 0017), or as a plugin's `PluginTool`, which the turn runs itself over grit's store; grit's own are the kit's `Offered`, chosen, not supplied |
| `Provider` (`grit.core.provider`) | `OpenRouterProvider`, `StubProvider` | no: the kit builds one from `Secrets` |
| `Classifier` (`grit.core.classify`) | `JevClassifier`, `StubClassifier` | no: the kit's `Topics` chooses |
| `ContextAssembler` (`grit.core.context`), `TokenEstimator` (`grit.core.provider`) | `LinearAssembler`, `RetrievalAssembler`, `CharEstimate` | no: the kit's `Assembly` chooses; the assemblers are core (`core/assembly`), so a new one is a change to core |
| `Workspace`, `Edits`, `Shell`, `Instructions` (`grit.core.host`) | `grit.host`'s `Local*` | no: the kit and `grit.app` use `grit.host` |

So a generic provider, classifier or host is a shipped extension and a case in the kit's
choice. A deployment supplying its own `Provider`, `Classifier`, `ContextAssembler` or host
capabilities as values, as it does edges and plugins, is parked.

**A worked example: `Digest`.** `extensions/digest` is a shipped plugin extension that names
core alone. `Digest` implements `Plugin` (ADR 0027), a pure bundle of contributions: its
`DocumentPosting` keeps one document per room where conversations closed and per label
they were created at, at that room, holding its newest closings' lines, written in the
transaction that moves its cursor (ADR 0028). Retrieval draws a room's document into a window
whose scope reaches the room and whose turn reads it, ranked beside entries under the terms
Digest declares (its label, weight, retention and bound). It exports `Activity`, its lines
read back, and its tool, `recent_activity`, reads through it for the turn that called it. A
deployment turns it on as a value,
`Deployment.of(plugins = Vector(new Digest(name)), …)`; the reference deployment does so for
`GRIT_PLUGINS=digest`. The kit names no plugin: it posts every plugin and offers every
plugin's tools the same way, each bound when the engine starts over the plugin's own
documents. A plugin that reads another takes it in its constructor, lists it in `needs`, and
reads it only through the service it `Exports`. `Deployment.of` refuses two plugins of one
name, a need no plugin of its name meets, a tool name taken twice (grit's own included), and a
tool asking for a plugin its own does not need. A deployment's own plugin can contribute
anything a shipped one can.

**Jobs and schedules** ([ADR 0029](decisions/0029-a-job-is-versioned-code-a-schedule-is-data-and-a-clock-edge-starts-each-due-slot.md)).
A `Job[P]` is versioned code: its `name`, its `version`, a codec for its parameters `P`
(`write`, `read`), its `limits` (how many asks and calls one run may make, none unless it
says), and `run`, which makes its moves and returns the text the run replies with. A schedule runs a job
on a `SlotRule`: `Once` at an instant with a `Grace` (later than that, the slot is missed and
never run), or `Daily`, `Weekdays` or `Weekly` at a local time in a zone (after downtime only
the latest missed slot runs). Each slot's run is a turn of that slot's own conversation, at
the task place of its job, closed without a model call. Schedules come from two sources:

- **Declared.** A plugin's `schedules` or the deployment's own, each a `Declared` (a key, a job
  of the same plugin or deployment, a rule, its parameters), run for grit and kept
  (`Report.Kept`): the run's reply stays in its own conversation. Every start makes the stored
  declared schedules exactly the declared ones: a new one is written, a changed one rewritten,
  one no longer declared ended, one declared again revived.
- **Asked.** A plugin's `PluginTool` books its own plugin's jobs when bound
  (`bind(own, needs, jobs)`, `jobs.of(job)`, a `Booking`) and writes once-only schedules
  through the `ScheduleDesk` its run is handed per call: `ask`, `pending`, `cancel`. The desk
  takes who asked and where the reply goes from the call's turn, so a run reports at the asking
  turn's destination (`Report.Posted`) and is posted by that turn's edge; a conversation whose
  replies nobody posts cannot ask (`Unaddressed`). Reconciliation never touches an asked
  schedule. `extensions/remind` is the worked example.

A job's version moves when what its runs do changes: a run started under another version
is superseded, and its slot runs again at the current one when no later slot is due. What
`Deployment.of` refuses of jobs and schedules is in its doc, beside every other refusal.

**A job's moves** ([ADR 0034](decisions/0034-turns-and-jobs-act-through-three-moves-ask-call-and-keep-made-under-an-acting-value.md)).
A job is pure: everything its run does outside itself it does through the moves it is handed,
from `grit.core.act`, each under a `MoveName` used once per run. A `PlainJob`, the deployment's
or a plugin's, is handed `Moves`: `ask`, one reply to what it poses (a `Posed.Text` request,
answered in words by the catalog's summary model), admitted against the day's cap and its cost recorded under the run's conversation; and `call`,
one tool request to an edge serving a service place, never one that asks a person first, its
request recording the schedule's principal. A plugin's `KeepingJob` is handed `Keeping`, those
and `keep`: one transaction over its plugin's own documents (a `DocumentKeeper`, at the run's
floor), kept whole or not at all, whose pure result is recorded as the step's output in its own
`Journaled` form. A keeping job needs its plugin to declare `documents` (their `terms`, and a
`DocumentPosting` only if closed periods should also write them); `Deployment.of` refuses one
whose plugin declares none (`KeepsUnshelved`). A rerun of a run whose move's input differs from
what it recorded is refused that move and every later one (`MoveError.Diverged`), so change a
job's moves under a new `version`. `core/act` (`grit.act`) is grit's own module that makes
these moves as durable steps, shared with the turn; a job or plugin never names it, only
`grit.core.act`'s traits.

**Visibility** ([ADR 0030](decisions/0030-visibility-is-a-label-lattice-read-down-write-up-and-a-room-admits-its-members-to-its-own-speech.md)).
Who may see what is the one value a deployment injects into core about it,
`Deployment.of(visibility = …)`, a `Visibility` built by `Visibility.of`, which refuses a
mistake in it; left out, it is `Visibility.Shipped`, under which every label is public. Each
part is declared data or a pure function:

- `compartments`: the named areas (a team, a client, a project) its labels may hold, beside
  core's fixed levels; `unmapped` is always among them.
- `rooms`: a `Labeller[Room]`, the label each room takes when none is set for it through
  grit ([ADR 0033](decisions/0033-an-edge-hears-where-its-bot-is-a-member-and-a-rooms-label-is-set-through-grit-by-one-rule-in-core.md)).
  A `Room` is its place and the access its edge reports: `Open` (a public channel) or
  `Invited` (a private one). `RoomLabels.of(declared, otherwise, open)` is the declared
  table: the label declared at the room's own place; else, for a reported access, `open` for
  an open room and `unmapped` for an invited one, which fails high until its members label
  it; else the longest declared place it is within, else `otherwise`. A deployment may write
  its own pure function. A label it returns holding a compartment not declared is kept at
  `unmapped` instead, so what a mapping invents is read by fewer people, never more.
- `administrators`: the declared group whose declared members may make the changes only an
  administrator may (`grit.core.admin.Authority.decide` lists them; relabelling a public
  room and lowering a level among them), and read another person's clearance. With none, no
  one may. `Visibility.of` refuses a compartment's own group here, since people cleared for
  it through grit join it.
- `stewards`: per compartment, a `Steward(compartment, group)` whose declared members may
  remove the compartment from a private room's label, give a private room a first label whose
  compartments they all steward, and clear people for it or remove them from it. The group
  is the compartment's own or one with no compartment of its own. People added through grit
  are never administrators or stewards.
- `groups` and `grants`: people grouped by the accounts sources know them by
  (`slack:{team}/{user}`), and by the realms whose full members a group takes in (a
  `Realm`, such as every `slack:{team}/` account, as the attester `identities` trusts for it
  attests them); and what each group's members are cleared for: a person is cleared for the
  join of the grants of every group any of their accounts puts them in.
- `trusts`: what each outside service (`service:{name}`) is trusted with, as a label
  ([ADR 0031](decisions/0031-a-write-out-of-grit-names-its-place-and-a-service-is-a-place-and-a-party.md));
  public for a service it does not name. It is the most a turn may send the service as a
  call's arguments. What the service contains is another label, its place's, given by
  `rooms` as any room's. Grants clear people, trusts clear services, and rooms label content.

Every other part of a deployment that names a compartment declares it, and `Deployment.of`
refuses one the visibility does not declare: a plugin's `compartments`, an edge's
`compartments`, and a declared schedule's `clearance`.

A deployment names no room an edge hears: an edge hears where its bot is a member, and the
room's members label it by command, under one rule in core (`grit.core.admin.Authority`),
each change kept with an audit row. Who may label what, and the setup tiers a deployment
chooses among: [`deployments/app/README.md`](../deployments/app/README.md).

What leaves grit is written to a place, and a turn writes only to a place whose label is set
through grit or that `rooms` maps explicitly, never a quiet one, at a label dominating its
room's ([ADR 0031](decisions/0031-a-write-out-of-grit-names-its-place-and-a-service-is-a-place-and-a-party.md)):
a writing tool's destinations, such as Slack's channels for `slack_post`
(`slack:{team}/{channel id}`), and a review's place, where only messages whose room's label
it dominates are picked and prompted. So a place a deployment posts to that its members do
not label and its access does not map, such as a review's, needs a declared label; an
unmapped one is written to by no one.

A conversation takes its room's label when it is created and keeps it, whatever the
deployment declares later; a job's run takes its schedule's. A direct message is the
exception ([ADR 0032](decisions/0032-a-principal-is-a-person-edges-name-accounts-and-trusted-realms-attest-who-they-are.md)):
its room, `direct:{namespace}/{name}`, is one person's, spelled by their account; `rooms`
may not label it (`Visibility.of` refuses a room declared within `direct`). Its conversation
is created at that person's clearance, read at most at their clearance now, and what is said
in it is read only there; a thread begun when they were cleared for more takes no new message. A database remembers the
compartments each start declared: a start may add some, but one that drops or renames a
compartment the database ran under does not open, and says which, since a label
holding it could then be read by a clearance that could not read it before.

**Identities** ([ADR 0032](decisions/0032-a-principal-is-a-person-edges-name-accounts-and-trusted-realms-attest-who-they-are.md)).
A deployment declares no people. It says which sources it trusts to say who their accounts
are, and grit keeps who is whom from what they say. This is injected beside its visibility,
`Deployment.of(identities = …)`, an `Identities` built by `Identities.of`, which refuses a
realm trusted to two attesters (`IdentityRefusal`); left out, it is `Identities.Shipped`: no
realm trusted and no domain claimed, so every account is a person of its own. Both parts are
declared data:

- `vouchings`: for each `Realm` (one source's accounts, such as one Slack workspace's), the
  one attester (`AttesterName`) trusted to say what its source says of them: whether each is
  a full member, and the email the source verified for one. An attester is a source of who a
  realm's accounts are; today each is an edge served that says it is one
  (`ServedEdge.attester`). Trusting a realm trusts its administrators, who decide both.
  `Deployment.of` refuses an attester no edge served is, two edges saying they are one
  attester, and a group of the visibility naming a realm no attester is trusted for.

  An extension that attests a realm names its attester (`ServedEdge.attester`,
  `CatchUp.attester`) and implements `RealmSource`: `ask`, what its source says of one
  account now, and `all`, the same of every account of a realm in one whole listing, either
  `Unreached` when the source could not be asked. It decides nothing else. Its stores' `Attesting`
  holds core's rules: it is called before each message the edge records (`before`, which
  refuses an account its source never answered for while it cannot be reached), on the
  source's word of a change (`changed`), and from the edge's `Open.attest`, which the kit
  calls every `Attesting.Every` (`round`); when to ask, what a failure keeps, and which
  addresses count are core's.

  The Slack edge is the attester `slack` (`SlackAccounts.Attester`) for its own workspace,
  the realm `SlackAccounts.realm(team)`, every `slack:{team}/` account; a user of a workspace
  sharing a channel is spelled in their own team, so is in no realm trusted for this one. A
  deployment trusts it with `Vouching(SlackAccounts.Attester, SlackAccounts.realm(team))`,
  and writes the workspace's full members as a group naming that realm. The scopes and
  events its Slack app needs for that are in [`extensions/slack/README.md`](../extensions/slack/README.md).
  The reference deployment trusts it for the workspace its bot token is installed in
  (`SlackEdge.installedIn`).
- `domains`: the email domains (`Domain`, each matched exactly, so a subdomain is its own)
  the deployment claims. Accounts a trusted realm attests one address in a claimed domain
  are one person, across realms; an address in any other domain is not kept. With none
  claimed, nothing links by email, and a realm's word that an account is a full member still
  counts.

**A plugin is parametric in labels.** A `Label` is opaque: a plugin compares labels
(`dominates`, equality), combines them (`join`, `meet`) and passes one to core as a key, and
never takes one apart. Opacity buys independence from how labels are stored, not secrecy: a
plugin can test any compartment it can name. What a transaction may read will be offered
to a plugin as a label, for choosing which variant of its own derived data to serve; that
API is still to come. A plugin never enforces visibility: ADR 0030 makes that core's, which
filters what a transaction reads and floors what it writes, so a plugin that chooses wrongly
is served nothing it may not read.

## What an extension may import

An extension names core alone; core includes `grit.prose` and `grit.edge`, which is how
`grit.slack` and `grit.mcp` name them. A plugin extension may also name plugin extensions,
since a plugin that reads another takes it in its constructor; it never names the kit, an
edge extension or a deployment. A Java library lives only in the extension whose job needs
it ([STYLE.md](../STYLE.md), rule 8). Only the kit, a deployment and a plugin extension
name an extension, and the kit names no plugin, no edge extension, no terminal UI and no
deployment: a plugin reaches it as a `Plugin` value and an edge as a `ServedEdge` value. Mill's `moduleDeps` hold each of these on the
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
`McpEdge`, `Digest`, `Reminders`), and runs its `Deployment` with `Kit.serve` or `Kit.catchUp`
([`kit/README.md`](../kit/README.md)).
