# 0025. Triage's answers shape a turn's offering and width through a declared recipe, and a message said to grit is weighed in its own turn

Status: accepted (2026-10-03); revised (2026-10-16)

Context: a heard message's turn was offered every tool its conversation linked, at the
window width the deployment assembled, whatever triage had found. Triage already asks, per
knowledge source, whether that source could supply what the message asks, and heard turns
paid for the schemas of tools whose every source read low and which they did not call.
Where the decision lives was a choice:

- **In triage**, passing a verdict to the turn it starts. Turned down: the turn is what a
  recipe shapes, the harness recomputes a recipe's decision from recorded data, and a
  verdict computed in triage would have to be stored where both read it.
- **A deployment's code deciding per turn** (a function over the turn). Turned down: a
  function is opaque to the harness and to `Deployment.of`, which can then check nothing.
- **For a message said to grit, a triage workflow of its own.** Turned down: it would put a
  second workflow and a cross-workflow wait in every mention's path, and keep a row that
  speech, shadows and reviews would each have to skip.

Decision:

- **A deployment declares a `TurnRecipe`** (`grit.core.recipe`), data only: for a turn rooted
  on a heard message, by the focus it was said at, and for one rooted on a message said to
  grit, a `Shaping`: the window's `Width` and its `Offering`, every tool or a service's only
  when one of the knowledge sources supplying it (`KnowledgeSource.supplies`) reads at least
  a threshold. `TurnRecipe.Shipped`, every tool at the deployed width, is the default.
- **The turn's `weigh` step records what it was weighed with**: the tags triage kept, for a
  heard root; for a message said to grit, only when the recipe's addressed offering gates a
  service the conversation links by a source covering its place, live triage's set asked in
  the turn (`grit.core.triage.Weighing`); none otherwise. A mention is weighed on the state
  `TriageInput.read` builds as for a heard message, after its opening's placement (waited for
  at most `Mentions.PlacedWithin`), and asked once, waiting at most `Mentions.AskWithin`. Its
  answers are the step's output and its call a usage-ledger row of the turn; no tags are
  kept, so speech, earning, shadows and reviews never see a mention. A placement or
  classifier that fails or is late leaves it unweighed, recorded by kind alone. It adds one
  classifier call before the turn's first model call. The step never fails the turn, and
  runs on every turn whatever the recipe, so a turn recovered under another configuration
  replays the same steps.
- **The `offer` step decides each service once** through `Offering.decide` and records the
  decision as its `TurnShape`: the width, the tool set before anything was withheld, and each
  service's sources, tools and verdict. A service is withheld only when its gate fails;
  unread or unweighed, it is offered.
- **`Deployment.of` refuses** a recipe whose gates read what live triage does not ask
  (`RecipeUnread`), a source supplying a service no link offers (`OffersUnlinked`), and a
  width beyond the assembly's window (`Widens`).

Consequences: the harness costs a variant exactly from a turn's recorded shape, by the same
`Offering.decide` the turn ran. A recipe is never a function, so a shaping that depends on
something else the turn knows needs a new case of data. Withholding by answers makes a
triage mistake a missing tool on that turn. Enforced by the replay gate (`TurnReplayTests`,
every recorded weigh output written back byte for byte) and `DeploymentTests`.
