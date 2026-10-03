# 0025. Triage's answers shape a turn's offering and width, through a recipe its deployment declares

Status: accepted (2026-10-03); revised (2026-10-03): a message said to grit is weighed in
its turn when the recipe reads its answers

Context: a heard message's turn was offered every tool its conversation linked, at the
window width the deployment assembled, whatever triage had found. Triage already asks, per
knowledge source, whether that source could supply what the message asks
(`Tags.V2.source`), and a measured corpus showed heard turns paying for the schemas of
tools whose every source read low, which those turns did not call. Where the decision lives
was a choice:

- **In triage**, passing a verdict to the turn it starts. Turned down: the turn is what a
  recipe shapes, the harness recomputes a recipe's decision from recorded data, and a
  verdict computed in triage would have to be stored where the turn and the harness both
  read it.
- **A deployment's code deciding per turn** (a function over the turn). Turned down: a
  function is opaque to the harness and to `Deployment.of`, which can then check nothing.
- **Asking the classifier again in the turn** for every root. Turned down for a heard
  message, whose answers are already kept; a message said to grit has none, so it is asked
  in the turn, and only when its recipe reads the answers (revised 2026-10-03).

Decision: a deployment declares a `TurnRecipe` (`grit.core.recipe`), data only: for a turn
rooted on a heard message, by the focus it was said at, and for one rooted on a message said
to grit, a `Shaping`: the window's `Width` and its `Offering`, every tool or a service's only
when one of the knowledge sources supplying it (`KnowledgeSource.supplies`) reads at least a
threshold. The turn records what it was weighed with in its `weigh` step (the tags triage
kept for a heard root; for a message said to grit, when the recipe's addressed offering gates
a service the conversation links by a source covering its place, live triage's set asked of
it through `grit.core.triage.Weighing`, its call's cost in the usage ledger under the `weigh`
role; none otherwise), never failing for it, and its `offer` step decides
each service once through `Offering.decide` and records the decision as its `TurnShape`:
the width, the tool set before anything was withheld, and each service's sources, tools and
verdict. A service is withheld only when its gate fails; unread or unweighed, it is offered.
The `weigh` step runs on every turn whatever the recipe, so a turn recovered under another
configuration replays the same steps. `TurnRecipe.Shipped`, every tool at the deployed
width, is the default and changes nothing. `Deployment.of` refuses a recipe whose gates read
what live triage does not ask, a source supplying a service no link offers, and a width
beyond the assembly's window until a model's context is known.

Consequences: the harness costs a variant exactly from a turn's recorded shape, by the same
`Offering.decide` the turn ran. A recipe is never a function, so a shaping that depends on
something else the turn knows needs a new case of data. Withholding by answers makes a
triage mistake a missing tool on that turn. Enforced by `TurnReplayTests` (histories with a
service withheld and turns in flight across the step's patch), `TurnOfferTests`,
`TurnWeighTests` and `DeploymentTests`.
