package grit.eval.harness.reply

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.context.Width
import grit.core.id.QuestionName
import grit.core.message.Tokens
import grit.core.period.Probability
import grit.core.place.Service
import grit.core.recipe.{ByFocus, Offering, Rooted, ServiceOffer, Shaping, TurnRecipe}
import grit.core.tool.{ToolName, ToolSet}
import grit.core.triage.KnowledgeSources
import grit.eval.harness.corpus.TurnCase
import grit.turn.TurnOffer

/** A named way to build a turn other than as shipped: a recipe, applied to a recorded turn as
  * a deployment declaring it would apply it.
  */
final case class TurnVariant(name: String, recipe: TurnRecipe)

/** What a recorded turn is given under a variant: the `tools` it is offered, in the order
  * recorded; each service it took tools from as the variant's offering decided it
  * (`services`: its workspace's first, then those it reached, by name); and the
  * `width` its window is drawn at.
  */
final case class Shaped(tools: Vector[ToolSet.Entry], services: Vector[ServiceOffer], width: Width)

object TurnVariant {

  /** The turn as recorded. */
  val Shipped: TurnVariant = TurnVariant("shipped", TurnRecipe.Shipped)

  /** The variants a run can name, over a deployment whose windows are built as `assembled`
    * says: [[Shipped]]; `wide-heard` and `wide-addressed`, twice the window on that root's
    * turns, each search ranking as many hits as deployed; `offer-0.2`, `offer-0.3` and
    * `offer-0.5`, a service's tools offered on every turn only when one of the knowledge
    * sources supplying it reads at least that ([[Offering.BySource]]); `offer-0.3-0.1`, at 0.3
    * on a heard turn and 0.1 on one said to grit; and `offer-heard-0.2`, at 0.2 on a heard
    * turn and every tool on one said to grit.
    */
  def all(assembled: Assembled): Vector[TurnVariant] = {
    val deployed = TurnRecipe.Shipped.addressed
    val wide = deployed.copy(width =
      Width.Within(Tokens(Tokens.value(assembled.window) * 2), assembled.hits)
    )
    def by(at: Double) = deployed.copy(offering = Offering.BySource(Probability.clamped(at)))
    def recipe(heard: Shaping, addressed: Shaping) = TurnRecipe(ByFocus.both(heard), addressed)
    Vector(
      Shipped,
      TurnVariant("wide-heard", recipe(wide, deployed)),
      TurnVariant("wide-addressed", recipe(deployed, wide))
    ) ++ Vector(0.2, 0.3, 0.5).map(at => TurnVariant(s"offer-$at", recipe(by(at), by(at)))) ++
      Vector(
        TurnVariant("offer-0.3-0.1", recipe(by(0.3), by(0.1))),
        TurnVariant("offer-heard-0.2", recipe(by(0.2), deployed))
      )
  }

  /** The variant of [[all]] named `name`, or why not, naming every variant there is. */
  def named(name: String, assembled: Assembled): Either[String, TurnVariant] = {
    val each = all(assembled)
    each.find(_.name == name).toRight(s"no variant $name: ${each.map(_.name).mkString(", ")}")
  }

  /** What `t` answers, as a recipe tells it apart. */
  def rooted(t: TurnCase): Rooted = t.root match {
    case TurnOffer.Root.Heard => Rooted.Heard(t.focus)
    case TurnOffer.Root.Addressed => Rooted.Addressed
  }

  /** What a turn rooted as `rooted` is given under `v`: what it recorded it was offered
    * (`offer`, `None` when it recorded none, and its tool set `set`) less the tools of each
    * service `v`'s offering withholds ([[Offering.decide]]) by the root's `answers` (`None`:
    * it has none) and the services `knowledge` says its sources supply; and its width.
    */
  def shape(
      v: TurnVariant,
      rooted: Rooted,
      offer: Option[TurnOffer.Recorded],
      set: Vector[ToolSet.Entry],
      answers: Option[VectorMap[QuestionName, Answer]],
      knowledge: KnowledgeSources
  ): Shaped = {
    val shaping = v.recipe.at(rooted)
    val services = offer.toVector.flatMap(o =>
      (o.workspace.flatMap(_.service).toVector ++
        o.reached.values.flatMap(_.service).toVector.sortBy(_.name)).distinct
    )
    val decided = Offering.decide(shaping.offering, knowledge.supplied, services, answers)
    val withheld = decided
      .filter(_.withheld)
      .flatMap(s => offer.toVector.flatMap(toolsOf(_, s.service)))
      .toSet
    Shaped(set.filterNot(e => withheld.contains(e.name)), decided, shaping.width)
  }

  /** The tools `offer` took from `service`'s edge's advert: its workspace's, when the service
    * is its workspace, and those reached there.
    */
  def toolsOf(offer: TurnOffer.Recorded, service: Service): Set[ToolName] =
    (if (offer.workspace.contains(service.place)) offer.advertised.toSet else Set.empty) ++
      offer.reached.collect { case (n, p) if p == service.place => n }
}
