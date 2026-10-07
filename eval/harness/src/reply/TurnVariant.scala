package grit.eval.harness.reply

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.context.Width
import grit.core.id.QuestionName
import grit.core.message.Tokens
import grit.core.period.Probability
import grit.core.place.Service
import grit.core.recipe.{ByFocus, Offering, Rooted, Shaping, TurnRecipe}
import grit.core.tool.ToolName
import grit.eval.harness.capture.TurnCase
import grit.turn.{TurnOffer, TurnShape}

/** A named way to build a turn other than as shipped: a recipe, applied to a recorded turn as
  * a deployment declaring it would apply it.
  */
final case class TurnVariant(name: String, recipe: TurnRecipe)

/** What a recorded turn is given under a variant: how its tools are offered, and the `width`
  * its window is drawn at.
  */
final case class Shaped(offering: Shaped.Offering, width: Width) {

  /** Whether it is offered `name`, a tool of the set it draws from
    * ([[grit.eval.harness.capture.Offered.drawn]]).
    */
  def offers(name: ToolName): Boolean = offering match {
    case Shaped.Offering.AsRecorded(tools) => tools.contains(name)
    case Shaped.Offering.Decided(services) =>
      !services.exists(t => t.offer.withheld && t.tools.contains(name))
  }

  /** The services it is offered a tool of; `None` when its services were not recorded. */
  def services: Option[Set[Service]] = offering match {
    case Shaped.Offering.AsRecorded(_) => None
    case Shaped.Offering.Decided(services) =>
      Some(services.filter(t => !t.offer.withheld && t.tools.nonEmpty).map(_.offer.service).toSet)
  }
}

object Shaped {

  enum Offering {

    /** As it was offered, `tools`: a turn recorded before shapes, or with no offer, under every
      * variant.
      */
    case AsRecorded(tools: Vector[ToolName])

    /** Each service its shape recorded, in order, as the variant decided it, with the tools
      * the turn took from it: withheld or offered whole.
      */
    case Decided(services: Vector[TurnShape.Took])
  }
}

object TurnVariant {

  /** Every tool at the deployed width: the turn as recorded when its deployment declared no
    * recipe.
    */
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
    case TurnOffer.Root.Heard | TurnOffer.Root.Named => Rooted.Heard(t.focus)
    case TurnOffer.Root.Addressed | TurnOffer.Root.ByName => Rooted.Addressed
  }

  /** What `t` is given under `v`, by its root's `answers` (`None`: it has none;
    * [[TurnAnswers.of]]): a turn that recorded a shape is offered every tool of its whole set
    * but those of each service `v`'s offering withholds, decided by [[Offering.decide]] over
    * the services and supplying sources the shape recorded; any other turn is offered as
    * recorded. Its window is drawn at `v`'s width for its root either way.
    */
  def shape(
      v: TurnVariant,
      t: TurnCase,
      answers: Option[VectorMap[QuestionName, Answer]]
  ): Shaped = {
    val shaping = v.recipe.at(rooted(t))
    val offering = t.offered.flatMap(_.shape) match {
      case Some(recorded) => Shaped.Offering.Decided(decided(shaping.offering, recorded, answers))
      case None => Shaped.Offering.AsRecorded(t.offered.fold(Vector.empty)(_.tools))
    }
    Shaped(offering, shaping.width)
  }

  /** Whether `v`, by `answers`, decides each service `t`'s shape recorded as the turn decided
    * it; `None` when `t` recorded no shape. Given the answers the turn recorded, the variant
    * whose recipe it ran under decides every one as recorded.
    */
  def asRecorded(
      v: TurnVariant,
      t: TurnCase,
      answers: Option[VectorMap[QuestionName, Answer]]
  ): Option[Boolean] =
    t.offered
      .flatMap(_.shape)
      .map(recorded =>
        decided(v.recipe.at(rooted(t)).offering, recorded, answers).map(_.offer) ==
          recorded.services.map(_.offer)
      )

  /** Each service `shape` recorded, in order, as `offering` decides it by `answers`. */
  private def decided(
      offering: Offering,
      shape: TurnShape,
      answers: Option[VectorMap[QuestionName, Answer]]
  ): Vector[TurnShape.Took] = {
    val supplied = VectorMap.from(shape.services.map(t => t.offer.service -> t.offer.sources))
    // decide gives one verdict per service, in order.
    shape.services
      .zip(Offering.decide(offering, supplied, shape.services.map(_.offer.service), answers))
      .map((took, offer) => took.copy(offer = offer))
  }
}
