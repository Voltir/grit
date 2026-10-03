package grit.eval.harness.reply

import scala.collection.immutable.VectorMap
import scala.util.Try

import grit.core.classify.Answer
import grit.core.context.Width
import grit.core.id.{KnowledgeSourceName, QuestionName}
import grit.core.message.Tokens
import grit.core.period.Probability
import grit.core.place.{Place, Service}
import grit.core.tool.ToolName
import grit.core.triage.{KnowledgeSource, KnowledgeSources}
import grit.eval.harness.corpus.Fields
import grit.turn.TurnOffer

/** How a variant sizes a turn's window against the deployment's ([[Assembled]]). */
enum Sizing {

  /** As the deployment draws it. */
  case Deployed

  /** Half its window budget, each search ranking as many hits as deployed. */
  case Half

  /** Twice its window budget, each search ranking as many hits as deployed. */
  case Twice
}

/** What a variant offers of a turn's tools. */
enum Offering {

  /** What the turn was offered. */
  case Shipped

  /** What the turn was offered less each service whose every knowledge source triage answered
    * below `at` ([[TurnVariant.withheld]]).
    */
  case Gated(at: Probability)
}

/** A deployment's knowledge sources, each with the service whose tools supply it, if any. grit
  * keeps no such link (a deployment declares its sources and its services apart), so a run is
  * given it.
  */
final case class Supplies private (
    sources: KnowledgeSources,
    services: Map[KnowledgeSourceName, Service]
)

object Supplies {

  /** No source. */
  val Empty: Supplies = Supplies(KnowledgeSources.Empty, Map.empty)

  /** The supplies `text` holds: `{"sources": [{"name", "line", "within", "service"}, …]}` in
    * the order declared, `within` a place as written ([[Place.read]]) and `service` a service's
    * name, or `null` for a source no service's tools supply. Why not, naming the source and
    * field, when it is not of that form or names a source twice.
    */
  def read(text: String): Either[String, Supplies] =
    for {
      root <- Try(ujson.read(text)).toOption.toRight("supplies: not JSON")
      all <- Fields("supplies", root).arr("sources")
      each <- Fields.each(all) { v =>
        val f = Fields("supplies: a source", v)
        for {
          name <- f.str("name").flatMap(KnowledgeSourceName.of)
          what = s"supplies: ${KnowledgeSourceName.value(name)}"
          line <- f.str("line")
          within <- f.str("within").flatMap(Place.read).left.map(w => s"$what: within: $w")
          service <- f
            .optional("service")
            .flatMap(Fields.opt(_)(s => Fields.str(s"$what: service", s).flatMap(Service.of)))
        } yield (KnowledgeSource(name, line, within), service)
      }
      sources <- KnowledgeSources
        .of(each.map(_._1))
        .left
        .map(n => s"supplies: ${KnowledgeSourceName.value(n)} is declared twice")
    } yield Supplies(sources, each.flatMap((s, service) => service.map(s.name -> _)).toMap)
}

/** A named way to build a turn other than as shipped: the window each kind of turn is drawn at,
  * and what it is offered of its tools.
  *
  * @param answered
  *   a heard turn's sizing in place of `heard` when its triage reads `open` (what its message
  *   asks is still unanswered) below one half; `None` for `heard`'s alone
  */
final case class TurnVariant(
    name: String,
    addressed: Sizing,
    heard: Sizing,
    answered: Option[Sizing],
    offering: Offering
)

object TurnVariant {

  /** The turn as recorded. */
  val Shipped: TurnVariant =
    TurnVariant("shipped", Sizing.Deployed, Sizing.Deployed, None, Offering.Shipped)

  /** The variants a run can name: [[Shipped]]; `narrow-when-answered`, half the window on a
    * heard turn whose triage reads its message answered; `wide-heard` and `wide-addressed`,
    * twice the window on that root's turns; and `offer-0.2`, `offer-0.3` and `offer-0.5`, a
    * service's tools offered only when triage answers one of its sources at least that.
    */
  val all: Vector[TurnVariant] = Vector(
    Shipped,
    Shipped.copy(name = "narrow-when-answered", answered = Some(Sizing.Half)),
    Shipped.copy(name = "wide-heard", heard = Sizing.Twice),
    Shipped.copy(name = "wide-addressed", addressed = Sizing.Twice)
  ) ++ Vector("0.2", "0.3", "0.5").map(at =>
    Shipped.copy(
      name = s"offer-$at",
      offering = Offering.Gated(Probability.clamped(at.toDouble))
    )
  )

  /** The variant of [[all]] named `name`. */
  def named(name: String): Option[TurnVariant] = all.find(_.name == name)

  /** Triage's question of whether what a message asks is still unanswered. */
  private val Open: Option[QuestionName] = QuestionName.of("open").toOption

  /** The prefix of triage's question asked once per knowledge source. */
  private val Source: Option[QuestionName] = QuestionName.of("source").toOption

  /** The width `v` draws a turn of `root` at, whose triage answered `answers` (empty when it
    * has none), the deployment's built as `assembled` says.
    */
  def width(
      v: TurnVariant,
      root: TurnOffer.Root,
      answers: VectorMap[QuestionName, Answer],
      assembled: Assembled
  ): Width = {
    val answered = Open.flatMap(answers.get).exists {
      case Answer.YesNo(yes) => yes < 0.5
      case Answer.Choice(_, _, _) => false
    }
    val sizing = root match {
      case TurnOffer.Root.Addressed => v.addressed
      case TurnOffer.Root.Heard => v.answered.filter(_ => answered).getOrElse(v.heard)
    }
    budget(sizing, assembled).fold(Width.Deployed)(Width.Within(_, assembled.hits))
  }

  /** The widest budget `v` draws any turn at over `limit`, the reply model's context in tokens;
    * `None` when none is over it.
    */
  def pastContext(v: TurnVariant, assembled: Assembled, limit: Tokens): Option[Tokens] =
    (Vector(v.addressed, v.heard) ++ v.answered)
      .map(s => budget(s, assembled).getOrElse(assembled.window))
      .filter(b => Tokens.value(b) > Tokens.value(limit))
      .maxByOption(Tokens.value)

  /** The services `v` withholds from a turn whose triage answered `answers`: under
    * [[Offering.Gated]], each service of `supplies` every source of which triage answered
    * below its line (a source it did not ask about holds its service offered); none under
    * [[Offering.Shipped]].
    */
  def withheld(
      v: TurnVariant,
      answers: VectorMap[QuestionName, Answer],
      supplies: Supplies
  ): Set[Service] =
    v.offering match {
      case Offering.Shipped => Set.empty
      case Offering.Gated(at) =>
        def below(source: KnowledgeSourceName) =
          Source.map(QuestionName.per(_, source)).flatMap(answers.get).exists {
            case Answer.YesNo(yes) => yes < Probability.value(at)
            case Answer.Choice(_, _, _) => false
          }
        val bySource =
          supplies.sources.all.flatMap(s => supplies.services.get(s.name).map(s.name -> _))
        bySource.map(_._2).toSet.filter(s => bySource.filter(_._2 == s).forall((n, _) => below(n)))
    }

  /** The tools `offer` took from `service`'s edge's advert: its workspace's, when the service
    * is its workspace, and those reached there.
    */
  def toolsOf(offer: TurnOffer.Recorded, service: Service): Set[ToolName] =
    (if (offer.workspace.contains(service.place)) offer.advertised.toSet else Set.empty) ++
      offer.reached.collect { case (n, p) if p == service.place => n }

  /** `sizing`'s budget over `assembled`'s; `None` for the deployment's own. */
  private def budget(sizing: Sizing, assembled: Assembled): Option[Tokens] = {
    val w = Tokens.value(assembled.window)
    sizing match {
      case Sizing.Deployed => None
      case Sizing.Half => Some(Tokens(w / 2))
      case Sizing.Twice => Some(Tokens(w * 2))
    }
  }
}
