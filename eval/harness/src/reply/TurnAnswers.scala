package grit.eval.harness.reply

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.id.{CorpusName, QuestionName, WorkflowId}
import grit.core.recipe.{Rooted, ServiceOffer}
import grit.core.triage.{Corpora, Tags}
import grit.eval.harness.corpus.TurnCase
import grit.turn.{TurnOffer, TurnRecord}

/** The answers a variant decides a recorded turn's offering by, and which turns the harness
  * asks for them.
  */
object TurnAnswers {

  /** `t`'s root's answers: those its `weigh` step recorded, kept by live triage or asked by the
    * turn. None when the step recorded none for a heard root, recorded its asking failed (of
    * any kind), or is not recorded (the turn ran with weighing off): what the turn decided by,
    * never asked again. For a root said to grit whose step recorded none, its recipe not
    * reading there: `asked`, the harness's ([[TurnTriage.ask]]; `None`: not asked, or
    * unanswered).
    */
  def of(
      t: TurnCase,
      asked: Option[VectorMap[QuestionName, Answer]]
  ): Option[VectorMap[QuestionName, Answer]] =
    t.weighed match {
      case TurnRecord.Weigh.Recorded(Some(weighed)) =>
        weighed.answers.collect { case Tags.Weighed(answers, _, _) => answers }
      case TurnRecord.Weigh.Recorded(None) =>
        t.root match {
          case TurnOffer.Root.Addressed => asked
          case TurnOffer.Root.Heard | TurnOffer.Root.Named | TurnOffer.Root.ByName => None
        }
      case TurnRecord.Weigh.Unrecorded => None
    }

  /** Whether [[of]] would take `t`'s answers from `asked`, and one of `variants` reads them:
    * `t` recorded a shape, and some variant's offering at its root gates a service the shape
    * recorded.
    */
  def asks(t: TurnCase, variants: Vector[TurnVariant]): Boolean =
    (t.root, t.weighed) match {
      case (TurnOffer.Root.Addressed, TurnRecord.Weigh.Recorded(None)) =>
        t.offered
          .flatMap(_.shape)
          .exists(shape =>
            variants.exists(v =>
              shape.services.exists(took =>
                v.recipe.at(Rooted.Addressed).offering.gate(took.offer.sources).nonEmpty
              )
            )
          )
      case _ => false
    }

  /** Whether `knowledge` is the declaration `t`'s shape was decided under: it declares every
    * source the shape recorded, and says each service the shape recorded is supplied by the
    * sources recorded for it. `Left` naming the first source undeclared or service that
    * disagrees.
    */
  def declared(t: TurnCase, knowledge: Corpora): Either[String, Unit] = {
    val turn = WorkflowId.value(t.workflow)
    val names = knowledge.all.map(_.name).toSet
    val supplied = knowledge.supplied
    def written(sources: Vector[CorpusName]) =
      if (sources.isEmpty) "nothing" else sources.map(CorpusName.value).mkString(", ")
    val services =
      t.offered.flatMap(_.shape).fold(Vector.empty[ServiceOffer])(_.services.map(_.offer))
    services
      .flatMap(_.sources)
      .find(!names.contains(_))
      .map(n =>
        s"knowledge.json does not declare ${CorpusName.value(n)}, " +
          s"which $turn's shape records"
      )
      .orElse(
        services.collectFirst {
          case o if supplied.getOrElse(o.service, Vector.empty) != o.sources =>
            s"knowledge.json says ${o.service.name} is supplied by " +
              s"${written(supplied.getOrElse(o.service, Vector.empty))}; $turn's shape records " +
              written(o.sources)
        }
      )
      .toLeft(())
  }
}
