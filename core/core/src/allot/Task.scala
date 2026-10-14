package grit.core.allot

import scala.collection.immutable.VectorMap

import grit.core.classify.Decision
import grit.core.id.TurnRef
import grit.core.message.{Tokens, Usage}
import grit.core.period.Probability
import grit.core.triage.Weighing

/** What a turn's root asks grit to do, as the task questions name it ([[Features]]). `word` is
  * the key the classifier chooses and the journal writes.
  */
enum TaskKind(val word: String) derives CanEqual {
  case Chat extends TaskKind("chat")
  case Lookup extends TaskKind("lookup")
  case Investigation extends TaskKind("investigation")
  case Analysis extends TaskKind("analysis")
  case Writing extends TaskKind("writing")
  case Coding extends TaskKind("coding")
  case Decision extends TaskKind("decision")
}

/** What the task questions answered of a turn's root: each kind's probability, as answered, and
  * the probability that the root is hard.
  */
final case class Features private (kinds: VectorMap[TaskKind, Probability], hard: Probability) {

  /** The most probable kind; the first in [[TaskKind]]'s order on a tie. */
  def kind: TaskKind =
    kinds
      .foldLeft((TaskKind.Chat, -1.0)) { case (best @ (_, top), (k, p)) =>
        if (Probability.value(p) > top) (k, Probability.value(p)) else best
      }
      ._1
}

object Features {

  /** What `decision` and `hard` answered: each kind's probability as `decision` weighs it,
    * `hard` clamped to [0, 1] ([[Probability.clamped]]).
    */
  def from(decision: Decision[TaskKind], hard: Double): Features =
    new Features(
      VectorMap.from(
        TaskKind.values.map(k => k -> Probability.clamped(decision.probability(k)))
      ),
      Probability.clamped(hard)
    )

  /** `kinds` in [[TaskKind]]'s order, a kind not named weighing 0, and `hard`; `None` when no
    * kind weighs above 0. What a recorded answer is read back with.
    */
  def of(kinds: Map[TaskKind, Probability], hard: Probability): Option[Features] =
    Option.when(kinds.values.exists(_ > Probability.Zero))(
      new Features(
        VectorMap.from(TaskKind.values.map(k => k -> kinds.getOrElse(k, Probability.Zero))),
        hard
      )
    )
}

/** The task questions put to a turn's root, for the turn's `route` step. */
trait Tasking extends caps.SharedCapability {

  /** What the task questions make of the person's message that is `turn`'s first, asked once
    * its placement has ended when it is an opening; why not.
    */
  def task(turn: TurnRef): Either[Weighing.Unweighed, Tasking.Asked]
}

object Tasking {

  /** What one classifier call answered, `features`; whether the state it was asked over showed
    * the strand its message joins (`strand`); the model that answered, what the call consumed,
    * and grit's `estimate` of its input, which its ledger row keeps beside the provider's count.
    */
  final case class Asked(
      features: Features,
      strand: Boolean,
      model: String,
      usage: Usage,
      estimate: Tokens
  )
}
