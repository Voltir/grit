package grit.core.triage

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.id.QuestionName
import grit.core.message.Usage
import grit.core.period.Probability

/** What triage made of one heard message ([[grit.core.store.Payload.Heard]]). */
enum Tags {

  /** Its question set's answers, each under its question's name, in the order asked; the
    * `model` that weighed them, and what the call consumed. A message triaged before v2 was
    * shipped holds v1's names ([[Tags.V1]]): kind, waiting, durable and helps.
    */
  case Weighed(answers: VectorMap[QuestionName, Answer], model: String, usage: Usage)

  /** The classifier gave no answer, for `why`. */
  case Unanswered(why: String)
}

object Tags {

  /** v1, the question set triage asked before v2: the names it asks under, and the gate it
    * drafts by.
    */
  object V1 {

    private def named(text: String): QuestionName =
      // Each is a lowercase word, a declared name by QuestionName.of's rule, so no Left is
      // taken; TagsTests reads every one.
      QuestionName.of(text).fold(why => throw new IllegalStateException(why), identity)

    val kind: QuestionName = named("kind")
    val waiting: QuestionName = named("waiting")
    val durable: QuestionName = Earning.Durable
    val helps: QuestionName = named("helps")

    /** v1's gate: `kind`'s most weighted key not `chatter`, and `helps` at least 0.5. */
    val gate: Gate = {
      val half = Probability.clamped(0.5)
      Gate(
        Vector(
          Bound.Below(Reading.Chosen(kind, Kind.written(Kind.Chatter)), half),
          Bound.AtLeast(Reading.Yes(helps), half)
        )
      )
    }

    /** v1's answers as triage kept them before they were named: the most probable `kind` at
      * `kindP` (a choice weighing only that kind), and the probabilities of yes to `waiting`,
      * `durable` and `helps`, each under its name.
      */
    def answers(
        kind: Kind,
        kindP: Probability,
        waiting: Probability,
        durable: Probability,
        helps: Probability
    ): VectorMap[QuestionName, Answer] = {
      val chosen = Kind.written(kind)
      val p = Probability.value(kindP)
      VectorMap(
        V1.kind -> Answer
          .Choice(chosen, Vector(Answer.Weight(chosen, p)), Answer.confidence(Vector(p))),
        V1.waiting -> Answer.YesNo(Probability.value(waiting)),
        V1.durable -> Answer.YesNo(Probability.value(durable)),
        V1.helps -> Answer.YesNo(Probability.value(helps))
      )
    }
  }
}
