package grit.core.triage

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.id.QuestionName
import grit.core.message.Usage
import grit.core.period.Probability

/** What triage made of one heard message ([[grit.core.store.Payload.Heard]]). */
enum Tags {

  /** Its most probable `kind`, with that probability (`kindP`); the probability that someone
    * waits on a reply to it (`waiting`), that it states something worth keeping (`durable`),
    * and that a reply from grit would help (`helps`); the `model` that weighed them, and what
    * the call consumed.
    */
  case Weighed(
      kind: Kind,
      kindP: Probability,
      waiting: Probability,
      durable: Probability,
      helps: Probability,
      model: String,
      usage: Usage
  )

  /** The classifier gave no answer, for `why`. */
  case Unanswered(why: String)
}

object Tags {

  /** v1, the question set [[Tags.Weighed]] holds the answers of: the names it asks under, and
    * the gate it drafts by.
    */
  object V1 {

    private def named(text: String): QuestionName =
      // Each is a lowercase word, a declared name by QuestionName.of's rule, so no Left is
      // taken; TagsTests reads every one.
      QuestionName.of(text).fold(why => throw new IllegalStateException(why), identity)

    val kind: QuestionName = named("kind")
    val waiting: QuestionName = named("waiting")
    val durable: QuestionName = named("durable")
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

    /** `tags` as v1's answers, each under its name: `kind` a choice weighing only its kind, at
      * `kindP`; `waiting`, `durable` and `helps` yes/nos.
      */
    def answers(tags: Tags.Weighed): VectorMap[QuestionName, Answer] = {
      val chosen = Kind.written(tags.kind)
      val p = Probability.value(tags.kindP)
      VectorMap(
        kind -> Answer
          .Choice(chosen, Vector(Answer.Weight(chosen, p)), Answer.confidence(Vector(p))),
        waiting -> Answer.YesNo(Probability.value(tags.waiting)),
        durable -> Answer.YesNo(Probability.value(tags.durable)),
        helps -> Answer.YesNo(Probability.value(tags.helps))
      )
    }
  }
}
