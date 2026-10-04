package grit.core.triage

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.id.{KnowledgeSourceName, QuestionName}
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
  private def named(text: String): QuestionName =
    // Each is a lowercase word, a declared name by QuestionName.of's rule, so no Left is
    // taken; TagsTests and PartsTests read every one.
    QuestionName.of(text).fold(why => throw new IllegalStateException(why), identity)

  object V1 {

    val kind: QuestionName = named("kind")
    val waiting: QuestionName = named("waiting")
    val durable: QuestionName = Earning.Durable
    val helps: QuestionName = named("helps")

    /** v1's gate: `kind`'s most weighted key not `chatter`, and `helps` at least 0.5. */
    val gate: Gate = {
      val half = Probability.clamped(0.5)
      Gate.bounds(
        Bound.Below(Reading.Chosen(kind, Kind.written(Kind.Chatter)), half),
        Bound.AtLeast(Reading.Yes(helps), half)
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

  /** v2, the question set triage asks since it replaced v1: the names it asks under, the
    * gates over its answers a deployment composes, and the gate it drafts by.
    */
  object V2 {

    /** A choice of what its message leaves open: `asks`, `owes`, `closes` or `nothing`. */
    val gap: QuestionName = named("gap")
    val open: QuestionName = named("open")
    val to: QuestionName = named("to")
    val durable: QuestionName = Earning.Durable
    val anchor: QuestionName = named("anchor")

    /** The prefix of its yes/no asked once per knowledge source ([[QuestionName.per]]). */
    val sourcePrefix: QuestionName = named("source")

    private val half = Probability.clamped(0.5)

    /** Its message asks for something, of anyone: gap's weight on `asks` at least `at`. */
    def asks(at: Probability): Gate = Gate.Holds(Bound.AtLeast(Reading.Key(gap, "asks"), at))

    /** What it asks is still unanswered in its thread: `open` at least `at`. */
    def stillOpen(at: Probability): Gate = Gate.Holds(Bound.AtLeast(Reading.Yes(open), at))

    /** It is not directed at one person: `to` below `at`. */
    def notToSomeone(at: Probability): Gate = Gate.Holds(Bound.Below(Reading.Yes(to), at))

    /** What it asks for is something a stored record could supply: `anchor` below `at`. */
    def notAnchored(at: Probability): Gate = Gate.Holds(Bound.Below(Reading.Yes(anchor), at))

    /** `name` could supply what it asks for: `source:<name>` at least `at`. Unread on the
      * answers about a message where `name` is not declared ([[KnowledgeSources.at]]).
      */
    def source(name: KnowledgeSourceName, at: Probability): Gate =
      Gate.Holds(Bound.AtLeast(Reading.Yes(QuestionName.per(sourcePrefix, name)), at))

    /** v2's draft gate: it asks, still open, not to someone and not anchored, each at one
      * half.
      */
    val drafts: Gate = Gate.all(asks(half), stillOpen(half), notToSomeone(half), notAnchored(half))
  }

  /** v3, the question set triage asks since it replaced v2: [[V2]]'s names and parts, and
    * `to-grit`; the gate over them by which a heard message is answered as directed at grit,
    * and the gate it drafts by.
    */
  object V3 {

    /** Whether its message is directed at grit, by its persona's name or as a reply to it.
      * Stored under this name: a pin of recorded answers.
      */
    val toGrit: QuestionName = named("to-grit")

    private val half = Probability.clamped(0.5)

    /** It is directed at grit: `to-grit` at least `at`. */
    def atGrit(at: Probability): Gate = Gate.Holds(Bound.AtLeast(Reading.Yes(toGrit), at))

    /** It asks, still open, and is directed at grit, each at one half: the branch of
      * [[drafts]] by which grit answers it as named.
      */
    val directed: Gate = Gate.all(V2.asks(half), V2.stillOpen(half), atGrit(half))

    /** v3's draft gate: it asks and is still open, and either it is directed at grit
      * ([[directed]]) or it is directed at no one person and not anchored; each at one half.
      * A message directed at grit is not held for asking what no record could supply: it was
      * asked of grit.
      */
    val drafts: Gate = Gate.either(directed, V2.drafts)
  }
}
