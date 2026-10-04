package grit.lifecycle.triage

import scala.collection.immutable.VectorMap

import grit.core.classify.{Answer, Answered, Ask, Classifier, ClassifierError, Question, Request}
import grit.core.id.QuestionName
import grit.core.persona.Persona
import grit.core.triage.{Gate, Kind, KnowledgeSources, Reading, Tags}

/** Questions about a heard message ([[TriageQuestion.State]]), asked together in one
  * classifier call, each answer kept under its question's name, and the gate a draft is
  * derived from (`speak`).
  */
final case class TriageQuestions private (
    first: TriageQuestions.Item.One,
    rest: Vector[TriageQuestions.Item],
    speak: Gate
) {
  import TriageQuestions.Item

  def items: Vector[Item] = first +: rest

  /** The questions asked with `sources` (those covering the message's conversation,
    * [[KnowledgeSources.at]]), in order: each `One`; for each `PerSource`, one yes/no per
    * source in the catalog's order, named [[QuestionName.per]], none when there are none.
    */
  def questions(sources: KnowledgeSources): VectorMap[QuestionName, Question] =
    VectorMap.from(items.flatMap {
      case Item.One(name, question) => Vector(name -> question)
      case Item.PerSource(prefix, before, after) =>
        sources.all.map(s =>
          QuestionName.per(prefix, s.name) -> Question.YesNo(before + s.line + after, None, None)
        )
    })

  /** The request [[ask]] sends for `state` with `sources`. */
  def request(state: TriageQuestion.State, sources: KnowledgeSources): Request =
    Request.of(state, asked(sources))

  /** What `classifier` answered for `state`, each answer under its question's name, in the
    * order of [[questions]], with the model and what the call consumed; `Unavailable`, or
    * `Unreadable` when an answer is not of its question's kind or a choice names no key of
    * its question's.
    */
  def ask(
      classifier: Classifier^,
      state: TriageQuestion.State,
      sources: KnowledgeSources
  ): Either[ClassifierError, Answered[VectorMap[QuestionName, Answer]]] =
    classifier.ask(state, asked(sources))

  /** The first of `gate`'s readings ([[Gate.reads]]) this set does not ask with `sources`
    * ([[questions]]) in its kind, or a `Key` or `Chosen` of a key its choice lacks; `None`
    * when it asks them all.
    */
  def unread(gate: Gate, sources: KnowledgeSources): Option[Reading] = {
    val declared = questions(sources)
    def asks(name: QuestionName, choice: Question.Choice => Boolean, yesNo: Boolean) =
      declared.get(name).exists {
        case c: Question.Choice => choice(c)
        case Question.YesNo(_, _, _) => yesNo
      }
    gate.reads.find {
      case Reading.Yes(name) => !asks(name, _ => false, yesNo = true)
      case Reading.Key(name, key) => !asks(name, _.keys.exists(_.name == key), yesNo = false)
      case Reading.Chosen(name, key) => !asks(name, _.keys.exists(_.name == key), yesNo = false)
    }
  }

  /** Every question of [[questions]], each read back as given under its name; never empty,
    * since `first` is asked whatever the catalog.
    */
  private def asked(
      sources: KnowledgeSources
  ): Ask[TriageQuestion.State, VectorMap[QuestionName, Answer]] = {
    val start =
      Ask.answer[TriageQuestion.State](first.question).map(a => VectorMap(first.name -> a))
    questions(sources).toVector.drop(1).foldLeft(start) { case (acc, (name, question)) =>
      acc.zip(Ask.answer[TriageQuestion.State](question)).map((m, a) => m.updated(name, a))
    }
  }
}

object TriageQuestions {

  enum Item {

    /** One question, under `name`. */
    case One(name: QuestionName, question: Question)

    /** A yes/no asked once for each knowledge source, in the words `before + line + after`. */
    case PerSource(prefix: QuestionName, before: String, after: String)
  }

  /** Why [[of]] builds no set. */
  enum Refusal {

    /** Two items share a name, or a `One`'s name is a `PerSource`'s prefix. */
    case NameRepeated(name: QuestionName)

    /** `speak` reads `reading`, which the set does not ask without a knowledge source
      * ([[TriageQuestions.unread]]): a per-source question is never one a draft gate reads.
      */
    case Unbounded(reading: Reading)
  }

  /** `first` then `rest`, gated by `speak`, or why not. */
  def of(first: Item.One, rest: Vector[Item], speak: Gate): Either[Refusal, TriageQuestions] = {
    val items = first +: rest
    val names = items.map {
      case Item.One(name, _) => name
      case Item.PerSource(prefix, _, _) => prefix
    }
    names.diff(names.distinct).headOption.map(Refusal.NameRepeated(_)) match {
      case Some(repeated) => Left(repeated)
      case None =>
        val set = new TriageQuestions(first, rest, speak)
        set.unread(speak, KnowledgeSources.Empty).map(Refusal.Unbounded(_)).toLeft(set)
    }
  }

  /** v1, in `wording`: [[Tags.V1]]'s questions, `kind` (a choice of [[Kind]]'s keys),
    * `waiting`, `durable` and `helps`, gated by [[Tags.V1.gate]]. In the shipped wording its
    * request is the one triage sent before it asked a question set.
    */
  def v1(wording: TriageQuestion.Wording): TriageQuestions = {
    val k = wording.kinds
    def yesNo(words: String) = Question.YesNo(words, None, None)
    def key(kind: Kind, means: String) = Question.Key(Kind.written(kind), Some(means))
    val built = for {
      kindQuestion <- Question
        .choice(
          wording.kind,
          key(Kind.Question, k.question),
          key(Kind.Answer, k.answer),
          key(Kind.Decision, k.decision),
          key(Kind.Announcement, k.announcement),
          key(Kind.Chatter, k.chatter)
        )
        .left
        .map(d => s"kind repeats ${d.key}")
      set <- of(
        Item.One(Tags.V1.kind, kindQuestion),
        Vector(
          Item.One(Tags.V1.waiting, yesNo(wording.waiting)),
          Item.One(Tags.V1.durable, yesNo(wording.durable)),
          Item.One(Tags.V1.helps, yesNo(wording.helps))
        ),
        Tags.V1.gate
      ).left.map(_.toString)
    } yield set
    // Its kind keys are Kind's distinct names whatever the wording, its names are distinct and
    // every bound of its gate reads a One of its kind, so no Left is taken;
    // TriageQuestionsTests builds it.
    built.fold(why => throw new IllegalStateException(why), identity)
  }

  /** [[v1]] in the shipped wording ([[TriageQuestion.Wording.Shipped]]). */
  val V1: TriageQuestions = v1(TriageQuestion.Wording.Shipped)

  /** v2, the set that replaced [[V1]], in [[Tags.V2]]'s names: `gap` (what the message leaves
    * open: `asks`, `owes`, `closes` or `nothing`), `open`, `to`, `durable`, `anchor`, then one
    * `source:<name>` question per knowledge source; gated by [[Tags.V2.drafts]].
    */
  val V2: TriageQuestions = v2With(None, None, Tags.V2.drafts)

  /** V2's questions with `directed` after `to` and `record` after `anchor`, each when given,
    * gated by `speak`.
    */
  private def v2With(
      directed: Option[Item.One],
      record: Option[Item.One],
      speak: Gate
  ): TriageQuestions = {
    import Tags.V2.{anchor, gap, open, sourcePrefix, to}
    def yesNo(words: String) = Question.YesNo(words, None, None)
    def key(name: String, means: String) = Question.Key(name, Some(means))
    val built = for {
      gapQuestion <- Question
        .choice(
          "Read new_message, said by author, and its thread. What does new_message leave open?",
          key(
            "asks",
            "it asks for information, a document, or for something to be done, of anyone"
          ),
          key("owes", "its author commits to doing something"),
          key("closes", "it answers or settles something asked earlier"),
          key("nothing", "greetings, thanks, reactions, or news with nothing asked")
        )
        .left
        .map(d => s"gap repeats ${d.key}")
      set <- of(
        Item.One(gap, gapQuestion),
        Vector(
          Item.One(
            open,
            yesNo(
              "Read new_message and thread. Is what new_message asks for still unanswered in " +
                "the thread?"
            )
          ),
          Item.One(
            to,
            yesNo(
              "Read new_message and thread. Is new_message directed at a particular person, " +
                "named or the one it replies to, rather than at the room? Mentioning someone, " +
                "or replying in a thread they wrote in, does not by itself direct it at them."
            )
          )
        ) ++ directed ++ Vector(
          Item.One(
            Tags.V2.durable,
            yesNo(
              "Read new_message and thread. Does new_message state something worth keeping " +
                "for later: a decision, a date, a name, a number, how something works, or an " +
                "answer to a question? Greetings, thanks and small talk do not count. Count " +
                "only what new_message itself states; the thread is only for reading it."
            )
          ),
          Item.One(
            anchor,
            yesNo(
              "Read new_message and thread. Is what new_message asks for something no stored " +
                "record could supply, such as an opinion, someone's current availability, or " +
                "a choice nobody has made yet?"
            )
          )
        ) ++ record ++ Vector(
          Item.PerSource(
            sourcePrefix,
            "Read new_message and thread. Could ",
            " supply what new_message asks for?"
          )
        ),
        speak
      ).left.map(_.toString)
    } yield set
    // Its gap's keys are distinct, its names are distinct (to-grit and anchor-record are none of
    // V2's) and every bound of V2's, v3's and v4's gates reads a One of its kind, so no Left is
    // taken;
    // TriageQuestionsTests builds it.
    built.fold(why => throw new IllegalStateException(why), identity)
  }

  /** v3, the set that replaced [[V2]]: V2's questions in V2's order with `to-grit` after `to`,
    * worded with `persona`'s name ([[Tags.V3.toGrit]]); gated by [[Tags.V3.drafts]], whatever
    * the name.
    */
  def v3(persona: Persona): TriageQuestions =
    v2With(Some(toGrit(persona)), None, Tags.V3.drafts)

  /** v3's `to-grit`, worded with `persona`'s name. */
  private def toGrit(persona: Persona): Item.One =
    Item.One(
      Tags.V3.toGrit,
      Question.YesNo(
        "Read new_message and thread. Is new_message directed at the assistant, whom " +
          s"people here call ${persona.name}, rather than at someone else or at the room? " +
          "Saying its name to it, or replying to what Assistant said in thread, directs " +
          "it at the assistant; talking about it does not.",
        None,
        None
      )
    )

  /** v4, the set that replaced [[v3]]: v3's questions in v3's order and words, worded with
    * `persona`'s name, and `anchor-record` ([[Tags.V4.anchorRecord]]) after `anchor`; gated by
    * [[Tags.V4.drafts]], whatever the name.
    */
  def v4(persona: Persona): TriageQuestions =
    v2With(
      Some(toGrit(persona)),
      Some(
        Item.One(
          Tags.V4.anchorRecord,
          Question.YesNo(
            "Read new_message and thread. Is what new_message asks for something no stored " +
              "record could supply, such as an opinion, someone's current availability, their " +
              "own plans or work, or a choice nobody has made yet? A fact asked of a " +
              "particular person may still be in a record.",
            None,
            None
          )
        )
      ),
      Tags.V4.drafts
    )

  /** The set live triage asks for a deployment presenting as `persona`: [[v4]]. */
  def shipped(persona: Persona): TriageQuestions = v4(persona)

  /** The gate live triage's set drafts by, the same for every persona: [[Tags.V4.drafts]]. */
  val ShippedSpeak: Gate = Tags.V4.drafts
}
