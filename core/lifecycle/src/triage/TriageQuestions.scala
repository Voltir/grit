package grit.lifecycle.triage

import scala.collection.immutable.VectorMap

import grit.core.classify.{Answer, Answered, Ask, Classifier, ClassifierError, Question, Request}
import grit.core.id.QuestionName
import grit.core.persona.Persona
import grit.core.triage.{Corpora, Gate, Kind, Reading, Tags}

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
    * [[Corpora.at]]), in order: each `One`; for each `PerSource`, one yes/no per
    * source in the catalog's order, named [[QuestionName.per]], none when there are none.
    */
  def questions(sources: Corpora): VectorMap[QuestionName, Question] =
    VectorMap.from(items.flatMap {
      case Item.One(name, question) => Vector(name -> question)
      case Item.PerSource(prefix, before, after) =>
        sources.all.map(s =>
          QuestionName.per(prefix, s.name) -> Question.YesNo(before + s.line + after, None, None)
        )
    })

  /** The request [[ask]] sends for `state` with `sources`. */
  def request(state: TriageQuestion.State, sources: Corpora): Request =
    Request.of(state, asked(sources))

  /** What `classifier` answered for `state`, each answer under its question's name, in the
    * order of [[questions]], with the model and what the call consumed; `Unavailable`, or
    * `Unreadable` when an answer is not of its question's kind, a choice names no key of its
    * question's, or a score weighs other than one weight per level.
    */
  def ask(
      classifier: Classifier^,
      state: TriageQuestion.State,
      sources: Corpora
  ): Either[ClassifierError, Answered[VectorMap[QuestionName, Answer]]] =
    classifier.ask(state, asked(sources))

  /** The first of `gate`'s readings ([[Gate.reads]]) this set does not ask with `sources`
    * ([[questions]]) in its kind, or a `Key` or `Chosen` of a key its choice lacks; `None`
    * when it asks them all. A fact is never one: code reads it, not the set.
    */
  def unread(gate: Gate, sources: Corpora): Option[Reading] = {
    val declared = questions(sources)
    def asks(name: QuestionName, choice: Question.Choice => Boolean, yesNo: Boolean) =
      declared.get(name).exists {
        case c: Question.Choice => choice(c)
        case Question.YesNo(_, _, _) => yesNo
        case Question.Score(_, _, _, _) => false
      }
    gate.reads.find {
      case Reading.Yes(name) => !asks(name, _ => false, yesNo = true)
      case Reading.Key(name, key) => !asks(name, _.keys.exists(_.name == key), yesNo = false)
      case Reading.Chosen(name, key) => !asks(name, _.keys.exists(_.name == key), yesNo = false)
      case Reading.Fact(_) => false
    }
  }

  /** Every question of [[questions]], each read back as given under its name; never empty,
    * since `first` is asked whatever the catalog.
    */
  def asked(
      sources: Corpora
  ): Ask[TriageQuestion.State, VectorMap[QuestionName, Answer]] = {
    val named = questions(sources)
    Ask
      .all(Ask.answer(first.question), named.values.toVector.drop(1).map(Ask.answer))
      .map(a => VectorMap.from(named.keys.zip(a)))
  }
}

object TriageQuestions {

  enum Item {

    /** One question, under `name`. */
    case One(name: QuestionName, question: Question)

    /** A yes/no asked once for each corpus, in the words `before + line + after`. */
    case PerSource(prefix: QuestionName, before: String, after: String)
  }

  /** Why [[of]] builds no set. */
  enum Refusal {

    /** Two items share a name, or a `One`'s name is a `PerSource`'s prefix. */
    case NameRepeated(name: QuestionName)

    /** `speak` reads `reading`, which the set does not ask without a corpus
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
        set.unread(speak, Corpora.Empty).map(Refusal.Unbounded(_)).toLeft(set)
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
    * `source:<name>` question per corpus; gated by [[Tags.V2.drafts]].
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
    import Tags.V2.{anchor, gap, sourcePrefix, to}
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
          StillOpen,
          Item.One(
            to,
            yesNo(
              "Read new_message and thread. Is new_message directed at a particular person, " +
                "named or the one it replies to, rather than at the room? Mentioning someone, " +
                "or replying in a thread they wrote in, does not by itself direct it at them."
            )
          )
        ) ++ directed ++ Vector(
          Durable,
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

  /** v2's to v4's `open`; v5 words its own. */
  private def StillOpen: Item.One = Item.One(
    Tags.V2.open,
    Question.YesNo(
      "Read new_message and thread. Is what new_message asks for still unanswered in the thread?",
      None,
      None
    )
  )

  /** v2's to v5's `durable`. */
  private def Durable: Item.One = Item.One(
    Tags.V2.durable,
    Question.YesNo(
      "Read new_message and thread. Does new_message state something worth keeping " +
        "for later: a decision, a date, a name, a number, how something works, or an " +
        "answer to a question? Greetings, thanks and small talk do not count. Count " +
        "only what new_message itself states; the thread is only for reading it.",
      None,
      None
    )
  )

  /** v5, the set that replaced [[v4]], worded with `persona`'s name: v4's questions in v4's
    * order but `anchor`; `gap` with [[Tags.V5.states]], a statement that asks nothing, before
    * `nothing`; `open`, `to-grit` and `durable` in v4's words, and `to`, `anchor-record` and
    * each `source:<name>` in plainer ones; gated by [[Tags.V5.drafts]], whatever the name.
    */
  def v5(persona: Persona): TriageQuestions =
    v5With(
      persona,
      Item.One(
        Tags.V4.anchorRecord,
        Question.YesNo(
          "Read new_message and thread. Is what new_message asks for something nobody " +
            "could find in what the team has written down, such as an opinion, someone's " +
            "current availability, their own plans or work, or a choice nobody has made " +
            "yet? A fact asked of a particular person may still be written down somewhere.",
          None,
          None
        )
      ),
      "Read new_message and thread. ",
      Tags.V5.drafts
    )

  /** v6, the set that replaced [[v5]], worded with `persona`'s name: v5's questions in v5's
    * order and words, but `records-hold` ([[Tags.V6.recordsHold]]) in place of `anchor-record`,
    * and each `source:<name>` told to read the records listed; gated by [[Tags.V6.drafts]],
    * whatever the name.
    */
  def v6(persona: Persona): TriageQuestions =
    v5With(
      persona,
      Item.One(
        Tags.V6.recordsHold,
        Question.YesNo(
          "Read new_message, thread, exchanges_in_channel and records_near (when present). " +
            "Could something the team has written down, such as the records listed, hold an " +
            "answer to what new_message asks for?",
          None,
          None
        )
      ),
      "Read new_message, thread and records_near (when present). ",
      Tags.V6.drafts
    )

  /** v5's questions in v5's order, with `record` after `durable` and each `source:<name>` read
    * after `sourceReads`, gated by `speak`.
    */
  private def v5With(
      persona: Persona,
      record: Item.One,
      sourceReads: String,
      speak: Gate
  ): TriageQuestions = {
    import Tags.V2.{gap, sourcePrefix, to}
    def yesNo(words: String) = Question.YesNo(words, None, None)
    def key(name: String, means: String) = Question.Key(name, Some(means))
    val built = for {
      gapQuestion <- Question
        .choice(
          "Read new_message, said by author, thread and earlier_exchange (when present). What " +
            "does new_message leave open?",
          key(
            "asks",
            "it asks for information, a document, or for something to be done, of anyone"
          ),
          key("owes", "its author commits to doing something"),
          key("closes", "it answers or settles something asked earlier"),
          key(
            Tags.V5.states,
            "it shares a view, a plan, a proposal or a report, and asks nothing of anyone"
          ),
          key("nothing", "greetings, thanks, reactions, or news with nothing asked")
        )
        .left
        .map(d => s"gap repeats ${d.key}")
      set <- of(
        Item.One(gap, gapQuestion),
        Vector(
          Item.One(
            Tags.V2.open,
            yesNo(
              "Read new_message, thread and earlier_exchange (when present). Is what " +
                "new_message asks for still unanswered in the thread or earlier_exchange?"
            )
          ),
          Item.One(
            to,
            yesNo(
              "Read new_message, thread and earlier_exchange (when present). Is new_message " +
                "directed at a particular person rather than at the room, by naming them or by " +
                "answering what they said in thread or earlier_exchange? Mentioning someone, or " +
                "writing in a thread they wrote in, does not by itself direct it at them."
            )
          ),
          toGrit(persona),
          Durable,
          record,
          Item.PerSource(
            sourcePrefix,
            sourceReads + "Could the source below supply what new_message asks for? The " +
              "source is ",
            "."
          )
        ),
        speak
      ).left.map(_.toString)
    } yield set
    // Its gap's keys are distinct, its names are distinct and every bound of v5's and v6's gates
    // reads a One of its kind or a fact, so no Left is taken; TriageQuestionsTests builds both.
    built.fold(why => throw new IllegalStateException(why), identity)
  }

  /** The set live triage asks for a deployment presenting as `persona`: [[v6]]. */
  def shipped(persona: Persona): TriageQuestions = v6(persona)

  /** The gate live triage's set drafts by, the same for every persona: [[Tags.V6.drafts]]. */
  val ShippedSpeak: Gate = Tags.V6.drafts
}
