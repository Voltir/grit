package grit.turn

import java.time.Instant

import grit.core.classify.{Answered, Ask, Classifier, ClassifierError, Criterion, QuestionId}
import grit.core.id.{EntryId, TurnRef, TurnSeq, WorkflowId}
import grit.core.message.{AssistantBlock, Message, Tokens, Usage}
import grit.core.provider.TokenEstimator
import grit.core.store.{Db, Entry, EntryStore, Payload, StoreError, Tx, UsageLedger}
import grit.core.topic.{Band, Placement, Topic, TopicEvent, TopicId, Topics, Weights}

/** Where a turn's message goes among its conversation's topics, before its window is
  * assembled: the `classify` step's work and the `record-topic` step's record.
  *
  * The first message of a conversation (or the first since topics began) opens a topic
  * with no call. Every other is put to the classifier as a yes/no, "is it about the
  * current topic?", and its p(same) read as a [[Band]]: same, it stays; changed, the
  * classifier chooses among the earlier topics and a new one; uncertain, it stays for now
  * and the main model is asked. A classifier that does not answer
  * leaves it where it is, unclassified: topics never fail a turn.
  */
object TurnTopics {

  /** A topic as the classifier was shown it: its id and the key it went by. */
  final case class Shown(id: TopicId, key: String)

  /** What `classify` decided: the `events` to record (a topic `Opened`, then the message
    * `Placed`; none when the store could not be read, as `note` says), the `current`
    * topic and the `earlier` ones as they stood before the message, and what the
    * classifier's calls cost, when it was called: its model, their usage, and the
    * estimated input.
    */
  final case class Classification(
      events: Vector[TopicEvent],
      current: Option[Shown],
      earlier: Vector[Shown],
      cost: Option[(String, Usage, Tokens)],
      note: Option[String]
  ) {

    /** The classifier's placement of the message. */
    def placed: Option[TopicEvent.Placed] = events.collectFirst { case p: TopicEvent.Placed => p }

    /** Whether the classifier was unsure, so the main model is to be asked. */
    def uncertain: Boolean = placed.exists {
      _.by match {
        case Placement.Classified(_, Band.Uncertain, _) => true
        case _ => false
      }
    }
  }

  /** The id of the entry recording where `turn`'s message was placed. */
  def placedId(turn: TurnRef): EntryId = EntryId(
    s"topic:${WorkflowId.value(turn.workflowId)}:placed"
  )

  /** The key the classifier and the model are offered for a new topic. */
  val NewKey = "something new"

  private val SameId = QuestionId("same_topic")
  private val WhichId = QuestionId("which_topic")

  private val same: Ask[Double] = Ask.noul(
    SameId,
    "Is `new_message` about `current_topic`?",
    Some(
      "Yes: it carries on `current_topic`, answers or follows up on the recent messages, or " +
        "goes deeper into one part of it."
    ),
    Some(
      "No: it is about something else: a quick unrelated question, an earlier subject, or a " +
        "new one."
    )
  )

  /** The `classify` step: where `turn`'s message goes, from every entry of its
    * conversation read through `db`, asking `classifier` as the bands say. Never fails:
    * what goes wrong is in the result.
    */
  def classify(
      classifier: Classifier^,
      entries: EntryStore,
      db: Db^,
      estimator: TokenEstimator,
      turn: TurnRef
  ): Classification =
    db.read(entries.list(turn.conversationId)) match {
      case Left(error) =>
        Classification(Vector.empty, None, Vector.empty, None, Some(s"store: ${describe(error)}"))
      case Right(all) => place(classifier, estimator, turn, all)
    }

  /** [[classify]], over `all` of the conversation's entries. */
  def place(
      classifier: Classifier^,
      estimator: TokenEstimator,
      turn: TurnRef,
      all: Vector[Entry]
  ): Classification = {
    val before = all.filter(e => TurnSeq.value(e.turnSeq) < TurnSeq.value(turn.turnSeq))
    val topics = Topics.fold(before.flatMap(e => events(e.payload)))
    val asked = all
      .filter(_.turnSeq == turn.turnSeq)
      .collectFirst { case Entry(_, _, _, _, _, Payload.Message(Message.User(t)), _) => t }
      .getOrElse("")
    val keyed = Topics.keys(topics.topics).map((t, k) => (t, Shown(t.id, k)))
    val current =
      topics.current.flatMap(c => keyed.collectFirst { case (t, s) if t.id == c.id => s })
    val earlier = keyed.collect { case (t, s) if !current.exists(_.id == t.id) => (t, s) }
    val opened = TopicId.openedBy(turn)
    def classification(
        events: Vector[TopicEvent],
        cost: Option[(String, Usage, Tokens)]
    ): Classification = Classification(events, current, earlier.map(_._2), cost, None)
    def placed(weights: (Vector[(TopicId, Double)], Double), by: Placement) =
      TopicEvent.Placed(turn.turnSeq, weights._1, weights._2, by)

    current.flatMap(c => topics.get(c.id).map(c -> _)) match {
      case None =>
        classification(
          Vector(
            TopicEvent.Opened(opened),
            placed(Weights.whole(opened), Placement.First)
          ),
          None
        )
      case Some((shown, topic)) =>
        val recent = recentMessages(before)
        val first = ujson.Obj(
          "current_topic" -> describeTopic(topic),
          "recent_messages" -> recent,
          "new_message" -> asked
        )
        classifier.ask(first, same) match {
          case Left(error) =>
            classification(
              Vector(placed(Weights.whole(shown.id), Placement.Unclassified(why(error)))),
              None
            )
          case Right(Answered(p, u1, model)) =>
            val cost1 = (model, u1, estimator.system(ujson.write(first)))
            Band.of(p) match {
              case band @ (Band.Same | Band.Uncertain) =>
                classification(
                  Vector(
                    placed(Weights.same(shown.id, p), Placement.Classified(p, band, Vector.empty))
                  ),
                  Some(cost1)
                )
              case Band.Changed if earlier.isEmpty =>
                // Nowhere to go back to: it can only be new, and nothing need be asked.
                val choice = Vector(Option.empty[TopicId] -> 1.0)
                classification(
                  Vector(
                    TopicEvent.Opened(opened),
                    placed(
                      Weights.changed(shown.id, p, choice, Some(opened)),
                      Placement.Classified(p, Band.Changed, choice)
                    )
                  ),
                  Some(cost1)
                )
              case Band.Changed =>
                val second = ujson.Obj(
                  "left_topic" -> describeTopic(topic),
                  "recent_messages" -> recent,
                  "new_message" -> asked
                )
                classifier.ask(second, which(earlier)) match {
                  case Left(error) =>
                    classification(
                      Vector(
                        placed(
                          Weights.whole(shown.id),
                          Placement.Unclassified(s"choosing where it went: ${why(error)}")
                        )
                      ),
                      Some(cost1)
                    )
                  case Right(Answered(decision, u2, _)) =>
                    val choice = decision.probabilities
                    val isNew = decision.choice.isEmpty
                    val cost = (
                      model,
                      sum(u1, u2),
                      cost1._3 + estimator.system(ujson.write(second))
                    )
                    classification(
                      Option.when(isNew)(TopicEvent.Opened(opened)).toVector :+
                        placed(
                          Weights.changed(shown.id, p, choice, Option.when(isNew)(opened)),
                          Placement.Classified(p, Band.Changed, choice)
                        ),
                      Some(cost)
                    )
                }
            }
        }
    }
  }

  /** The `record-topic` step: `classification`'s events as `turn`'s entry
    * [[placedId]], after everything in the conversation, and the classifier's cost beside
    * it in the ledger. Nothing to record is recorded as nothing.
    */
  def record(
      entries: EntryStore,
      ledger: UsageLedger,
      turn: TurnRef,
      classification: Classification
  )(using Tx^): Either[TurnFailure, EntryId] =
    writeEvents(entries, ledger, turn, placedId(turn), classification.events, classification.cost)

  /** Records `events` as `turn`'s entry `id`, after everything in the conversation, with
    * `cost` (a model, its usage and the estimated input) in the ledger beside it. No
    * events, no entry: `id` is returned all the same.
    */
  private[turn] def writeEvents(
      entries: EntryStore,
      ledger: UsageLedger,
      turn: TurnRef,
      id: EntryId,
      events: Vector[TopicEvent],
      cost: Option[(String, Usage, Tokens)]
  )(using Tx^): Either[TurnFailure, EntryId] =
    if (events.isEmpty) Right(id)
    else
      (for {
        next <- entries.lockNext(turn.conversationId)
        _ <- entries.insert(
          Entry(
            id,
            turn.conversationId,
            turn.turnSeq,
            None,
            next.seq,
            Payload.Topic(events),
            Instant.now()
          )
        )
        _ <- cost.fold[Either[StoreError, Unit]](Right(())) { (model, usage, estimate) =>
          ledger.record(id, turn.workflowId, model, usage, estimate)
        }
      } yield id).left.map(e => TurnFailure.Store(describe(e)))

  /** The topic events `payload` holds, if it is a topic record. */
  def events(payload: Payload): Vector[TopicEvent] = payload match {
    case Payload.Topic(events) => events
    case _ => Vector.empty
  }

  /** The choice among `earlier` topics and a new one (`None`), most recent first. */
  private def which(
      earlier: Vector[(Topic, Shown)]
  ): Ask[grit.core.classify.Decision[Option[TopicId]]] =
    Ask.choice(
      WhichId,
      "`new_message` has moved away from `left_topic`. Which of these topics is it about?",
      earlier.map((t, s) => Criterion(Option(t.id), s.key, t.summary)) :+
        Criterion(
          Option.empty[TopicId],
          NewKey,
          Some("None of the others: a subject not discussed before in this conversation.")
        )
    )

  private def describeTopic(t: Topic): ujson.Value =
    ujson.Obj("name" -> t.shown, "summary" -> t.summary.fold[ujson.Value](ujson.Null)(ujson.Str(_)))

  /** The last two messages of `before`, the user's and the replies' text. */
  private def recentMessages(before: Vector[Entry]): ujson.Value =
    ujson.Arr.from(
      before
        .flatMap {
          _.payload match {
            case Payload.Message(Message.User(text)) => Some("user" -> text)
            case Payload.Message(Message.Assistant(blocks, _, _, _)) =>
              val text = blocks.collect { case AssistantBlock.Text(t) => t }.mkString
              Option.when(text.nonEmpty)("assistant" -> text)
            case _ => None
          }
        }
        .takeRight(2)
        .map((from, text) => ujson.Obj("from" -> from, "text" -> text))
    )

  private def sum(a: Usage, b: Usage): Usage =
    Usage(
      a.input + b.input,
      a.output + b.output,
      a.cachedInput + b.cachedInput,
      a.costUsd.zip(b.costUsd).map(_ + _).orElse(a.costUsd).orElse(b.costUsd)
    )

  private def why(error: ClassifierError): String = error match {
    case ClassifierError.Unavailable(cause) => s"unavailable: $cause"
    case ClassifierError.Unreadable(cause) => s"unreadable: $cause"
    case ClassifierError.Invalid(cause) => s"invalid: $cause"
  }

  private def describe(error: StoreError): String = error match {
    case StoreError.DuplicateId(id) => s"entry ${EntryId.value(id)} already exists"
    case StoreError.DatabaseError(cause) => cause
  }
}
