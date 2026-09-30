package grit.turn

import java.time.Instant

import grit.core.classify.{
  Answered,
  Ask,
  Classifier,
  ClassifierError,
  Criterion,
  Decision,
  StateJson
}
import grit.core.id.{EntryId, TurnRef, TurnSeq, WorkflowId}
import grit.core.message.{AssistantBlock, Message, Tokens, Usage}
import grit.core.provider.TokenEstimator
import grit.core.store.{Db, Entry, EntryStore, EntryTopics, Payload, StoreError, Tx, UsageLedger}
import grit.core.topic.{Band, Placement, Topic, TopicEvent, TopicId, Weights}

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
        case Placement.Classified(_, Placement.Outcome.Uncertain) => true
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

  /** The JSON field names of [[SameTopic]] and [[WhichTopic]], for their codecs and the
    * instructions that refer to them.
    */
  private object Field {
    val CurrentTopic = "current_topic"
    val LeftTopic = "left_topic"
    val Recent = "recent_messages"
    val NewMessage = "new_message"
  }

  /** One of the recent messages, as the classifier is shown it. */
  private final case class Said(byUser: Boolean, text: String)

  /** Whether `newMessage` stays on `current`, as the classifier is shown it: `{current_topic:
    * {name, summary}, recent_messages: [{from, text}], new_message}`.
    */
  private final case class SameTopic(current: Topic, recent: Vector[Said], newMessage: String)

  /** Where `newMessage`, having left `left`, went: `{left_topic: {name, summary},
    * recent_messages: [{from, text}], new_message}`.
    */
  private final case class WhichTopic(left: Topic, recent: Vector[Said], newMessage: String)

  private given StateJson[SameTopic] = StateJson.instance(s =>
    ujson.Obj(
      Field.CurrentTopic -> topicJson(s.current),
      Field.Recent -> saidJson(s.recent),
      Field.NewMessage -> s.newMessage
    )
  )

  private given StateJson[WhichTopic] = StateJson.instance(s =>
    ujson.Obj(
      Field.LeftTopic -> topicJson(s.left),
      Field.Recent -> saidJson(s.recent),
      Field.NewMessage -> s.newMessage
    )
  )

  private val same: Ask[SameTopic, Double] = Ask.yesNo(
    s"Is `${Field.NewMessage}` about `${Field.CurrentTopic}`?",
    Some(
      s"Yes: it carries on `${Field.CurrentTopic}`, answers or follows up on the recent " +
        "messages, or goes deeper into one part of it."
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
    val topics = EntryTopics.before(all, turn.turnSeq)
    // What its person said: a message to grit, or the heard one an unprompted turn answers.
    val asked = all
      .filter(_.turnSeq == turn.turnSeq)
      .flatMap(_.payload.said)
      .headOption
      .getOrElse("")
    val keyed = keys(topics.topics)
    val current =
      topics.current.flatMap(c => keyed.collectFirst { case (t, s) if t.id == c.id => s })
    val earlier = keyed.collect { case (t, s) if !current.exists(_.id == t.id) => (t, s) }
    val opened = TopicId.openedBy(turn)
    def classification(
        events: Vector[TopicEvent],
        cost: Option[(String, Usage, Tokens)]
    ): Classification = Classification(events, current, earlier.map(_._2), cost, None)
    def placed(weights: Weights, by: Placement) = TopicEvent.Placed(turn.turnSeq, weights, by)

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
        val sameState = SameTopic(topic, recent, asked)
        classifier.ask(sameState, same) match {
          case Left(error) =>
            classification(
              Vector(placed(Weights.whole(shown.id), Placement.Unclassified(why(error)))),
              None
            )
          case Right(Answered(p, u1, model)) =>
            val cost1 =
              (model, u1, estimator.system(ujson.write(StateJson[SameTopic].json(sameState))))
            def stays(outcome: Placement.Outcome) = classification(
              Vector(placed(Weights.same(shown.id, p), Placement.Classified(p, outcome))),
              Some(cost1)
            )
            Band.of(p) match {
              case Band.Same => stays(Placement.Outcome.Same)
              case Band.Uncertain => stays(Placement.Outcome.Uncertain)
              case Band.Changed =>
                earlier match {
                  case latest +: older =>
                    val whichState = WhichTopic(topic, recent, asked)
                    which(latest, older).left
                      .map(d => s"two options share the key ${d.key}")
                      .flatMap(classifier.ask(whichState, _).left.map(why)) match {
                      case Left(reason) =>
                        classification(
                          Vector(
                            placed(
                              Weights.whole(shown.id),
                              Placement.Unclassified(s"choosing where it went: $reason")
                            )
                          ),
                          Some(cost1)
                        )
                      case Right(Answered(decision, u2, _)) =>
                        val choice =
                          decision.probabilities.map(w => Placement.Chance(w.value, w.probability))
                        val isNew = decision.choice.isEmpty
                        val cost = (
                          model,
                          sum(u1, u2),
                          cost1._3 + estimator.system(
                            ujson.write(StateJson[WhichTopic].json(whichState))
                          )
                        )
                        classification(
                          Option.when(isNew)(TopicEvent.Opened(opened)).toVector :+
                            placed(
                              Weights.changed(shown.id, p, choice, Option.when(isNew)(opened)),
                              Placement.Classified(p, Placement.Outcome.Changed(choice))
                            ),
                          Some(cost)
                        )
                    }
                  case _ =>
                    // Nowhere to go back to: it can only be new, and nothing need be asked.
                    val choice = Vector(Placement.Chance(None, 1.0))
                    classification(
                      Vector(
                        TopicEvent.Opened(opened),
                        placed(
                          Weights.changed(shown.id, p, choice, Some(opened)),
                          Placement.Classified(p, Placement.Outcome.Changed(choice))
                        )
                      ),
                      Some(cost1)
                    )
                }
            }
        }
    }
  }

  /** The `record-topic` step: `classification`'s events as `turn`'s entry
    * [[placedId]], dated `at`, after everything in the conversation, and the classifier's
    * cost beside it in the ledger. Nothing to record is recorded as nothing.
    */
  def record(
      entries: EntryStore,
      ledger: UsageLedger,
      turn: TurnRef,
      classification: Classification,
      at: Instant
  )(using Tx^): Either[TurnFailure, EntryId] =
    writeEvents(
      entries,
      ledger,
      turn,
      placedId(turn),
      classification.events,
      classification.cost,
      at
    )

  /** Records `events` as `turn`'s entry `id`, dated `at`, after everything in the
    * conversation, with `cost` (a model, its usage and the estimated input) in the ledger
    * beside it. No events, no entry: `id` is returned all the same.
    */
  private[turn] def writeEvents(
      entries: EntryStore,
      ledger: UsageLedger,
      turn: TurnRef,
      id: EntryId,
      events: Vector[TopicEvent],
      cost: Option[(String, Usage, Tokens)],
      at: Instant
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
            at
          )
        )
        _ <- cost.fold[Either[StoreError, Unit]](Right(())) { (model, usage, estimate) =>
          ledger.record(id, turn, turn.workflowId, model, usage, estimate)
        }
      } yield id).left.map(e => TurnFailure.Store(describe(e)))

  /** The id of the entry recording the description `turn`'s summary gave its topic. */
  def describedId(turn: TurnRef): EntryId =
    EntryId(s"topic:${WorkflowId.value(turn.workflowId)}:described")

  /** The topic `turn`'s message is in, as `all` of its conversation's entries leave it. */
  def topicOf(all: Vector[Entry], turn: TurnRef): Option[Topic] = {
    val topics = EntryTopics.through(all, turn.turnSeq)
    topics.placed(turn.turnSeq).flatMap(topics.get)
  }

  /** What the summary's `read` says of `topic`: its name, kept once it has one, and what it
    * covers now. Nothing when the summary said nothing of it, or nothing new.
    */
  def described(topic: Topic, read: TurnSummary.Read): Vector[TopicEvent] =
    read.topic.toVector.flatMap { (name, about) =>
      val kept = topic.name.getOrElse(name)
      Option
        .when(!(topic.name.contains(kept) && topic.summary.contains(about)))(
          TopicEvent.Described(topic.id, kept, about)
        )
        .toVector
    }

  /** The choice among the earlier topics, `first` then `more`, and a new one (`None`). */
  private def which(
      first: (Topic, Shown),
      more: Vector[(Topic, Shown)]
  ): Either[Ask.DuplicateKey, Ask[WhichTopic, Decision[Option[TopicId]]]] = {
    def criterion(ts: (Topic, Shown)) = Criterion(Option(ts._1.id), ts._2.key, ts._1.summary)
    val fresh = Criterion(
      Option.empty[TopicId],
      NewKey,
      Some("None of the others: a subject not discussed before in this conversation.")
    )
    Ask.choice(
      s"`${Field.NewMessage}` has moved away from `${Field.LeftTopic}`. Which of these topics " +
        "is it about?",
      criterion(first),
      // The new topic comes last, as the second option or after the others.
      more.headOption.map(criterion).getOrElse(fresh),
      (more.drop(1).map(criterion) ++ Option.when(more.nonEmpty)(fresh))*
    )
  }

  /** Each of `topics`, in order, with the key it goes by: its shown name, and a number after
    * it when an earlier one in `topics` is shown the same or the name is [[NewKey]]. No two
    * keys are equal, and none is [[NewKey]].
    */
  private[turn] def keys(topics: Vector[Topic]): Vector[(Topic, Shown)] = {
    // A numbered key skips any number that would make it a shown name or an earlier key.
    val names = topics.map(_.shown).toSet + NewKey
    @annotation.tailrec
    def numbered(name: String, n: Int, used: Set[String]): String = {
      val key = s"$name ($n)"
      if (names(key) || used(key)) numbered(name, n + 1, used) else key
    }
    topics.zipWithIndex
      .foldLeft((Vector.empty[(Topic, Shown)], Set.empty[String])) { case ((done, used), (t, i)) =>
        val before = topics.take(i).count(_.shown == t.shown)
        val key =
          if (before == 0 && t.shown != NewKey) t.shown else numbered(t.shown, before + 1, used)
        (done :+ (t -> Shown(t.id, key)), used + key)
      }
      ._1
  }

  private def topicJson(t: Topic): ujson.Value =
    ujson.Obj("name" -> t.shown, "summary" -> t.summary.fold[ujson.Value](ujson.Null)(ujson.Str(_)))

  private def saidJson(said: Vector[Said]): ujson.Value =
    ujson.Arr.from(
      said.map(s => ujson.Obj("from" -> (if (s.byUser) "user" else "assistant"), "text" -> s.text))
    )

  /** The last two messages of `before`, the user's and the replies' text. */
  private def recentMessages(before: Vector[Entry]): Vector[Said] =
    before
      .flatMap {
        _.payload match {
          case Payload.Message(Message.User(text)) => Some(Said(true, text))
          case Payload.Message(Message.Assistant(blocks, _, _, _, _)) =>
            val text = blocks.collect { case AssistantBlock.Text(t) => t }.mkString
            Option.when(text.nonEmpty)(Said(false, text))
          case _ => None
        }
      }
      .takeRight(2)

  private def sum(a: Usage, b: Usage): Usage = a + b

  private def why(error: ClassifierError): String = error match {
    case ClassifierError.Unavailable(cause) => s"unavailable: $cause"
    case ClassifierError.Unreadable(cause) => s"unreadable: $cause"
  }

  private def describe(error: StoreError): String = error match {
    case StoreError.DuplicateId(id) => s"entry ${EntryId.value(id)} already exists"
    case StoreError.DatabaseError(cause) => cause
    case StoreError.Invalid(cause) => cause
  }
}
