package grit.core.stitch

import java.time.{Duration as JDuration, Instant}

import grit.core.classify.{
  Answered,
  Ask,
  Classifier,
  ClassifierError,
  Criterion,
  Decision,
  Request,
  StateJson
}
import grit.core.id.ConversationId
import grit.core.id.{EntryId, TurnRef}
import grit.core.message.{AssistantBlock, Message}
import grit.core.period.{CloseReason, Probability}
import grit.core.place.{Place, Scope}
import grit.core.store.{
  Conversation,
  ConversationStore,
  Db,
  Entry,
  EntrySearch,
  EntryStore,
  LifecycleStore,
  Payload,
  Principals,
  Speakers,
  StoreError
}

/** One exchange as the classifier is offered it: its `root` (the conversation it began in),
  * the root's `opening` message, its `latest` messages, its newest closing's headline when it
  * closed with a record, and why it was `offered`. Only [[Stitching.offer]] makes one.
  */
final case class Exchange private[stitch] (
    root: ConversationId,
    opening: Said,
    latest: Vector[Said],
    record: Option[String],
    offered: Offered
)

/** Where stitching reads: a conversation's `entries` and origin (`conversations`), the scope
  * in force (`lifecycle`), the placements kept (`stitches`), the room's `search`, and who said
  * each message (`principals`).
  */
final case class StitchReads(
    entries: EntryStore,
    conversations: ConversationStore,
    lifecycle: LifecycleStore,
    stitches: StitchStore,
    search: EntrySearch,
    principals: Principals
)

/** A first message as it is put to the classifier: the message, who said it, the exchanges
  * it is offered, most recently spoken in first, and who said their messages. Only
  * [[Stitching.offered]] makes one, with at least one exchange.
  */
final case class Offer private[stitch] (
    first: Said,
    author: String,
    exchanges: Vector[Exchange],
    speakers: Speakers
)

/** Whether a stitchable conversation's first message continues an exchange in its room
  * (ADR 0023), and how a reader shows the strand it joins.
  */
object Stitching {

  /** How many of an exchange's latest messages it is offered with. */
  val Latest = 2

  /** How many characters of one message the classifier and a reader's excerpt show. */
  val MessageChars = 300

  /** The key the classifier is offered for beginning something new. */
  val NewKey = "something new"

  /** How many BM25 hits in the room a first message's lexical slots are chosen from. */
  val Hits = 30

  /** What `turn`'s message is offered ([[offer]]) when its entry is its stitchable
    * conversation's first and a person said it, read through `db` from `reads` under
    * `tuning`, in the scope in force, **whether or not a placement is kept for it**. `None`
    * when it is not such a message or nothing is offered; `Left` when the store fails.
    */
  def offered(
      reads: StitchReads,
      db: Db^,
      turn: TurnRef,
      tuning: Tuning
  ): Either[StoreError, Option[Offer]] =
    opening(reads, db, turn, tuning, keptAsks = true).map(_.map(_._2))

  /** Where `turn`'s message goes ([[place]]), when its entry is its conversation's first and
    * a person said it, in the scope in force, read through `db` from `reads`: that entry and its
    * placement. `None`, asking nothing, otherwise, when a placement is kept for it already, or
    * when nothing is offered. `Left` when the store fails.
    */
  def turn(
      classifier: Classifier^,
      reads: StitchReads,
      db: Db^,
      turn: TurnRef,
      tuning: Tuning
  ): Either[StoreError, Option[(EntryId, Placed)]] =
    opening(reads, db, turn, tuning, keptAsks = false).map(
      _.map((first, offer) => first -> place(classifier, offer, tuning))
    )

  /** `turn`'s message, when it is its stitchable conversation's first and said, and what it is
    * offered; a message with a placement kept for it is offered nothing unless `keptAsks`.
    */
  private def opening(
      reads: StitchReads,
      db: Db^,
      turn: TurnRef,
      tuning: Tuning,
      keptAsks: Boolean
  ): Either[StoreError, Option[(EntryId, Offer)]] =
    db.read {
      for {
        all <- reads.entries.list(turn.conversationId)
        conversation <- reads.conversations.get(turn.conversationId)
        settings <- reads.lifecycle.current()
      } yield Opening(
        all.minByOption(_.seq).filter(e => e.turnSeq == turn.turnSeq && e.payload.said.nonEmpty),
        conversation,
        settings.locality.scope
      )
    }.flatMap { o =>
      (o.conversation, o.first) match {
        case (Some(c), Some(first)) if c.origin.stitchable =>
          room(reads, db, c, first, o.scope, tuning, keptAsks).map(_.map(first.id -> _))
        case _ => Right(None)
      }
    }

  /** What `first`, `conversation`'s first message, is offered among the exchanges of its room
    * that `scope` holds, read through `db`; `None` when nothing is, `first` is not its first
    * message, or, unless `keptAsks`, a placement is kept for it already.
    */
  private def room(
      reads: StitchReads,
      db: Db^,
      conversation: Conversation,
      first: Entry,
      scope: Scope,
      tuning: Tuning,
      keptAsks: Boolean
  ): Either[StoreError, Option[Offer]] = {
    val stitches = reads.stitches
    val room = conversation.origin.room
    val at = first.createdAt
    val from = at.minusNanos(tuning.horizon.toNanos)
    val text = first.payload.said.getOrElse("")
    db.read {
      for {
        opening <- stitches.openings(Vector(conversation.id))
        kept <- stitches.placed(first.id)
        said <-
          if ((kept.nonEmpty && !keptAsks) || !opening.exists(_.entry.id == first.id))
            Right(Vector.empty)
          else stitches.spokenIn(room, from, at)
        hits <-
          if (said.isEmpty) Right(Vector.empty)
          else reads.search.room(room, from, at, text, Hits)
        links <- stitches.links(said.map(_.conversation).distinct)
        roots = said
          .map(s => links.find(_.conversation == s.conversation).fold(s.conversation)(_.root))
          .distinct
        openings <- stitches.openings(roots)
        speakers <- reads.principals.speakers(
          (first +: (said ++ openings).map(_.entry)).map(_.id).distinct
        )
      } yield Room(said, hits, links, openings, speakers)
    }.map { r =>
      val mine = Said(conversation.id, conversation.origin.place, first)
      val exchanges = offer(mine, room, r.said, r.openings, r.hits, r.links, scope, tuning)
      Option.when(exchanges.nonEmpty)(
        Offer(mine, r.speakers.of(first.id).getOrElse("Someone"), exchanges, r.speakers)
      )
    }
  }

  /** What [[room]] reads of a room. */
  private final case class Room(
      said: Vector[Said],
      hits: Vector[EntrySearch.Hit],
      links: Vector[Link],
      openings: Vector[Said],
      speakers: Speakers
  )

  /** What [[opening]] reads: the conversation's first entry when it is the turn's and said,
    * its conversation, and the scope in force.
    */
  private final case class Opening(
      first: Option[Entry],
      conversation: Option[Conversation],
      scope: Scope
  )

  /** A reader's thread of at most `chars`, by part: what it shows of the strand and of the
    * conversation's own messages, and `cut`, the start of its own messages it leaves out
    * (`cut` then `own` are all of them). [[text]] is the thread.
    */
  final case class Fitted private[stitch] (strand: String, cut: String, own: String) {
    def text: String =
      if (strand.isEmpty) own else if (own.isEmpty) strand else s"$strand\n$own"

    /** Where in [[text]] the shown own messages begin, so where `cut` was taken from. */
    def at: Int = text.length - own.length
  }

  /** [[thread]] by part ([[Fitted]]). A strand longer than `chars` loses its end, which `cut`
    * does not hold.
    */
  def fit(strand: String, own: String, chars: Int): Fitted =
    if (strand.isEmpty) Fitted("", own.dropRight(chars), own.takeRight(chars))
    else {
      val room = math.max(0, chars - strand.length - 1)
      if (own.isEmpty || room == 0) Fitted(strand.take(chars), own, "")
      else Fitted(strand, own.dropRight(room), own.takeRight(room))
    }

  /** A reader's thread of at most `chars`: `strand` (its excerpt, [[excerpt]]) first, then as
    * much of `own` (the conversation's own messages) as fits, cut from its start. `own` alone
    * when there is no strand.
    */
  def thread(strand: String, own: String, chars: Int): String = fit(strand, own, chars).text

  /** The exchanges `first`, a stitchable conversation's first message in `room`, may continue,
    * from `said` (what `room` said within `tuning.horizon` before it,
    * [[StitchStore.spokenIn]]), `openings` (each exchange's first message,
    * [[StitchStore.openings]]), `hits` (its text's BM25 hits in the room, best first,
    * [[grit.core.store.EntrySearch.room]]) and `links`: only conversations `scope` holds for
    * `room`, never `first`'s own, keyed by root; the `tuning.recent` most recently spoken in,
    * then the `tuning.lexical` best matched of the rest, the next most recent filling the
    * lexical slots nothing matched. Most recently spoken in first.
    */
  def offer(
      first: Said,
      room: Place,
      said: Vector[Said],
      openings: Vector[Said],
      hits: Vector[EntrySearch.Hit],
      links: Vector[Link],
      scope: Scope,
      tuning: Tuning
  ): Vector[Exchange] = {
    val at = first.entry.createdAt
    val from = at.minusNanos(tuning.horizon.toNanos)
    def rootOf(c: ConversationId): ConversationId =
      links.find(_.conversation == c).fold(c)(_.root)
    val held = said.filter(s =>
      s.conversation != first.conversation && s.place.within(room) && scope.holds(room, s.place) &&
        !s.entry.createdAt.isBefore(from) && s.entry.createdAt.isBefore(at)
    )
    val byRoot = held.groupBy(s => rootOf(s.conversation)).filter(_._1 != first.conversation)
    val recency = byRoot.toVector
      .map((r, ss) => r -> ss.map(_.entry.createdAt).maxOption.getOrElse(Instant.EPOCH))
      .sortBy((r, t) => (-t.toEpochMilli, ConversationId.value(r)))
      .map(_._1)
    val recent = recency.take(tuning.recent).zipWithIndex.map((r, i) => r -> Offered.Recent(i + 1))
    val lexical = hits
      .map(h => rootOf(h.turn.conversationId) -> h.score)
      .filter((r, _) => byRoot.contains(r) && !recent.exists(_._1 == r))
      .distinctBy(_._1)
      .take(tuning.lexical)
      .map((r, s) => r -> Offered.Lexical(s))
    val filled = recency
      .filterNot(r => recent.exists(_._1 == r) || lexical.exists(_._1 == r))
      .take(tuning.lexical - lexical.size)
      .zipWithIndex
      .map((r, i) => r -> Offered.Recent(tuning.recent + i + 1))
    val chosen = (recent ++ lexical ++ filled).toMap
    recency.filter(chosen.contains).flatMap { r =>
      val group = byRoot.getOrElse(r, Vector.empty).sortBy(_.entry.createdAt)
      val messages = group.filter(s => spoken(s).isDefined)
      val opening = openings.find(_.conversation == r).orElse(messages.headOption)
      opening.map { o =>
        val record = group.reverse.map(_.entry.payload).collectFirst {
          case Payload.Closed(_, reason, closing) if reason != CloseReason.Unearned =>
            closing.headline
        }
        Exchange(
          r,
          o,
          messages.filterNot(_.entry.id == o.entry.id).takeRight(Latest),
          record,
          chosen.getOrElse(r, Offered.Recent(0))
        )
      }
    }
  }

  /** Where `classifier` places `offer`'s message under `tuning`: [[Placed.Follows]] the top
    * choice when it is an exchange at `tuning.followsAt` or above, else [[Placed.Begins]];
    * [[Placed.Unread]] when the classifier fails or its answer does not read.
    */
  def place(classifier: Classifier^, offer: Offer, tuning: Tuning): Placed = {
    val sent = shown(offer)
    def seen(p: Option[Decision[Option[ConversationId]]]): Seen =
      Seen(
        sent,
        offer.exchanges.map(e =>
          Seen.Offer(
            e.root,
            e.offered,
            p.map(d => Probability.clamped(d.probability(Some(e.root))))
          )
        ),
        tuning
      )
    question(offer) match {
      case Left(why) => Placed.Unread(why, seen(None))
      case Right((state, ask)) =>
        classifier.ask(state, ask) match {
          case Left(ClassifierError.Unavailable(why)) =>
            Placed.Unread(s"unavailable: $why", seen(None))
          case Left(ClassifierError.Unreadable(why)) =>
            Placed.Unread(s"unreadable: $why", seen(None))
          case Right(Answered(d, usage, model)) =>
            val top = Probability.clamped(d.top)
            d.choice match {
              case Some(root) if top >= tuning.followsAt =>
                Placed.Follows(root, top, seen(Some(d)), model, usage)
              case _ =>
                val likeliest = offer.exchanges
                  .map(e => d.probability(Some(e.root)))
                  .maxOption
                  .getOrElse(0.0)
                Placed.Begins(Probability.clamped(likeliest), seen(Some(d)), model, usage)
            }
        }
    }
  }

  /** The state [[place]] shows the classifier for `offer`, as [[Seen.state]] keeps it. */
  def shown(offer: Offer): ujson.Value = StateJson[State].json(state(offer))

  /** The request [[place]] sends for `offer`; `None` when it sends none, because its question
    * repeats a key.
    */
  def request(offer: Offer): Option[Request] =
    question(offer).toOption.map((state, ask) => Request.of(state, ask))

  /** `offer` as the question's state, each exchange under its key. */
  private def state(offer: Offer): State =
    State(
      offer.first,
      offer.author,
      offer.exchanges.zipWithIndex.map((e, i) => (s"exchange ${i + 1}", e)),
      offer.speakers
    )

  /** The question [[place]] asks about `offer`: its state and the choice among its exchanges'
    * keys and [[NewKey]]; why not, when the choice cannot be made.
    */
  private def question(
      offer: Offer
  ): Either[String, (State, Ask[State, Decision[Option[ConversationId]]])] = {
    val s = state(offer)
    val criteria = s.exchanges.map((k, e) => Criterion(Option(e.root), k, None)) :+
      Criterion(
        Option.empty[ConversationId],
        NewKey,
        Some("none of them: it starts something new")
      )
    criteria match {
      case a +: b +: rest =>
        Ask
          .choice[State, Option[ConversationId]](Instructions, a, b, rest*)
          .left
          .map(d => s"the question repeats ${d.key}")
          .map(s -> _)
      case _ => Left("fewer than two options")
    }
  }

  /** `strand` as a reader shows it within `chars`: `opening`, then the messages nearest the
    * end, newest first while they fit, in the order said, a `…` line where any were left out;
    * each message one line under its speaker's name (`speakers`; "Assistant" for grit's
    * replies, "Someone" for a name not known), at most [[MessageChars]] of it. Empty when there
    * is nothing to show.
    */
  def excerpt(
      opening: Option[Said],
      strand: Vector[Said],
      speakers: Speakers,
      chars: Int
  ): String = {
    val head = opening.flatMap(line(_, speakers)).toVector
    val rest = strand
      .filterNot(s => opening.exists(_.entry.id == s.entry.id))
      .sortBy(_.entry.createdAt)
      .flatMap(line(_, speakers))
    def shown(k: Int): String =
      (head ++ Option.when(k < rest.size)("…").toVector ++ rest.takeRight(k)).mkString("\n")
    (rest.size to 0 by -1).iterator
      .map(shown)
      .find(_.length <= chars)
      .getOrElse(shown(0).take(chars))
  }

  /** `s`'s words, when it is a message: a person's, grit's reply, or grit's post. */
  private def spoken(s: Said): Option[String] = s.entry.payload match {
    case Payload.Message(Message.Assistant(blocks, _, _, _, _)) =>
      Some(blocks.collect { case AssistantBlock.Text(t) => t }.mkString).filter(_.nonEmpty)
    case Payload.Posted(text) => Some(text).filter(_.nonEmpty)
    case p => p.said
  }

  private def speaker(s: Said, speakers: Speakers): String = s.entry.payload match {
    case Payload.Message(Message.Assistant(_, _, _, _, _)) | Payload.Posted(_) => "Assistant"
    case _ => speakers.of(s.entry.id).getOrElse("Someone")
  }

  private def line(s: Said, speakers: Speakers): Option[String] =
    spoken(s).map(t => s"${speaker(s, speakers)}: ${t.take(MessageChars)}")

  private val Instructions =
    "Read new_message, said by author at the top level of a team's channel, not in any " +
      "thread. Each exchange is a recent conversation in that channel: how it opened, its " +
      "latest messages, and a line of its record once it closed. Does new_message continue, " +
      "answer or react to one of them? If it starts something new, choose " + NewKey + "."

  /** The stitch question's state: `{new_message, author, exchanges: [{key, opening, latest,
    * record}]}`, each message `{from, text, ago}`.
    */
  private final case class State(
      first: Said,
      author: String,
      exchanges: Vector[(String, Exchange)],
      speakers: Speakers
  )

  private given StateJson[State] = StateJson.instance { s =>
    val at = s.first.entry.createdAt
    def said(m: Said): ujson.Value = ujson.Obj(
      "from" -> speaker(m, s.speakers),
      "text" -> spoken(m).getOrElse("").take(MessageChars),
      "ago" -> ago(m.entry.createdAt, at)
    )
    ujson.Obj(
      "new_message" -> spoken(s.first).getOrElse("").take(MessageChars),
      "author" -> s.author,
      "exchanges" -> ujson.Arr.from(s.exchanges.map { (key, e) =>
        ujson.Obj(
          "key" -> key,
          "opening" -> said(e.opening),
          "latest" -> ujson.Arr.from(e.latest.map(said)),
          "record" -> e.record.fold[ujson.Value](ujson.Null)(ujson.Str(_))
        )
      })
    )
  }

  /** How long before `at` `past` was, in the largest whole unit: "40 seconds", "3 minutes",
    * "2 hours", "5 days".
    */
  private[core] def ago(past: Instant, at: Instant): String = {
    val s = math.max(0L, JDuration.between(past, at).getSeconds)
    def unit(n: Long, name: String) = if (n == 1) s"1 $name" else s"$n ${name}s"
    if (s < 60) unit(s, "second")
    else if (s < 3_600) unit(s / 60, "minute")
    else if (s < 86_400) unit(s / 3_600, "hour")
    else unit(s / 86_400, "day")
  }
}
