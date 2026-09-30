package grit.core.stitch

import java.time.{Duration as JDuration, Instant}

import grit.core.classify.{
  Answered,
  Ask,
  Classifier,
  ClassifierError,
  Criterion,
  Decision,
  StateJson
}
import grit.core.id.ConversationId
import grit.core.message.{AssistantBlock, Message}
import grit.core.period.{CloseReason, Probability}
import grit.core.place.{Place, Scope}
import grit.core.store.{EntrySearch, Payload, Speakers}

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

  /** Where `classifier` places `first`, said by `author`, among `exchanges`, `speakers` naming
    * who said each message, under `tuning`: [[Placed.Follows]] the top choice when it is an
    * exchange at `tuning.followsAt` or above, else [[Placed.Begins]]; [[Placed.Unread]] when
    * the classifier fails or its answer does not read. `None`, asking nothing, when there are
    * no exchanges.
    */
  def place(
      classifier: Classifier^,
      first: Said,
      author: String,
      exchanges: Vector[Exchange],
      speakers: Speakers,
      tuning: Tuning
  ): Option[Placed] =
    exchanges.headOption.map { _ =>
      val keyed = exchanges.zipWithIndex.map((e, i) => (s"exchange ${i + 1}", e))
      val question = State(first, author, keyed, speakers)
      val sent = StateJson[State].json(question)
      def seen(p: Option[Decision[Option[ConversationId]]]): Seen =
        Seen(
          sent,
          exchanges.map(e =>
            Seen.Offer(
              e.root,
              e.offered,
              p.map(d => Probability.clamped(d.probability(Some(e.root))))
            )
          ),
          tuning
        )
      val criteria = keyed.map((k, e) => Criterion(Option(e.root), k, None)) :+
        Criterion(
          Option.empty[ConversationId],
          NewKey,
          Some("none of them: it starts something new")
        )
      criteria match {
        case a +: b +: rest =>
          Ask.choice[State, Option[ConversationId]](Instructions, a, b, rest*) match {
            case Left(Ask.DuplicateKey(k)) => Placed.Unread(s"the question repeats $k", seen(None))
            case Right(ask) =>
              classifier.ask(question, ask) match {
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
                      val likeliest = exchanges
                        .map(e => d.probability(Some(e.root)))
                        .maxOption
                        .getOrElse(0.0)
                      Placed.Begins(Probability.clamped(likeliest), seen(Some(d)), model, usage)
                  }
              }
          }
        case _ => Placed.Unread("fewer than two options", seen(None))
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

  /** `s`'s words, when it is a message: a person's, or grit's reply. */
  private def spoken(s: Said): Option[String] = s.entry.payload match {
    case Payload.Message(Message.Assistant(blocks, _, _, _, _)) =>
      Some(blocks.collect { case AssistantBlock.Text(t) => t }.mkString).filter(_.nonEmpty)
    case p => p.said
  }

  private def speaker(s: Said, speakers: Speakers): String = s.entry.payload match {
    case Payload.Message(Message.Assistant(_, _, _, _, _)) => "Assistant"
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
  private[stitch] def ago(past: Instant, at: Instant): String = {
    val s = math.max(0L, JDuration.between(past, at).getSeconds)
    def unit(n: Long, name: String) = if (n == 1) s"1 $name" else s"$n ${name}s"
    if (s < 60) unit(s, "second")
    else if (s < 3_600) unit(s / 60, "minute")
    else if (s < 86_400) unit(s / 3_600, "hour")
    else unit(s / 86_400, "day")
  }
}
