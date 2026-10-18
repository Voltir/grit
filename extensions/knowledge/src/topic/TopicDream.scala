package grit.knowledge.topic

import java.time.{Instant, LocalDate, ZoneId}

import grit.core.act.{Dreaming, MoveError, MoveLimits, MoveName}
import grit.core.document.DocumentKeeper
import grit.core.flow.Trusted
import grit.core.id.{DocumentVersion, JobName}
import grit.core.job.{DreamingJob, JobRun}
import grit.core.need.Exported
import grit.core.schema.{JsonSchema, Typed}
import grit.core.store.{StoreError, Tx}
import grit.knowledge.card.CardShelf
import grit.knowledge.ref.{Ref, TopicKey}
import grit.knowledge.topic.TopicScan.{Scanned, Touch}
import grit.knowledge.topic.TopicWrite.{Held, Kept, Today}

/** The job `topics-dream`, version 1, its days reckoned in `zone`, its cards through `cards`
  * when its plugin needs one that exports them. A run for its `day` ([[TopicParams]]), kept at
  * its floor, reading strands as they stood at that day's end (or now, if sooner) and the cards
  * written by then: scans its topics, the strands of rooms at its floor spoken in within `recent`,
  * and the cards changed within `cardRecent`; withdraws from a topic each passage naming a
  * strand it no longer reads at its floor (its room at another label now, or its messages
  * gone), the topic itself when none is left; attaches each strand or card a topic already
  * refers to (a card by the unit its pointer reads, which a topic refers to when it was written
  * from that card or links it), and asks once (`attach`) where each other one goes, at most
  * `perRun` strands and `perRun` cards: to one of the topics shown (those attached to, and at
  * most [[TopicScan.Leads]] more, newest first, each by its lead, its text's head within
  * [[TopicDream.LeadChars]]), to a new topic of its own, or to none: set aside (`aside`), in a
  * document of its floor under `topic:aside` holding no topic, while within `recent` or
  * `cardRecent` at the digest or version set aside; then, for each topic touched, at most `perRun` and at most
  * `perRunMost`, new topics first, asks once (`write-{i}`) for its new lead and the day's
  * passage, from the strands and cards attached and its current text, and keeps it
  * (`keep-{i}`): the day's passage replacing one the topic holds for that day, its lineage
  * joined. A topic's version refers to each strand it was written from, each card's unit, each
  * topic it links and the unit of each other link; a ref [[Ref.written]] refuses is left out
  * and counted. It puts up at most [[TopicDream.Reviewed]] of the topics it writes for review,
  * new first. A strand or card already in a topic's lineage or set aside at the same digest or
  * version touches nothing. Replies [[Gathered.line]], which holds no cost: its asks' cost is
  * read from the spend ledger ([[grit.core.store.UsageLedger]]), each recorded under its run's
  * workflow. Its `limits`: asks `1 + perRunMost`; no calls or judgments.
  */
final class TopicDream(perRunMost: Int, zone: ZoneId, cards: Option[Exported[CardShelf]])
    extends DreamingJob[TopicParams] {

  val name: JobName = TopicDream.Name

  val version: Int = 1

  def write(params: TopicParams): ujson.Value = TopicParams.write(params)

  def read(params: ujson.Value): Either[String, TopicParams] = TopicParams.read(params)

  override val limits: MoveLimits =
    MoveLimits.of(1 + perRunMost.max(0), 0).getOrElse(MoveLimits.Zero)

  def run(run: Trusted[JobRun[TopicParams]], moves: Dreaming^): String = {
    val params = run.extract.params
    val day = params.day.getOrElse(LocalDate.ofInstant(run.extract.nominal, zone))
    val end = day.plusDays(1).atStartOfDay(zone).toInstant
    val most = params.perRun.min(perRunMost.max(0))
    val shelf = cards
    moves.scan(TopicDream.Scan, run.tainted.map(_.params))((p, keeper, strands, needs, at) =>
      TopicScan
        .scanned(p, most, if (end.isBefore(at)) end else at, shelf, keeper, strands, needs, at)
    ) match {
      case Left(e) => Gathered.stopped(s"scanning its topics: ${MoveError.said(e)}").line
      case Right(scan) =>
        val begun =
          Gathered(0, 0, scan.withdrew, scan.unchanged, 0, scan.left, 0, Vector.empty, None)
        val (placed, done) = attach(run, moves, scan, begun)
        val touched = TopicDream.joined(scan.attached ++ placed)
        val ordered = touched.filter(_.opened) ++ touched.filterNot(_.opened)
        val (now, later) = ordered.splitAt(most)
        now.zipWithIndex
          .foldLeft((done.copy(left = done.left + TopicDream.items(later)), 0)) {
            case ((g, reviewed), (t, i)) =>
              written(run, moves, day, t, i, reviewed < TopicDream.Reviewed, g) match {
                case (next, up) => (next, if (up) reviewed + 1 else reviewed)
              }
          }
          ._1
          .line
    }
  }

  /** The attach ask over `scan`'s strands and cards: the topics they go to, those it set aside
    * joined to the floor's [[Aside]] (`aside`), and `g` counting those set aside, those it could
    * not place or was not shown as left, and each strand that could not open the topic it was
    * placed in as not written.
    */
  private def attach(
      run: Trusted[JobRun[TopicParams]],
      moves: Dreaming^,
      scan: Scanned,
      g: Gathered
  ): (Vector[Touch], Gathered) = {
    val n = scan.strands.size + scan.cards.size
    if (n == 0) (Vector.empty, g)
    else {
      val summary = Dreaming.Summary(
        TopicDream.attachSystem(scan.strands.size),
        TopicDream.AttachChars,
        TopicDream.LeadChars,
        TopicDream.AttachReply
      )
      moves.summarise(
        TopicDream.Attach,
        scan.strands,
        scan.leads.map(_._2) ++ scan.cards.map(_.version),
        run.tainted.map(_ => summary)
      ) match {
        case Left(e) =>
          (
            Vector.empty,
            g.copy(left = g.left + n, unwritten = g.unwritten :+ ("attach", MoveError.said(e)))
          )
        case Right(None) => (Vector.empty, g.copy(left = g.left + n))
        case Right(Some(s)) =>
          // Where each goes decides only which topics are written; each written topic is
          // believed as its own write ask's reply is, so the placing need not be checked.
          val placing = Trusted.asIs(s.asked.reply).extract
          val topics = s.shown.flatMap(v => scan.leads.find(_._2 == v))
          val shownCards = scan.cards.filter(c => s.shown.contains(c.version))
          def to(p: Placing.Place): Option[Either[Unit, (TopicKey, DocumentVersion)]] = p match {
            case Placing.Place.In(k) => topics.lift(k - 1).map(Right(_))
            case Placing.Place.New => Some(Left(()))
            case Placing.Place.Aside => None
          }
          if (placing.strands.size != scan.strands.size || placing.cards.size != shownCards.size)
            (
              Vector.empty,
              g.copy(
                left = g.left + n,
                unwritten =
                  g.unwritten :+ ("attach", "its reply placed a different number than shown")
              )
            )
          else {
            val strands = scan.strands
              .zip(placing.strands)
              .flatMap((st, p) =>
                to(p).map {
                  case Right((k, v)) => Right(Touch(k, Some(v), Vector(st), Vector.empty))
                  case Left(()) =>
                    TopicKey.strand(st.root).map(Touch(_, None, Vector(st), Vector.empty))
                }
              )
            val unopened = strands.collect { case Left(why) => ("a strand", why) }
            val cardsPlaced = shownCards
              .zip(placing.cards)
              .flatMap((c, p) =>
                to(p).map {
                  case Right((k, v)) => Touch(k, Some(v), Vector.empty, Vector(c))
                  case Left(()) => Touch(TopicKey.card(c.key), None, Vector.empty, Vector(c))
                }
              )
            val placed = strands.collect { case Right(t) => t } ++ cardsPlaced
            val setAside =
              scan.strands.zip(placing.strands).collect { case (st, Placing.Place.Aside) =>
                Lineage.Tag.Strand(st.root, st.room, st.digest)
              } ++ shownCards.zip(placing.cards).collect { case (c, Placing.Place.Aside) =>
                Lineage.Tag.Card(c.version)
              }
            val kept =
              if (setAside.isEmpty) Vector.empty
              else
                moves.keep(TopicDream.SetAside, s.asked.reply.map(_ => setAside))(
                  (tags, keeper, at) => Aside.joined(keeper, tags, at)
                ) match {
                  case Left(e) => Vector(("aside", MoveError.said(e)))
                  case Right(_) => Vector.empty
                }
            // Placed in no topic shown: left, and asked of again.
            val unplaced =
              n - (scan.cards.size - shownCards.size) - unopened.size - setAside.size -
                TopicDream.items(placed)
            (
              placed,
              g.copy(
                aside = g.aside + setAside.size,
                left = g.left + (scan.cards.size - shownCards.size) + unplaced,
                unwritten = g.unwritten ++ unopened ++ kept
              )
            )
          }
      }
    }
  }

  /** `t`'s write ask and keep, the `i`th; whether it was put up for review. */
  private def written(
      run: Trusted[JobRun[TopicParams]],
      moves: Dreaming^,
      day: LocalDate,
      t: Touch,
      i: Int,
      review: Boolean,
      g: Gathered
  ): (Gathered, Boolean) = {
    val name = TopicKey.value(t.key)
    val summary = Dreaming.Summary(
      TopicDream.writeSystem(day),
      TopicDream.Chars,
      // The whole topic, bounded by Topic.Passages.
      Int.MaxValue,
      TopicDream.WriteReply
    )
    moves.summarise(
      TopicDream.move("write", i),
      t.strands,
      t.version.toVector ++ t.cards.map(_.version),
      run.tainted.map(_ => summary)
    ) match {
      case Left(e) => (g.copy(unwritten = g.unwritten :+ (name, MoveError.said(e))), false)
      case Right(None) => (g.copy(left = g.left + TopicDream.items(Vector(t))), false)
      case Right(Some(s)) if t.version.exists(v => !s.shown.contains(v)) =>
        (g.copy(left = g.left + TopicDream.items(Vector(t))), false)
      case Right(Some(s)) =>
        val shownCards = t.cards.filter(c => s.shown.contains(c.version))
        val touch = t.copy(cards = shownCards)
        moves.keep(TopicDream.move("keep", i), s.asked.reply)((w, keeper, at) =>
          TopicDream.kept(touch, day, w, review, keeper, at)
        ) match {
          case Left(e) => (g.copy(unwritten = g.unwritten :+ (name, MoveError.said(e))), false)
          case Right(Kept(None, _)) => (g.copy(left = g.left + TopicDream.items(Vector(t))), false)
          case Right(Kept(Some(_), skipped)) =>
            (
              g.copy(
                written = g.written + 1,
                opened = g.opened + (if (t.opened) 1 else 0),
                skipped = g.skipped + skipped
              ),
              review
            )
        }
    }
  }
}

object TopicDream {

  val Name: JobName =
    JobName.of("topics-dream").fold(e => throw new IllegalStateException(e), identity)

  /** 8: the most topics one run puts up for review. */
  val Reviewed: Int = 8

  /** 6,000: the most characters of a strand a write ask is shown. */
  val Chars: Int = 6000

  /** 1,500: the most characters of a strand the attach ask is shown. */
  val AttachChars: Int = 1500

  /** 400: the most characters of a topic's text, its lead first, or of a card, the attach ask
    * is shown, and the most a lead is asked to be.
    */
  val LeadChars: Int = 400

  private val Scan: MoveName = move("scan")

  private val Attach: MoveName = move("attach")

  private val SetAside: MoveName = move("aside")

  private def move(kind: String, i: Int): MoveName = move(s"$kind-$i")

  private def move(name: String): MoveName =
    // Lower case words and digits joined by `-`: MoveName.of never refuses them.
    MoveName.of(name).fold(e => throw new IllegalStateException(e), identity)

  /** How many strands and cards `ts` attach. */
  private def items(ts: Vector[Touch]): Int = ts.map(t => t.strands.size + t.cards.size).sum

  /** `ts` with those of one key made one, in the order each key first stands. */
  private def joined(ts: Vector[Touch]): Vector[Touch] =
    ts.map(_.key)
      .distinct
      .map(k =>
        ts.filter(_.key == k)
          .reduce((a, b) =>
            Touch(
              k,
              a.version.orElse(b.version),
              (a.strands ++ b.strands).distinctBy(_.root),
              (a.cards ++ b.cards).distinctBy(_.version)
            )
          )
      )

  /** What the attach ask is told, before what it is shown: how many discussions, all of which
    * it is shown or it asks nothing, and no count of the topics or cards, since one it cannot
    * read is not shown.
    */
  private def attachSystem(strands: Int): String =
    "You sort a team's recent discussions and documents into topics: durable subjects the " +
      s"team comes back to across rooms and days. You are shown, in this order, ${count(strands, "discussion")}, " +
      "each a block of messages under their speakers' names, oldest first; then " + "the topics, each under `topic:`, numbered from 1 in the order shown, how its text " +
      "begins; then the cards, each a document describing a file, under its label. Reply " +
      "by calling `reply` with `discussions`, one entry per discussion in the order shown, " +
      "and `cards`, one per card in the order shown, each the number of the topic it belongs " +
      "to, `new` when it opens a subject no topic shown covers that the team is likely to " +
      "come back to, or `none` when it is too slight to keep."

  private def count(n: Int, what: String): String = if (n == 1) s"1 $what" else s"$n ${what}s"

  /** What a write ask is told, before what it is shown. */
  private def writeSystem(day: LocalDate): String =
    "You keep one topic of a team's knowledge: a durable subject, written as a lead and dated " +
      s"passages. You are shown the discussions and the cards attached to it on $day, each " +
      "discussion a block of messages under their speakers' names and each card a document " +
      "describing a file under its label; then, when the topic exists, its text under " +
      "`topic:`: its lead, then its passages newest first, each under its day. Reply by " +
      s"calling `reply` with `lead`, what the topic is about now, in at most $LeadChars " +
      s"characters of markdown, and `passage`, in markdown, what $day's discussions and cards " +
      "add to it: what was decided, what is known and what is still open, naming people as " +
      s"the messages do. When the topic holds a passage for $day already, yours replaces it, " +
      "so keep what it says. Keep links to other topics (`topic:…`) and to files as written."

  private val AttachReply: Typed[Placing] = {
    def placed(what: String) = ujson.Obj(
      "type" -> "array",
      "items" -> ujson.Obj("type" -> "string"),
      "description" -> s"For each $what in the order shown: a topic's number, `new` or `none`."
    )
    typed(
      ujson.Obj(
        "type" -> "object",
        "properties" -> ujson.Obj("discussions" -> placed("discussion"), "cards" -> placed("card")),
        "required" -> ujson.Arr("discussions", "cards"),
        "additionalProperties" -> false
      ),
      Placing.read
    )
  }

  private val WriteReply: Typed[Wrote] = typed(
    ujson.Obj(
      "type" -> "object",
      "properties" -> ujson.Obj(
        "lead" -> ujson.Obj(
          "type" -> "string",
          "description" -> s"What the topic is about now, in at most $LeadChars characters."
        ),
        "passage" -> ujson.Obj(
          "type" -> "string",
          "description" -> "What the day's discussions and cards add to the topic."
        )
      ),
      "required" -> ujson.Arr("lead", "passage"),
      "additionalProperties" -> false
    ),
    Wrote.read
  )

  private def typed[T](schema: ujson.Value, read: ujson.Value -> Either[String, T]): Typed[T] =
    // A literal in JsonSchema's subset but for the descriptions, which are text: every test of
    // a run throws here if not.
    JsonSchema
      .read(schema)
      .fold(why => throw new IllegalStateException(why.toString), s => Typed(s, c => read(c.json)))

  /** `w` kept as `t`'s topic for `day`, put up for review when `review` and written. */
  private def kept(
      t: Touch,
      day: LocalDate,
      w: Wrote,
      review: Boolean,
      keeper: DocumentKeeper,
      at: Instant
  )(using tx: Tx^): Either[StoreError, Kept] = {
    val floor = Tx.floor(tx)
    keeper
      .current(TopicKey.doc(t.key), floor)
      .flatMap { now =>
        (now, t.version) match {
          case (None, None) => Right(None)
          case (Some(d), Some(v)) if d.version == v =>
            TopicWrite.read(t.key, d.data).map(Some(_)).left.map(StoreError.Invalid(_))
          case _ => Left(StoreError.Invalid("the topic was written since its scan"))
        }
      }
      .flatMap { old =>
        val tags = t.strands.map(s => Lineage.Tag.Strand(s.root, s.room, s.digest)) ++
          t.cards.map(c => Lineage.Tag.Card(c.version))
        val passages = old.fold(Vector.empty[Passage])(_.topic.passages)
        val same = passages.filter(_.day == day)
        val passage = Passage(
          day,
          Lineage.of(same.flatMap(_.from.tags) ++ tags),
          TopicWrite.prose(w.passage)
        )
        val before = old.flatMap(_.today).filter(_.day == day)
        val held = Held(
          Topic.of(TopicWrite.prose(w.lead), passages.filterNot(_.day == day) :+ passage),
          (old.fold(Vector.empty[Ref])(_.units) ++ t.cards.flatMap(_.unit)).distinct,
          Some(
            Today(
              day,
              old.isEmpty || before.exists(_.opened),
              (before.fold(Vector.empty[String])(_.cards) ++ t.cards.map(_.title)).distinct
            )
          )
        )
        TopicWrite.kept(keeper, t.key, floor, held, at).flatMap {
          case k @ Kept(Some(v), _) if review => keeper.forReview(v, day, at).map(_ => k)
          case k => Right(k)
        }
      }
  }
}

/** Where the attach ask placed each strand and each card shown, in their order. */
private[topic] final case class Placing(
    strands: Vector[Placing.Place],
    cards: Vector[Placing.Place]
) extends caps.Pure

private[topic] object Placing {

  enum Place extends caps.Pure {

    /** In the `n`th topic shown, from 1. */
    case In(n: Int)

    /** In a topic of its own. */
    case New

    /** In none. */
    case Aside
  }

  def read(v: ujson.Value): Either[String, Placing] = {
    def places(key: String): Either[String, Vector[Place]] =
      v.objOpt
        .flatMap(_.get(key))
        .flatMap(_.arrOpt)
        .toRight(s"`$key` is a list")
        .flatMap(
          _.toVector.foldLeft[Either[String, Vector[Place]]](Right(Vector.empty))((acc, e) =>
            acc.flatMap(done =>
              e.strOpt.map(_.trim.toLowerCase) match {
                case Some("new") => Right(done :+ Place.New)
                case Some("none") => Right(done :+ Place.Aside)
                case Some(n) if n.nonEmpty && n.forall(_.isDigit) && n.length < 6 =>
                  Right(done :+ Place.In(n.toInt))
                case _ =>
                  Left(s"each of `$key` is a topic's number, `new` or `none`: ${ujson.write(e)}")
              }
            )
          )
        )
    for {
      strands <- places("discussions")
      cards <- places("cards")
    } yield Placing(strands, cards)
  }
}

/** A write ask's reply: the topic's new `lead` and the day's `passage`, in markdown. */
private[topic] final case class Wrote(lead: String, passage: String) extends caps.Pure

private[topic] object Wrote {
  def read(v: ujson.Value): Either[String, Wrote] =
    (for {
      o <- v.objOpt
      lead <- o.get("lead").flatMap(_.strOpt)
      passage <- o.get("passage").flatMap(_.strOpt)
    } yield Wrote(lead, passage)).toRight("a reply holds `lead` and `passage`, each a string")
}
