package grit.knowledge.topic

import java.time.{Instant, LocalDate, ZoneId}

import grit.core.act.{Dreaming, MoveError, MoveLimits, MoveName}
import grit.core.document.DocumentKeeper
import grit.core.flow.Trusted
import grit.core.id.{DocumentVersionId, JobName}
import grit.core.job.{DreamingJob, JobRun}
import grit.core.need.Exported
import grit.core.schema.{JsonSchema, Typed}
import grit.core.store.{StoreError, Tx}
import grit.knowledge.card.{CardMoves, CardShelf}
import grit.knowledge.ref.{Ref, TopicKey}
import grit.knowledge.topic.TopicScan.{Scanned, Touch}
import grit.knowledge.topic.TopicWrite.{Held, Kept, Today}

/** The job `topics-dream`, version 3, its days reckoned in `zone`, its cards through `cards` when
  * its plugin needs one that exports them. A run for its `day` ([[TopicParams]]), kept at its
  * floor, reading strands as they stood at that day's end (or now, if sooner) and the cards written
  * by then: scans its topics, the strands of rooms at its floor spoken in within `recent`, and the
  * cards changed within `cardRecent` ([[TopicParams.SeedReach]] on a night up to its `seedUntil`); withdraws from a topic each passage naming a strand it no
  * longer reads at its floor (its room at another label now, or its messages gone), the topic
  * itself when none is left; attaches each strand or card a topic already refers to (a card by the
  * unit its pointer reads, which a topic refers to when it was written from that card, links it,
  * or a strand of it links it); pairs each card a strand it takes up links ([[TopicScan.Linked]],
  * read whatever its age or whether it was set aside) with that strand, to go wherever the strand
  * goes, the aside included, never shown to the attach ask; and asks (`attach`) where the others
  * go but a directory's cards and those kept below its floor, which attach only by ref or a
  * strand's link, at most `perRun` strands and [[TopicScan.Cards]] cards past those already placed, each
  * numbered in the order shown, as are the topics shown (those attached to, and at most
  * [[TopicScan.Leads]] more, newest first, each by its lead, its text's head within
  * [[TopicDream.LeadChars]]). Its reply groups them by the topic they go to: one shown, or a new
  * one, keyed by the group's first strand; groups naming one topic are one. A new group holding
  * no strand is set aside, so no card alone opens a topic; shown no strand, the ask is offered
  * neither discussions nor a new topic to name, and with no topic to show either it is not
  * made, its cards left for a later run. Those it puts in none are set aside
  * (`aside`), in a document of its floor under `topic:aside` holding no topic, while within
  * `recent` or `cardRecent` at the digest or version set aside, and for at most `asideFor`. A
  * card a reply placing anything names nowhere is set aside too; a discussion it names nowhere
  * is counted unnamed and offered again. A number or topic not shown is dropped from the reply, its topic's
  * group with it, and a strand or card it names in two groups, or in a group and the aside, is
  * dropped from both, so unnamed; each counted dropped. A reply that places nothing it was shown
  * is asked once more (`attach-again`), and when that one does not either, all are left. Then, for each topic touched, at most `perRun` and at most `perRunMost`, new
  * topics first, asks once (`write-{i}`) for its new lead and the day's passage, from the strands
  * and cards attached, the current cards of at most [[TopicScan.Holds]] more units it refers to,
  * and its current text, and keeps it (`keep-{i}`): the day's passage replacing
  * one the topic holds for that day, its lineage joined. A topic's version refers to each strand it
  * was written from, each card's unit and each card unit its strands link (newest first), each
  * topic it links and the unit of each other link; a ref [[Ref.written]] refuses, or one past
  * [[grit.core.document.DocRef.Most]] (the oldest units first), is left out and counted. It puts up at most [[TopicDream.Reviewed]] of
  * the topics it writes for review, new first. A strand or card already in a topic's lineage or set
  * aside at the same digest or version touches nothing. Replies [[Gathered.line]], which holds no
  * cost: its asks' cost is read from the spend ledger ([[grit.core.store.UsageLedger]]), each
  * recorded under its run's workflow. Its `limits`: asks `2 + perRunMost`; no calls or judgments.
  */
final class TopicDream(perRunMost: Int, zone: ZoneId, cards: Option[Exported[CardShelf]])
    extends DreamingJob[TopicParams] {

  val name: JobName = TopicDream.Name

  val version: Int = 3

  def write(params: TopicParams): ujson.Value = TopicParams.write(params)

  def read(params: ujson.Value): Either[String, TopicParams] = TopicParams.read(params)

  override val limits: MoveLimits =
    MoveLimits.of(2 + perRunMost.max(0), 0).getOrElse(MoveLimits.Zero)

  def run(run: Trusted[JobRun[TopicParams]], moves: Dreaming^): String = {
    val params = run.extract.params
    val day = params.day.getOrElse(LocalDate.ofInstant(run.extract.nominal, zone))
    val end = day.plusDays(1).atStartOfDay(zone).toInstant
    val most = params.perRun.min(perRunMost.max(0))
    val shelf = cards
    // A seeding night reads every card written within SeedReach in place of cardRecent.
    val seeding = params.seedUntil.exists(last => !day.isAfter(last))
    moves.scan(TopicDream.Scan, run.tainted.map(_.params))((p, keeper, strands, needs, at) =>
      TopicScan.scanned(
        if (seeding) p.copy(cardRecent = TopicParams.SeedReach) else p,
        most,
        if (end.isBefore(at)) end else at,
        shelf,
        keeper,
        strands,
        needs,
        at
      )
    ) match {
      case Left(e) => Gathered.stopped(s"scanning its topics: ${MoveError.said(e)}").line
      case Right(scan) =>
        val begun = Gathered(
          0,
          0,
          scan.withdrew,
          scan.unreadable,
          scan.later,
          scan.unchanged,
          0,
          scan.left,
          0,
          0,
          0,
          0,
          Vector.empty,
          None
        )
        val (placed, done) = attach(run, moves, scan, begun)
        // Each topic takes the cards its strands link, wherever those strands went.
        val touched = TopicDream
          .joined(scan.attached ++ placed)
          .map(t => t.copy(cards = (t.cards ++ scan.of(t.strands)._2).distinctBy(_.version)))
        val ordered = touched.filter(_.opened) ++ touched.filterNot(_.opened)
        val (now, later) = ordered.splitAt(most)
        now.zipWithIndex
          .foldLeft((done.copy(left = done.left + TopicDream.items(later)), 0)) {
            case ((g, reviewed), (t, i)) =>
              written(
                run,
                moves,
                day,
                t,
                scan.of(t.strands)._1,
                scan.holding(t.key),
                i,
                reviewed < TopicDream.Reviewed,
                g
              ) match {
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
    // With no discussion and no topic shown, a card could only be set aside: none is asked of.
    else if (scan.strands.isEmpty && scan.leads.isEmpty) (Vector.empty, g.copy(left = g.left + n))
    else {
      val system =
        TopicDream.attachSystem(scan.strands.size, scan.leads.nonEmpty, scan.cards.nonEmpty)
      def asked(name: MoveName, system: String) = {
        val summary = Dreaming.Summary(
          system,
          TopicDream.AttachChars,
          TopicDream.LeadChars,
          TopicDream.attachReply(scan.strands.nonEmpty)
        )
        moves.summarise(
          name,
          scan.strands,
          scan.leads.map(_._2) ++ scan.cards.map(_.version),
          run.tainted.map(_ => summary)
        )
      }
      // `s`'s reply resolved over what it was shown; read as the placing below is.
      def placed(s: Dreaming.Summarised[Placing]) =
        Trusted
          .asIs(s.asked.reply)
          .extract
          .placed(
            scan.strands,
            scan.cards.filter(c => s.shown.contains(c.version)),
            s.shown.flatMap(v => scan.leads.find(_._2 == v))
          )
      val (answered, again) = asked(TopicDream.Attach, system) match {
        case Right(Some(s)) if placed(s).isEmpty =>
          (asked(TopicDream.AttachAgain, system + " " + TopicDream.Again), 1)
        case first => (first, 0)
      }
      // Where each goes decides only which topics are written; each written topic is believed
      // as its own write ask's reply is, so the placing need not be checked.
      answered.map(_.map(s => (s, placed(s)))) match {
        case Left(e) =>
          (
            Vector.empty,
            g.copy(left = g.left + n, unwritten = g.unwritten :+ ("attach", MoveError.said(e)))
          )
        case Right(None) => (Vector.empty, g.copy(left = g.left + n))
        case Right(Some((_, None))) =>
          (
            Vector.empty,
            g.copy(left = g.left + n, unwritten = g.unwritten :+ ("attach", TopicDream.Uncovered))
          )
        case Right(Some((s, Some(p)))) =>
          // A new topic is keyed by its first discussion.
          val touches = p.topics.map {
            case Placing.Went.Into((k, v), ds, cs) => Right(Touch(k, Some(v), ds, cs))
            case Placing.Went.Opened(first, more, cs) =>
              TopicKey.strand(first.root).map(Touch(_, None, first +: more, cs))
          }
          val unopened = touches.collect { case Left(why) => ("a strand", why) }
          // A reply that placed something and named a card nowhere judged it fits no topic shown.
          val setAside =
            p.asideDiscussions.map(st => Lineage.Tag.Strand(st.root, st.room, st.digest)) ++
              (p.asideCards ++ p.omittedCards ++ scan.of(p.asideDiscussions)._2)
                .distinctBy(_.version)
                .map(c => Lineage.Tag.Card(c.version))
          val kept =
            if (setAside.isEmpty) Vector.empty
            else
              moves.keep(TopicDream.SetAside, s.asked.reply.map(_ => setAside))(
                (tags, keeper, at) => Aside.joined(keeper, tags, at)
              ) match {
                case Left(e) => Vector(("aside", MoveError.said(e)))
                case Right(_) => Vector.empty
              }
          (
            touches.collect { case Right(t) => t },
            g.copy(
              aside = g.aside + setAside.size,
              // Those not shown.
              left = g.left + scan.cards.count(c => !s.shown.contains(c.version)),
              unnamed = g.unnamed + p.unnamedDiscussions + p.unnamedCards,
              dropped = g.dropped + p.dropped,
              again = g.again + again,
              unwritten = g.unwritten ++ unopened ++ kept
            )
          )
      }
    }
  }

  /** `t`'s write ask, shown besides the cards it `holds`, and keep, the `i`th; whether it was
    * put up for review.
    */
  private def written(
      run: Trusted[JobRun[TopicParams]],
      moves: Dreaming^,
      day: LocalDate,
      t: Touch,
      units: Vector[Ref],
      holds: Vector[DocumentVersionId],
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
      CardMoves.numbered("write", i),
      t.strands,
      t.version.toVector ++ (t.cards.map(_.version) ++ holds).distinct,
      run.tainted.map(_ => summary)
    ) match {
      case Left(e) => (g.copy(unwritten = g.unwritten :+ (name, MoveError.said(e))), false)
      case Right(None) => (g.copy(left = g.left + TopicDream.items(Vector(t))), false)
      case Right(Some(s)) if t.version.exists(v => !s.shown.contains(v)) =>
        (g.copy(left = g.left + TopicDream.items(Vector(t))), false)
      case Right(Some(s)) =>
        val shownCards = t.cards.filter(c => s.shown.contains(c.version))
        val touch = t.copy(cards = shownCards)
        moves.keep(CardMoves.numbered("keep", i), s.asked.reply)((w, keeper, at) =>
          TopicDream.kept(touch, units, day, w, review, keeper, at)
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

  private val Scan: MoveName = MoveName("scan")

  private val Attach: MoveName = MoveName("attach")

  /** The attach ask again, made only when its reply placed nothing it was shown. */
  private val AttachAgain: MoveName = MoveName("attach-again")

  /** What the attach ask asked again is told besides. */
  private val Again: String =
    "Your previous reply placed nothing shown: each discussion or card it named was named more " +
      "than once across `topics` and `aside`, or was a number not shown, or went to a topic not " +
      "shown. Name each exactly once, from those shown."

  /** Why a run left all its attach ask was shown. */
  private val Uncovered: String =
    "its reply placed nothing shown: each it named was named twice, or not shown, " +
      "or in a topic not shown"

  private val SetAside: MoveName = MoveName("aside")

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
    * it is shown or it asks nothing; that it is shown no topic or no card when the scan holds
    * none (`topics`, `cards`), and otherwise no count of them, since one it cannot read is not
    * shown. Shown no discussion, it is told only of the topics and cards, and that a card goes to
    * a topic shown or `aside`.
    */
  private def attachSystem(strands: Int, topics: Boolean, cards: Boolean): String = {
    val topicsShown =
      "the topics, each under `topic:`, how its text begins, numbered from 1 in the order shown"
    val cardsShown =
      "the cards, each a document describing a file under its label, numbered from 1 in the " +
        "order shown"
    if (strands == 0)
      "You sort a team's recent documents into topics: what the team comes back to across " +
        "rooms and days. You are shown, in this order, " +
        (if (topics) s"$topicsShown; then " else "") + s"$cardsShown. " +
        (if (topics) "" else "No topic is shown yet: `topics` is an empty list. ") +
        "Reply by calling `reply` with `topics`, one entry per topic shown that cards go to: " +
        "its `topic`, the number of the topic, and the `cards` that go to it, by number. A " +
        "card goes to the topic shown it is about, and to `aside` when none is. Give `aside` " +
        "the cards too slight to keep. Name each card shown exactly once."
    else
      "You sort a team's recent discussions and documents into topics: what the team comes " +
        s"back to across rooms and days. You are shown, in this order, ${count(strands, "discussion")}, " +
        "each a block of messages under their speakers' names, oldest first, numbered from 1 in " +
        "the order shown" +
        (if (topics) s"; then $topicsShown" else "") +
        (if (cards) s"; then $cardsShown. " else ". ") +
        (if (topics) "" else "No topic is shown yet, so none has a number. ") +
        (if (cards) "" else "No card is shown: every `cards` is an empty list. ") +
        "Reply by calling `reply` with `topics`, one entry per topic discussions or cards go to: " +
        "its `topic`, the number of a topic shown, or `new` for a new topic, one no topic shown " +
        "covers that the team is likely to come back to; and the `discussions` and `cards` that " +
        "go to it, by number. Each entry is one topic, about one matter: a discussion of a " +
        "matter no other shown is about gets a `new` entry of its own, however short. Put two " +
        "discussions in one `new` entry only when they are about the same matter; never two " +
        "different matters, even related ones. A `new` entry holds at least one discussion: a " +
        "card goes to the topic shown or the discussion it is about, and to `aside` when none " +
        "is. Give `aside` the discussions and cards too slight to keep. Name each discussion " +
        "and card shown exactly once."
  }

  private def count(n: Int, what: String): String = if (n == 1) s"1 $what" else s"$n ${what}s"

  /** What a write ask is told, before what it is shown. */
  private def writeSystem(day: LocalDate): String =
    "You keep one topic of a team's knowledge: a durable subject, written as a lead and dated " +
      s"passages. You are shown the discussions and the cards attached to it on $day and the " +
      "other cards it holds, each discussion a block of messages under their speakers' names " +
      "and each card a document describing a file under its label; then, when the topic " +
      "exists, its text under `topic:`: its lead, then its passages newest first, each under " +
      s"its day. Reply by calling `reply` with `lead`, what the topic is about now, in at most " +
      s"$LeadChars characters of markdown, and `passage`, in markdown, what $day's discussions " +
      "and cards add to it: what was decided, what is known and what is still open, naming " +
      "people as the messages do. Write about the matter, not the exchange: never about the " +
      "assistant's own conduct, such as a message it sent twice, its corrections of its own " +
      "earlier answers, or a test of it. Write each day as its date (yyyy-mm-dd), never as " +
      "\"today\", \"tomorrow\", \"yesterday\" or a weekday's name: a message's relative day is " +
      s"reckoned from the day it was said, $day for that day's messages. The passage is shown " +
      "under its day, so it begins with what it adds, with no line naming its day. Never call " +
      "something unverified, unknown or unconfirmed that a card shown states. When the topic " +
      s"holds a passage for $day already, yours replaces it, so keep what it says. Keep links " +
      "to other topics (`topic:…`) and to files as written."

  /** The attach ask's reply: with no `discussions` and no `new` topic when it was shown no
    * discussion (`discussions`).
    */
  private def attachReply(discussions: Boolean): Typed[Placing] = {
    def numbers(what: String) = ujson.Obj(
      "type" -> "array",
      "items" -> ujson.Obj("type" -> "integer", "minimum" -> 1),
      "description" -> s"The numbers of the ${what}s, from 1 in the order shown."
    )
    def items(description: Option[String])(properties: (String, ujson.Value)*) = ujson.Obj.from(
      Vector[(String, ujson.Value)](
        "type" -> "object",
        "properties" -> ujson.Obj.from(properties),
        "required" -> ujson.Arr.from(properties.map(_._1)),
        "additionalProperties" -> false
      ) ++ description.map(d => "description" -> ujson.Str(d))
    )
    val placed = Option.when(discussions)("discussions" -> numbers("discussion")).toVector :+
      ("cards" -> numbers("card"))
    val what = if (discussions) "discussions and cards" else "cards"
    typed(
      items(None)(
        "topics" -> ujson.Obj(
          "type" -> "array",
          "items" -> items(None)(
            ("topic" -> ujson.Obj(
              "type" -> "string",
              "description" -> (
                if (discussions)
                  "The number of the topic shown these go to, or `new` for a new one."
                else "The number of the topic shown these go to."
              )
            )) +: placed*
          ),
          "description" -> s"One entry per topic its $what go to."
        ),
        "aside" -> items(Some(s"The $what too slight to keep, in no topic."))(placed*)
      ),
      Placing.read(discussions)
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

  /** `w` kept as `t`'s topic for `day`, referring besides to the `units` its strands link, put
    * up for review when `review` and written.
    */
  private def kept(
      t: Touch,
      units: Vector[Ref],
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
            TopicWrite.read(t.key, d.data).map(Some(_)).left.map(u => StoreError.Invalid(u.why))
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
          (old.fold(Vector.empty[Ref])(_.units) ++ t.cards.flatMap(_.unit) ++ units).distinct,
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

/** The attach ask's reply as it gave it: the `topics` it put discussions and cards in, and those
  * it set `aside`, each item by its number in the order shown, from 1.
  */
private[topic] final case class Placing(topics: Vector[Placing.Group], aside: Placing.Items)
    extends caps.Pure {

  /** Where each of `discussions`, `cards` and `topics`, as numbered in their order, went: the
    * groups holding any, each into a topic shown or opening one from its discussions, then those
    * set aside, the cards of a new group holding no discussion among them; the cards it named in
    * no group and not in the aside (`omittedCards`); how many discussions it placed nowhere, and
    * how many cards it named but placed nowhere; and how many names it `dropped`: each number not
    * shown, each topic not among `topics` (its group with it), and each discussion or card named
    * in more than one group or the aside, dropped from all of them and so placed nowhere. `None`
    * when nothing is left to place.
    */
  def placed[S, C, T](
      discussions: Vector[S],
      cards: Vector[C],
      topics: Vector[T]
  ): Option[Placing.Placed[S, C, T]] = {
    val lists = this.topics.map(_.items) :+ aside
    // How many of the lists name each number.
    def places(of: Placing.Items => Vector[Int]) =
      lists.flatMap(l => of(l).distinct).groupMapReduce(identity)(_ => 1)(_ + _)
    val dPlaces = places(_.discussions)
    val cPlaces = places(_.cards)
    def unclear(named: Map[Int, Int], n: Int) = named.count((i, k) => i < 1 || i > n || k > 1)
    def of[A](as: Vector[A], named: Map[Int, Int], is: Vector[Int]) =
      is.distinct.filter(i => named.get(i).contains(1)).flatMap(i => as.lift(i - 1))
    def ds(is: Vector[Int]) = of(discussions, dPlaces, is)
    def cs(is: Vector[Int]) = of(cards, cPlaces, is)
    val groups = this.topics.flatMap(g =>
      (g.to match {
        case Placing.To.In(k) => topics.lift(k - 1).map(Some(_))
        case Placing.To.New => Some(None)
      }).map(t => (t, ds(g.items.discussions), cs(g.items.cards)))
    )
    val unshown = this.topics.collect {
      case Placing.Group(Placing.To.In(k), _) if !topics.isDefinedAt(k - 1) => k
    }.distinct
    val went = groups.flatMap {
      case (Some(t), d, c) if d.nonEmpty || c.nonEmpty => Some(Placing.Went.Into(t, d, c))
      case (None, first +: more, c) => Some(Placing.Went.Opened(first, more, c))
      case _ => None
    }
    val asideDiscussions = ds(aside.discussions)
    val asideCards = cs(aside.cards) ++ groups.collect {
      case (None, d, c) if d.isEmpty => c
    }.flatten
    val placedDiscussions = went.map {
      case Placing.Went.Into(_, d, _) => d.size
      case Placing.Went.Opened(_, more, _) => 1 + more.size
    }.sum + asideDiscussions.size
    val placedCards = went.map {
      case Placing.Went.Into(_, _, c) => c.size
      case Placing.Went.Opened(_, _, c) => c.size
    }.sum + asideCards.size
    // Named in no list at all, as against named in two and dropped.
    val omittedCards = cards.zipWithIndex.collect {
      case (c, i) if !cPlaces.contains(i + 1) => c
    }
    Option.when(placedDiscussions + placedCards > 0)(
      Placing.Placed(
        went,
        asideDiscussions,
        asideCards,
        omittedCards,
        discussions.size - placedDiscussions,
        cards.size - placedCards - omittedCards.size,
        unclear(dPlaces, discussions.size) + unclear(cPlaces, cards.size) + unshown.size
      )
    )
  }
}

private[topic] object Placing {

  /** Discussions and cards, by number. */
  final case class Items(discussions: Vector[Int], cards: Vector[Int]) extends caps.Pure

  /** The items a reply puts in one topic. */
  final case class Group(to: To, items: Items) extends caps.Pure

  /** The topic a group goes to. */
  enum To extends caps.Pure {

    /** The `n`th topic shown, from 1. */
    case In(n: Int)

    /** A new topic of its own. */
    case New
  }

  /** What [[Placing.placed]] resolves a reply to. */
  final case class Placed[S, C, T](
      topics: Vector[Went[S, C, T]],
      asideDiscussions: Vector[S],
      asideCards: Vector[C],
      omittedCards: Vector[C],
      unnamedDiscussions: Int,
      unnamedCards: Int,
      dropped: Int
  )

  /** Where a group's discussions and cards went. */
  enum Went[S, C, T] {

    /** Into the topic `to`, shown. */
    case Into(to: T, discussions: Vector[S], cards: Vector[C])

    /** Into a new topic, opened by its `first` discussion: a new topic always holds one. */
    case Opened(first: S, more: Vector[S], cards: Vector[C])
  }

  /** `v` as an attach reply, its `discussions` read only when it was offered them. */
  def read(discussions: Boolean)(v: ujson.Value): Either[String, Placing] = {
    def numbers(o: ujson.Value, key: String, where: String): Either[String, Vector[Int]] =
      o.objOpt
        .flatMap(_.get(key))
        .flatMap(_.arrOpt)
        .toRight(s"$where `$key` is a list")
        .flatMap(
          _.toVector.foldLeft[Either[String, Vector[Int]]](Right(Vector.empty))((acc, e) =>
            acc.flatMap(done =>
              number(e)
                .map(done :+ _)
                .toRight(s"each of $where `$key` is a number: ${ujson.write(e)}")
            )
          )
        )
    def items(o: ujson.Value, where: String): Either[String, Items] =
      for {
        ds <- if (discussions) numbers(o, "discussions", where) else Right(Vector.empty)
        cs <- numbers(o, "cards", where)
      } yield Items(ds, cs)
    def group(o: ujson.Value): Either[String, Group] =
      for {
        to <- o.objOpt
          .flatMap(_.get("topic"))
          .flatMap(t =>
            if (t.strOpt.exists(_.trim.toLowerCase == "new")) Some(To.New)
            else number(t).map(To.In(_))
          )
          .toRight("each of `topics` holds `topic`, a topic's number or `new`")
        is <- items(o, "a topic's")
      } yield Group(to, is)
    for {
      groups <- v.objOpt
        .flatMap(_.get("topics"))
        .flatMap(_.arrOpt)
        .toRight("`topics` is a list")
        .flatMap(
          _.toVector.foldLeft[Either[String, Vector[Group]]](Right(Vector.empty))((acc, g) =>
            acc.flatMap(done => group(g).map(done :+ _))
          )
        )
      aside <- v.objOpt
        .flatMap(_.get("aside"))
        .toRight("`aside` is an object")
        .flatMap(items(_, "the aside's"))
    } yield Placing(groups, aside)
  }

  /** A whole number from 1, or a string of its digits. */
  private def number(v: ujson.Value): Option[Int] =
    v.numOpt
      .filter(n => n.isWhole && n >= 1 && n < 100000)
      .map(_.toInt)
      .orElse(
        v.strOpt
          .map(_.trim)
          .filter(n => n.nonEmpty && n.forall(_.isDigit) && n.length < 6)
          .map(_.toInt)
          .filter(_ >= 1)
      )
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
