package grit.knowledge.topic

import java.time.{Instant, LocalDate, ZoneId}

import grit.core.act.{Dreaming, MoveError, MoveLimits, MoveName, Sorting}
import grit.core.classify.{Criterion, Decision}
import grit.core.document.DocumentKeeper
import grit.core.flow.{Tainted, Trusted}
import grit.core.id.{DocumentVersionId, JobName}
import grit.core.job.{DreamingJob, JobRun}
import grit.core.need.Exported
import grit.core.prompt.Tag
import grit.core.schema.{JsonSchema, Typed}
import grit.core.store.{StoreError, Tx}
import grit.knowledge.card.{CardMoves, CardShelf}
import grit.knowledge.ref.{Ref, TopicKey}
import grit.knowledge.topic.TopicScan.{Scanned, Touch}
import grit.knowledge.topic.TopicWrite.{Held, Kept, Today}
import grit.std.either.Proven

/** The job `topics-dream`, its days reckoned in `zone`, its cards through `cards` when its plugin
  * needs one that exports them. A run for its `day` ([[TopicParams]]), kept at its floor, reading
  * strands as they stood at that day's end (or now, if sooner) and the cards written by then,
  * places the strands spoken in within `recent` and the cards changed within `cardRecent` in
  * topics, each by a judgment of its own (`sort-{i}`, [[grit.core.act.Dreaming.sort]]), setting
  * aside (`aside`) for at most `asideFor` those it puts in none; then, for
  * each topic touched, at most `perRun` and at most `perRunMost`, new topics first, asks once
  * (`write-{i}`) for its new lead and the day's passage and keeps it (`keep-{i}`). It puts up at
  * most [[TopicDream.Reviewed]] of the topics it writes for review, new first. What it scans and
  * withdraws, what each ask is shown, how a reply is read and what a topic's version refers to:
  * the module's README, "A topics run". Replies [[Gathered.line]], which holds no
  * cost: its asks' cost is read from the spend ledger ([[grit.core.store.UsageLedger]]), each
  * recorded under its run's workflow. Its `limits`: asks `perRunMost`, judgments ten per ask
  * ([[MoveLimits.JudgmentsPerAsk]]); no calls.
  */
final class TopicDream(perRunMost: Int, zone: ZoneId, cards: Option[Exported[CardShelf]])
    extends DreamingJob[TopicParams] {

  val name: JobName = TopicDream.Name

  // 4: its asks are shown discussions and documents fenced, and told so (their digests moved).
  // 5: strands and cards are placed by sorts, a judgment each, not by one attach ask.
  val version: Int = 5

  def write(params: TopicParams): ujson.Value = TopicParams.write(params)

  def read(params: ujson.Value): Either[String, TopicParams] = TopicParams.read(params)

  override val limits: MoveLimits =
    MoveLimits.of(perRunMost.max(0), 0).getOrElse(MoveLimits.Zero)

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

  /** Each of `scan`'s strands, then each of its cards, placed by a sort of its own (`sort-{i}`,
    * numbered from 0 in that order) among the topics it shows: a strand joins the topic chosen
    * at [[TopicDream.TopicAt]] or more, is set aside when `aside` is chosen, and otherwise opens
    * a topic of its own (counted `unsure` when a topic was chosen below the mark); a card goes
    * to the topic likeliest about it at [[TopicDream.CardAt]] or more, and is otherwise set
    * aside. With no topic shown, cards are left and nothing is asked of them. Those set aside,
    * with the cards their strands link, are joined to the floor's [[Aside]] (`aside`); `g` counts
    * them, those left (changed since the scan, or a sort refused, named by its move), and each
    * strand that could not open its topic as not written.
    */
  private def attach(
      run: Trusted[JobRun[TopicParams]],
      moves: Dreaming^,
      scan: Scanned,
      g: Gathered
  ): (Vector[Touch], Gathered) = {
    val topics = scan.leads.map((k, v) => (TopicDream.Placed.Into(k, v), v))
    // Where each goes decides only which topics are written; each written topic is believed
    // as its own write ask's reply is, so a sort's answer need not be checked.
    type Sorted = (Vector[Touch], Tainted[Vector[Lineage.Tag]], Gathered)
    def failed(s: Sorted, name: MoveName, e: Option[MoveError]): Sorted =
      (
        s._1,
        s._2,
        s._3.copy(
          left = s._3.left + 1,
          unwritten = s._3.unwritten ++ e.map(e => (MoveName.value(name), MoveError.said(e)))
        )
      )
    val discussion = run.tainted.map(_ => TopicDream.sortDiscussion)
    val strands = scan.strands.zipWithIndex.foldLeft[Sorted](
      (Vector.empty, run.tainted.map(_ => Vector.empty), g)
    ) { case (s @ (ts, aside, g), (st, i)) =>
      val name = CardMoves.numbered("sort", i)
      moves.sort(name, Dreaming.Item.Discussion(st), topics, discussion) match {
        case Left(e) => failed(s, name, Some(e))
        case Right(None) => failed(s, name, None)
        case Right(Some(sorted)) =>
          val d = Trusted.asIs(sorted.asked.reply).extract
          d.choice match {
            case TopicDream.Placed.Into(k, v) if d.top >= TopicDream.TopicAt =>
              (ts :+ Touch(k, Some(v), Vector(st), Vector.empty), aside, g)
            case TopicDream.Placed.Aside =>
              val tags = Lineage.Tag.Strand(st.root, st.room, st.digest) +:
                scan.of(Vector(st))._2.map(c => Lineage.Tag.Card(c.version))
              (ts, aside.zip(sorted.asked.reply).map((a, _) => a ++ tags), g)
            case chosen =>
              val unsure = chosen match {
                case TopicDream.Placed.Into(_, _) => g.unsure + 1
                case _ => g.unsure
              }
              // A new topic is keyed by its discussion.
              TopicKey.strand(st.root) match {
                case Left(why) =>
                  (ts, aside, g.copy(unsure = unsure, unwritten = g.unwritten :+ ("a strand", why)))
                case Right(k) =>
                  (ts :+ Touch(k, None, Vector(st), Vector.empty), aside, g.copy(unsure = unsure))
              }
          }
      }
    }
    val card = run.tainted.map(_ => TopicDream.sortCard)
    val (touches, aside, sorted) =
      if (topics.isEmpty) strands match {
        case (ts, aside, g) => (ts, aside, g.copy(left = g.left + scan.cards.size))
      }
      else
        scan.cards.zipWithIndex.foldLeft[Sorted](strands) { case (s @ (ts, aside, g), (c, j)) =>
          val name = CardMoves.numbered("sort", scan.strands.size + j)
          moves.sort(name, Dreaming.Item.Document(c.version), topics, card) match {
            case Left(e) => failed(s, name, Some(e))
            case Right(None) => failed(s, name, None)
            case Right(Some(sorted)) =>
              Trusted.asIs(sorted.asked.reply).extract.headOption match {
                case Some(Decision.Weight(TopicDream.Placed.Into(k, v), p))
                    if p >= TopicDream.CardAt =>
                  (ts :+ Touch(k, Some(v), Vector.empty, Vector(c)), aside, g)
                case _ =>
                  (
                    ts,
                    aside.zip(sorted.asked.reply).map((a, _) => a :+ Lineage.Tag.Card(c.version)),
                    g
                  )
              }
          }
        }
    val tags = Trusted.asIs(aside).extract.distinct
    val kept =
      if (tags.isEmpty) Vector.empty
      else
        moves.keep(TopicDream.SetAside, aside.map(_.distinct))((tags, keeper, at) =>
          Aside.setAside(keeper, tags, at)
        ) match {
          case Left(e) => Vector(("aside", MoveError.said(e)))
          case Right(_) => Vector.empty
        }
    (
      touches,
      sorted.copy(aside = sorted.aside + tags.size, unwritten = sorted.unwritten ++ kept)
    )
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

  val Name: JobName = JobName.literal("topics-dream")

  /** 8: the most topics one run puts up for review. */
  val Reviewed: Int = 8

  /** 6,000: the most characters of a strand a write ask is shown. */
  val Chars: Int = 6000

  /** 1,500: the most characters of a strand a sort is shown. */
  val AttachChars: Int = 1500

  /** 400: the most characters a lead is asked to be. */
  val LeadChars: Int = 400

  private val Scan: MoveName = MoveName.literal("scan")

  private val SetAside: MoveName = MoveName.literal("aside")

  /** Where a sort may put a strand or a card: into a topic shown, by key and version; `Opened`,
    * a topic of its own; or set `Aside`.
    */
  private[topic] enum Placed extends caps.Pure {
    case Into(key: TopicKey, version: DocumentVersionId)
    case Opened, Aside
  }

  /** 0.72: the least probability of yes at which a card goes to the topic likeliest about it;
    * below it, the card is set aside. Provisional: it rests on agreement with the model ask it
    * replaced, until a person marks a night's card placements.
    */
  private[topic] val CardAt: Double = 0.72

  /** 0.86: the least probability at which a strand joins the topic chosen for it; below it, the
    * strand opens a topic of its own, counted unsure. Provisional: measured on discussions
    * against the topics they opened, an easier set than a real continuation.
    */
  private[topic] val TopicAt: Double = 0.86

  /** 700: the most characters of a topic's text a sort shows, its lead (at most [[LeadChars]])
    * and the head of its newest passage, and of a card's, its title and summary.
    */
  private[topic] val SortChars: Int = 700

  /** How a strand is sorted: one choice among the topics shown, each as its text's head within
    * [[SortChars]], then `new` (a topic of its own) and `aside`, the strand shown whole within
    * [[AttachChars]].
    */
  private[topic] val sortDiscussion: Sorting[Placed, Decision[Placed]] = Proven.right(
    Sorting
      .choice(
        // Draft wording, for Nick.
        "Which topic is this discussion part of? A topic is one matter the team comes back " +
          "to. Choose a topic only when the discussion is about that same matter, not only the " +
          "same project or the same people.",
        AttachChars,
        SortChars,
        Criterion(
          Placed.Opened,
          "new",
          // Draft wording, for Nick.
          Some("A matter none of these topics covers, that the team is likely to come back to.")
        ),
        Criterion(
          Placed.Aside,
          "aside",
          // Draft wording, for Nick.
          Some("Too slight to keep: a greeting, a test, a one-off question already answered.")
        )
      )
      // `new` and `aside` are two keys, neither a number.
      .left
      .map(_.toString)
  )

  /** How a card is sorted: a yes/no for each of the 8 topics shown whose text its own matches
    * best, each topic and the card as their text's head within [[SortChars]].
    */
  private[topic] val sortCard: Sorting[Placed, Vector[Decision.Weight[Placed]]] = Proven.right(
    Sorting.each(
      // Draft wording, for Nick.
      "Is this file about this topic's matter?",
      "It documents, specifies or holds data for the matter.",
      "It is about something else, or shares only a project or a word with it.",
      AttachChars,
      SortChars,
      // 8, within Sorting.Most.
      8
    )
  )

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

  // The tags the prompts name, as Dreaming.shown fences what an ask is shown.
  private val Discussion = Tag.Discussion.name
  private val Document = Tag.Document.name

  /** What a write ask is told, before what it is shown. */
  private def writeSystem(day: LocalDate): String =
    "You keep one topic of a team's knowledge: a durable subject, written as a lead and dated " +
      s"passages. You are shown the discussions and the cards attached to it on $day and the " +
      s"other cards it holds, each discussion a `<$Discussion>` of messages under their speakers' " +
      s"names and each card a `<$Document>` describing a file, its label in `label`; then, when " +
      s"the topic exists, its text in a `<$Document label=\"topic\">`: its lead, then its " +
      "passages newest first, each under " +
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
    Typed(Proven.right(JsonSchema.read(schema).left.map(_.toString)), c => read(c.json))

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
