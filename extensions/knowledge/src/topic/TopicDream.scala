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
import grit.core.stitch.Strand
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
  * (`write-{i}`) for its new lead and the day's passage and keeps it (`keep-{i}`); then sorts at
  * most `merges` topics of one discussion against the floor's others (`merge-{i}`), replying
  * the folds it would make and making none. It puts up at
  * most [[TopicDream.Reviewed]] of the topics it writes for review, new first. What it scans and
  * withdraws, what each ask is shown, how a reply is read and what a topic's version refers to:
  * the module's README, "A topics run". Replies [[Gathered.line]], which holds no
  * cost: its asks' cost is read from the spend ledger ([[grit.core.store.UsageLedger]]), each
  * recorded under its run's workflow. Its `limits`: asks `perRunMost`, judgments
  * [[TopicParams.Sorts]] + [[TopicScan.Cards]] + [[TopicParams.Merges]]; no calls.
  */
final class TopicDream(perRunMost: Int, zone: ZoneId, cards: Option[Exported[CardShelf]])
    extends DreamingJob[TopicParams] {

  val name: JobName = TopicDream.Name

  // 4: its asks are shown discussions and documents fenced, and told so (their digests moved).
  // 5: strands and cards are placed by sorts, a judgment each, not by one attach ask.
  // 6: a passage is withdrawn past keepFor or for its room's label, no longer for its strand's
  // messages gone (its scan's step reads no strands to prune).
  // 7: after its writes, its topics of one discussion are sorted to merge (`merge-{i}`) and set
  // aside (`merge-aside`); its scan records them and its params `merges`.
  // 8: a room's top level is cut into runs and its threads read whole (its params `cut` and
  // `sorts`, its scan `meetings`); a sort chooses new_topic, new_meeting or aside with no mark,
  // and a topic it opens is written at once (`open-{i}`, `keep-open-{i}`).
  // 9: a legacy Slack conversation no stitched strand holds is offered whole, as a thread is.
  // 10: a meeting's lead drops a line repeating its head.
  val version: Int = 10

  def write(params: TopicParams): ujson.Value = TopicParams.write(params)

  def read(params: ujson.Value): Either[String, TopicParams] = TopicParams.read(params)

  override val limits: MoveLimits =
    MoveLimits
      .of(perRunMost.max(0), 0, TopicParams.Sorts + TopicScan.Cards + TopicParams.Merges)
      .getOrElse(MoveLimits.Zero)

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
        p.sorts,
        if (end.isBefore(at)) end else at,
        day,
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
          None,
          Vector.empty
        )
        // Opens take at most half the run's writes, at least one of any; joins take the rest.
        val opens = if (most == 0) 0 else (most / 2).max(1)
        val (placed, opened, done) = attach(run, moves, scan, day, opens, begun)
        val meetings = scan.meetings.map(_.key).toSet ++ opened.collect {
          case (k, _, Topic.Kind.Meeting(_)) => k
        }
        // Each topic takes the cards its strands link, wherever those strands went.
        val touched = TopicDream
          .joined(scan.attached ++ placed)
          .map(t => t.copy(cards = (t.cards ++ scan.of(t.strands)._2).distinctBy(_.version)))
        val ordered = touched.filter(_.opened) ++ touched.filterNot(_.opened)
        val (now, later) = ordered.splitAt((most - opened.size).max(0))
        val kept = now.filterNot(_.opened).map(_.key)
        val (holds, read) =
          if (kept.isEmpty) (TopicScan.Holding(Vector.empty), done)
          else
            moves.scan(TopicDream.Holds, run.tainted.map(_ => kept))((keys, keeper, _, needs, at) =>
              TopicScan.holding(keys, if (end.isBefore(at)) end else at, shelf, keeper, needs)
            ) match {
              case Left(e) =>
                val why = (MoveName.value(TopicDream.Holds), MoveError.said(e))
                (TopicScan.Holding(Vector.empty), done.copy(unwritten = done.unwritten :+ why))
              case Right(h) => (h, done)
            }
        val (wrote, _, versions) = now.zipWithIndex
          .foldLeft(
            (
              read.copy(left = read.left + TopicDream.items(later)),
              opened.size.min(TopicDream.Reviewed),
              // Those opened tonight, oldest first, so a later write of one stands after it.
              opened.reverse.map(o => (o._1, o._2))
            )
          ) { case ((g, reviewed, versions), (t, i)) =>
            val review = reviewed < TopicDream.Reviewed
            written(
              run,
              moves,
              day,
              t,
              scan.of(t.strands)._1,
              holds.of(t.key),
              i,
              review,
              meetings.contains(t.key),
              g
            ) match {
              case (next, Some(v)) =>
                (next, if (review) reviewed + 1 else reviewed, versions :+ (t.key, v))
              case (next, None) => (next, reviewed, versions)
            }
          }
        merged(run, moves, scan, versions, wrote).line
    }
  }

  /** Each of `scan`'s strands, then each of its cards, placed by a sort of its own (`sort-{i}`,
    * numbered from 0 in that order). A strand's is shown, by ids alone, the topics attached to
    * by ref, the meetings near its day ([[Scanned.meetingsNear]]), those it shares something
    * with ([[TopicDream.Structural]]) and the newest ([[TopicDream.Newest]], none for a short
    * strand), then filled from every topic by text ([[TopicDream.Fill]], [[TopicDream.ShortFill]]
    * for a short one), the topics opened earlier in the run among them; it joins the topic
    * chosen, whatever its probability, and is set aside when `aside` is chosen. One chosen
    * `new_topic` or `new_meeting` opens a topic of that kind keyed by it ([[TopicKey.of]]),
    * written and kept at once (`open-{i}`, `keep-open-{i}`), so a later strand may join it; past
    * `opens` topics opened, it is left for the next run, counted in `budget`. A card's is the 8
    * topics its text matches best; it goes to the topic likeliest about it at
    * [[TopicDream.CardAt]] or more, and is otherwise set aside. With no topic kept, cards are
    * left and nothing is asked of them. Those set aside, with the cards their strands link, are
    * joined to the floor's [[Aside]] (`aside`); `g` counts them, those left (changed since the
    * scan, or a sort refused, named by its move), and each strand that could not open its topic
    * as not written. Its topics opened, newest first, come back beside the joins.
    */
  private def attach(
      run: Trusted[JobRun[TopicParams]],
      moves: Dreaming^,
      scan: Scanned,
      day: LocalDate,
      opens: Int,
      g: Gathered
  ): (Vector[Touch], Vector[(TopicKey, DocumentVersionId, Topic.Kind)], Gathered) = {
    val byRef = scan.attached.map(_.key)
    def into(topics: Vector[(TopicKey, DocumentVersionId)]) =
      topics.map((k, v) => (TopicDream.Placed.Into(k, v), v))
    // A strand's sort shows what it must, by ids alone, then fills from every topic by text.
    def among(
        st: Strand.Digested,
        topics: Vector[(TopicKey, DocumentVersionId)]
    ): Sorting.Among[TopicDream.Placed] = {
      val versions = topics.toMap
      val short = st.chars < TopicDream.ShortChars
      val newest = if (short) Vector.empty else topics.take(TopicDream.Newest).map(_._1)
      val meetings = scan.meetingsNear(LocalDate.ofInstant(st.touched, zone))
      val pinned = (byRef ++ meetings ++ scan.nearOf(st.id) ++ newest).distinct.flatMap(k =>
        versions.get(k).map(v => (TopicDream.Placed.Into(k, v), v))
      )
      Sorting.Among(
        pinned,
        into(topics),
        if (short) TopicDream.ShortFill else TopicDream.Fill - pinned.size
      )
    }
    // Where each goes decides only which topics are written; each written topic is believed
    // as its own write ask's reply is, so a sort's answer need not be checked.
    type Sorted =
      (
          Vector[Touch],
          Tainted[Vector[Lineage.Tag]],
          Gathered,
          Vector[(TopicKey, DocumentVersionId, Topic.Kind)]
      )
    def failed(s: Sorted, name: MoveName, e: Option[MoveError]): Sorted =
      (
        s._1,
        s._2,
        s._3.copy(
          left = s._3.left + 1,
          unwritten = s._3.unwritten ++ e.map(e => (MoveName.value(name), MoveError.said(e)))
        ),
        s._4
      )
    val discussion = run.tainted.map(_ => TopicDream.sortDiscussion)
    val strands = scan.strands.zipWithIndex.foldLeft[Sorted](
      (Vector.empty, run.tainted.map(_ => Vector.empty), g, Vector.empty)
    ) { case (s @ (ts, aside, g, opened), (st, i)) =>
      val name = CardMoves.numbered("sort", i)
      moves.sort(
        name,
        Dreaming.Item.Discussion(st),
        among(st, opened.map(o => (o._1, o._2)) ++ scan.topics),
        discussion
      ) match {
        case Left(e) => failed(s, name, Some(e))
        case Right(None) => failed(s, name, None)
        case Right(Some(sorted)) =>
          Trusted.asIs(sorted.asked.reply).extract.choice match {
            case TopicDream.Placed.Into(k, v) =>
              (ts :+ Touch(k, Some(v), Vector(st), Vector.empty), aside, g, opened)
            case TopicDream.Placed.Opened | TopicDream.Placed.Meets if opened.size >= opens =>
              // Past the run's opens, left whole for the next run rather than set aside.
              (ts, aside, g.copy(left = g.left + 1, budget = g.budget + 1), opened)
            case chosen @ (TopicDream.Placed.Opened | TopicDream.Placed.Meets) =>
              val kind = chosen match {
                case TopicDream.Placed.Meets => Topic.Kind.Meeting(Meeting.Opened)
                case _ => Topic.Kind.Subject
              }
              TopicKey.of(st) match {
                case Left(why) =>
                  (ts, aside, g.copy(unwritten = g.unwritten :+ ("a strand", why)), opened)
                case Right(k) =>
                  val (units, cards) = scan.of(Vector(st))
                  written(
                    run,
                    moves,
                    day,
                    Touch(k, None, Vector(st), cards, kind),
                    units,
                    Vector.empty,
                    i,
                    opened.size < TopicDream.Reviewed,
                    kind != Topic.Kind.Subject,
                    g,
                    ("open", "keep-open")
                  ) match {
                    case (g, Some(v)) => (ts, aside, g, (k, v, kind) +: opened)
                    case (g, None) => (ts, aside, g, opened)
                  }
              }
            case TopicDream.Placed.Aside =>
              // Set aside at its digest, so it is offered again once its thread grows.
              val tags = Lineage.Tag.Strand(st.root, st.room, st.digest, st.id.first) +:
                scan.of(Vector(st))._2.map(c => Lineage.Tag.Card(c.version))
              (ts, aside.zip(sorted.asked.reply).map((a, _) => a ++ tags), g, opened)
          }
      }
    }
    val card = run.tainted.map(_ => TopicDream.sortCard)
    // A card's nearest topics are those its text matches best, those opened tonight among them.
    val all = into(strands._4.map(o => (o._1, o._2)) ++ scan.topics)
    val near = Sorting.Among(Vector.empty, all, Sorting.Most)
    val (touches, aside, sorted, opened) =
      if (all.isEmpty) strands match {
        case (ts, aside, g, opened) => (ts, aside, g.copy(left = g.left + scan.cards.size), opened)
      }
      else
        scan.cards.zipWithIndex.foldLeft[Sorted](strands) {
          case (s @ (ts, aside, g, opened), (c, j)) =>
            val name = CardMoves.numbered("sort", scan.strands.size + j)
            moves.sort(name, Dreaming.Item.Document(c.version), near, card) match {
              case Left(e) => failed(s, name, Some(e))
              case Right(None) => failed(s, name, None)
              case Right(Some(sorted)) =>
                Trusted.asIs(sorted.asked.reply).extract.headOption match {
                  case Some(Decision.Weight(TopicDream.Placed.Into(k, v), p))
                      if p >= TopicDream.CardAt =>
                    (ts :+ Touch(k, Some(v), Vector.empty, Vector(c)), aside, g, opened)
                  case _ =>
                    (
                      ts,
                      aside.zip(sorted.asked.reply).map((a, _) => a :+ Lineage.Tag.Card(c.version)),
                      g,
                      opened
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
      opened,
      sorted.copy(aside = sorted.aside + tags.size, unwritten = sorted.unwritten ++ kept)
    )
  }

  /** `t`'s write ask, shown besides the cards it `holds`, and keep, the `i`th, put up for
    * review when `review`; the version kept, when it was.
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
      meeting: Boolean,
      g: Gathered,
      names: (String, String) = ("write", "keep")
  ): (Gathered, Option[DocumentVersionId]) = {
    val name = TopicKey.value(t.key)
    val ahead = run.extract.params.keepFor.toDays
    val summary = Dreaming.Summary(
      TopicDream.writeSystem(day, meeting),
      TopicDream.Chars,
      // The whole topic, bounded by Topic.Passages.
      Int.MaxValue,
      TopicDream.WriteReply
    )
    moves.summarise(
      CardMoves.numbered(names._1, i),
      t.strands,
      t.version.toVector ++ (t.cards.map(_.version) ++ holds).distinct,
      run.tainted.map(_ => summary)
    ) match {
      case Left(e) => (g.copy(unwritten = g.unwritten :+ (name, MoveError.said(e))), None)
      case Right(None) => (g.copy(left = g.left + TopicDream.items(Vector(t))), None)
      case Right(Some(s)) if t.version.exists(v => !s.shown.contains(v)) =>
        (g.copy(left = g.left + TopicDream.items(Vector(t))), None)
      case Right(Some(s)) =>
        val shownCards = t.cards.filter(c => s.shown.contains(c.version))
        val touch = t.copy(cards = shownCards)
        moves.keep(CardMoves.numbered(names._2, i), s.asked.reply)((w, keeper, at) =>
          TopicDream.kept(touch, units, day, w, review, ahead, keeper, at)
        ) match {
          case Left(e) => (g.copy(unwritten = g.unwritten :+ (name, MoveError.said(e))), None)
          case Right(Kept(None, _)) => (g.copy(left = g.left + TopicDream.items(Vector(t))), None)
          case Right(Kept(Some(v), skipped)) =>
            (
              g.copy(
                written = g.written + 1,
                opened = g.opened + (if (t.opened) 1 else 0),
                skipped = g.skipped + skipped
              ),
              Some(v)
            )
        }
    }
  }

  /** Each of `scan`'s topics of one discussion ([[Scanned.single]]) not written this run (not
    * among `wrote`), oldest first, sorted (`merge-{i}`, numbered from 0 in that order) as a
    * document against the floor's other topics at their versions now (`wrote`'s for those
    * written this run), the [[TopicDream.sortTopic]] most its text matches best, but those an
    * earlier sort of the run proposed to fold away; a topic proposed as another's keeper, or
    * with no other topic to be shown, is not sorted. A yes at [[TopicDream.FoldAt]] or more
    * for the likeliest proposes folding the topic into it, held in `g`'s `proposed`; nothing is
    * withdrawn. Each topic sorted, and each proposed as a keeper, is set aside at its version
    * (`merge-aside`), so no run sorts it again within `asideFor` unless it is written; a sort or
    * that keep refused is named in `g` as not written.
    */
  private def merged(
      run: Trusted[JobRun[TopicParams]],
      moves: Dreaming^,
      scan: Scanned,
      wrote: Vector[(TopicKey, DocumentVersionId)],
      g: Gathered
  ): Gathered = {
    val now = scan.topics.map((k, v) => wrote.find(_._1 == k).getOrElse((k, v))) ++
      wrote.filterNot((k, _) => scan.topics.exists(_._1 == k))
    val sorting = run.tainted.map(_ => TopicDream.sortTopic)
    type Merging = (Vector[Gathered.Fold], Tainted[Vector[Lineage.Tag]], Vector[(String, String)])
    val (proposed, aside, refused) = scan.single
      .filterNot((k, _) => wrote.exists(_._1 == k))
      .zipWithIndex
      .foldLeft[Merging]((Vector.empty, run.tainted.map(_ => Vector.empty), Vector.empty)) {
        case (m @ (folds, aside, refused), ((k, v), i)) =>
          val pool = now.collect {
            case (c, cv) if c != k && !folds.exists(_.from == c) =>
              (new TopicDream.Placed.Into(c, cv), cv)
          }
          if (pool.isEmpty || folds.exists(_.into == k)) m
          else {
            val name = CardMoves.numbered("merge", i)
            val among = Sorting.Among(Vector.empty, pool, TopicDream.sortTopic.most)
            moves.sort(name, Dreaming.Item.Document(v), among, sorting) match {
              case Left(e) => (folds, aside, refused :+ (MoveName.value(name), MoveError.said(e)))
              case Right(None) => m
              case Right(Some(sorted)) =>
                def set(tags: Lineage.Tag*) =
                  aside.zip(sorted.asked.reply).map((a, _) => a ++ tags)
                Trusted.asIs(sorted.asked.reply).extract.headOption match {
                  case Some(Decision.Weight(TopicDream.Placed.Into(into, iv), p))
                      if p >= TopicDream.FoldAt =>
                    // Its keeper would hold two discussions once folded, so it is set aside too.
                    (
                      folds :+ Gathered.Fold(k, into, p),
                      set(Lineage.Tag.Topic(v), Lineage.Tag.Topic(iv)),
                      refused
                    )
                  case _ => (folds, set(Lineage.Tag.Topic(v)), refused)
                }
            }
          }
      }
    val kept =
      if (Trusted.asIs(aside).extract.isEmpty) Vector.empty
      else
        moves.keep(TopicDream.MergeAside, aside.map(_.distinct))((tags, keeper, at) =>
          Aside.setAside(keeper, tags, at)
        ) match {
          case Left(e) => Vector((MoveName.value(TopicDream.MergeAside), MoveError.said(e)))
          case Right(_) => Vector.empty
        }
    g.copy(proposed = proposed, unwritten = g.unwritten ++ refused ++ kept)
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

  private val MergeAside: MoveName = MoveName.literal("merge-aside")

  private val Holds: MoveName = MoveName.literal("holds")

  /** Where a sort may put a strand or a card: into a topic shown, by key and version; a topic
    * of its own, a subject (`Opened`) or a meeting (`Meets`); or set `Aside`.
    */
  private[topic] enum Placed extends caps.Pure {
    case Into(key: TopicKey, version: DocumentVersionId)
    case Opened, Meets, Aside
  }

  /** 0.72: the least probability of yes at which a card goes to the topic likeliest about it;
    * below it, the card is set aside. Provisional: it rests on agreement with the model ask it
    * replaced, until a person marks a night's card placements.
    */
  private[topic] val CardAt: Double = 0.72

  /** 0.90: the least probability of yes at which a topic of one discussion would fold into the
    * topic its merge sort finds likeliest the same subject; below it, the topic is set aside at
    * its version. Provisional, until a person marks a night's proposals.
    */
  private[topic] val FoldAt: Double = 0.90

  /** 16: the most topics a strand's sort is shown for what they share with it
    * ([[TopicScan.Scanned.near]]), after those attached to by ref.
    */
  private[topic] val Structural: Int = 16

  /** 8: the newest topics a strand's sort is shown, after those it shares something with,
    * unless the strand is short ([[ShortChars]]).
    */
  private[topic] val Newest: Int = 8

  /** 32: the most topics a strand's sort shows once filled with those its text matches best:
    * the placement spike found a choice over 32 exactly as accurate as one over 64. Those it
    * must show may take it past 32, up to [[Sorting.Chosen]]; the text never does.
    */
  private[topic] val Fill: Int = 32

  /** 150: under this many characters ([[grit.core.stitch.Strand.Digested.chars]]) a strand is
    * short: its sort is shown the topics attached by ref and those it shares something with,
    * then at most [[ShortFill]] its text matches best, and none for being newest. A short
    * strand's words are few, so the newest crowd out its matches; measured, this matched a
    * choice over 64 at half the tokens.
    */
  private[topic] val ShortChars: Int = 150

  /** 16: the most topics a short strand's sort is filled with by text ([[ShortChars]]). */
  private[topic] val ShortFill: Int = 16

  /** 700: the most characters of a topic's text a sort shows, its lead (at most [[LeadChars]])
    * and the head of its newest passage, and of a card's, its title and summary.
    */
  private[topic] val SortChars: Int = 700

  /** How a strand is sorted: one choice among the topics shown, each as its text's head within
    * [[SortChars]], then `new_topic`, `new_meeting` and `aside`, the strand shown whole within
    * [[AttachChars]]; the choice stands whatever its probability.
    */
  private[topic] val sortDiscussion: Sorting[Placed, Decision[Placed]] = Proven.right(
    Sorting
      .choice(
        // W2, as a replay of two labelled rooms measured it.
        "Which document does this conversation from a team's chat room belong to? Each " +
          "document is a topic or a meeting. A topic is an ongoing subject of work. A meeting is one planned occurrence: a call or " +
          "get-together on a particular day. Talk that plans, moves, or reports on a specific " +
          "meeting belongs to that meeting, not to the topic it will discuss.",
        AttachChars,
        SortChars,
        Sorting.Chosen,
        Criterion(
          Placed.Opened,
          "new_topic",
          // Draft wording, for Nick.
          Some(
            "A subject of work none of these documents covers, that the team is likely to " +
              "come back to."
          )
        ),
        Criterion(
          Placed.Meets,
          "new_meeting",
          // Draft wording, for Nick.
          Some(
            "A planned meeting none of these documents is: a call or get-together on a " +
              "particular day."
          )
        ),
        Criterion(
          Placed.Aside,
          "aside",
          // Draft wording, for Nick.
          Some(
            "Too slight to keep: chatter, a greeting, a joke, or nothing the team will come " +
              "back to."
          )
        )
      )
      // Three keys, none a number, and Chosen is within its bounds.
  )

  /** How a card is sorted: a yes/no for each of the 8 topics shown whose text its own matches
    * best, each topic and the card as their text's head within [[SortChars]].
    */
  private[topic] val sortCard: Sorting[Placed, Vector[Decision.Weight[Placed]]] = yesNo(
    // Draft wording, for Nick.
    "Is this file about this topic's matter?",
    "It documents, specifies or holds data for the matter.",
    "It is about something else, or shares only a project or a word with it."
  )

  /** How a topic of one discussion is sorted to merge: a yes/no for each of the 8 other topics
    * shown whose text its own matches best, each topic as its text's head within [[SortChars]].
    */
  private[topic] val sortTopic: Sorting[Placed.Into, Vector[Decision.Weight[Placed.Into]]] =
    yesNo(
      // Draft wording, for Nick.
      "Is this document about the same subject as this topic, so one page should hold both?",
      "They are one subject: a reader of either needs what the other says.",
      "They are different subjects, or share only a project, a person or a word."
    )

  /** A yes/no sort asking `instructions` of the 8 candidates its item's text matches best,
    * each and the item as their text's head within [[SortChars]].
    */
  private def yesNo[C <: caps.Pure](
      instructions: String,
      yes: String,
      no: String
  ): Sorting[C, Vector[Decision.Weight[C]]] =
    // 8, within Sorting.Most.
    Proven.right(Sorting.each(instructions, yes, no, AttachChars, SortChars, 8))

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
              (a.strands ++ b.strands).distinctBy(_.id),
              (a.cards ++ b.cards).distinctBy(_.version),
              a.kind
            )
          )
      )

  // The tags the prompts name, as Dreaming.shown fences what an ask is shown.
  private val Discussion = Tag.Discussion.name
  private val Document = Tag.Document.name

  /** What a write ask is told, before what it is shown: of a `meeting`, how to give its head. */
  private def writeSystem(day: LocalDate, meeting: Boolean): String =
    writeTopic(day) + " " + (if (meeting) {
                               "This topic is one meeting. Give `meeting`: its `state`, " +
                                 "`proposed` until a day is agreed, `planned` once it is, " +
                                 "`held` once the messages say it took place; its `day` " +
                                 "(yyyy-mm-dd) and `start` (HH:MM) when the messages fix " +
                                 "them, else null; and `who`: everyone taking part, those " +
                                 "the topic already names included, as the messages write " +
                                 "them, else null."
                             } else "Give `meeting` null.")

  private def writeTopic(day: LocalDate): String =
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
        ),
        "meeting" -> ujson.Obj(
          "type" -> ujson.Arr("object", "null"),
          "description" -> "A meeting's head; null for any other topic.",
          "properties" -> ujson.Obj(
            "state" -> ujson.Obj(
              "type" -> ujson.Arr("string", "null"),
              "description" -> "proposed, planned or held"
            ),
            "day" -> ujson.Obj(
              "type" -> ujson.Arr("string", "null"),
              "description" -> "yyyy-mm-dd"
            ),
            "start" -> ujson.Obj(
              "type" -> ujson.Arr("string", "null"),
              "description" -> "HH:MM"
            ),
            "who" -> ujson.Obj(
              "type" -> ujson.Arr("array", "null"),
              "items" -> ujson.Obj("type" -> "string")
            )
          ),
          "required" -> ujson.Arr("state", "day", "start", "who"),
          "additionalProperties" -> false
        )
      ),
      "required" -> ujson.Arr("lead", "passage", "meeting"),
      "additionalProperties" -> false
    ),
    Wrote.read
  )

  private def typed[T](schema: ujson.Value, read: ujson.Value -> Either[String, T]): Typed[T] =
    // A literal in JsonSchema's subset but for the descriptions, which are text: every test of
    // a run throws here if not.
    Typed(Proven.right(JsonSchema.read(schema).left.map(_.toString)), c => read(c.json))

  /** `w` kept as `t`'s topic for `day`, referring besides to the `units` its strands link, put
    * up for review when `review` and written; a meeting's head refined by `w` ([[Meeting.refined]], days up to `ahead` after `day`), a subject's kind left as it opened.
    */
  private def kept(
      t: Touch,
      units: Vector[Ref],
      day: LocalDate,
      w: Wrote,
      review: Boolean,
      ahead: Long,
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
        val tags = t.strands.map(s => Lineage.Tag.Strand(s.root, s.room, s.digest, s.id.first)) ++
          t.cards.map(c => Lineage.Tag.Card(c.version))
        val passages = old.fold(Vector.empty[Passage])(_.topic.passages)
        // The day's passage replaces one the topic holds for that day, its lineage joined.
        val passage = Passage(day, Lineage.of(tags), TopicWrite.prose(w.passage))
        val before = old.flatMap(_.today).filter(_.day == day)
        // Fixed when it opens; a meeting's head refined by the reply, a subject's left be.
        val (kind, heads) = old.fold(t.kind)(_.topic.kind) match {
          case Topic.Kind.Meeting(m) =>
            val now = Meeting.refined(m, w.meeting, day, ahead)
            (Topic.Kind.Meeting(now), Set(Meeting.head(m), Meeting.head(now)))
          case Topic.Kind.Subject => (Topic.Kind.Subject, Set.empty[String])
        }
        // A write ask is shown the topic's text, head first, and may echo the head in its lead:
        // the text puts the head atop the lead itself.
        val lead = w.lead.linesIterator.filterNot(l => heads.contains(l.trim)).mkString("\n").trim
        val held = Held(
          Topic.of(
            t.key,
            TopicWrite.prose(lead),
            Topic.joined(passages, passage)((_, now) => now),
            kind
          ),
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

/** A write ask's reply: the topic's new `lead` and the day's `passage`, in markdown, and the
  * `meeting` head it gives, as [[Meeting.refined]] reads it, when it gives one.
  */
private[topic] final case class Wrote(lead: String, passage: String, meeting: Option[ujson.Value])
    extends caps.Pure

private[topic] object Wrote {
  def read(v: ujson.Value): Either[String, Wrote] =
    (for {
      o <- v.objOpt
      lead <- o.get("lead").flatMap(_.strOpt)
      passage <- o.get("passage").flatMap(_.strOpt)
    } yield Wrote(lead, passage, o.get("meeting").filterNot(_.isNull)))
      .toRight("a reply holds `lead` and `passage`, each a string")
}
