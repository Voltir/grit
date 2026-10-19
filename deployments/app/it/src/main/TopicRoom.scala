package grit.app.main

import java.time.temporal.ChronoUnit
import java.time.{Duration, Instant, LocalDate, LocalTime, ZoneOffset}

import scala.concurrent.duration.*

import grit.core.document.{DocLabel, DocWeight, DocumentTerms, Pointer}
import grit.core.id.{
  ConversationId,
  CorpusName,
  Declarer,
  DocumentVersion,
  PluginName,
  ScheduleId,
  ScheduleKey
}
import grit.core.job.{Declared, Grace, Slot, SlotRule}
import grit.core.message.{AssistantBlock, Message}
import grit.core.need.PluginReads
import grit.core.place.Place
import grit.core.plugin.{DocumentPosting, Documents, Exports, Plugin}
import grit.core.spend.DailyCap
import grit.core.stitch.StrandShelf
import grit.core.store.{Entry, Payload}
import grit.core.tool.ToolName
import grit.core.visibility.{Integrity, Label, Subject, Visibility}
import grit.dbos.engine.{Engine, LiveEngine}
import grit.dbos.sql.DbConfig
import grit.eval.{Room, TopicScore}
import grit.kit.run.Launch
import grit.knowledge.card.{Card, CardShelf, CardSummary, UnitVersion}
import grit.knowledge.plugin.TopicDocuments
import grit.knowledge.ref.{CardKey, Ref, TopicKey}
import grit.knowledge.topic.{Aside, Lineage, Passage, Topic, TopicDream, TopicParams}

/** A real topics run over a room of the eval's shape (`eval/resources/rooms`), one run per past
  * day, scored against the room's truth: run by hand, never by the suites.
  *
  * Args: the room's JSON file, the day's cap in USD, then optionally the most topics a run
  * writes (default [[TopicDocuments.PerRun]]). The database is `GRIT_DATABASE_*`'s, which must
  * be empty and is left as the runs leave it; OpenRouter's key and the models' variables come
  * from the environment over `GRIT_ENV_FILE` (`.env` when unset). The room is moved whole days
  * so its last day is yesterday (UTC), its messages written as [[RoomRun.seeded]] writes them,
  * and its cards kept by a plugin `knowledge` exporting them, each a Drive file's under its id,
  * written when the room says. The topics plugin at public then runs `topics-dream` once for
  * each of the room's days in order, each due a minute after the last finished, reading the
  * strands of the [[TopicDocuments.Recent]] before that day's end and every card. Prints, by
  * shape alone: each day's run, how its schedule ended, the topics then current and its reply
  * line; the score at opening level ([[TopicScore]]), each opening placed in the topics
  * referring to a strand holding it, each card in those referring to its file; each truth item
  * sharing no topic with another of its truth topic, by id, and where it went (the topics
  * holding it, set aside, or left); each topic holding two truth topics, by key, each truth
  * topic with the day it entered and whether it came in the group that opened the topic or
  * joined it later; and the day's spend.
  */
object TopicRoom {

  def main(args: Array[String]): Unit =
    run(args.toVector).left.foreach { why =>
      System.err.println(s"topic-room: $why")
      sys.exit(1)
    }

  import RoomRun.right

  private val Topics = PluginName.of("topics").fold(sys.error, identity)

  private val Cards = PluginName.of("knowledge").fold(sys.error, identity)

  private def key(i: Int): ScheduleKey =
    ScheduleKey.of(s"topics-room-$i").fold(sys.error, identity)

  /** The longest one day's run is waited for. */
  private val Longest: Duration = Duration.ofMinutes(5)

  private def run(args: Vector[String]): Either[String, Unit] =
    for {
      file <- args.headOption.toRight("no room file")
      cap <- args.lift(1).toRight("no cap").flatMap(DailyCap.of)
      perRun <- args
        .lift(2)
        .fold(Right(TopicDocuments.PerRun))(n =>
          n.toIntOption.filter(_ > 0).toRight(s"no number of topics: $n")
        )
      env <- RoomRun.env
      config <- DbConfig.fromEnv(env).left.map(_.toString)
      room <- RoomRun.room(file)
      days = (room.messages.map(_.at) ++ room.cards.map(_.at))
        .map(LocalDate.ofInstant(_, ZoneOffset.UTC))
        .distinct
        .sorted
      last <- days.lastOption.toRight("an empty room")
      moved = ChronoUnit.DAYS.between(last, LocalDate.now(ZoneOffset.UTC).minusDays(1))
      cards = Option.when(room.cards.nonEmpty)(RoomCards)
      topics <- TopicDocuments.of(
        Topics,
        Vector(Label.Public),
        LocalTime.of(5, 0),
        ZoneOffset.UTC,
        Visibility.Shipped,
        cards,
        None
      )
      dream <- topics.jobs.collectFirst { case d: TopicDream => d }.toRight("no topics job")
      params = (d: LocalDate) =>
        TopicParams(perRun, TopicDocuments.Recent, (days.size + 2).days, Some(d), None)
      day = (i: Int, at: Instant) =>
        RoomRun.deployment(
          env,
          Vector(DayRun(topics, dream, i, at, params(days(i).plusDays(moved)))) ++ cards,
          cap
        )
      firstAt = Instant.now().plusSeconds(60)
      first <- day(0, firstAt)
    } yield {
      val (deployment, secrets) = first
      val engine = LiveEngine.open(
        config,
        deployment.runsUnder,
        deployment.budget,
        deployment.visibility
      )
      try {
        val conversations = RoomRun.seeded(engine, room, Duration.ofDays(moved))
        Launch(engine, deployment, secrets, Launch.Run.Served, sweeping = true, println)
        cardsKept(engine, room, Duration.ofDays(moved))
        val terms = topics.documents.map(_.terms).getOrElse(sys.error("the topics' terms"))
        days.indices.foreach { i =>
          val at = if (i == 0) firstAt else Instant.now().plusSeconds(60)
          val ran =
            if (i == 0) Right(deployment)
            else day(i, at).map(_._1)
          ran match {
            case Left(why) => println(s"day ${i + 1}: not run: $why")
            case Right(d) =>
              if (i > 0) right(Launch.reconcile(engine.schedules, engine.jot, d, Instant.now()))
              println(
                s"day ${i + 1} (${days(i).plusDays(moved)}): ${waited(engine, i)}; " +
                  s"${current(engine, terms).size} topics; spent ${RoomRun.spent(engine)}"
              )
              println(s"  replied: ${replied(engine, i, at)}")
          }
        }
        val truth = TopicScore.truth(room, ZoneOffset.UTC)
        val where = placed(engine, terms, room, conversations)
        val score = TopicScore.of(truth, where.map((id, w) => id -> w.topics))
        def rate(items: Vector[(String, Boolean)]) = s"${items.count(_._2)}/${items.size}"
        println(
          s"precision ${rate(score.precision)}, recall ${rate(score.recall)}, " +
            s"splits ${score.splits}, merges ${score.merges}, mergeable ${score.mergeable}, " +
            s"cross-day ${score.crossDay}, unplaced ${score.unplaced}"
        )
        missed(truth, where).foreach(println)
        merged(truth, where, opened(engine, terms)).foreach(println)
        println(s"spent ${RoomRun.spent(engine)}")
      } finally engine.close()
    }

  /** The topics plugin, its one schedule `topics-room-{i}`, once `at`, with `params`. */
  private final class DayRun(
      topics: TopicDocuments,
      dream: TopicDream,
      i: Int,
      at: Instant,
      params: TopicParams
  ) extends Plugin {
    val name: PluginName = topics.name
    val version: Int = topics.version
    override val needs = topics.needs
    override val documents: Option[Documents] = topics.documents
    override val jobs = Vector(dream)
    override val schedules: Vector[Declared[?]] = Vector(
      Declared(
        key(i),
        dream,
        SlotRule.Once(at, Grace.of(10.minutes).getOrElse(Grace.Zero)),
        params,
        Label.Public
      )
    )
  }

  /** The cards' plugin: their documents, exported as the knowledge plugin exports its own. */
  private object RoomCards extends Exports[CardShelf] {
    val name: PluginName = Cards
    val version: Int = 1
    override val documents: Option[Documents] = Some(new Documents {
      val terms: DocumentTerms = DocLabel
        .of("knowledge card")
        .flatMap(DocumentTerms.of(_, DocWeight.Unscaled, 30.days, 100))
        .fold(sys.error, identity)
      val posting: Option[DocumentPosting] = None
    })
    def service(own: PluginReads): CardShelf = CardShelf.over(own.documents)
  }

  /** Each of `room`'s cards kept, `shift` later, as a Drive file's under its id. */
  private def cardsKept(engine: Engine^, room: Room, shift: Duration): Unit =
    RoomCards.documents.foreach { d =>
      room.cards.foreach { c =>
        val card = (for {
          corpus <- CorpusName.of("room")
          version <- UnitVersion.of(c.at.plus(shift).toString)
          summary <- CardSummary.of(c.text, Vector.empty)
          tool <- ToolName.of("drive_read")
          pointer <- Pointer.of(tool, "file" -> ujson.Str(c.id))
        } yield Card.file(corpus, c.id, version, summary, pointer, Some(c.title)))
          .fold(sys.error, identity)
        val _ = right(engine.jot.write(Subject.Public) {
          engine
            .keeper(Cards, d.terms)
            .write(
              card.docKey,
              Label.Public,
              Integrity.Member,
              Place.Everywhere,
              card.text,
              card.data,
              Some(card.pointer),
              c.at.plus(shift)
            )
        })
      }
    }

  /** How day `i`'s schedule ended, waited for at most [[Longest]]. */
  private def waited(engine: Engine^, i: Int): String = {
    val id = ScheduleId.declared(Declarer.Plugin(Topics), key(i))
    val until = Instant.now().plus(Longest)
    def ended = right(engine.db.read(Subject.Public)(engine.schedules.read(id))).flatMap(_.ended)
    while ((ended.isEmpty || !engine.unfinished().contains(0)) && Instant.now().isBefore(until))
      Thread.sleep(2000)
    ended.fold("not ended")(e => s"ended ${e.word}")
  }

  /** The topics current at public, by key. */
  private def current(engine: Engine^, terms: DocumentTerms): Vector[String] =
    right(engine.db.read(Subject.Public)(engine.keeper(Topics, terms).newest(Int.MaxValue)))
      .flatMap(d => TopicKey.read(d.key).map(TopicKey.value))

  /** Day `i`'s run's reply, its schedule's once slot due `at`; or that it has none. */
  private def replied(engine: Engine^, i: Int, at: Instant): String = {
    val slot = Slot(ScheduleId.declared(Declarer.Plugin(Topics), key(i)), Slot.kept(at))
    right(engine.db.read(Subject.Public) {
      engine.conversations
        .find(slot.origin(TopicDream.Name))
        .flatMap(_.fold(Right(Vector.empty))(c => engine.entries.list(c.id)))
    }).reverseIterator
      .collectFirst { case Entry(_, _, _, _, _, Payload.Message(a: Message.Assistant), _, _) =>
        a.blocks.collect { case AssistantBlock.Text(t) => t }.mkString
      }
      .getOrElse("no reply")
  }

  /** Where a truth item went: an `opening` or a card, in `topics` (by key), and whether it is
    * in its floor's aside at any digest or version (`aside`).
    */
  private final case class Where(
      opening: Boolean,
      topics: Set[String],
      aside: Boolean,
      entered: Map[String, LocalDate]
  )

  /** Each topic holding items of two truth topics or more: `merge {key} (opened {day}):`, then
    * for each truth topic the day its first item entered, and `in the new group` when that is
    * the day the topic opened (the attach ask's `new` group held both), else `joined` (a group
    * placed it in the topic as it stood).
    */
  private def merged(
      truth: Vector[TopicScore.Truth],
      where: Map[String, Where],
      opened: Map[String, LocalDate]
  ): Vector[String] = {
    val topical = truth.collect { case TopicScore.Truth(id, Some(t), _) => id -> t }
    where.values.flatMap(_.topics).toVector.distinct.sorted.flatMap { key =>
      val held = topical
        .flatMap((id, t) => where.get(id).flatMap(_.entered.get(key)).map(t -> _))
        .groupMapReduce(_._1)(_._2)((a, b) => if (a.isBefore(b)) a else b)
      Option.when(held.size > 1) {
        val day = opened.get(key)
        s"merge $key (opened ${day.fold("?")(_.toString)}): " + held.toVector
          .sortBy(_._2)
          .map((t, d) => s"$t on $d ${if (day.contains(d)) "in the new group" else "joined"}")
          .mkString(", ")
      }
    }
  }

  /** Each topical item of `truth` that shares no topic with one of its truth topic's others:
    * `missed {id} ({opening|card} of {topic}): {where}`, where is the topics holding it, `set
    * aside`, or `left`.
    */
  private def missed(truth: Vector[TopicScore.Truth], where: Map[String, Where]): Vector[String] = {
    val topical = truth.collect { case TopicScore.Truth(id, Some(t), _) => id -> t }
    def in(id: String) = where.get(id).fold(Set.empty[String])(_.topics)
    topical.collect {
      case (id, t)
          if topical.exists((o, ot) => o != id && ot == t && (in(id) intersect in(o)).isEmpty) =>
        val w = where.get(id)
        val kind = if (w.forall(_.opening)) "opening" else "card"
        val went =
          if (in(id).nonEmpty) in(id).toVector.sorted.mkString("in ", ", ", "")
          else if (w.exists(_.aside)) "set aside"
          else "left"
        s"missed $id ($kind of $t): $went"
    }
  }

  /** Each opening, by id, in the topics referring to a strand holding its conversation; each
    * card in those referring to its file; each with whether the aside holds it.
    */
  private def placed(
      engine: Engine^,
      terms: DocumentTerms,
      room: Room,
      conversations: Map[String, ConversationId]
  ): Map[String, Where] = {
    val now = Instant.now()
    val aside = right(
      engine.db.read(Subject.Public)(Aside.read(engine.keeper(Topics, terms), Label.Public))
    ).tags
    val cardTerms = RoomCards.documents.map(_.terms).getOrElse(sys.error("the cards' terms"))
    def cardVersion(id: String): Option[DocumentVersion] =
      CorpusName
        .of("room")
        .toOption
        .flatMap(corpus =>
          right(
            engine.db.read(Subject.Public)(
              engine
                .keeper(Cards, cardTerms)
                .current(CardKey.docKey(CardKey.of(corpus, id)), Label.Public)
            )
          )
        )
        .map(_.version)
    def referring(r: Ref): Set[String] =
      Ref
        .written(r)
        .toOption
        .toVector
        .flatMap { w =>
          right(engine.db.read(Subject.Public)(engine.keeper(Topics, terms).referring(w, 1000)))
            .flatMap(d => TopicKey.read(d.key).map(TopicKey.value))
        }
        .toSet
    val strands = right(
      engine.db.read(Subject.Public)(
        StrandShelf
          .over(engine.stitches, engine.conversations)
          .atFloor(now.minus(Duration.ofDays(30)), now)
      )
    )
    // Each topic's passages, oldest first, by key.
    val passages: Map[String, Vector[Passage]] =
      kept(engine, terms).map((k, t) => k -> t.passages.sortBy(_.day))
    // The day of each topic's first passage holding a tag `holds` accepts.
    def entered(holds: Lineage.Tag => Boolean): Map[String, LocalDate] =
      passages.flatMap((k: String, ps: Vector[Passage]) =>
        ps.find(_.from.tags.exists(holds)).map(p => k -> p.day)
      )
    val byStrand = strands.map(s => s -> referring(Ref.Strand(s.root)))
    val openings = conversations.map { (id, c) =>
      val holding = byStrand.filter(_._1.members.contains(c))
      def ofHolding(tag: Lineage.Tag) = tag match {
        case Lineage.Tag.Strand(root, _, _) => holding.exists(_._1.root == root)
        case _ => false
      }
      id -> Where(
        true,
        holding.flatMap(_._2).toSet,
        aside.exists(ofHolding),
        entered(ofHolding)
      )
    }
    openings ++ room.cards.map { c =>
      val version = cardVersion(c.id)
      def ofCard(tag: Lineage.Tag) = version.exists(v => tag == Lineage.Tag.Card(v))
      c.id -> Where(false, referring(Ref.Drive(c.id)), aside.exists(ofCard), entered(ofCard))
    }
  }

  /** The day each current topic's first passage is for, by key. */
  private def opened(engine: Engine^, terms: DocumentTerms): Map[String, LocalDate] =
    kept(engine, terms).flatMap((k, t) => t.passages.map(_.day).minOption.map(k -> _))

  /** The topics current at public, read, by key; one whose data does not read is left out. */
  private def kept(engine: Engine^, terms: DocumentTerms): Map[String, Topic] =
    right(engine.db.read(Subject.Public)(engine.keeper(Topics, terms).newest(Int.MaxValue)))
      .flatMap(d =>
        TopicKey
          .read(d.key)
          .flatMap(k => Topic.read(k, d.data).toOption.map(t => TopicKey.value(k) -> t))
      )
      .toMap
}
