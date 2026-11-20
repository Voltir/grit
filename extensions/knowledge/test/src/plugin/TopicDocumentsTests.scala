package grit.knowledge.plugin

import java.time.{Instant, LocalDate, LocalTime, ZoneId, ZoneOffset}

import scala.util.chaining.*

import grit.core.clock.SetClock
import grit.core.document.DocLabel
import grit.core.flow.TestTainted
import grit.core.id.{PluginName, ScheduleKey, TestCallSlots, ToolCallId}
import grit.core.job.{Grace, InMemorySchedules, OwnJobs, ScheduleDesk, SlotRule}
import grit.core.message.AssistantBlock
import grit.core.need.{Needs, PluginReads}
import grit.core.place.Place
import grit.core.plugin.{Exports, InMemoryPlugins, PluginTool}
import grit.core.store.{Db, StoreError, Tx}
import grit.core.tool.{Bound, CallError, Outcome, Repairs, ToolName, Toolbox}
import grit.core.visibility.{Clearance, Integrity, Label, Level, Subject, TestLabels, Visibility}
import grit.dbos.sql.TestTx
import grit.knowledge.card.CardShelf
import grit.knowledge.topic.TestMeetings.{Kept, meeting, text}
import grit.knowledge.topic.{Meeting, MeetingAhead, MeetingLookup, TopicDream, TopicParams}

import utest.*

/** The topics plugin as a deployment declares it: its schedules, its needs and its documents'
  * terms.
  */
object TopicDocumentsTests extends TestSuite {

  private val Name: PluginName = PluginName.of("topics").fold(sys.error, identity)

  private val At: LocalTime = LocalTime.of(5, 0)

  private def once(at: Instant): SlotRule = SlotRule.Once(at, Grace.Zero)

  private def of(
      labels: Vector[Label],
      visibility: Visibility = Visibility.Shipped,
      cards: Option[Exports[CardShelf]] = None,
      seedUntil: Option[java.time.LocalDate] = None,
      foldAt: Option[Double] = None,
      aheadIn: Vector[Place] = Vector.empty
  ) =
    TopicDocuments.of(
      Name,
      labels,
      At,
      ZoneOffset.UTC,
      visibility,
      cards,
      seedUntil,
      foldAt,
      aheadIn
    )

  private val Internal: Label = Label.at(Level.Internal)
  private val Secret: Label = Label.at(Level.Confidential)

  private def planned(d: Int): Meeting.State =
    Meeting.State.Planned(Meeting.When(LocalDate.of(2026, 11, d), None))

  /** The plugin at internal, its days in a zone other than UTC. */
  private val plugin: TopicDocuments = TopicDocuments
    .of(
      Name,
      Vector(Internal),
      At,
      ZoneId.of("Pacific/Auckland"),
      Visibility.Shipped,
      None,
      None,
      None
    )
    .fold(sys.error, identity)

  private final class FakeDb(clearance: Label) extends Db {
    def read[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using TestTx.fake(Clearance.of(clearance)))
  }

  /** `plugin`'s `meetings` called with `args` by a turn cleared at `clearance`, over `kept`. */
  private def call(kept: Kept, clearance: Label, args: (String, ujson.Value)*): Outcome = {
    val db: Db^ = new FakeDb(clearance)
    val desk: ScheduleDesk^ =
      new InMemorySchedules().desk(Name, Vector.empty, new SetClock(Instant.EPOCH))
    val tool = plugin.tools
      .collectFirst { case t: PluginTool[MeetingLookup] @unchecked => t }
      .getOrElse(sys.error("no meetings tool"))
    tool
      .bind(
        PluginReads(new InMemoryPlugins().docs(Name), kept.docs.shelf(Name)),
        Needs.over(Name, Vector.empty),
        OwnJobs.over(Name, Vector.empty)
      )
      .fold(u => sys.error(u.toString), identity)
      .pipe(r =>
        Toolbox.of[caps.CapSet^{db, desk}](
          tool.described.calling((l, at) => r.run(l, at, db.as(Subject.Turn(at.turn)), desk))
        )
      )
      .fold(d => sys.error(d.toString), identity)
      .bind(
        TestTainted.authority(
          AssistantBlock.ToolCall(ToolCallId("c1"), "meetings", ujson.Obj.from(args))
        ),
        Repairs.All
      ) match {
      case Right(free: Bound.Free) => free(TestCallSlots.First)
      case Left(CallError.BadArgs(_, refused, _)) => Outcome.Failed(refused.message)
      case other => sys.error(s"not free: $other")
    }
  }

  /** A plugin exporting its cards, as the knowledge plugin does. */
  private val cards: Exports[CardShelf] = new Exports[CardShelf] {
    val name: PluginName = PluginName.of("knowledge").fold(sys.error, identity)
    val version: Int = 1
    def service(own: PluginReads): CardShelf = CardShelf.over(own.documents)
  }

  val tests = Tests {
    test(
      "a schedule per label, daily, keyed by the label's written form and declared at it, writing PerRun topics of the last Recent; documents bounded per label"
    ) {
      val internal = Label.at(Level.Internal)
      val params =
        TopicParams(TopicDocuments.PerRun, TopicDocuments.Recent, TopicDocuments.Recent, None, None)
      of(Vector(Label.Public, internal)).map(p =>
        (
          p.schedules.map(d =>
            (ScheduleKey.value(d.key), d.rule, Label.written(d.clearance), d.params)
          ),
          p.documents.map(d => (DocLabel.value(d.terms.label), d.terms.bound))
        )
      ) ==> Right(
        (
          Vector(
            ("topics-dream-public", SlotRule.Daily(At, ZoneOffset.UTC), "public", params),
            ("topics-dream-internal", SlotRule.Daily(At, ZoneOffset.UTC), "internal", params)
          ),
          Some(("topic", 2 * TopicDocuments.Bound))
        )
      )
    }

    test("a deployment's aheadIn reaches every label's nightly run, and no backfill's") {
      val rooms = Vector(TestLabels.place("slack:T1/C-a"))
      of(Vector(Label.Public, Label.at(Level.Internal)), aheadIn = rooms).flatMap(p =>
        p.backfill(Vector(LocalDate.parse("2026-10-30")), 1, Instant.EPOCH)
          .map(b =>
            (
              p.schedules.map(_.params).collect { case t: TopicParams => t.aheadIn },
              b.map(_.params.aheadIn)
            )
          )
      ) ==> Right((Vector.fill(2)(rooms), Vector.fill(2)(Vector.empty)))
    }

    test("its jobs are the topics run and meeting-ahead, which no schedule declares") {
      of(Vector(Label.Public)).map(p =>
        (p.jobs.map(_.name), p.schedules.map(_.job.name).distinct)
      ) ==> Right((Vector(TopicDream.Name, MeetingAhead.Name), Vector(TopicDream.Name)))
    }

    test("a seed day reaches every label's schedule as its runs' seedUntil") {
      val until = java.time.LocalDate.parse("2026-10-30")
      of(Vector(Label.Public, Label.at(Level.Internal)), seedUntil = Some(until))
        .map(_.schedules.map(_.params)) ==> Right(
        Vector.fill(2)(
          TopicParams(
            TopicDocuments.PerRun,
            TopicDocuments.Recent,
            TopicDocuments.Recent,
            None,
            Some(until)
          )
        )
      )
    }

    test(
      "a deployment's foldAt reaches every label's nightly and backfill runs; one outside 0.90 to 1 is refused, naming foldAt"
    ) {
      val plugin = of(Vector(Label.Public, Internal), foldAt = Some(0.95))
      val day = java.time.LocalDate.parse("2026-10-30")
      (
        plugin.map(_.schedules.map(_.params)),
        plugin.flatMap(_.backfill(Vector(day), 2, Instant.EPOCH)).map(_.map(_.params.foldAt)),
        of(Vector(Label.Public), foldAt = Some(0.89)).left.map(_.contains("foldAt")),
        of(Vector(Label.Public), foldAt = Some(1.01)).left.map(_.contains("foldAt"))
      ) ==> (
        Right(
          Vector.fill(2)(
            TopicParams(
              TopicDocuments.PerRun,
              TopicDocuments.Recent,
              TopicDocuments.Recent,
              None,
              None,
              foldAt = Some(0.95)
            )
          )
        ),
        Right(Vector(Some(0.95), Some(0.95))),
        Left(true),
        Left(true)
      )
    }

    test("it needs the knowledge plugin when given it, and nothing otherwise") {
      (
        of(Vector(Label.Public), cards = Some(cards)).map(_.needs.map(_.name)),
        of(Vector(Label.Public)).map(_.needs.map(_.name))
      ) ==> (Right(Vector(cards.name)), Right(Vector()))
    }

    test(
      "backfill runs each day in order at each label in turn, keyed by label, day and start, each its day's and at most perRun"
    ) {
      val internal = Label.at(Level.Internal)
      val (first, second) = (LocalDate.of(2026, 10, 1), LocalDate.of(2026, 10, 2))
      def params(day: LocalDate) =
        TopicParams(3, TopicDocuments.Recent, TopicDocuments.Recent, Some(day), None)
      val started = Instant.ofEpochSecond(1000)
      of(Vector(Label.Public, internal))
        .flatMap(_.backfill(Vector(first, second), 3, started))
        .map(
          _.map(d => (ScheduleKey.value(d.key), d.rule, Label.written(d.clearance), d.params))
        ) ==>
        Right(
          Vector(
            ("topics-backfill-public-2026-10-01-1000", once(started), "public", params(first)),
            ("topics-backfill-internal-2026-10-01-1000", once(started), "internal", params(first)),
            ("topics-backfill-public-2026-10-02-1000", once(started), "public", params(second)),
            ("topics-backfill-internal-2026-10-02-1000", once(started), "internal", params(second))
          )
        )
    }

    test("backfill of under one topic a run is refused") {
      of(Vector(Label.Public))
        .flatMap(_.backfill(Vector(LocalDate.of(2026, 10, 1)), 0, Instant.EPOCH)) ==>
        Left("a backfill writes at least one topic a run, not 0")
    }

    test("no label, one given twice, or one of a compartment not declared is refused") {
      (
        of(Vector()).map(_ => ()),
        of(Vector(Label.Public, Label.Public)).map(_ => ()),
        of(Vector(TestLabels.Trial)).map(_ => ()),
        of(Vector(TestLabels.Trial), TestLabels.Trialled).isRight
      ) ==> (
        Left("the topics plugin is declared at a label at least"),
        Left("the topics plugin is declared at public twice"),
        Left(
          s"the topics plugin's label ${Label.written(TestLabels.Trial)} holds a compartment not declared"
        ),
        true
      )
    }

    test(
      "the backfill question names the days, the labels, the runs and the most it costs, in cents"
    ) {
      TopicDocuments.backfillQuestion(
        java.time.LocalDate.of(2026, 10, 1),
        java.time.LocalDate.of(2026, 10, 3),
        Vector(Label.Public, Label.at(Level.Internal)),
        6,
        3
      ) ==> "Backfill topics for 2026-10-01 to 2026-10-03 at [level: public], [level: internal]: " +
        "6 runs, at most 3 topics each, about 2.7¢ at most (0.15¢ a topic); nothing caps it but today's cap."
    }

    test("a topics backfill ends saying nothing ran when declined, else what its runs spent") {
      (
        TopicDocuments.backfilled(Vector.empty),
        TopicDocuments.backfilled(Vector(BigDecimal("0.001"), BigDecimal("0.0005")))
      ) ==>
        ("nothing ran", "spent 0.15¢")
    }

    test(
      "meetings lists a meeting kept at what the turn reads, under its header, believed as it is"
    ) {
      val m = meeting(planned(18), Vector("Ana"), "Engine sync.")
      val kept = Kept(("a", m, Internal, Integrity.Outside))
      call(kept, Internal, "from" -> ujson.Str("2026-11-16")) ==> Outcome.Done(
        "Meetings from Monday 2026-11-16 to Sunday 2026-11-29 (Pacific/Auckland time), soonest first, as of the last write-up:" +
          s"\n\n${text(m)}",
        Some(Integrity.Outside)
      )
    }

    test("meetings lists no meeting kept above what the turn reads") {
      val kept =
        Kept(("a", meeting(planned(18), Vector("Ana"), "Engine sync."), Secret, Integrity.Member))
      call(kept, Internal, "from" -> ujson.Str("2026-11-16")) ==> Outcome.Done(
        "No meeting this conversation may see from Monday 2026-11-16 to Sunday 2026-11-29 has been written up.",
        None
      )
    }

    test("meetings refuses a day that is no date, and an until before from, saying why") {
      val kept = Kept()
      (
        call(kept, Internal, "from" -> ujson.Str("Monday")),
        call(kept, Internal, "from" -> ujson.Str("2026-11-16"), "until" -> ujson.Str("2026-11-15"))
      ) ==> (
        Outcome.Failed("`from` takes a date as yyyy-mm-dd, not \"Monday\"."),
        Outcome.Failed("`until` is before `from`; give a day on or after it.")
      )
    }

    test("the plugin offers meetings once, its from field naming the plugin's zone") {
      val tools = plugin.tools.map(_.described)
      (
        tools.map(_.name),
        tools.headOption.exists(
          _.schema(strict = false).parameters
            .render()
            .contains("yyyy-mm-dd, in Pacific/Auckland time")
        )
      ) ==> (Vector(ToolName("meetings")), true)
    }
  }
}
