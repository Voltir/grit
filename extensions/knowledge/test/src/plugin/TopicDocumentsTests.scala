package grit.knowledge.plugin

import java.time.{Instant, LocalDate, LocalTime, ZoneOffset}

import grit.core.document.DocLabel
import grit.core.id.{PluginName, ScheduleKey}
import grit.core.job.{Grace, SlotRule}
import grit.core.need.PluginReads
import grit.core.plugin.Exports
import grit.core.visibility.{Label, Level, TestLabels, Visibility}
import grit.knowledge.card.CardShelf
import grit.knowledge.topic.TopicParams

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
      seedUntil: Option[java.time.LocalDate] = None
  ) =
    TopicDocuments.of(Name, labels, At, ZoneOffset.UTC, visibility, cards, seedUntil)

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
  }
}
