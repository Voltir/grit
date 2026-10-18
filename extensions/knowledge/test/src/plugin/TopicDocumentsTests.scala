package grit.knowledge.plugin

import java.time.{LocalTime, ZoneOffset}

import grit.core.document.DocLabel
import grit.core.id.{PluginName, ScheduleKey}
import grit.core.job.SlotRule
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

  private def of(
      labels: Vector[Label],
      visibility: Visibility = Visibility.Shipped,
      cards: Option[Exports[CardShelf]] = None
  ) =
    TopicDocuments.of(Name, labels, At, ZoneOffset.UTC, visibility, cards)

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
        TopicParams(TopicDocuments.PerRun, TopicDocuments.Recent, TopicDocuments.Recent, None)
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

    test("it needs the knowledge plugin when given it, and nothing otherwise") {
      (
        of(Vector(Label.Public), cards = Some(cards)).map(_.needs.map(_.name)),
        of(Vector(Label.Public)).map(_.needs.map(_.name))
      ) ==> (Right(Vector(cards.name)), Right(Vector()))
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
