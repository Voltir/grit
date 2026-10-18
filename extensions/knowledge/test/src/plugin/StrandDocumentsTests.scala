package grit.knowledge.plugin

import java.time.{LocalTime, ZoneOffset}

import grit.core.document.DocLabel
import grit.core.id.{PluginName, ScheduleKey}
import grit.core.job.SlotRule
import grit.core.visibility.{Label, Level, TestLabels, Visibility}

import utest.*

/** The strands plugin as a deployment declares it: its schedules and its documents' terms. */
object StrandDocumentsTests extends TestSuite {

  private val Name: PluginName = PluginName.of("strands").fold(sys.error, identity)

  private val At: LocalTime = LocalTime.of(4, 30)

  private def of(labels: Vector[Label], visibility: Visibility = Visibility.Shipped) =
    StrandDocuments.of(Name, labels, At, ZoneOffset.UTC, visibility)

  val tests = Tests {
    test(
      "a schedule per label, daily, keyed by the label's written form and declared at it; documents bounded per label"
    ) {
      val internal = Label.at(Level.Internal)
      of(Vector(Label.Public, internal)).map(p =>
        (
          p.schedules.map(d => (ScheduleKey.value(d.key), d.rule, Label.written(d.clearance))),
          p.documents.map(d => (DocLabel.value(d.terms.label), d.terms.bound))
        )
      ) ==> Right(
        (
          Vector(
            ("strands-dream-public", SlotRule.Daily(At, ZoneOffset.UTC), "public"),
            ("strands-dream-internal", SlotRule.Daily(At, ZoneOffset.UTC), "internal")
          ),
          Some(("room discussion", 2 * StrandDocuments.Bound))
        )
      )
    }

    test("no label, one given twice, or one of a compartment not declared is refused") {
      (
        of(Vector()).map(_ => ()),
        of(Vector(Label.Public, Label.Public)).map(_ => ()),
        of(Vector(TestLabels.Trial)).map(_ => ()),
        of(Vector(TestLabels.Trial), TestLabels.Trialled).isRight
      ) ==> (
        Left("the strands plugin is declared at a label at least"),
        Left("the strands plugin is declared at public twice"),
        Left(
          s"the strands plugin's label ${Label.written(TestLabels.Trial)} holds a compartment not declared"
        ),
        true
      )
    }
  }
}
