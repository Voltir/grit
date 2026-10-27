package grit.knowledge.plugin

import java.time.{LocalTime, ZoneOffset}

import scala.concurrent.duration.*

import grit.core.document.DocLabel
import grit.core.id.{PluginName, ScheduleKey}
import grit.core.job.{Interval, SlotRule}
import grit.core.need.PluginReads
import grit.core.plugin.Exports
import grit.core.visibility.{Label, Level, TestLabels}
import grit.knowledge.card.CardShelf
import grit.knowledge.declassify.CardDeclassify

import utest.*

object DeclassifiedCardsTests extends TestSuite {

  private val Name =
    PluginName.of("declassified").fold(e => throw new java.lang.AssertionError(e), identity)

  private object Cards extends Exports[CardShelf] {
    val name: PluginName =
      PluginName.of("knowledge").fold(e => throw new java.lang.AssertionError(e), identity)
    val version: Int = 1
    def service(own: PluginReads): CardShelf = CardShelf.over(own.documents)
  }

  private val Rule = SlotRule.Daily(LocalTime.of(3, 0), ZoneOffset.UTC)

  private def of(at: Label) =
    DeclassifiedCards.of(Name, Cards, at, TestLabels.Trialled, Rule)

  val tests = Tests {
    test(
      "its one schedule runs cards-declassify at version 4 at its label by its rule, keyed by the label, needing the cards' plugin; its documents are labelled document"
    ) {
      val at = Label.at(Level.Confidential, TestLabels.trial)
      of(at).map(p =>
        (
          p.schedules.map(d =>
            (ScheduleKey.value(d.key), d.job.name, d.job.version, d.rule, d.clearance)
          ),
          p.needs.map(_.name),
          p.documents.map(d => DocLabel.value(d.terms.label))
        )
      ) ==> Right(
        (
          Vector(("cards-declassify-confidential-trial", CardDeclassify.Name, 4, Rule, at)),
          Vector(Cards.name),
          Some("document")
        )
      )
    }

    test("given no rule, its runs fall every 30 minutes") {
      val at = Label.at(Level.Confidential, TestLabels.trial)
      DeclassifiedCards.of(Name, Cards, at, TestLabels.Trialled).map(_.schedules.map(_.rule)) ==>
        Right(
          Vector(
            SlotRule.Every(
              Interval.of(30.minutes).getOrElse(throw new java.lang.AssertionError("30 min"))
            )
          )
        )
    }

    test("a label holding the unmapped mark, or a compartment not declared, is refused") {
      (
        of(Label.Unmapped.join(Label.at(Level.Confidential))).left.toOption,
        of(Label.at(Level.Confidential, TestLabels.compartment("other"))).left.toOption
      ) ==> (
        Some("the declassified cards plugin is declared at a label holding what no one labelled"),
        Some(
          "the declassified cards plugin's label confidential+other holds a compartment not declared"
        )
      )
    }
  }
}
