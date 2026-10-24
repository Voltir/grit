package grit.knowledge.plugin

import java.time.{LocalTime, ZoneOffset}

import grit.core.id.{PluginName, ScheduleKey}
import grit.core.job.SlotRule
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
    DeclassifiedCards.of(Name, Cards, at, Rule, TestLabels.Trialled)

  val tests = Tests {
    test(
      "its one schedule runs cards-declassify at its label by its rule, keyed by the label, needing the cards' plugin"
    ) {
      val at = Label.at(Level.Confidential, TestLabels.trial)
      of(at).map(p =>
        (
          p.schedules.map(d => (ScheduleKey.value(d.key), d.job.name, d.rule, d.clearance)),
          p.needs.map(_.name)
        )
      ) ==> Right(
        (
          Vector(("cards-declassify-confidential-trial", CardDeclassify.Name, Rule, at)),
          Vector(Cards.name)
        )
      )
    }

    test("a label holding the unmapped mark, or a compartment not declared, is refused") {
      (
        of(Label.Unmapped.join(Label.at(Level.Confidential))).isLeft,
        of(Label.at(Level.Confidential, TestLabels.compartment("other"))).isLeft
      ) ==> (true, true)
    }
  }
}
