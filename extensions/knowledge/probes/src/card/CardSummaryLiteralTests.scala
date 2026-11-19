package grit.knowledge.card

import grit.core.durable.Probes

import utest.*

/** What the compiler refuses of [[CardSummary.literal]]: a summary [[CardSummary.of]] refuses,
  * with its reason, and one that is not a constant; and outside the plugin, both the literal and
  * the constructor it builds through. Pinned by compiling probe sources against knowledge.
  */
object CardSummaryLiteralTests extends TestSuite {

  private val within =
    """package grit.knowledge.probe
      |import grit.knowledge.card.CardSummary
      |""".stripMargin

  private val outside =
    """package probe
      |import grit.knowledge.card.CardSummary
      |""".stripMargin

  private val accepted = """val s: CardSummary = CardSummary.literal("An empty file.")"""

  private val blank = """val s: CardSummary = CardSummary.literal("  ")"""

  private val built = """def s(t: String): CardSummary = CardSummary.literal(t)"""

  private val literalOutside = """val s: CardSummary = CardSummary.literal("An empty file.")"""

  private val newOutside =
    """val s: CardSummary = new CardSummary("An empty file.", Vector.empty)"""

  private lazy val inside = Probes.batch(within, List(accepted, blank, built))

  private lazy val beyond = Probes.batch(outside, List(literalOutside, newOutside))

  /** `errs` with each message cut before it names the probe's own object, which the batch numbers. */
  private def named(errs: List[String]): List[String] = errs.map(e => e.take(e.indexOf(" from ")))

  val tests = Tests {
    test("control: a constant summary of accepts compiles") {
      inside.errors(accepted) ==> Nil
    }

    test("a constant summary of refuses does not compile, with of's reason") {
      inside.errors(blank) ==> List("a summary is not blank")
    }

    test("a summary that is not a constant does not compile") {
      inside.errors(built) ==> List("a literal is a string constant")
    }

    test("outside the plugin, neither the literal nor the constructor compiles") {
      List(literalOutside, newOutside).map(p => named(beyond.errors(p))) ==> List(
        List(
          "method literal cannot be accessed as a member of grit.knowledge.card.CardSummary.type"
        ),
        List(
          "constructor CardSummary cannot be accessed as a member of grit.knowledge.card.CardSummary"
        )
      )
    }
  }
}
