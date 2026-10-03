package grit.core.recipe

import grit.core.context.Width
import grit.core.id.KnowledgeSourceName
import grit.core.message.Tokens
import grit.core.period.Probability
import grit.core.place.{Place, Service}
import grit.core.store.Focus
import grit.core.triage.{KnowledgeSource, KnowledgeSources, Tags}

import utest.*

/** A recipe's shaping chosen by root and then focus, and how far it widens. */
object TurnRecipeTests extends TestSuite {

  private def shaping(budget: Int, at: Double) =
    Shaping(Width.Within(Tokens(budget), 5), Offering.BySource(Probability.clamped(at)))

  // Each root and focus its own budget and line, so a pick of the wrong one shows.
  private val recipe = TurnRecipe(
    heard = ByFocus(focused = shaping(1000, 0.1), open = shaping(2000, 0.2)),
    addressed = shaping(3000, 0.3)
  )

  val tests = Tests {
    test("a heard turn is shaped by its focus, an addressed one by addressed") {
      recipe.at(Rooted.Heard(Focus.Focused)) ==> shaping(1000, 0.1)
      recipe.at(Rooted.Heard(Focus.Open)) ==> shaping(2000, 0.2)
      recipe.at(Rooted.Addressed) ==> shaping(3000, 0.3)
    }

    test("its gates are each shaping's offering gate of every supplied service, each once") {
      def name(n: String) = KnowledgeSourceName.of(n).fold(e => sys.error(e), identity)
      def service(n: String) = Service.of(n).fold(e => sys.error(e), identity)
      val knowledge = KnowledgeSources
        .of(
          Vector(
            KnowledgeSource(
              name("repo"),
              "the repository",
              Place.Everywhere,
              Some(service("github"))
            ),
            KnowledgeSource(name("past"), "past conversations", Place.Everywhere, None),
            KnowledgeSource(name("wiki"), "the wiki", Place.Everywhere, Some(service("docs")))
          )
        )
        .fold(n => sys.error(KnowledgeSourceName.value(n)), identity)
      def source(n: String, at: Double) = Tags.V2.source(name(n), Probability.clamped(at))
      val twice = recipe.copy(heard = recipe.heard.copy(open = shaping(2000, 0.1)))
      (recipe.gates(knowledge), twice.gates(knowledge), TurnRecipe.Shipped.gates(knowledge)) ==> (
        Vector(
          source("repo", 0.1),
          source("wiki", 0.1),
          source("repo", 0.2),
          source("wiki", 0.2),
          source("repo", 0.3),
          source("wiki", 0.3)
        ),
        Vector(source("repo", 0.1), source("wiki", 0.1), source("repo", 0.3), source("wiki", 0.3)),
        Vector.empty
      )
    }

    test("widens names the widest budget over the window, and none at or under it") {
      recipe.widens(Tokens(1500)) ==> Some(Tokens(3000))
      recipe.widens(Tokens(3000)) ==> None
      TurnRecipe.Shipped.widens(Tokens(1)) ==> None
    }
  }
}
