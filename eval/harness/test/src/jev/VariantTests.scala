package grit.eval.harness.jev

import scala.concurrent.duration.*

import grit.core.recipe.{Pool, Source}
import grit.lifecycle.triage.TriageRecipe

import utest.*

/** What a variant changes, as its log records it. */
object VariantTests extends TestSuite {

  private val nearby = Pool(Vector(Source.Channel(10.minutes, 5)), 600)
  private val recipe = TriageRecipe(Pool.empty, nearby)

  val tests = Tests {
    test("a recipe's digest is equal for equal recipes, and differs with any one field") {
      Variant.digest(
        TriageRecipe(Pool.empty, Pool(Vector(Source.Channel(600.seconds, 5)), 600))
      ) ==>
        Variant.digest(recipe)
      val changed = Vector(
        TriageRecipe.Shipped,
        TriageRecipe(nearby, Pool.empty),
        TriageRecipe(nearby, nearby),
        TriageRecipe(Pool.empty, nearby.copy(budget = 300)),
        TriageRecipe(Pool.empty, Pool(Vector(Source.Channel(10.minutes, 4)), 600)),
        TriageRecipe(Pool.empty, Pool(Vector(Source.Channel(11.minutes, 5)), 600)),
        TriageRecipe(Pool.empty, Pool(Vector(Source.Author(10.minutes, 5)), 600)),
        TriageRecipe(Pool.empty, Pool(Vector(Source.Exchanges), 600)),
        TriageRecipe(Pool.empty, Pool(nearby.sources :+ Source.Exchanges, 600)),
        TriageRecipe(Pool.empty, Pool(Source.Exchanges +: nearby.sources, 600))
      )
      val digests = (recipe +: changed).map(Variant.digest)
      digests.distinct.size ==> digests.size
    }

    test("every variant but a recipe's builds by the shipped recipe") {
      (Variants.all :+ Variant.Recipe("r", recipe)).map(Variant.recipe) ==>
        (Variants.all.map(_ => TriageRecipe.Shipped) :+ recipe)
    }
  }
}
