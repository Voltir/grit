package grit.eval.harness.jev

import scala.concurrent.duration.*

import grit.core.recipe.{Pool, Source}
import grit.lifecycle.triage.{TriageQuestion, TriageRecipe}

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

    test("the open-pool candidates are named, each changing only the open pool, within 600") {
      def open(sources: Source*) = TriageRecipe(Pool.empty, Pool(sources.toVector, 600))
      Vector("nearby-open", "exchanges-open", "nearby-author-open")
        .map(n => Variants.named(n).map(Variant.recipe)) ==> Vector(
        Some(open(Source.Channel(10.minutes, 5))),
        Some(open(Source.Exchanges)),
        Some(open(Source.Channel(10.minutes, 5), Source.Author(30.minutes, 3)))
      )
      Variants.all.map(Variant.name).distinct.size ==> Variants.all.size
    }

    test("every variant but a recipe's builds by the shipped recipe") {
      val others = Variants.all.filter {
        case Variant.Recipe(_, _, _) => false
        case _ => true
      }
      (others :+ Variant.Recipe("r", recipe, TriageQuestion.Wording.Shipped))
        .map(Variant.recipe) ==>
        (others.map(_ => TriageRecipe.Shipped) :+ recipe)
    }

    test("a recipe's variant asks in the wording it carries, and its header digests both") {
      val named = TriageQuestion.Wording.Shipped.copy(kind = "Read new_message and nearby.")
      val v = Variant.Recipe("r", recipe, named)
      (Variant.digest(Variant.recipe(v)), Variant.digest(Variant.wording(v))) ==>
        (Variant.digest(recipe), Variant.digest(named))
    }

    test("sections-named changes only where the shipped words say what to read") {
      val shipped = TriageQuestion.Wording.Shipped
      val opening = "Read new_message and thread."
      def reread(q: String) =
        q.replace(opening, "Read new_message, its thread, and any channel context given.")
      Variants.named("sections-named").map(v => (Variant.recipe(v), Variant.wording(v))) ==>
        Some(
          (
            TriageRecipe.Shipped,
            shipped.copy(
              kind = "Read new_message, said by author in a team's thread; thread is what came " +
                "before it in its conversation. nearby_in_channel and exchanges_in_channel, when " +
                "present, are what was said around it elsewhere in the channel. Nobody said it to " +
                "the assistant. What kind of message is it?",
              waiting = reread(shipped.waiting),
              durable = reread(shipped.durable),
              helps = reread(shipped.helps)
            )
          )
        )
      Vector(shipped.waiting, shipped.durable, shipped.helps).map(_.startsWith(opening)) ==>
        Vector(true, true, true)
    }

    test("each open-pool candidate has a +named twin: its recipe, asked in sections-named") {
      val named = Variants.named("sections-named").map(Variant.wording)
      Vector("nearby-open", "exchanges-open", "nearby-author-open").map { n =>
        Variants.named(s"$n+named").map(v => (Variant.recipe(v), Some(Variant.wording(v))))
      } ==> Vector("nearby-open", "exchanges-open", "nearby-author-open").map { n =>
        Variants.named(n).map(v => (Variant.recipe(v), named))
      }
      named.isDefined ==> true
    }
  }
}
