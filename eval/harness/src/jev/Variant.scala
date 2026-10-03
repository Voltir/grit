package grit.eval.harness.jev

import scala.concurrent.duration.*

import grit.core.recipe.{Pool, Source}
import grit.core.stitch.Tuning
import grit.eval.harness.corpus.Digest
import grit.lifecycle.triage.{TriageQuestion, TriageRecipe}

/** What a run changes from the shipped call, named in its log: one thing each, but a recipe
  * may bring the words that name what it adds.
  */
enum Variant {

  /** The shipped wording and tuning, of the model the live tags recorded
    * ([[Variants.LiveModel]]).
    */
  case Live

  /** As [[Live]], of `model`. */
  case Model(name: String, model: String)

  /** As [[Live]], triage asked in `wording`. */
  case Words(name: String, wording: TriageQuestion.Wording)

  /** As [[Live]], every case's inputs rebuilt under `tuning` instead of its own. */
  case Tuned(name: String, tuning: Tuning)

  /** As [[Live]], triage's question showing what `recipe` adds to it, asked in `wording`. */
  case Recipe(name: String, recipe: TriageRecipe, wording: TriageQuestion.Wording)
}

object Variant {

  def name(v: Variant): String = v match {
    case Live => "live"
    case Model(name, _) => name
    case Words(name, _) => name
    case Tuned(name, _) => name
    case Recipe(name, _, _) => name
  }

  /** The model `v` requests. */
  def model(v: Variant): String = v match {
    case Model(_, model) => model
    case Live | Words(_, _) | Tuned(_, _) | Recipe(_, _, _) => Variants.LiveModel
  }

  /** The words `v` asks triage in. */
  def wording(v: Variant): TriageQuestion.Wording = v match {
    case Words(_, wording) => wording
    case Recipe(_, _, wording) => wording
    case Live | Model(_, _) | Tuned(_, _) => TriageQuestion.Wording.Shipped
  }

  /** The tuning `v` rebuilds inputs under; `None` when each case's own. */
  def tuning(v: Variant): Option[Tuning] = v match {
    case Tuned(_, tuning) => Some(tuning)
    case Live | Model(_, _) | Words(_, _) | Recipe(_, _, _) => None
  }

  /** The recipe `v` builds triage's question by. */
  def recipe(v: Variant): TriageRecipe = v match {
    case Recipe(_, recipe, _) => recipe
    case Live | Model(_, _) | Words(_, _) | Tuned(_, _) => TriageRecipe.Shipped
  }

  /** The digest of `recipe`, field by field: equal for equal recipes. */
  def digest(recipe: TriageRecipe): Digest =
    Digest.json(ujson.Obj("focused" -> pool(recipe.focused), "open" -> pool(recipe.open)))

  private def pool(p: Pool): ujson.Value = ujson.Obj(
    "sources" -> ujson.Arr.from(p.sources.map {
      case Source.Channel(within, most) =>
        ujson.Obj("channel" -> ujson.Obj("within_ns" -> within.toNanos.toString, "most" -> most))
      case Source.Author(within, most) =>
        ujson.Obj("author" -> ujson.Obj("within_ns" -> within.toNanos.toString, "most" -> most))
      case Source.Exchanges => ujson.Str("exchanges")
    }),
    "budget" -> p.budget
  )

  /** The digest of `wording`'s words, field by field: equal for equal words. */
  def digest(wording: TriageQuestion.Wording): Digest = {
    val k = wording.kinds
    Digest.json(
      ujson.Obj(
        "kind" -> wording.kind,
        "kinds" -> ujson.Obj(
          "question" -> k.question,
          "answer" -> k.answer,
          "decision" -> k.decision,
          "announcement" -> k.announcement,
          "chatter" -> k.chatter
        ),
        "waiting" -> wording.waiting,
        "durable" -> wording.durable,
        "helps" -> wording.helps
      )
    )
  }
}

/** The variants a run can name. */
object Variants {

  /** The model every live tag in the first corpus recorded. */
  val LiveModel = "jev-1.13.0"

  /** `durable`'s question reworded to name what lasts, for comparing against the shipped. */
  val DurableLasting: Variant = Variant.Words(
    "durable-lasting",
    TriageQuestion.Wording.Shipped.copy(durable =
      "Read new_message and thread. Will what new_message says still matter to this team in a " +
        "month: a decision, a commitment, a date, a name, a number, or how something works? " +
        "Greetings, thanks, status chatter and questions alone do not count."
    )
  )

  /** The live wording, of Jev's moving alias instead of the pinned release. */
  val JevLatest: Variant = Variant.Model("jev-latest", "jev-latest")

  /** What a candidate's open pool shows at most, in characters: about 150 tokens, so a pool
    * stays cheap beside the thread it follows.
    */
  val OpenBudget = 600

  /** Open focus: what anyone said in the channel in the 10 minutes before, the latest 5. */
  val NearbyOpen: Variant = Variant.Recipe(
    "nearby-open",
    TriageRecipe(Pool.empty, Pool(Vector(Source.Channel(10.minutes, 5)), OpenBudget)),
    TriageQuestion.Wording.Shipped
  )

  /** Open focus: the exchanges stitching offered the message's thread. */
  val ExchangesOpen: Variant = Variant.Recipe(
    "exchanges-open",
    TriageRecipe(Pool.empty, Pool(Vector(Source.Exchanges), OpenBudget)),
    TriageQuestion.Wording.Shipped
  )

  /** Open focus: [[NearbyOpen]]'s, then its author's own in the 30 minutes before, the latest
    * 3.
    */
  val NearbyAuthorOpen: Variant = Variant.Recipe(
    "nearby-author-open",
    TriageRecipe(
      Pool.empty,
      Pool(Vector(Source.Channel(10.minutes, 5), Source.Author(30.minutes, 3)), OpenBudget)
    ),
    TriageQuestion.Wording.Shipped
  )

  val all: Vector[Variant] =
    Vector(Variant.Live, DurableLasting, JevLatest, NearbyOpen, ExchangesOpen, NearbyAuthorOpen)

  /** The variant named `name`; `None` for none's. */
  def named(name: String): Option[Variant] = all.find(Variant.name(_) == name)
}
