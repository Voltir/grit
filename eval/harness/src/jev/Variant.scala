package grit.eval.harness.jev

import grit.core.stitch.Tuning
import grit.eval.harness.corpus.Digest
import grit.lifecycle.triage.TriageQuestion

/** What a run changes from the shipped call; one thing each, named in its log. */
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
}

object Variant {

  def name(v: Variant): String = v match {
    case Live => "live"
    case Model(name, _) => name
    case Words(name, _) => name
    case Tuned(name, _) => name
  }

  /** The model `v` requests. */
  def model(v: Variant): String = v match {
    case Model(_, model) => model
    case Live | Words(_, _) | Tuned(_, _) => Variants.LiveModel
  }

  /** The words `v` asks triage in. */
  def wording(v: Variant): TriageQuestion.Wording = v match {
    case Words(_, wording) => wording
    case Live | Model(_, _) | Tuned(_, _) => TriageQuestion.Wording.Shipped
  }

  /** The tuning `v` rebuilds inputs under; `None` when each case's own. */
  def tuning(v: Variant): Option[Tuning] = v match {
    case Tuned(_, tuning) => Some(tuning)
    case Live | Model(_, _) | Words(_, _) => None
  }

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

  val all: Vector[Variant] = Vector(Variant.Live, DurableLasting, JevLatest)

  /** The variant named `name`; `None` for none's. */
  def named(name: String): Option[Variant] = all.find(Variant.name(_) == name)
}
