package grit.eval.harness.jev

import grit.core.id.QuestionName
import grit.core.triage.Earning
import grit.lifecycle.triage.TriageQuestions

/** A question set a comparison names: `name`, the set (`questions`), whose gate derives its
  * draft decision; `durable`, its question read as worth keeping, against the other side's;
  * and `to`, its question read against a verdict's `toPerson` (each `None` when it asks none).
  */
final case class QuestionSet(
    name: String,
    questions: TriageQuestions,
    durable: Option[QuestionName],
    to: Option[QuestionName]
)

/** The question sets a comparison can name. */
object Sets {

  /** [[TriageQuestions.V2]], live triage's set since it was shipped, named `v2`, with its
    * `durable` ([[Earning.Durable]]) and its `to`.
    */
  val V2: QuestionSet =
    QuestionSet(
      "v2",
      TriageQuestions.V2,
      Some(Earning.Durable),
      QuestionName.of("to").toOption
    )

  /** [[TriageQuestions.V1]], the set live triage asked before v2, named `v1`, with its
    * `durable` ([[Earning.Durable]]); it asks no `to`.
    */
  val V1: QuestionSet =
    QuestionSet("v1", TriageQuestions.V1, Some(Earning.Durable), None)

  val all: Vector[QuestionSet] = Vector(V1, V2)

  /** The set named `n`; `None` for no set's. */
  def named(n: String): Option[QuestionSet] = all.find(_.name == n)
}
