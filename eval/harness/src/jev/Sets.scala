package grit.eval.harness.jev

import grit.core.id.QuestionName
import grit.lifecycle.triage.TriageQuestions

/** A question set a comparison names: `name`, the set (`questions`), whose gate derives its
  * draft decision; `durable`, its question read against live triage's `durable` tag; and `to`,
  * its question read against a verdict's `toPerson` (each `None` when it asks none).
  */
final case class QuestionSet(
    name: String,
    questions: TriageQuestions,
    durable: Option[QuestionName],
    to: Option[QuestionName]
)

/** The question sets a comparison can name. */
object Sets {

  /** [[TriageQuestions.V2]], named `v2`, its `durable` read against live's and its `to`
    * against a verdict's.
    */
  val V2: QuestionSet =
    QuestionSet(
      "v2",
      TriageQuestions.V2,
      QuestionName.of("durable").toOption,
      QuestionName.of("to").toOption
    )

  /** [[TriageQuestions.V1]], live's own question as a set, named `v1`, its `durable` read
    * against live's; it asks no `to`.
    */
  val V1: QuestionSet =
    QuestionSet("v1", TriageQuestions.V1, QuestionName.of("durable").toOption, None)

  val all: Vector[QuestionSet] = Vector(V1, V2)

  /** The set named `n`; `None` for no set's. */
  def named(n: String): Option[QuestionSet] = all.find(_.name == n)
}
