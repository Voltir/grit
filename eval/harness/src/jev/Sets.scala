package grit.eval.harness.jev

import grit.core.id.QuestionName
import grit.core.triage.{Earning, Gate, Tags}

/** A question set a comparison names: `name`, the gate its draft decision is derived by
  * (`speak`); `durable`, its question read as worth keeping, against the other side's; and
  * `to`, its question read against a verdict's `toPerson` (each `None` when it asks none).
  */
final case class QuestionSet(
    name: String,
    speak: Gate,
    durable: Option[QuestionName],
    to: Option[QuestionName]
)

/** The question sets a comparison can name. */
object Sets {

  /** v1's gate ([[Tags.V1.gate]]), the set live triage asked before v2, named `v1`, with its
    * `durable` ([[Earning.Durable]]); it asks no `to`.
    */
  val V1: QuestionSet = QuestionSet("v1", Tags.V1.gate, Some(Earning.Durable), None)

  /** v2's gate ([[Tags.V2.drafts]]), live triage's from v2 until v3, named `v2`, with its
    * `durable` ([[Earning.Durable]]) and its `to`.
    */
  val V2: QuestionSet = QuestionSet("v2", Tags.V2.drafts, Some(Earning.Durable), Some(Tags.V2.to))

  /** v3's gate ([[Tags.V3.drafts]]), live triage's from v3 until v4, named `v3`, with its
    * `durable` ([[Earning.Durable]]) and its `to`. It reads only questions v4 asks in v3's
    * words, so it reads v4's answers too.
    */
  val V3: QuestionSet = QuestionSet("v3", Tags.V3.drafts, Some(Earning.Durable), Some(Tags.V2.to))

  /** v4's gate ([[Tags.V4.drafts]]), live triage's since it replaced v3, named `v4`, with its
    * `durable` ([[Earning.Durable]]) and its `to`.
    */
  val V4: QuestionSet = QuestionSet("v4", Tags.V4.drafts, Some(Earning.Durable), Some(Tags.V2.to))

  val all: Vector[QuestionSet] = Vector(V1, V2, V3, V4)

  /** The set named `n`; `None` for no set's. */
  def named(n: String): Option[QuestionSet] = all.find(_.name == n)
}
