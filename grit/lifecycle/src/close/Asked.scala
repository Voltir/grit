package grit.lifecycle.close

/** Which parts of a closing the summary model is asked to write; the prose always is.
  * `open`, `standing` and `settled` are what the gate judged new: an item left open, a
  * decision or fact that stands, and a known line answered, finished or overturned. With
  * none of them, a period anything was said to grit in is carried with no model call.
  */
final case class Asked(outcome: Boolean, open: Boolean, standing: Boolean, settled: Boolean) {

  /** Nothing new: no open, standing or settled part is asked for. */
  def nothingNew: Boolean = !(open || standing || settled)

  /** What a period grit only heard is asked for: its outcome, when this asks for one, and
    * nothing open, standing or settled: what people said to each other is reported in the
    * prose, never kept as the conversation's own.
    */
  def heard: Asked = Asked(outcome, open = false, standing = false, settled = false)
}

object Asked {

  /** Every part: what a close asks for when it cannot tell which the period needs. */
  val Every: Asked = Asked(true, true, true, true)

  /** No part: what an unearned close asks for, of no classifier and no model. */
  val NoPart: Asked = Asked(false, false, false, false)
}
