package grit.lifecycle.close

/** Which parts of a closing the summary model is asked to write; the prose always is.
  * `open`, `standing` and `settled` are what the gate judged new: an item left open, a
  * decision or fact that stands, and a known line answered, finished or overturned. With
  * none of them, the period is carried with no model call.
  */
final case class Asked(outcome: Boolean, open: Boolean, standing: Boolean, settled: Boolean) {

  /** Nothing new: no open, standing or settled part is asked for. */
  def nothingNew: Boolean = !(open || standing || settled)
}

object Asked {

  /** Every part: what a close asks for when it cannot tell which the period needs. */
  val Every: Asked = Asked(true, true, true, true)
}
