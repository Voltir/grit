package grit.knowledge.card

/** The tail of a knowledge run's reply: what it did not write, and why it stopped. */
object NotWritten {

  /** 5: the most things not written a reply names under one heading. */
  val Shown: Int = 5

  /** `; {heading}: {name} ({why})` for the first [[Shown]] of `unwritten`, separated by `, `,
    * and `and {n} more` for the rest; empty when `unwritten` is.
    */
  def tail(unwritten: Vector[(String, String)], heading: String = "not written"): String = {
    val named = unwritten.take(Shown).map((name, why) => s"$name ($why)")
    val more = unwritten.size - named.size
    if (unwritten.isEmpty) ""
    else s"; $heading: ${(named ++ Option.when(more > 0)(s"and $more more")).mkString(", ")}"
  }

  /** `; stopped: {why}` when the run stopped; empty when it did not. */
  def stopped(why: Option[String]): String = why.fold("")(w => s"; stopped: $w")
}
