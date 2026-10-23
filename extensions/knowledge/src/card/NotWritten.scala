package grit.knowledge.card

/** The tail of a knowledge run's reply that names what it did not write. */
object NotWritten {

  /** `; {heading}: {name} ({why})` for the first `shown` of `unwritten`, separated by `, `,
    * and `and {n} more` for the rest; empty when `unwritten` is.
    */
  def tail(
      unwritten: Vector[(String, String)],
      shown: Int,
      heading: String = "not written"
  ): String = {
    val named = unwritten.take(shown).map((name, why) => s"$name ($why)")
    val more = unwritten.size - named.size
    if (unwritten.isEmpty) ""
    else s"; $heading: ${(named ++ Option.when(more > 0)(s"and $more more")).mkString(", ")}"
  }
}
