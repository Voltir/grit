package grit.knowledge.topic

/** What a topics run came to: how many topics it `written`, of them how many it `opened`; how
  * many passages it `withdrawn`; how many strands and cards it found `unchanged` since the
  * topic they are in was written, and how many it `left` for the next run (past its most, or
  * changed since its scan); how many refs it `skipped` (too long to keep, their links still in
  * the text); what it did not write, each named, and why; and why it stopped, if it did.
  * [[line]] is its reply.
  */
final case class Gathered(
    written: Int,
    opened: Int,
    withdrawn: Int,
    unchanged: Int,
    left: Int,
    skipped: Int,
    unwritten: Vector[(String, String)],
    stopped: Option[String]
) extends caps.Pure {

  /** `topics: {written} written ({opened} opened), {withdrawn} withdrawn, {unchanged}
    * unchanged, {left} left for the next run`, then, when some were, `; {n} refs too long to
    * keep`, then `; not written: {name} ({why})` for at most [[Gathered.Shown]] of them,
    * separated by `, `, and `and {n} more` for the rest; then, when it stopped, `; stopped:
    * {why}`.
    */
  def line: String = {
    val counts = s"topics: $written written ($opened opened), $withdrawn withdrawn, " +
      s"$unchanged unchanged, $left left for the next run"
    val refs =
      if (skipped == 0) ""
      else if (skipped == 1) "; 1 ref too long to keep"
      else s"; $skipped refs too long to keep"
    val named = unwritten.take(Gathered.Shown).map((n, why) => s"$n ($why)")
    val more = unwritten.size - named.size
    val notWritten =
      if (unwritten.isEmpty) ""
      else s"; not written: ${(named ++ Option.when(more > 0)(s"and $more more")).mkString(", ")}"
    counts + refs + notWritten + stopped.fold("")(why => s"; stopped: $why")
  }
}

object Gathered {

  /** 5: the most things not written a reply names. */
  val Shown: Int = 5

  /** A run that stopped before writing anything, and why. */
  private[topic] def stopped(why: String): Gathered =
    Gathered(0, 0, 0, 0, 0, 0, Vector.empty, Some(why))
}
