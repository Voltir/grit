package grit.knowledge.topic

import grit.knowledge.card.NotWritten

/** What a topics run came to: how many topics it `written`, of them how many it `opened`; how
  * many passages it `withdrawn`, and how many topics it `unreadable`-withdrew, their data
  * refused (an earlier version's, say), and how many it left as they are for data of a `later`
  * version; how many strands and cards it found `unchanged` since the topic they are in was
  * written or they were set aside, how many its sorts placed in no topic (`aside`), and how
  * many it `left` for the next run (past its most, changed since its scan, or a sort refused);
  * how many of those set aside were strands a topic was chosen for below the mark (`unsure`);
  * how many refs it `skipped` (too long to keep, their links still in the
  * text); what it did not write, each named, and why; and why it stopped, if it did. [[line]]
  * is its reply.
  */
final case class Gathered(
    written: Int,
    opened: Int,
    withdrawn: Int,
    unreadable: Int,
    later: Int,
    unchanged: Int,
    aside: Int,
    left: Int,
    unsure: Int,
    skipped: Int,
    unwritten: Vector[(String, String)],
    stopped: Option[String]
) extends caps.Pure {

  /** `topics: {written} written ({opened} opened), {withdrawn} withdrawn, {unchanged}
    * unchanged, {aside} set aside, {left} left for the next run`, then, when some were, `,
    * {unreadable} unreadable withdrawn`, then, when some were, `, {later} of a later version
    * left`, then, when some were, `; {unsure} set aside unsure`, then, when some were, `; {n} refs
    * too long to keep`, then `; not written: {name} ({why})` for at most [[NotWritten.Shown]] of
    * them, separated by `, `, and `and {n} more` for the rest; then, when it stopped, `;
    * stopped: {why}`.
    */
  def line: String = {
    val counts = s"topics: $written written ($opened opened), $withdrawn withdrawn, " +
      s"$unchanged unchanged, $aside set aside, $left left for the next run" +
      (if (unreadable == 0) "" else s", $unreadable unreadable withdrawn") +
      (if (later == 0) "" else s", $later of a later version left")
    // Draft wording, for Nick.
    val doubted = if (unsure == 0) "" else s"; $unsure set aside unsure"
    val refs =
      if (skipped == 0) ""
      else if (skipped == 1) "; 1 ref too long to keep"
      else s"; $skipped refs too long to keep"
    counts + doubted + refs + NotWritten.tail(unwritten) + NotWritten.stopped(stopped)
  }
}

object Gathered {

  /** A run that stopped before writing anything, and why. */
  private[topic] def stopped(why: String): Gathered =
    Gathered(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, Vector.empty, Some(why))
}
