package grit.knowledge.topic

import grit.knowledge.card.NotWritten

/** What a topics run came to: how many topics it `written`, of them how many it `opened`; how
  * many passages it `withdrawn`, and how many topics it `unreadable`-withdrew, their data
  * refused (an earlier version's, say), and how many it left as they are for data of a `later`
  * version; how many strands and cards it found `unchanged` since the
  * topic they are in was written or they were set aside, how many its attach ask placed in no topic (`aside`), and how
  * many it `left` for the next run (past its most, not shown, or changed since its scan), and
  * apart from those, how many the attach ask's reply named nowhere (`unnamed`), offered again; how
  * many names that reply gave were `dropped` (named in two places, or not shown); how many times its attach ask was asked `again` and the second reply placed what the first did
  * not; how many refs it `skipped` (too long to keep, their links still in the text); what it
  * did not write, each named, and why; and why it stopped, if it did. [[line]] is its reply.
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
    unnamed: Int,
    dropped: Int,
    again: Int,
    skipped: Int,
    unwritten: Vector[(String, String)],
    stopped: Option[String]
) extends caps.Pure {

  /** `topics: {written} written ({opened} opened), {withdrawn} withdrawn, {unchanged}
    * unchanged, {aside} set aside, {left} left for the next run`, then, when some were, `,
    * {unreadable} unreadable withdrawn`, then, when some were, `, {later} of a later version
    * left`, then, when some were, `; {unnamed} unnamed, offered
    * again`, then, when some were, `; {dropped} named twice or not
    * shown, dropped`, then, when it was, `; {again}
    * asked again`, then, when some were, `; {n} refs too long to keep`, then `; not written: {name} ({why})` for at most [[Gathered.Shown]] of them,
    * separated by `, `, and `and {n} more` for the rest; then, when it stopped, `; stopped:
    * {why}`.
    */
  def line: String = {
    val counts = s"topics: $written written ($opened opened), $withdrawn withdrawn, " +
      s"$unchanged unchanged, $aside set aside, $left left for the next run" +
      (if (unreadable == 0) "" else s", $unreadable unreadable withdrawn") +
      (if (later == 0) "" else s", $later of a later version left")
    val missed = if (unnamed == 0) "" else s"; $unnamed unnamed, offered again"
    val wrong = if (dropped == 0) "" else s"; $dropped named twice or not shown, dropped"
    val asked = if (again == 0) "" else s"; $again asked again"
    val refs =
      if (skipped == 0) ""
      else if (skipped == 1) "; 1 ref too long to keep"
      else s"; $skipped refs too long to keep"
    val notWritten = NotWritten.tail(unwritten, Gathered.Shown)
    counts + missed + wrong + asked + refs + notWritten + stopped.fold("")(why =>
      s"; stopped: $why"
    )
  }
}

object Gathered {

  /** 5: the most things not written a reply names. */
  val Shown: Int = 5

  /** A run that stopped before writing anything, and why. */
  private[topic] def stopped(why: String): Gathered =
    Gathered(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, Vector.empty, Some(why))
}
