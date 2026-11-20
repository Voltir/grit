package grit.knowledge.topic

import grit.knowledge.card.NotWritten
import grit.knowledge.ref.TopicKey

/** What a topics run came to: how many topics it `written`, of them how many it `opened`; how
  * many passages it `withdrawn`, and how many topics it `unreadable`-withdrew, their data
  * refused (an earlier version's, say), and how many it left as they are for data of a `later`
  * version; how many strands and cards it found `unchanged` since the topic they are in was
  * written or they were set aside, how many its sorts placed in no topic (`aside`), and how
  * many it `left` for the next run (past its most, changed since its scan, or a sort refused);
  * how many of those left were strands that would have opened a topic past the run's opens (`budget`);
  * how many refs it `skipped` (too long to keep, their links still in the
  * text); the folds its merge sorts made (`folded`), and those they would make, not made
  * (`proposed`); how many people's cards it kept with a paragraph asked (`people`); what it did not
  * write, each named, and why; and why it stopped, if it did. [[line]] is its reply.
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
    budget: Int,
    skipped: Int,
    unwritten: Vector[(String, String)],
    stopped: Option[String],
    proposed: Vector[Gathered.Fold],
    folded: Vector[Gathered.Fold],
    people: Int = 0
) extends caps.Pure {

  /** `topics: {written} written ({opened} opened), {withdrawn} withdrawn, {unchanged}
    * unchanged, {aside} set aside, {left} left for the next run`, then, when some were, `,
    * {unreadable} unreadable withdrawn`, then, when some were, `, {later} of a later version
    * left`, then, when some were, `; {budget} new topics left for the next run` (those past the
    * opens a run makes), then, when some were, `; {n} refs too long to keep`, then, when some
    * were, `; folded {from} into {into} ({p})` for each folded, separated by `, `, then, when
    * some were, `; would fold {from} into {into} ({p})` for each proposed, alike, `p` to two
    * decimals, then, when some were, `; {people} person cards` (`1 person card`), then `; not written: {name} ({why})` for at most [[NotWritten.Shown]] of
    * them, separated by `, `, and `and {n} more` for the rest; then, when it stopped, `;
    * stopped: {why}`.
    */
  def line: String = {
    val counts = s"topics: $written written ($opened opened), $withdrawn withdrawn, " +
      s"$unchanged unchanged, $aside set aside, $left left for the next run" +
      (if (unreadable == 0) "" else s", $unreadable unreadable withdrawn") +
      (if (later == 0) "" else s", $later of a later version left")
    val doubted = if (budget == 0) "" else s"; $budget new topics left for the next run"
    val refs =
      if (skipped == 0) ""
      else if (skipped == 1) "; 1 ref too long to keep"
      else s"; $skipped refs too long to keep"
    def folds(said: String, fs: Vector[Gathered.Fold]) =
      if (fs.isEmpty) ""
      else
        fs.map(f =>
          s"$said ${TopicKey.value(f.from)} into ${TopicKey.value(f.into)} (${f"${f.p}%.2f"})"
        ).mkString("; ", ", ", "")
    // Draft wording, for Nick: `folded` for the folds made, `would fold` for those proposed.
    // Draft wording, for Nick (grit/568): the cards a run wrote.
    val cards =
      if (people == 0) "" else if (people == 1) "; 1 person card" else s"; $people person cards"
    counts + doubted + refs + folds("folded", folded) + folds("would fold", proposed) + cards +
      NotWritten.tail(unwritten) + NotWritten.stopped(stopped)
  }
}

object Gathered {

  /** A fold a merge sort made or would make: the topic `from` into the topic `into`, the sort's
    * probability `p` that they are one subject.
    */
  final case class Fold(from: TopicKey, into: TopicKey, p: Double) extends caps.Pure

  /** A run that stopped before writing anything, and why. */
  private[topic] def stopped(why: String): Gathered =
    Gathered(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, Vector.empty, Some(why), Vector.empty, Vector.empty)
}
