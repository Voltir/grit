package grit.knowledge.sync

import grit.core.id.CorpusName
import grit.knowledge.card.NotWritten

/** A unit whose card a run could not write, at `path`, and `why`. */
final case class Unwritten(path: String, why: String) extends caps.Pure

/** What a run came to: how many cards it `written`, `relabelled` (copied up to its floor, or to
  * its source's place),
  * `withdrawn` and found `unchanged`; how many units it saw changed, new or held only at
  * another label, and `left` unreached; how many new units it `refused`, the source holding its
  * most cards; the units it reached and could not write (`unwritten`), each tried again by the
  * next run; and why it stopped, if it did. [[line]] is its reply.
  */
final case class Synced(
    source: CorpusName,
    written: Int,
    relabelled: Int,
    withdrawn: Int,
    unchanged: Int,
    left: Int,
    refused: Int,
    unwritten: Vector[Unwritten],
    stopped: Option[String]
) extends caps.Pure {

  /** `{source}: {written} written, {relabelled} relabelled, {withdrawn} withdrawn, {unchanged}
    * unchanged, {left plus unwritten} left for the next run`, then, when some were refused,
    * `; {refused} new units not taken, the source holding its most cards`, then, when some are
    * unwritten, `; not written: {path} ({why})` for at most [[Synced.Shown]] of them, separated
    * by `, `, and `and {n} more` for the rest; then, when it stopped, `; stopped: {why}`.
    */
  def line: String = {
    val counts = s"${CorpusName.value(source)}: $written written, $relabelled relabelled, " +
      s"$withdrawn withdrawn, $unchanged unchanged, ${left + unwritten.size} left for the next run"
    val notWritten = NotWritten.tail(unwritten.map(u => (u.path, u.why)), Synced.Shown)
    val notTaken =
      if (refused == 0) ""
      else s"; $refused new units not taken, the source holding its most cards"
    counts + notTaken + notWritten + stopped.fold("")(why => s"; stopped: $why")
  }
}

object Synced {

  /** 5: the most unwritten units a reply names. */
  val Shown: Int = 5
}
