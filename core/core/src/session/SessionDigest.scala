package grit.core.session

import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneOffset}

/** A session's digest: what it has done, for an agent going on with it without its transcript. */
object SessionDigest {

  /** Whether the window the digest joins already holds what the session last said
    * ([[Session.summary]]): when it does, the digest leaves it out; when it does not, the digest
    * shows it, at most [[Said]] characters.
    */
  enum Window {
    case HoldsLastSaid
    case LacksLastSaid
  }

  /** 500: the most characters of what the session last said a digest shows. */
  val Said: Int = 500

  /** `lately`'s digest for `window`, at most `chars` characters, as markdown lines: a heading;
    * when the session started, was last active and ended if it did, with its counters; that its
    * log is full, when it is ([[Log.HardCap]]); what it last said, as [[Window]] says; then its
    * commits (short sha and subject), what it edited, and what it read, each newest first.
    *
    * Over `chars`, whole lines go: the oldest read first, then the oldest edited, then the
    * oldest commit (a list left empty loses its heading too); then what it last said is cut,
    * ending `…`, or left out; then the lines before it alone, cut, ending `…`. Empty when `chars`
    * is below 1.
    */
  def of(lately: Session.Lately, window: Window, chars: Int): String = {
    val s = lately.session
    val c = s.counters
    val head = Vector(
      "## This session so far",
      s"Started ${when(s.started)}, last active ${when(s.lastEvent)}" +
        s.ended.fold("")(e => s", ended ${when(e)}") +
        s": ${c.toolCalls} tool calls, ${c.edits} edits, ${c.commits} commits, " +
        s"${c.bytesRead / 1024} KB read."
    ) ++ Option.when(s.full)("Its log is full: nothing it did after it filled is shown.")
    val said = window match {
      case Window.HoldsLastSaid => None
      case Window.LacksLastSaid => s.summary.map(Session.Summary.value).map(cut(_, Said))
    }
    val commits = lately.commits.map(m => s"- ${m.sha.take(7)}${m.subject.fold("")(" " + _)}")
    val edited = lately.edited.map("- " + _)
    val read = lately.read.map("- " + _)
    def text(
        said: Option[String],
        commits: Vector[String],
        edited: Vector[String],
        read: Vector[String]
    ) =
      (head ++ said.map(SaidLead + _) ++ listed("Commits", commits) ++ listed("Edited", edited) ++
        listed("Read", read)).mkString("\n")
    val shortened =
      (read.size to 0 by -1).iterator.map(k => text(said, commits, edited, read.take(k))) ++
        (edited.size - 1 to 0 by -1).iterator.map(k =>
          text(said, commits, edited.take(k), Vector.empty)
        ) ++
        (commits.size - 1 to 0 by -1).iterator.map(k =>
          text(said, commits.take(k), Vector.empty, Vector.empty)
        )
    if (chars < 1) ""
    else
      shortened.find(_.length <= chars).getOrElse {
        val bare = text(None, Vector.empty, Vector.empty, Vector.empty)
        val room = chars - bare.length - 1 - SaidLead.length
        said match {
          case Some(line) if room >= 2 =>
            text(Some(cut(line, room)), Vector.empty, Vector.empty, Vector.empty)
          case _ => cut(bare, chars)
        }
      }
  }

  private val SaidLead = "Last said: "

  /** `text` at most `n` characters: whole, or its first `n - 1` and `…`. */
  private def cut(text: String, n: Int): String =
    if (text.length <= n) text else text.take(n - 1) + "…"

  /** `lines` under `name`'s heading; nothing when there are none. */
  private def listed(name: String, lines: Vector[String]): Vector[String] =
    if (lines.isEmpty) Vector.empty else s"$name, newest first:" +: lines

  private val Minutes =
    DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm 'UTC'").withZone(ZoneOffset.UTC)

  private def when(at: Instant): String = Minutes.format(at)
}
