package grit.core.session

import java.nio.charset.StandardCharsets
import java.time.Instant

import grit.core.place.Place

/** One thing a working session did, in grit's words: `n` its source's own ordinal for it, unique
  * within the session (a hook's nanoseconds, a turn's entry seq), so an event recorded again is
  * known; `shape` what it was, never its content.
  */
final case class Event private (n: Long, at: Instant, kind: Event.Kind, shape: Event.Shape)

object Event {

  /** The event, or why not: `n` not above 0, or a read of fewer than 0 bytes. */
  def of(n: Long, at: Instant, kind: Kind, shape: Shape): Either[String, Event] =
    if (n <= 0) Left(s"an event's ordinal is above 0: $n")
    else
      kind match {
        case Kind.Read(bytes) if bytes < 0 => Left(s"a read is of 0 bytes or more: $bytes")
        case _ => Right(new Event(n, at, kind, shape))
      }

  /** What an event was. A read's bytes go to its session's counters, and a closing's summary
    * to its session's row, not to its log.
    */
  enum Kind {
    case Started
    case Read(bytes: Long)
    case Edited
    case Ran
    case Commit
    case Closing(summary: Session.Summary)
    case Ended

    /** Its stored name. */
    def name: String = this match {
      case Started => "started"
      case Read(_) => "read"
      case Edited => "edited"
      case Ran => "ran"
      case Commit => "commit"
      case Closing(_) => "closing"
      case Ended => "ended"
    }
  }

  /** What an event was, never its content (a path, an exit code, not a file's text): a JSON
    * object of at most [[Shape.Longest]] UTF-8 bytes as written.
    */
  opaque type Shape = String

  object Shape {

    /** 300: the most UTF-8 bytes a shape takes, written compactly. */
    val Longest: Int = 300

    val Empty: Shape = "{}"

    /** `o` as a shape, or why not: over [[Longest]] bytes, written compactly. */
    def of(o: ujson.Obj): Either[String, Shape] = {
      val text = ujson.write(o)
      val bytes = text.getBytes(StandardCharsets.UTF_8).length
      Either.cond(bytes <= Longest, text, s"an event's shape is $bytes bytes, over $Longest")
    }

    /** `worktree`: the key a commit's shape names the worktree it was made in under, as the
      * place is written ([[Place.written]]); what a watch for a commit at a place matches
      * ([[grit.core.edge.WatchFor.Committed]]). An edit's or a read's shape names under it, the
      * same way, the worktree its [[Path]] is relative to, when that is not the one the event
      * ran in.
      */
    val Worktree: String = "worktree"

    /** `path`: the key an edit's or a read's shape names its file under; what
      * [[Sessions.lately]] lists.
      */
    val Path: String = "path"

    /** `named`: what a read named when it was no file: a symbol, a test, a query; what
      * [[Sessions.lately]] lists beside [[Path]].
      */
    val Named: String = "named"

    /** `sha`: the key a commit's shape names its commit under; a commit without one is not in
      * [[Sessions.lately]].
      */
    val Sha: String = "sha"

    /** `subject`: the key a commit's shape names its subject line under, when it kept one. */
    val Subject: String = "subject"

    /** `child`: the key a commit's shape holds `true` under when it was made by an agent the
      * session started; [[Sessions.lately]] counts such a commit rather than listing it.
      */
    val Child: String = "child"

    /** `replaces`: the key a commit's shape lists the commits this one rewrote under, each by
      * the first [[ReplacedChars]] characters of its sha; [[Sessions.lately]] leaves a commit so
      * replaced out.
      */
    val Replaces: String = "replaces"

    /** 12: how many leading characters of a rewritten commit's sha [[Replaces]] holds. */
    val ReplacedChars: Int = 12

    /** A commit's shape: `o` with the worktree `at` it was made in under [[Worktree]], in place of
      * any value `o` held there; or why not, as [[of]].
      */
    def committed(at: Place, o: ujson.Obj): Either[String, Shape] =
      of(ujson.Obj.from(o.value.toSeq :+ (Worktree -> ujson.Str(at.written))))

    def value(s: Shape): String = s
  }
}
