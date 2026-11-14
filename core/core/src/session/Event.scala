package grit.core.session

import java.nio.charset.StandardCharsets
import java.time.Instant

import grit.core.message.Cut
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

  object Kind {

    /** Every kind's [[Kind.name]], in the order the kinds are declared. */
    val Names: Vector[String] =
      Vector("started", "read", "edited", "ran", "commit", "closing", "ended")

    /** The kind whose [[Kind.name]] is `name`, a read's count of bytes from `bytes` and a
      * closing's summary from `summary`, each asked for only by its own kind; `None` when no kind
      * is so named.
      */
    def named[E](
        name: String,
        bytes: => Either[E, Long],
        summary: => Either[E, Session.Summary]
    ): Option[Either[E, Kind]] = name match {
      case "started" => Some(Right(Started))
      case "read" => Some(bytes.map(Read(_)))
      case "edited" => Some(Right(Edited))
      case "ran" => Some(Right(Ran))
      case "commit" => Some(Right(Commit))
      case "closing" => Some(summary.map(Closing(_)))
      case "ended" => Some(Right(Ended))
      case _ => None
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

    private val Own = Set(Worktree, Sha, Subject)

    /** A commit's shape: the worktree `at` it was made in under [[Worktree]], `sha` under
      * [[Sha]], every key of `besides` but those three (such as [[Child]] and [[Replaces]]), and
      * `subject` under [[Subject]], cut as [[Cut.head]] cuts it to fit [[Longest]], or left out
      * when not even `…` fits; `Left` when the rest alone do not fit.
      */
    def committed(
        at: Place,
        sha: String,
        subject: Option[String],
        besides: ujson.Obj = ujson.Obj()
    ): Either[String, Shape] = {
      val others = besides.value.toSeq.filterNot((k, _) => Own.contains(k))
      def shaped(s: Option[String]) =
        of(
          ujson.Obj.from(
            Seq(Worktree -> ujson.Str(at.written), Sha -> ujson.Str(sha)) ++ others ++
              s.map(Subject -> ujson.Str(_))
          )
        )
      subject.fold(shaped(None)) { whole =>
        (math.min(whole.length, Longest).max(1) to 1 by -1).iterator
          .map(k => shaped(Some(Cut.head(whole, k))))
          .collectFirst { case Right(shape) => shape }
          .toRight("")
          .orElse(shaped(None))
      }
    }

    def value(s: Shape): String = s
  }

  /** What a tool call that ran adds to its session's log ([[Sessions.record]]): an event's kind
    * and shape before the store gives it a seq and a time; a read, an edit, a command run or a
    * commit made.
    */
  enum Unrecorded extends caps.Pure {
    case Read(shape: Shape)
    case Edited(shape: Shape)
    case Ran(shape: Shape)
    case Commit(shape: Shape)

    /** Its kind, of a call whose result the model was shown as `shown`: a read counting its UTF-8
      * bytes.
      */
    def kind(shown: String): Kind = this match {
      case Read(_) => Kind.Read(shown.getBytes(StandardCharsets.UTF_8).length.toLong)
      case Edited(_) => Kind.Edited
      case Ran(_) => Kind.Ran
      case Commit(_) => Kind.Commit
    }

    /** Its shape. */
    def shape: Shape
  }

  object Unrecorded {

    /** A read of what `value` names under `key` ([[Shape.Path]] or [[Shape.Named]]); its shape
      * empty when that is over [[Shape.Longest]].
      */
    def read(key: String, value: String): Unrecorded = Read(one(key, value))

    /** An edit of the file at `path` ([[Shape.Path]]); its shape empty when that is over
      * [[Shape.Longest]].
      */
    def edited(path: String): Unrecorded = Edited(one(Shape.Path, path))

    private def one(key: String, value: String): Shape =
      Shape.of(ujson.Obj(key -> value)).getOrElse(Shape.Empty)
  }
}
