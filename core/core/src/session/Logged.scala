package grit.core.session

import java.nio.charset.StandardCharsets
import java.time.Instant

/** What a tool call that ran adds to its session's log ([[Sessions.record]]), short of which
  * call and when: a read, an edit, a command run or a commit made, each with its shape.
  */
enum Logged extends caps.Pure {
  case Read(shape: Event.Shape)
  case Edited(shape: Event.Shape)
  case Ran(shape: Event.Shape)
  case Commit(shape: Event.Shape)

  /** This as the event `n` at `at` of a call whose result the model was shown as `shown`, a
    * read counting its UTF-8 bytes; or why not, as [[Event.of]].
    */
  def event(n: Long, at: Instant, shown: String): Either[String, Event] = this match {
    case Read(shape) =>
      Event.of(n, at, Event.Kind.Read(shown.getBytes(StandardCharsets.UTF_8).length.toLong), shape)
    case Edited(shape) => Event.of(n, at, Event.Kind.Edited, shape)
    case Ran(shape) => Event.of(n, at, Event.Kind.Ran, shape)
    case Commit(shape) => Event.of(n, at, Event.Kind.Commit, shape)
  }
}

object Logged {

  /** `{key: value}` as a shape; the empty shape when that is over [[Event.Shape.Longest]]. */
  def shaped(key: String, value: String): Event.Shape =
    Event.Shape.of(ujson.Obj(key -> value)).getOrElse(Event.Shape.Empty)
}
