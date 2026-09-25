package grit.core.host

import scala.concurrent.duration.FiniteDuration

/** Why the machine did not do what a tool asked. `message` is what the model reads. */
enum HostError {

  /** Nothing is at `path`. */
  case NotFound(path: RelPath)

  /** `path` is a directory where a file was wanted. */
  case NotAFile(path: RelPath)

  /** `path` is a file where a directory was wanted. */
  case NotADirectory(path: RelPath)

  /** `path` is not UTF-8 text. */
  case NotText(path: RelPath)

  /** `path` is `bytes` long, over the `max` a tool reads. */
  case TooLarge(path: RelPath, bytes: Long, max: Long)

  /** `offset` is past the end of `path`, which has `lines` lines. */
  case PastEnd(path: RelPath, offset: Int, lines: Int)

  /** `path` is a link that leads outside the checkout, or to a secrets file. */
  case Outside(path: RelPath)

  /** `pattern` is not a regular expression: `why`. */
  case BadPattern(pattern: String, why: String)

  /** A command ran past `after` and was killed, with every process it started; `output` is
    * what it wrote until then.
    */
  case TimedOut(after: FiniteDuration, output: Clipped)

  /** The operating system refused or failed: a permission, a full disk, a command that
    * could not start. `why` is its own words.
    */
  case Failed(why: String)

  /** One or two sentences: what went wrong, and for a timeout the output so far. */
  def message: String = this match {
    case NotFound(p) => s"There is no file or directory `${RelPath.value(p)}`."
    case NotAFile(p) => s"`${RelPath.value(p)}` is a directory; use `list` to see inside it."
    case NotADirectory(p) => s"`${RelPath.value(p)}` is a file, not a directory."
    case NotText(p) => s"`${RelPath.value(p)}` is not UTF-8 text."
    case TooLarge(p, bytes, max) =>
      s"`${RelPath.value(p)}` is $bytes bytes, over the $max a tool reads."
    case PastEnd(p, offset, lines) =>
      s"Offset $offset is past the end of `${RelPath.value(p)}`, which has $lines lines."
    case Outside(p) =>
      s"`${RelPath.value(p)}` is a link that leads outside the checkout or to a secrets file."
    case BadPattern(pattern, why) => s"`$pattern` is not a regular expression: $why"
    case TimedOut(after, output) =>
      s"The command was killed after ${after.toSeconds} seconds. Its output until then:\n" +
        output.show
    case Failed(why) => why
  }
}
