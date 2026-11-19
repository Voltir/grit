package grit.commands.declare

import grit.core.edge.{CommandError, WatchFor, Watched}

/** A wait on a plugin's document that `/wait` reads from a body naming no commit: `keys`, the
  * body's keys `read` reads, beside `within`, `/wait` refusing any other; `read` the watch the
  * body asks for, lent the request's `ran` (to resolve what the body names, say), `None` when it
  * asks for none this reads, `Left` answered as a command's error is ([[CommandError.Unread]]
  * 400, saying its reason); `met`, what the answer says once the document holds what was watched
  * for.
  */
final case class HoldingWatch(
    keys: Set[String],
    read: (RunCommand, ujson.Obj) -> Either[CommandError, Option[WatchFor.Holding]],
    met: (WatchFor.Holding, Watched.Held) -> String
) extends caps.Pure
