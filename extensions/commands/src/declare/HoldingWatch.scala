package grit.commands.declare

import grit.core.edge.{WatchFor, Watched}

/** A wait on a plugin's document that `/wait` reads from a body naming no commit: `read` the
  * watch the body asks for, `None` when it asks for none this reads, `Left` saying why it cannot
  * be read; `met`, what the answer says once the document holds what was watched for.
  */
final case class HoldingWatch(
    read: ujson.Obj -> Option[Either[String, WatchFor.Holding]],
    met: (WatchFor.Holding, Watched.Held) -> String
) extends caps.Pure
