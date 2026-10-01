package grit.core.host

/** Which process on which machine something is: the name `machine` gives itself, and the
  * operating system's `pid` for the process. Read once, by `grit.host`, and passed to what
  * records it (the engine's row, an edge's registration).
  */
final case class ProcessIdentity(machine: String, pid: Long)
