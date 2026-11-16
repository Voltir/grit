package grit.core.edge

import java.time.Instant

/** The engine holding a database's lock: its `machine`, the host name its operating system gives
  * it ([[grit.core.host.ProcessIdentity]]), and process `pid`, its compatibility `epoch`, when it
  * `started`, and its last `heartbeat`. Not a [[grit.core.id.ClaimHolder]], who works a claimed
  * artifact, nor named by a deployment's configuration as a caller's machine is.
  */
final case class Holder(
    machine: String,
    pid: Long,
    epoch: String,
    started: Instant,
    heartbeat: Instant
)
