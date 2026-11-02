package grit.core.edge

import java.time.Instant

/** The engine holding a database's lock: its `machine` and process `pid`, its compatibility
  * `epoch`, when it `started`, and its last `heartbeat`. Not a [[grit.core.id.ClaimHolder]],
  * who works a claimed artifact.
  */
final case class Holder(
    machine: String,
    pid: Long,
    epoch: String,
    started: Instant,
    heartbeat: Instant
)
