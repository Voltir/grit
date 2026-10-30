package grit.core.edge

import java.time.Instant

/** The engine holding a database's lock: its `machine` and process `pid`, its compatibility
  * `epoch`, when it `started`, and its last `heartbeat`.
  */
final case class Holder(
    machine: String,
    pid: Long,
    epoch: String,
    started: Instant,
    heartbeat: Instant
)
