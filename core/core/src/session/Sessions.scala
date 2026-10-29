package grit.core.session

import grit.core.id.SessionId
import grit.core.store.{StoreError, Tx}

/** Every working session's record, a grit turn's or a harness's: one row each, whatever its
  * label, and its log ([[Log]]).
  */
trait Sessions extends caps.Pure {

  /** Records `events` on the session `header` names, beginning its row from `header` when there
    * is none, started at the earliest event's time; an existing row keeps its own parent,
    * person and label.
    *   - An event whose `n` a line of the log already holds is a repeat and is ignored, as is a
    *     second event of one `n` in `events`: recording again is a no-op.
    *   - The rest are logged in `n` order ([[Log.admit]]) and counted ([[Session.Counters]]); an
    *     `ended` event ends the session at its time, a `started` event after that opens it
    *     again ([[Session.ending]]), and a `closing` event's summary replaces its summary.
    *   - Past the log's overflow an event only moves the session's last event.
    *
    * Recordings of one session wait on each other. Empty `events` begin no row. `Left(Invalid)`
    * when the session exists at a label this transaction does not read.
    */
  def record(header: Session.Header, events: Vector[Event])(using
      Tx^
  ): Either[StoreError, Sessions.Recorded]

  /** The session `id`, when this transaction reads it. */
  def read(id: SessionId)(using Tx^): Either[StoreError, Option[Session]]
}

object Sessions {

  /** What one recording logged: `logged` lines, its overflow included; `full` when the log is
    * full after it.
    */
  final case class Recorded(logged: Int, full: Boolean)
}
