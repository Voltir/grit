package grit.core.session

import grit.core.id.SessionId
import grit.core.store.{StoreError, Tx}

/** Every working session's record, a grit turn's or a harness's: one row each, whatever its
  * label, and its log ([[Log]]).
  */
trait Sessions extends caps.Pure {

  /** Records `events` on the session `header` names, beginning its row from `header` when there
    * is none, started at the earliest event's time; an existing row keeps its own parent,
    * person, label and holder, except that a row with no holder takes the first header naming
    * one. A harness names its holder only on the event that starts a session, so a session
    * first recorded mid-life has none.
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

  /** The session `id` and what it did lately, when this transaction reads it: its last
    * [[Session.Lately.Most]] commits, leaving out any whose shape names no [[Event.Shape.Sha]]
    * and any a later commit of the session records again or [[Event.Shape.Replaces]], and
    * counting apart those it marks [[Event.Shape.Child]]; and the last
    * [[Session.Lately.Most]] distinct paths ([[Event.Shape.Path]], else [[Event.Shape.Named]]),
    * each with the worktree its shape names ([[Event.Shape.Worktree]], when it reads as a
    * place), among its last [[Session.Lately.Scanned]] edits, and likewise its reads. What it
    * last said is the record's [[Session.summary]], its last closing.
    */
  def lately(id: SessionId)(using Tx^): Either[StoreError, Option[Session.Lately]]
}

object Sessions {

  /** What one recording logged: `logged` lines, its overflow included; `full` when the log is
    * full after it.
    */
  final case class Recorded(logged: Int, full: Boolean)
}
