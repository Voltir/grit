package grit.core.edge

import grit.core.id.{ClaimHolder, SessionId}
import grit.core.identity.Account
import grit.core.place.Place
import grit.core.session.{Event, Session}
import grit.core.store.StoreError

/** The working sessions an edge records for the people it serves, in grit's words (ADR 0050). */
trait EdgeSessions extends caps.SharedCapability {

  /** Records each batch's events on its session as [[grit.core.session.Sessions.record]] does
    * (an event recorded again changes nothing), all of them in one transaction opened in `room`
    * ([[grit.core.visibility.Subject.Room]]): every batch is kept, or none. A session new to the
    * record begins with the person `by` is linked to then as its runner, the transaction's floor
    * as its label, its batch's `parent`, `at` and `holder`, and no room. How many lines were logged,
    * overflow lines included. [[StoreError.Invalid]] when no person is linked to `by`, or when a
    * session is kept where the transaction does not read; [[StoreError.DatabaseError]] when the
    * database could not open, write or commit it.
    */
  def record(by: Account, room: Place, batches: Vector[EdgeSessions.Batch]): Either[StoreError, Int]

  /** [[grit.core.session.Sessions.lately]] for the session `id`, in one transaction opened in
    * `room`: none when there is no such session, or it is kept where the transaction does not
    * read. [[StoreError.DatabaseError]] when the database could not open or read it.
    */
  def lately(room: Place, id: SessionId): Either[StoreError, Option[Session.Lately]]
}

object EdgeSessions {

  /** `events` of the session `id`, a child of `parent` when it has one, worked at `at`, as the
    * claim holder `holder` when its harness names one.
    */
  final case class Batch(
      id: SessionId,
      parent: Option[SessionId],
      at: Place,
      holder: Option[ClaimHolder],
      events: Vector[Event]
  )

  /** Records and reads nothing: every call is `Left(StoreError.Invalid)`, saying the edge was
    * built without the session record.
    */
  def Refusing: EdgeSessions = new EdgeSessions {
    def record(
        by: Account,
        room: Place,
        batches: Vector[Batch]
    ): Either[StoreError, Int] = Left(NoRecord)
    def lately(room: Place, id: SessionId): Either[StoreError, Option[Session.Lately]] =
      Left(NoRecord)
  }

  private val NoRecord = StoreError.Invalid("this edge was given no session record")
}
