package grit.core.edge

import java.time.Instant

import grit.core.document.Document
import grit.core.id.{ClaimHolder, Harness, SessionId}
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
    * overflow lines included. [[StoreError.Invalid]] when no person is linked to `by`, when a
    * batch's session is grit's own ([[EdgeSessions.refused]]), or when it was begun by another
    * person than the one `by` is linked to, or is kept where the transaction does not read
    * ([[EdgeSessions.notTheirs]]); [[StoreError.DatabaseError]] when the database could not open,
    * write or commit it.
    */
  def record(by: Account, room: Place, batches: Vector[EdgeSessions.Batch]): Either[StoreError, Int]

  /** [[grit.core.session.Sessions.lately]] for the session `id`, in one transaction opened in
    * `room`: none when there is no such session, or it is kept where the transaction does not
    * read. [[StoreError.DatabaseError]] when the database could not open or read it.
    */
  def lately(room: Place, id: SessionId): Either[StoreError, Option[Session.Lately]]

  /** What the session `own`, working as `holder`, opens with: the brief kept for `holder`, current
    * at `at` ([[grit.core.document.DocumentSearch.briefOf]]), and what `own` itself did lately
    * ([[grit.core.session.Sessions.lately]]), whichever other sessions work as `holder`; none when
    * it has no record. Both are read in one read-only transaction opened for the person `by` is
    * linked to then, in `room` ([[grit.core.visibility.Subject.Person]]), so each is one that
    * person reads there, and none when they read none; what it answers may be shown in `room` and
    * nowhere lower. [[BriefError.NotVouched]] for an account no person is linked to;
    * [[BriefError.Unavailable]] when the database could not open or read it, or the edge was built
    * without the session record.
    */
  def briefed(
      by: Account,
      room: Place,
      holder: ClaimHolder,
      own: SessionId,
      at: Instant
  ): Either[BriefError, EdgeSessions.Briefed]
}

object EdgeSessions {

  /** Why an edge may not record the session `id`: it is grit's own ([[Harness.Grit]]), which
    * only its turns record; `None` for any other harness's.
    */
  def refused(id: SessionId): Option[StoreError] =
    Option.when(id.harness == Harness.Grit)(
      StoreError.Invalid(
        s"session ${id.written} is grit's own: only its turns record it"
      )
    )

  /** Why a person may not record on the session `id`: someone else began it, or it is kept where
    * they do not read, told alike.
    */
  def notTheirs(id: SessionId): StoreError =
    StoreError.Invalid(
      s"the session ${id.written} was begun by someone else; only whoever began a session records on it"
    )

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

  /** Records and reads nothing: `record` and `lately` answer `Left(StoreError.Invalid)`, and
    * `briefed` `Left(BriefError.Unavailable)`, each saying the edge was built without the session
    * record.
    */
  def Refusing: EdgeSessions = new EdgeSessions {
    def record(
        by: Account,
        room: Place,
        batches: Vector[Batch]
    ): Either[StoreError, Int] = Left(NoRecord)
    def lately(room: Place, id: SessionId): Either[StoreError, Option[Session.Lately]] =
      Left(NoRecord)
    def briefed(
        by: Account,
        room: Place,
        holder: ClaimHolder,
        own: SessionId,
        at: Instant
    ): Either[BriefError, Briefed] = Left(BriefError.Unavailable(Unrecorded))
  }

  /** A session's opening as data: `brief`, its holder's brief, and `lately`, what the session
    * itself did lately. An edge shows them as a turn's opening does ([[grit.core.context.Shown.brief]], and
    * [[grit.core.session.SessionDigest.of]] at [[grit.core.session.SessionDigest.Chars]]).
    */
  final case class Briefed(brief: Option[Document], lately: Option[Session.Lately])
      extends caps.Pure

  private val Unrecorded = "this edge was given no session record"
  private val NoRecord = StoreError.Invalid(Unrecorded)
}

/** Why [[EdgeSessions.briefed]] read nothing. */
enum BriefError {

  /** No person is linked to the account `by`. */
  case NotVouched(by: Account)

  /** The database could not open or read the transaction, or the edge has no session record:
    * `cause` says why.
    */
  case Unavailable(cause: String)
}
