package grit.core.edge

import java.time.Instant

import grit.core.id.{ClaimHolder, Harness, SessionId}
import grit.core.place.{Namespace, Place}
import grit.core.session.Event
import grit.core.store.StoreError

import utest.*

/** What an edge's routes rely on of [[EdgeSessions]]: a batch recorded once, the sessions it
  * refuses, and the brief and newest session a holder opens with, besides the one named. Run by
  * the scripted fake in core and by the SQL sessions in grit.dbos. It leaves labels and rooms,
  * which the fake does not read, to the SQL sessions' own suite, and what a session did lately,
  * past which session it is, to the session store's; a database that cannot be read is untested.
  */
abstract class EdgeSessionsContract extends TestSuite {

  /** `body` lent the sessions of a store where [[EdgeContracts.Reader]] is linked to a person and
    * [[EdgeContracts.Stranger]] is not, holding what [[brief]] put there.
    */
  protected def recording[A](body: EdgeSessions^ => A): A

  /** Keeps `text` as `holder`'s brief, public, at [[EdgeSessionsContract.Worktree]], written at
    * `written`.
    */
  protected def brief(holder: ClaimHolder, text: String, written: Instant): Unit

  import EdgeContracts.*
  import EdgeSessionsContract.*

  private def record(by: grit.core.identity.Account, batches: EdgeSessions.Batch*) =
    recording(_.record(by, Room, batches.toVector))

  private def worked(local: String, holder: ClaimHolder, seconds: Long*) =
    record(Reader, batch(session(local), Some(holder), seconds*))

  private def opening(holder: ClaimHolder, besides: Option[SessionId]) =
    recording(_.briefed(Reader, Room, holder, besides, At.plusSeconds(1000)))
      .map(b =>
        (b.brief.map(d => grit.core.document.DocText.value(d.text)), b.lately.map(_.session.id))
      )

  val tests = Tests {
    test("a batch is logged once: its events counted, and recording it again logs none") {
      val b = batch(session("once"), None, 1, 2)
      (record(Reader, b), record(Reader, b)) ==> (Right(2), Right(0))
    }

    test("an account no person is linked to, or grit's own session, records nothing") {
      val own = got(SessionId.of(Harness.Grit, "contract-own"))
      (
        record(Stranger, batch(session("stranger"), None, 1)),
        record(Reader, batch(session("before-own"), None, 1), batch(own, None, 1)),
        record(Reader, batch(session("before-own"), None, 1))
      ) ==> (
        Left(
          StoreError.Invalid(
            s"no person is linked to ${grit.core.identity.Account.written(Stranger)}"
          )
        ),
        Left(
          StoreError.Invalid("session grit:contract-own is grit's own: only its turns record it")
        ),
        Right(1)
      )
    }

    test(
      "briefed answers the holder's brief and its newest session by last event, besides the one named"
    ) {
      val (h, other) =
        (got(ClaimHolder.of("contract/briefed")), got(ClaimHolder.of("contract/other")))
      brief(h, "Do the contracted thing.", At)
      worked("early", h, 1, 50)
      worked("late", h, 10, 20)
      worked("another", other, 100)
      (
        opening(h, None),
        opening(h, Some(session("early"))),
        opening(other, Some(session("another")))
      ) ==> (
        Right((Some("Do the contracted thing."), Some(session("early")))),
        Right((Some("Do the contracted thing."), Some(session("late")))),
        Right((None, None))
      )
    }

    test("briefed for an account no person is linked to is NotVouched") {
      recording(_.briefed(Stranger, Room, got(ClaimHolder.of("contract/briefed")), None, At)) ==>
        Left(BriefError.NotVouched(Stranger))
    }
  }
}

object EdgeSessionsContract {

  val Contracted: Harness = got(Harness.of("contracted"))

  /** The contracted harness's session `local`. */
  def session(local: String): SessionId = got(SessionId.of(Contracted, local))

  /** A worktree on a worker, where every session and brief here is. */
  val Worktree: Place = Place.under(Namespace.Worker, Vector("desk", "/work/c"))

  /** `id`'s edits at each of `seconds` after [[EdgeContracts.At]], each its own ordinal, at
    * [[Worktree]], as `holder` when given.
    */
  def batch(id: SessionId, holder: Option[ClaimHolder], seconds: Long*): EdgeSessions.Batch =
    EdgeSessions.Batch(
      id,
      None,
      Worktree,
      holder,
      seconds.toVector.map(s =>
        got(Event.of(s, EdgeContracts.At.plusSeconds(s), Event.Kind.Edited, Event.Shape.Empty))
      )
    )

  def got[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), a => a)
}
