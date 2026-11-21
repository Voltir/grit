package grit.core.edge

import java.time.Instant

import grit.core.document.{DocText, Takeover}
import grit.core.either.Got.got
import grit.core.id.{ClaimHolder, DocKey, Harness, SessionId}
import grit.core.place.{Namespace, Place}
import grit.core.session.Event
import grit.core.store.StoreError

import utest.*

/** What an edge's routes rely on of [[EdgeSessions]]: a batch recorded once, the sessions it
  * refuses, and the brief of a holder and the record of the session that opens with it. Run by
  * the scripted fake in core and by the SQL sessions in grit.dbos. It leaves labels and rooms,
  * which the fake does not read, to the SQL sessions' own suite, and what a session did lately
  * past its record (its commits, edits and reads) to the session store's; a database that cannot
  * be read is untested.
  */
abstract class EdgeSessionsContract extends TestSuite {

  /** `body` lent the sessions of a store where [[EdgeContracts.Reader]] and [[EdgeContracts.Other]]
    * are linked to two persons and [[EdgeContracts.Stranger]] to none, holding what [[brief]] put
    * there.
    */
  protected def recording[A](body: EdgeSessions^ => A): A

  /** Keeps `text` as `holder`'s brief, public, at [[EdgeSessionsContract.Worktree]], written at
    * `written`.
    */
  protected def brief(holder: ClaimHolder, text: String, written: Instant): Unit

  /** Keeps `text` as `holder`'s brief as [[brief]] does, its data naming the claim on `subject`
    * and [[EdgeSessionsContract.Moving]] ([[grit.core.document.BriefClaim]]), and that claim, live, for `holder`,
    * worked by `session`.
    */
  protected def claimedBrief(
      holder: ClaimHolder,
      text: String,
      written: Instant,
      subject: DocKey,
      session: SessionId
  ): Unit

  import EdgeContracts.*
  import EdgeSessionsContract.*

  private def record(by: grit.core.identity.Account, batches: EdgeSessions.Batch*) =
    recording(_.record(by, Room, batches.toVector))

  private def worked(local: String, holder: ClaimHolder, seconds: Long*) =
    record(Reader, batch(session(local), Some(holder), seconds*))

  // The holder the session `id` works as, when it has a record.
  private def holderOf(id: SessionId) = recording(_.lately(Room, id)).map(_.map(_.session.holder))

  private def opening(holder: ClaimHolder, own: SessionId) =
    recording(_.briefed(Reader, Room, holder, own, At.plusSeconds(1000)))
      .map(b => (shown(b.brief), b.lately.map(_.session.id)))

  /** What `brief` shows: its text, or for a claim another session works, that session. */
  private def shown(brief: EdgeSessions.Briefed.Brief): Option[String] = brief match {
    case EdgeSessions.Briefed.Brief.Absent => None
    case EdgeSessions.Briefed.Brief.Kept(d) => Some(DocText.value(d.text))
    case EdgeSessions.Briefed.Brief.Held(c, t) =>
      Some(s"held by ${c.session.map(_.written)} for ${ClaimHolder.value(c.holder)}, moved by $t")
  }

  val tests = Tests {
    test("a batch is logged once: its events counted, and recording it again logs none") {
      val b = batch(session("once"), None, 1, 2)
      (record(Reader, b), record(Reader, b)) ==> (Right(2), Right(0))
    }

    test("a batch partly recorded before logs only its new events") {
      val id = session("partly")
      (record(Reader, batch(id, None, 1, 2)), record(Reader, batch(id, None, 1, 2, 3, 4))) ==>
        (Right(2), Right(2))
    }

    test("a session recorded again as another holder keeps its first") {
      val (first, later) =
        (got(ClaimHolder.of("contract/first")), got(ClaimHolder.of("contract/later")))
      worked("kept", first, 1)
      worked("kept", later, 2)
      holderOf(session("kept")) ==> Right(Some(Some(first)))
    }

    test(
      "a batch with no new events begins no session, but gives a session with no holder its own"
    ) {
      val (none, late) =
        (got(ClaimHolder.of("contract/unbegun")), got(ClaimHolder.of("contract/late")))
      (
        record(Reader, batch(session("unbegun"), Some(none))),
        record(Reader, batch(session("holderless"), None, 1)),
        record(Reader, batch(session("holderless"), Some(late), 1)),
        holderOf(session("unbegun")),
        holderOf(session("holderless"))
      ) ==> (
        Right(0),
        Right(1),
        Right(0),
        Right(None),
        Right(Some(Some(late)))
      )
    }

    test("lately answers a recorded session as it was recorded, and none for one never recorded") {
      val h = got(ClaimHolder.of("contract/lately"))
      val _ = worked("lately", h, 1, 2)
      (
        recording(_.lately(Room, session("lately"))).map(
          _.map(l => (l.session.id, l.session.holder, l.session.counters, l.session.at))
        ),
        recording(_.lately(Room, session("never")))
      ) ==> (
        Right(
          Some(
            (
              session("lately"),
              Some(h),
              grit.core.session.Session.Counters.of(batch(session("lately"), None, 1, 2).events),
              Worktree
            )
          )
        ),
        Right(None)
      )
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
      "a batch on a session another person began is refused, keeping none of its call's batches"
    ) {
      val (begun, theirs) = (session("begun"), session("theirs"))
      (
        record(Reader, batch(begun, None, 1)),
        record(Other, batch(theirs, None, 1), batch(begun, None, 2)),
        record(Other, batch(theirs, None, 1)),
        record(Reader, batch(begun, None, 1, 2))
      ) ==> (
        Right(1),
        Left(EdgeSessions.notTheirs(begun)),
        Right(1),
        Right(1)
      )
    }

    test(
      "briefed answers the holder's brief and what the session named did lately, whatever else ran"
    ) {
      val (h, other) =
        (got(ClaimHolder.of("contract/briefed")), got(ClaimHolder.of("contract/other")))
      brief(h, "Do the contracted thing.", At)
      worked("early", h, 1, 50)
      worked("late", h, 10, 20)
      worked("another", other, 100)
      (
        opening(h, session("late")),
        opening(h, session("early")),
        opening(other, session("never"))
      ) ==> (
        Right((Some("Do the contracted thing."), Some(session("late")))),
        Right((Some("Do the contracted thing."), Some(session("early")))),
        Right((None, None))
      )
    }

    test(
      "briefed answers a session another session's claim in place of its brief, and the brief to the session working it"
    ) {
      val h = got(ClaimHolder.of("contract/held"))
      worked("worker", h, 1)
      claimedBrief(
        h,
        "Work the held thing.",
        At,
        got(DocKey.of("contract/held-1")),
        session("worker")
      )
      worked("bystander", h, 2)
      (opening(h, session("worker")), opening(h, session("bystander"))) ==> (
        Right((Some("Work the held thing."), Some(session("worker")))),
        Right(
          (
            Some(
              s"held by Some(${session("worker").written}) for contract/held, moved by ${Some(Moving)}"
            ),
            Some(session("bystander"))
          )
        )
      )
    }

    test("briefed for an account no person is linked to is NotVouched") {
      recording(
        _.briefed(Stranger, Room, got(ClaimHolder.of("contract/briefed")), session("asked"), At)
      ) ==>
        Left(BriefError.NotVouched(Stranger))
    }
  }
}

object EdgeSessionsContract {

  val Contracted: Harness = got(Harness.of("contracted"))

  /** The takeover every claimed brief here names. */
  val Moving: Takeover = Takeover("contract-cli claim", "c/held")

  /** The contracted harness's session `local`. */
  def session(local: String): SessionId = got(SessionId.of(Contracted, local))

  /** A worktree on a machine, where every session and brief here is. */
  val Worktree: Place = Place.under(Namespace.Machine, Vector("desk", "/work/c"))

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

}
