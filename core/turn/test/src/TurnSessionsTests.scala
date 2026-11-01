package grit.turn

import java.time.Instant

import grit.core.durable.InMemoryDurable
import grit.core.id.{ConversationId, EntrySeq, Harness, PrincipalId, SessionId}
import grit.core.identity.Principal
import grit.core.session.{Event, Session}
import grit.core.store.{InMemoryEntryStore, InMemoryUsageLedger, Payload}
import grit.core.visibility.Label
import grit.dbos.sql.TestTx

import utest.*

/** A turn's session ([[TurnSessions]]) beyond the calls [[TurnTools.Settling]] logs: what its
  * summary adds.
  */
object TurnSessionsTests extends TestSuite {

  import TurnFixtures.*

  /** A turn said "hello", run with its sessions recorded into `spy`; the summary entry's seq and
    * text.
    */
  private def summarised(
      spy: RecordedSessions,
      entries: InMemoryEntryStore
  ): Option[(Long, String)] = {
    val turn = say(entries, "hello")
    new InMemoryDurable().run(turn.workflowId)(
      turnBodyWith(
        entries,
        new RecordingProvider,
        new Before(entries),
        new InMemoryUsageLedger,
        sessions = TurnSessions(spy, _ => None)
      )
    )
    entries
      .list(turn.conversationId)(using TestTx.fake)
      .toOption
      .flatMap(
        _.flatMap(e =>
          e.payload match {
            case Payload.Summary(text) => Some((EntrySeq.value(e.seq), text))
            case _ => None
          }
        ).headOption
      )
  }

  val tests: Tests = Tests {
    test("a turn's summary closes its session when one is recorded, at its seq, and opens none") {
      val entries = new InMemoryEntryStore
      val id = SessionId
        .of(Harness.Grit, ConversationId.value(conversation))
        .fold(e => throw new java.lang.AssertionError(e), identity)
      val local: Principal.Person = Principal.Person(PrincipalId.Local, Set.empty)
      val held = Session(
        id,
        None,
        local,
        Label.Public,
        origin.place,
        Instant.EPOCH,
        None,
        Instant.EPOCH,
        None,
        Session.Counters.zero,
        false,
        None
      )
      val open = new RecordedSessions(held = Some(held))
      val summary = summarised(open, entries)
      val none = new RecordedSessions(held = None)
      summarised(none, new InMemoryEntryStore)
      (
        open.recorded.flatMap(_._2).map(e => (e.n, e.kind)),
        open.recorded.map(_._1.by),
        none.recorded
      ) ==> (
        summary.toVector.flatMap((n, text) =>
          Session.Summary.cut(text).map(s => (n, Event.Kind.Closing(s)))
        ),
        Vector(local),
        Vector.empty
      )
    }
  }
}
