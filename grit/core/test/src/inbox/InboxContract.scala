package grit.core.inbox

import java.time.ZoneOffset

import grit.core.id.{PrincipalId, SourceId}
import grit.core.message.{Cost, Message}
import grit.core.spend.{Budget, DailyCap}
import grit.core.store.{Origin, Payload}

import utest.*

/** What every [[Inbox]] keeps of recording messages and reading a turn's progress, run
  * against the in-memory fake in core and SqlInbox in grit.dbos.
  */
abstract class InboxContract extends TestSuite {

  /** Runs `body` over a store holding nothing from the origins these tests use, with its
    * inbox, taking new messages as `budget` allows, and the [[InboxContract.Store]] under it.
    */
  protected def withInbox[A](budget: Budget)(body: (Inbox, InboxContract.Store^) => A): A

  private val Uncapped = Budget(ZoneOffset.UTC, None)

  private def said(text: String): Message.User = Message.User(text)

  val tests = Tests {
    test("ingested: the turn a message was recorded as; none for one never recorded") {
      withInbox(Uncapped) { (inbox, _) =>
        val here = Origin.Task("inbox", "ingested")
        val there = Origin.Task("inbox", "elsewhere")
        val turn = inbox.ingest(here, SourceId("m1"), said("one"), PrincipalId.Local)
        inbox.ingested(here, SourceId("m1")) ==> turn.map(Some(_))
        inbox.ingested(here, SourceId("m2")) ==> Right(None)
        inbox.ingested(there, SourceId("m1")) ==> Right(None)
      }
    }

    test("a turn recorded but never started is Open") {
      withInbox(Uncapped) { (inbox, _) =>
        val here = Origin.Task("inbox", "open")
        val turn = inbox.ingest(here, SourceId("m1"), said("one"), PrincipalId.Local)
        turn.flatMap(inbox.progress) ==> Right(Progress.Open)
      }
    }

    test(
      "a heard message is recorded once, as heard, under its author's name, and is no turn ingested"
    ) {
      withInbox(Uncapped) { (inbox, store) =>
        val here = Origin.Task("inbox", "heard")
        val ana = PrincipalId("task:ana")
        store.enroll(ana, "Ana")
        inbox.hear(here, SourceId("m1"), "standup moves to 10:00", ana) ==> Right(())
        inbox.hear(here, SourceId("m1"), "standup moves to 10:00", ana) ==> Right(())
        store.written(here) ==> Vector((Payload.Heard("standup moves to 10:00"), Some("Ana")))
        inbox.ingested(here, SourceId("m1")) ==> Right(None)
      }
    }

    test(
      "once the day's spend reaches the cap a new message is refused, recording nothing, not even its conversation; one already recorded is still its turn; a heard one is still recorded"
    ) {
      val cap = DailyCap.of("1").fold(e => throw new java.lang.AssertionError(e), identity)
      withInbox(Budget(ZoneOffset.UTC, Some(cap))) { (inbox, store) =>
        val here = Origin.Task("inbox", "capped")
        val first = inbox.ingest(here, SourceId("m1"), said("one"), PrincipalId.Local)
        store.spend(BigDecimal("0.4"))
        store.spend(BigDecimal("0.6"))
        inbox.ingest(here, SourceId("m1"), said("one"), PrincipalId.Local) ==> first
        val refused = inbox.ingest(here, SourceId("m2"), said("two"), PrincipalId.Local)
        refused.left.map {
          case InboxError.OverCap(spent, c, _) => (spent.calls, spent.cost, c)
          case other => other
        } ==> Left((2, Cost.Exact(BigDecimal("1.0")), cap))
        inbox.ingested(here, SourceId("m2")) ==> Right(None)
        val fresh = Origin.Task("inbox", "capped-first")
        inbox.ingest(fresh, SourceId("m1"), said("one"), PrincipalId.Local).isLeft ==> true
        store.exists(fresh) ==> false
        val heard = Origin.Task("inbox", "capped-heard")
        inbox.hear(heard, SourceId("m1"), "lunch?", PrincipalId.Local) ==> Right(())
        store.written(heard) ==> Vector((Payload.Heard("lunch?"), None))
      }
    }
  }
}

object InboxContract {

  /** What a test reads and writes of the store under an inbox: `spend` records a call that
    * cost that many dollars now; `exists`, whether a conversation from an origin exists;
    * `written`, the entries of an origin's conversation in order, each with the name its
    * author was enrolled under; `enroll` names a person.
    */
  final case class Store(
      spend: BigDecimal => Unit,
      exists: Origin => Boolean,
      written: Origin => Vector[(Payload, Option[String])],
      enroll: (PrincipalId, String) => Unit
  )
}
