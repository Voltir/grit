package grit.core.inbox

import grit.core.id.{PrincipalId, SourceId}
import grit.core.message.Message
import grit.core.store.Origin

import utest.*

/** What every [[Inbox]] keeps of recording messages and reading a turn's progress, run
  * against the in-memory fake in core and SqlInbox in grit.dbos.
  */
abstract class InboxContract extends TestSuite {

  /** Runs `body` with an inbox over a store holding nothing from `origin`. */
  protected def withInbox[A](body: Inbox => A): A

  val tests = Tests {
    test("ingested: the turn a message was recorded as; none for one never recorded") {
      withInbox { inbox =>
        val here = Origin.Task("inbox", "ingested")
        val there = Origin.Task("inbox", "elsewhere")
        val turn = inbox.ingest(here, SourceId("m1"), Message.User("one"), PrincipalId.Local)
        inbox.ingested(here, SourceId("m1")) ==> turn.map(Some(_))
        inbox.ingested(here, SourceId("m2")) ==> Right(None)
        inbox.ingested(there, SourceId("m1")) ==> Right(None)
      }
    }

    test("a turn recorded but never started is Open") {
      withInbox { inbox =>
        val here = Origin.Task("inbox", "open")
        val turn = inbox.ingest(here, SourceId("m1"), Message.User("one"), PrincipalId.Local)
        turn.flatMap(inbox.progress) ==> Right(Progress.Open)
      }
    }
  }
}
