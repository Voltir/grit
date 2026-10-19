package grit.core.edge

import grit.core.id.ConversationId
import grit.core.store.{Origin, Tx}
import grit.core.visibility.{Clearance, Label, TestLabels}

import utest.*

/** The contract every [[Origins]] keeps, run against the in-memory fake in core and the SQL
  * store in grit.dbos.
  */
abstract class OriginsContract extends TestSuite {

  /** What the suite reads. */
  protected def origins: Origins

  /** The conversation `origin` names, created at `label` when new. */
  protected def conversation(origin: Origin, label: Label): ConversationId

  /** Runs `body` in one transaction opened at `clearance`. */
  protected def opened[A](clearance: Clearance)(body: (Tx^) ?=> A): A

  private val Public = Clearance.of(Label.Public)

  val tests = Tests {
    test("a conversation the transaction reads is read as its origin") {
      val o = Origin.Slack("T1", "C1", "1700000000.000100")
      val c = conversation(o, Label.Public)
      opened(Public)(origins.of(c)) ==> Right(Some(o))
    }

    test("a conversation above the reader's clearance is absent, and read by one cleared for it") {
      val o = Origin.Slack("T1", "C2", "1700000000.000200")
      val c = conversation(o, TestLabels.Trial)
      (opened(Public)(origins.of(c)), opened(Clearance.of(TestLabels.Trial))(origins.of(c))) ==>
        (Right(None), Right(Some(o)))
    }

    test("a conversation never created is absent") {
      conversation(Origin.Slack("T1", "C3", "1700000000.000300"), Label.Public)
      opened(Public)(
        origins.of(ConversationId("0190f3a0-0000-7000-8000-000000000000"))
      ) ==> Right(None)
    }
  }
}
