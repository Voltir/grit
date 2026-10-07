package grit.dbos.engine

import scala.annotation.unused
import scala.concurrent.duration.*

import grit.core.approval.Approval
import grit.core.durable.Durable
import grit.core.id.{SourceId, ToolCallId, TurnRef, WorkflowId}
import grit.core.identity.Account
import grit.core.inbox.InboxError
import grit.core.message.Message
import grit.core.store.Origin
import grit.dbos.sql.TestPostgres

import utest.*

/** [[Durable.recv]] over DBOS, answered by [[SqlInbox.answer]], against a real Postgres: a
  * workflow that waits for an answer, as a turn's gated call does.
  */
object AnswerLiveTests extends TestSuite {

  private val call = ToolCallId("call-1")

  private val approved = Approval.encode(Approval.Approved)

  /** Waits `within` for the answer to `call`, then a second before looking again; what each
    * look found.
    */
  private def waiting(within: FiniteDuration)(id: WorkflowId)(using d: Durable^): String = {
    val _ = id
    val first = d.recv(Approval.topic(call), within)
    val second = d.recv(Approval.topic(call), 1.second)
    s"$first;$second"
  }

  /** A close, or a post, that does nothing: none is enqueued here. */
  private def noClose(id: WorkflowId)(using @unused d: Durable^): String = WorkflowId.value(id)

  private def started(engine: Engine^, source: String): TurnRef = {
    val origin = Origin.Task("answer", source)
    val turn = for {
      t <- engine.inbox.ingest(origin, SourceId(source), Message.User(source), Account.Local)
      _ <- engine.inbox.startTurn(t)
    } yield t
    turn.fold(e => sys.error(s"inbox: $e"), identity)
  }

  /** Waits until `turn`'s workflow has begun its first wait: DBOS records the wait's end as
    * the wait begins.
    */
  private def waitingNow(engine: Engine^, turn: TurnRef): Unit = {
    val until = System.nanoTime() + 30.seconds.toNanos
    while (!engine.steps(turn).exists(_.name == "DBOS.sleep") && System.nanoTime() < until)
      Thread.sleep(50)
    assert(engine.steps(turn).exists(_.name == "DBOS.sleep"))
  }

  val tests = Tests {
    test("an answer reaches the waiting workflow once; a second answer is ignored") {
      val engine = LiveEngine.open(TestPostgres.freshDatabase("answer_once"), "test")
      try {
        engine.launch(
          waiting(1.minute),
          noClose,
          noClose,
          noClose,
          noClose,
          LiveEngine.Unplaced,
          Vector.empty
        )
        val turn = started(engine, "once")
        waitingNow(engine, turn)
        engine.inbox.answer(turn.workflowId, call, Approval.Approved) ==> Right(())
        engine.inbox.answer(turn.workflowId, call, Approval.Declined(None)) ==> Right(())
        engine.awaitTurn(turn) ==> s"Some($approved);None"
      } finally engine.close()
    }

    test("an answer sent before the wait begins is received when it does") {
      val engine = LiveEngine.open(TestPostgres.freshDatabase("answer_early"), "test")
      try {
        engine.launch(
          waiting(1.minute),
          noClose,
          noClose,
          noClose,
          noClose,
          LiveEngine.Unplaced,
          Vector.empty
        )
        val origin = Origin.Task("answer", "early")
        val turn = engine.inbox
          .ingest(origin, SourceId("early"), Message.User("early"), Account.Local)
          .fold(e => sys.error(s"inbox: $e"), identity)
        // The workflow must exist to be sent to: enqueued, then answered before it runs.
        engine.inbox.startTurn(turn) ==> Right(())
        engine.inbox.answer(turn.workflowId, call, Approval.Approved) ==> Right(())
        engine.awaitTurn(turn) ==> s"Some($approved);None"
      } finally engine.close()
    }

    test("nothing sent: the wait runs out, and says so") {
      val engine = LiveEngine.open(TestPostgres.freshDatabase("answer_none"), "test")
      try {
        engine.launch(
          waiting(1.second),
          noClose,
          noClose,
          noClose,
          noClose,
          LiveEngine.Unplaced,
          Vector.empty
        )
        val turn = started(engine, "none")
        engine.awaitTurn(turn) ==> "None;None"
      } finally engine.close()
    }

    test("an answer to a workflow that does not exist is NoSuchTurn") {
      val engine = LiveEngine.open(TestPostgres.freshDatabase("answer_missing"), "test")
      try {
        engine.launch(
          waiting(1.second),
          noClose,
          noClose,
          noClose,
          noClose,
          LiveEngine.Unplaced,
          Vector.empty
        )
        val nobody = WorkflowId("no-such-workflow")
        engine.inbox.answer(nobody, call, Approval.Approved) ==>
          Left(InboxError.NoSuchTurn(nobody))
      } finally engine.close()
    }

    test("a restart while waiting: the recovered workflow still waits, and gets the answer") {
      val config = TestPostgres.freshDatabase("answer_restart")
      val before = LiveEngine.open(config, "test")
      val turn =
        try {
          before.launch(
            waiting(1.minute),
            noClose,
            noClose,
            noClose,
            noClose,
            LiveEngine.Unplaced,
            Vector.empty
          )
          val t = started(before, "restart")
          waitingNow(before, t)
          t
        } finally before.close()
      val after = LiveEngine.open(config, "test")
      try {
        after.launch(
          waiting(1.minute),
          noClose,
          noClose,
          noClose,
          noClose,
          LiveEngine.Unplaced,
          Vector.empty
        )
        after.inbox.answer(turn.workflowId, call, Approval.Approved) ==> Right(())
        after.awaitTurn(turn) ==> s"Some($approved);None"
      } finally after.close()
    }
  }
}
