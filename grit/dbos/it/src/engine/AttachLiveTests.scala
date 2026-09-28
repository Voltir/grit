package grit.dbos.engine

import scala.annotation.unused
import scala.concurrent.duration.*

import grit.core.durable.Durable
import grit.core.id.{PrincipalId, SourceId, WorkflowId}
import grit.core.message.Message
import grit.core.place.{Directory, Place}
import grit.core.store.Origin
import grit.dbos.sql.TestPostgres

import utest.*

/** A grit refused the engine lock attaches (ADR 0015): its link sends turns the engine runs,
  * names the engine, and says when there is none.
  */
object AttachLiveTests extends TestSuite {

  private def noop(id: WorkflowId)(using @unused d: Durable^): String = WorkflowId.value(id)

  private def eventually(within: FiniteDuration)(ok: => Boolean): Boolean = {
    val until = System.nanoTime() + within.toNanos
    var done = ok
    while (!done && System.nanoTime() < until) { Thread.sleep(50); done = ok }
    done
  }

  val tests = Tests {
    test("a turn an attached link sends runs on the engine that holds the lock") {
      val config = TestPostgres.freshDatabase("attach_send")
      val engine = LiveEngine.open(config, "test")
      val link = Link.attach(config, "test", LiveEngine.Identity, LiveEngine.Uncapped)
      try {
        engine.launch(id => d ?=> s"ran ${WorkflowId.value(id)}", noop, noop, noop, Vector.empty)
        val turn = (for {
          t <- link.inbox.ingest(
            Origin.Task("attach", "send"),
            SourceId("m1"),
            Message.User("hi"),
            PrincipalId.Local
          )
          _ <- link.inbox.startTurn(t)
        } yield t).fold(e => sys.error(s"$e"), identity)
        link.awaitTurn(turn) ==> s"ran ${WorkflowId.value(turn.workflowId)}"
      } finally {
        link.close()
        engine.close()
      }
    }

    test("an attached link names the engine holding the lock, and none once it stops") {
      val config = TestPostgres.freshDatabase("attach_holder")
      val engine = LiveEngine.open(config, "test")
      val link = Link.attach(config, "test", LiveEngine.Identity, LiveEngine.Uncapped)
      try {
        link.holder().map(h => (h.machine, h.pid, h.epoch)) ==> Some(
          ("test-machine", 4242L, "test")
        )
        engine.close()
        assert(eventually(5.seconds)(link.holder().isEmpty))
      } finally {
        link.close()
        engine.close()
      }
    }

    test("an edge attached as another epoch than the engine's is refused, naming both") {
      val config = TestPostgres.freshDatabase("attach_epoch")
      val engine = LiveEngine.open(config, "test")
      val link = Link.attach(config, "older", LiveEngine.Identity, LiveEngine.Uncapped)
      try {
        val place =
          Place.of(Directory.of("/attach").getOrElse(throw new java.lang.AssertionError()))
        link.register(PrincipalId.Local, Set(place)).left.map(_.why) ==> Left(
          "the engine (pid 4242 on test-machine) runs epoch test, " +
            "and this grit older: it cannot serve that engine's turns"
        )
      } finally {
        link.close()
        engine.close()
      }
    }
  }
}
