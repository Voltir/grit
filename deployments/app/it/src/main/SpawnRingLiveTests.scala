package grit.app.main

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{CountDownLatch, LinkedBlockingQueue, TimeUnit}

import scala.concurrent.duration.DurationInt

import grit.act.phase.{Delegated, SpawnEnv, SpawnSteps, Spawning}
import grit.core.act.{Acting, ActsFor, Allowance, Gates}
import grit.core.agent.SubagentFixtures.{ok, scholar, services, subagent}
import grit.core.durable.{Durable, Flows, StepName}
import grit.core.id.{CallSlot, SourceId, TurnRef, WorkflowId}
import grit.core.identity.TestAccounts
import grit.core.inbox.TurnEnding
import grit.core.message.Message
import grit.core.store.Origin
import grit.core.visibility.{Integrity, Subject}
import grit.dbos.engine.LiveEngine
import grit.dbos.sql.TestPostgres
import grit.turn.Turn

import utest.*

/** A parent turn's spawn on a launched engine, its child a turn workflow of its own that rings
  * as a child's turn does ([[Turn.body]]): the parent takes the child's ending from the ring.
  */
object SpawnRingLiveTests extends TestSuite {

  private val ana = TestAccounts.account("task:ana-ring")

  private def key(r: Spawning.Request): String = s"${r.call.round}:${r.call.index}"

  private val steps = SpawnSteps[Spawning.Request](
    StepName.keyed[Spawning.Request]("narrow")(key),
    StepName.keyed[Spawning.Request]("spawn")(key),
    StepName.keyed[Spawning.Request]("await")(key),
    StepName.keyed[Spawning.Request]("ended")(key),
    StepName.keyed[Spawning.Request]("read")(key)
  )

  /** A scholar child of `turn`, asked at its first call by a parent acting for its asker. */
  private def request(turn: TurnRef, origin: Origin): Spawning.Request =
    CallSlot
      .of(turn, 0, 0)
      .flatMap(call =>
        Spawning.Request.of(
          call,
          Acting(
            turn,
            ActsFor.Asker,
            Allowance.Admitted,
            Gates.Asker(1.minute),
            Integrity.Authority
          ),
          origin,
          ok(subagent(scholar, services, Integrity.Outside)),
          "Find the paper.",
          Integrity.Member,
          ana,
          None
        )
      )
      .getOrElse(sys.error("a request"))

  val tests = Tests {
    test(
      "a parent waiting on its child takes the child's ending from its ring, long before one wait passes"
    ) {
      val engine = LiveEngine.open(TestPostgres.freshDatabase("spawn_ring"), Turn.Epoch)
      try {
        val asked = new AtomicReference[Option[Spawning.Request]](None)
        val came = new LinkedBlockingQueue[(Delegated, Long)]()
        val released = new CountDownLatch(1)
        val parked: WorkflowId -> Durable^ ?-> String =
          id => (_: Durable^) ?=> s"parked: ${WorkflowId.value(id)}"
        val turns: WorkflowId => Durable^ ?=> String = id =>
          (d: Durable^) ?=> {
            val turn = TurnRef.fromWorkflowId(id).getOrElse(sys.error("a turn"))
            val spawned = engine.db
              .read(Subject.Turn(turn))(engine.conversations.get(turn.conversationId))
              .toOption
              .flatten
              .exists(_.origin.isInstanceOf[Origin.Spawned])
            if (spawned) {
              // The child rings from inside its body, and its workflow's status says it ended
              // only once the parent is done: the parent cannot learn its end but by the ring.
              val _ = engine.returned(turn, TurnEnding.Answered)
              val _ = released.await(90, TimeUnit.SECONDS)
              s"${TurnEnding.Replied}: "
            } else
              asked.get() match {
                case Some(r) if r.call.turn == turn =>
                  val began = System.nanoTime()
                  val got = Flows.run(Spawning.spawn(steps, identity), SpawnEnv(engine), r).merge
                  came.put((got, (System.nanoTime() - began) / 1000000L))
                  released.countDown()
                  got.toString
                case _ => "parked"
              }
          }
        engine.launch(turns, parked, parked, parked, parked, parked, Vector.empty)
        val origin = Origin.Task("ring", "parent")
        val parent = engine.inbox
          .ingest(origin, SourceId("p"), Message.User("look it up"), ana, None)
          .fold(e => throw new java.lang.AssertionError(e), identity)
        asked.set(Some(request(parent, origin)))
        val _ = engine.inbox.startTurn(parent)
        // The child writes no reply, so the parent reads it as ending with none: Failed.
        val ended = Option(came.poll(60, TimeUnit.SECONDS)).map((got, millis) =>
          (got.getClass.getSimpleName, millis < Spawning.Every.toMillis / 5)
        )
        released.countDown()
        ended ==> Some(("Failed", true))
      } finally engine.close()
    }
  }
}
