package grit.dbos.engine

import java.time.Instant
import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, TimeUnit}

import scala.annotation.unused
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import grit.core.durable.Durable
import grit.core.id.{PeriodRef, PeriodSeq, PrincipalId, SourceId, TriageRef, TurnSeq, WorkflowId}
import grit.core.message.Message
import grit.core.store.Origin
import grit.dbos.sql.{LiveDb, SqlEntryStore, TestPostgres}

import utest.*

/** A heard message's triage over DBOS against a real Postgres, with a stand-in body: that
  * hearing enqueues it, once, not what a triage does.
  */
object TriageLiveTests extends TestSuite {

  private val nothing = (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id)

  private def eventually(done: => Boolean): Boolean = {
    val until = System.nanoTime() + 30.seconds.toNanos
    var held = done
    while (!held && System.nanoTime() < until) {
      Thread.sleep(50)
      held = done
    }
    held
  }

  val tests = Tests {
    test(
      "a heard message is triaged under its triage's id, once; a redelivery records and runs nothing again"
    ) {
      val config = TestPostgres.freshDatabase("triage_once")
      val ran = new ConcurrentLinkedQueue[String]()
      def triage(id: WorkflowId)(using @unused d: Durable^): String = {
        ran.add(WorkflowId.value(id)); "triaged"
      }
      val engine = LiveEngine.open(config, "test")
      try {
        engine.launch(nothing, nothing, nothing, nothing, triage, Vector.empty)
        val here = Origin.Task("triage", "once")
        engine.inbox.hear(
          here,
          SourceId("m1"),
          "standup moves to 10:00",
          PrincipalId.Local,
          Instant.now(),
          grit.core.speech.Reach.Nowhere
        ) ==>
          Right(())
        engine.inbox.hear(
          here,
          SourceId("m1"),
          "standup moves to 10:00",
          PrincipalId.Local,
          Instant.now(),
          grit.core.speech.Reach.Nowhere
        ) ==>
          Right(())
        val c = LiveDb.conversation(config, here).id
        val expected = TriageRef(PeriodRef(c, PeriodSeq.First), TurnSeq(0)).workflowId
        assert(eventually(ran.size >= 1))
        Thread.sleep(500)
        ran.asScala.toVector ==> Vector(WorkflowId.value(expected))
        LiveDb.transaction(config)(new SqlEntryStore().list(c)).map(_.size) ==> Right(1)
      } finally engine.close()
    }

    test("unfinished counts each workflow queued or running, and none once they have ended") {
      val config = TestPostgres.freshDatabase("triage_unfinished")
      val started = new CountDownLatch(1)
      val release = new CountDownLatch(1)
      def triage(id: WorkflowId)(using @unused d: Durable^): String = {
        started.countDown()
        release.await(30, TimeUnit.SECONDS)
        "triaged"
      }
      val engine = LiveEngine.open(config, "test")
      try {
        engine.launch(nothing, nothing, nothing, nothing, triage, Vector.empty)
        val here = Origin.Task("triage", "unfinished")
        engine.inbox.hear(
          here,
          SourceId("m1"),
          "one",
          PrincipalId.Local,
          Instant.now(),
          grit.core.speech.Reach.Nowhere
        ) ==>
          Right(())
        engine.inbox.hear(
          here,
          SourceId("m2"),
          "two",
          PrincipalId.Local,
          Instant.now(),
          grit.core.speech.Reach.Nowhere
        ) ==>
          Right(())
        assert(started.await(30, TimeUnit.SECONDS))
        // One runs; the other waits behind it in its conversation's partition.
        engine.unfinished() ==> Right(2)
        release.countDown()
        assert(eventually(engine.unfinished() == Right(0)))
      } finally engine.close()
    }

    test("a message ingested as a turn is not triaged when it is heard again") {
      val config = TestPostgres.freshDatabase("triage_addressed")
      val ran = new ConcurrentLinkedQueue[String]()
      def triage(id: WorkflowId)(using @unused d: Durable^): String = {
        ran.add(WorkflowId.value(id)); "triaged"
      }
      val engine = LiveEngine.open(config, "test")
      try {
        engine.launch(nothing, nothing, nothing, nothing, triage, Vector.empty)
        val here = Origin.Task("triage", "addressed")
        engine.inbox.ingest(here, SourceId("m1"), Message.User("@grit hi"), PrincipalId.Local)
        engine.inbox.hear(
          here,
          SourceId("m1"),
          "@grit hi",
          PrincipalId.Local,
          Instant.now(),
          grit.core.speech.Reach.Nowhere
        ) ==> Right(())
        engine.inbox.hear(
          here,
          SourceId("m2"),
          "and this",
          PrincipalId.Local,
          Instant.now(),
          grit.core.speech.Reach.Nowhere
        ) ==> Right(())
        val c = LiveDb.conversation(config, here).id
        assert(eventually(ran.size >= 1))
        Thread.sleep(500)
        ran.asScala.toVector ==>
          Vector(WorkflowId.value(TriageRef(PeriodRef(c, PeriodSeq.First), TurnSeq(1)).workflowId))
      } finally engine.close()
    }
  }
}
