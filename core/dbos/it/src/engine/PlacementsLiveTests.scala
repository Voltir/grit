package grit.dbos.engine

import java.time.Instant
import java.util.concurrent.{CountDownLatch, TimeUnit}

import scala.annotation.unused
import scala.concurrent.duration.*

import grit.core.clock.Clock
import grit.core.durable.Durable
import grit.core.id.{PrincipalId, SourceId, TurnRef, WorkflowId}
import grit.core.speech.Reach
import grit.core.stitch.Opening
import grit.core.store.{Origin, StoreError}
import grit.dbos.sql.TestPostgres

import utest.*

/** An opening's placement waited for over DBOS against a real Postgres, with a stand-in
  * placement body held until the test lets it go: what a bounded wait returns while it runs,
  * and once it has ended.
  */
object PlacementsLiveTests extends TestSuite {

  private val nothing = (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id)

  private def right[A](e: Either[StoreError, A]): A = e.fold(x => sys.error(x.toString), identity)

  val tests = Tests {
    test(
      "a bounded wait gives up while the placement runs, which goes on; once it has ended, a wait returns what it did"
    ) {
      val config = TestPostgres.freshDatabase("placements_within")
      val release = new CountDownLatch(1)
      def place(id: WorkflowId)(using @unused d: Durable^): String = {
        // Held until released, or long after every wait below that should give up.
        release.await(20, TimeUnit.SECONDS)
        "placed"
      }
      val engine = LiveEngine.open(config, "test")
      try {
        engine.launch(nothing, nothing, nothing, nothing, nothing, place, Vector.empty)
        val thread = Origin.Slack("T1", "C1", "1.0")
        engine.inbox.hear(
          thread,
          SourceId("1.0"),
          "anyone seen the staging logs?",
          PrincipalId.Local,
          Instant.now(),
          Reach.Nowhere
        ) ==> Right(())
        val opening = right(engine.db.read(engine.conversations.find(thread))).flatMap { c =>
          val all = right(engine.db.read(engine.entries.list(c.id)))
          all.headOption.flatMap(first => Opening.of(c, all, TurnRef(c.id, first.turnSeq)))
        }
        assert(opening.nonEmpty)
        opening.foreach { o =>
          val clock = Clock.system()
          val start = clock.millis()
          engine.placements.awaitedWithin(o, 500.millis, clock) ==>
            Left("not placed within 500 milliseconds")
          // Given up near its bound, not once the placement ended.
          assert(clock.millis() - start < 5_000)
          release.countDown()
          engine.placements.awaitedWithin(o, 30.seconds, clock) ==> Right("placed")
        }
      } finally {
        release.countDown()
        engine.close()
      }
    }
  }
}
