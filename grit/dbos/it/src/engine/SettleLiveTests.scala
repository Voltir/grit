package grit.dbos.engine

import java.time.Instant
import java.util.concurrent.ConcurrentLinkedQueue

import scala.annotation.unused
import scala.concurrent.duration.*

import grit.core.durable.Durable
import grit.core.id.{PeriodRef, PeriodSeq, SettleRef, SourceId, TurnRef, TurnSeq, WorkflowId}
import grit.core.message.Message
import grit.core.period.{Judgement, LifecycleSettings, Probability, Verdict, Windows}
import grit.core.store.Origin
import grit.dbos.sql.{
  DbConfig,
  LiveDb,
  SqlEntryStore,
  SqlLifecycleStore,
  SqlPeriodStore,
  TestPostgres
}

import utest.*

/** The sweep that asks whether anyone is waiting on a quiet period, over DBOS against a real
  * Postgres, with stand-in bodies: what the sweep does with a question and a verdict, not
  * what a settle does.
  */
object SettleLiveTests extends TestSuite {

  private val periods = new SqlPeriodStore(new SqlEntryStore())

  private def p(x: Double): Probability = Probability.of(x).getOrElse(sys.error(s"$x"))

  /** Ten minutes idle, a minute to settle, resolved at 0.8, three asks. */
  private def settling(config: DbConfig): Unit = {
    val settings = Windows
      .of(10.minutes, 1.day)
      .flatMap(LifecycleSettings.of(_, 4096, 1.minute, p(0.8), 3))
      .getOrElse(sys.error("settings"))
    LiveDb.transaction(config)(new SqlLifecycleStore().set(settings))
    ()
  }

  private def ingested(engine: Engine^, origin: Origin, text: String): TurnRef =
    engine.inbox
      .ingest(origin, SourceId(text), Message.User(text))
      .fold(e => sys.error(s"inbox: $e"), identity)

  private def eventually(done: => Boolean): Boolean = {
    val until = System.nanoTime() + 30.seconds.toNanos
    var held = done
    while (!held && System.nanoTime() < until) {
      Thread.sleep(50)
      held = done
    }
    held
  }

  private val nothing = (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id)

  val tests = Tests {
    test(
      "a period quiet for the settle window is asked once, under its question's id, however many sweeps"
    ) {
      val config = TestPostgres.freshDatabase("settle_once")
      val asked = new ConcurrentLinkedQueue[String]()
      def settle(id: WorkflowId)(using @unused d: Durable^): String = {
        asked.add(WorkflowId.value(id)); "asked"
      }
      val engine = Engine.open(config, "test")
      try {
        engine.launch(nothing, nothing, settle, nothing, Vector.empty)
        settling(config)
        val t0 = ingested(engine, Origin.Task("settle", "once"), "one")
        val p1 = PeriodRef(t0.conversationId, PeriodSeq.First)
        engine.sweep(Instant.now()).map(_.asked) ==> Right(Vector())
        val later = Instant.now().plusSeconds(90)
        val first = engine.sweep(later).map(_.asked.map(q => (q.period, q.last)))
        first ==> Right(Vector((p1, TurnSeq(0))))
        assert(eventually(asked.size == 1))
        Thread.sleep(500)
        (1 to 3).map(n => engine.sweep(later.plusSeconds(n.toLong)).map(_.asked)) ==>
          (1 to 3).map(_ => Right(Vector()))
        asked.size ==> 1
      } finally engine.close()
    }

    test(
      "a verdict of nobody waiting makes the next sweep close the period on the verdict's time, asking no more"
    ) {
      val config = TestPostgres.freshDatabase("settle_finished")
      // Keeps a verdict of nobody waiting, as a settle whose classifier says so does.
      def settle(id: WorkflowId)(using d: Durable^): String =
        SettleRef.fromWorkflowId(id) match {
          case None => "not a settle"
          case Some(q) =>
            val verdict = Verdict(
              Instant.now(),
              q.last,
              Judgement.Weighed(p(0.9), p(0.05), p(0.05), "jev")
            )
            d.transact("record")(periods.judged(q.period, verdict).toString)
        }
      val engine = Engine.open(config, "test")
      try {
        engine.launch(nothing, nothing, settle, nothing, Vector.empty)
        settling(config)
        val t0 = ingested(engine, Origin.Task("settle", "finished"), "one")
        val p1 = PeriodRef(t0.conversationId, PeriodSeq.First)
        val later = Instant.now().plusSeconds(90)
        engine.sweep(later).map(_.asked.size) ==> Right(1)
        assert(
          eventually(
            LiveDb.transaction(config)(periods.activity(p1)).exists(_.exists(_.asked == 1))
          )
        )
        val verdict =
          LiveDb.transaction(config)(periods.activity(p1)).toOption.flatten.flatMap(_.verdict)
        engine
          .sweep(later.plusSeconds(1))
          .map(s => (s.enqueued.map(a => (a.last, a.due)), s.asked)) ==>
          Right(
            (
              verdict
                .map(v => (TurnSeq(0), v.at.truncatedTo(java.time.temporal.ChronoUnit.MILLIS)))
                .toVector,
              Vector()
            )
          )
      } finally engine.close()
    }
  }
}
