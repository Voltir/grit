package grit.app.main

import java.time.Instant
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import java.util.concurrent.{ConcurrentHashMap, CountDownLatch, TimeUnit}

import scala.annotation.unused
import scala.concurrent.duration.*

import grit.core.classify.{Answers, Classifier, ClassifierError, Request}
import grit.core.clock.Clock
import grit.core.durable.Durable
import grit.core.id.{CloseRef, ConversationId, PrincipalId, SourceId, WorkflowId}
import grit.core.speech.{Reach, Speaking}
import grit.core.spend.Budget
import grit.core.stitch.{Placed, StitchReads, Tuning}
import grit.core.store.{Entry, Origin, StoreError}
import grit.dbos.engine.{Engine, LiveEngine}
import grit.dbos.sql.TestPostgres
import grit.lifecycle.stitch.{Stitch, StitchEnv}
import grit.lifecycle.triage.{Triage, TriageEnv, TriageRecords, TriageSpeech}
import grit.models.StubClassifier
import grit.turn.Turn

import utest.*

/** The order a room's first messages are placed in, over a live engine (ADR 0023): each is
  * placed only once every opening heard before it in its room has been, however long one
  * takes, while replies and closes keep their own order.
  */
object StitchOrderLiveTests extends TestSuite {

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

  private def right[A](e: Either[StoreError, A]): A = e.fold(x => sys.error(x.toString), identity)

  private val Now = Instant.now()

  private def thread(ts: String): Origin = Origin.Slack("T1", "C1", ts)

  /** How long A's placement is held when B's never comes. */
  private val Hold = 5.seconds

  /** The stub, except that the stitch question of the message marked `~hold` waits until a
    * stitch question about another message is asked after it, or [[Hold]] passes.
    */
  private final class Holding extends Classifier.Around {
    val held = new CountDownLatch(1)
    val released = new AtomicBoolean(false)
    private val other = new CountDownLatch(1)

    def apply(
        request: Request,
        ask: () => Either[ClassifierError, Answers]
    ): Either[ClassifierError, Answers] = {
      val state = request.state.objOpt
      val stitching = state.exists(_.contains("exchanges"))
      val message = state.flatMap(_.get("new_message")).flatMap(_.strOpt).getOrElse("")
      if (stitching && message.contains("~hold")) {
        held.countDown()
        other.await(Hold.toMillis, TimeUnit.MILLISECONDS)
        released.set(true)
      } else if (stitching && held.getCount == 0) other.countDown()
      ask()
    }
  }

  val tests = Tests {
    test(
      "an opening is placed after the one heard before it in its room, which it is shown grouped under its root; a reply is tagged meanwhile, and a close waits for its triage"
    ) {
      val config = TestPostgres.freshDatabase("stitch_order")
      val holding = new Holding
      val classifier = Classifier.around(new StubClassifier)(holding)
      // For each conversation closed, whether A's tags were kept when it ran.
      val closed = new ConcurrentHashMap[ConversationId, java.lang.Boolean]()
      val engine = LiveEngine.open(config, Turn.Epoch)
      try {
        val aFirst = new AtomicReference(Option.empty[Entry])
        def close(id: WorkflowId)(using @unused d: Durable^): String = {
          for {
            ref <- CloseRef.fromWorkflowId(id)
            a <- aFirst.get()
          } closed.put(
            ref.period.conversationId,
            right(engine.db.read(engine.triage.of(Vector(a.id)))).nonEmpty
          )
          "closed"
        }
        launch(engine, classifier, close)
        right(engine.jot.write(engine.principals.enroll(PrincipalId("slack:T1/U0NICK"), "Nick")))

        def hear(ts: String, source: String, text: String, ago: Long): Unit =
          engine.inbox.hear(
            thread(ts),
            SourceId(source),
            text,
            PrincipalId("slack:T1/U0NICK"),
            Now.minusSeconds(ago),
            Reach.Nowhere
          ) ==> Right(())
        def entries(ts: String): Vector[Entry] =
          right(engine.db.read(engine.conversations.find(thread(ts)))).toVector
            .flatMap(c => right(engine.db.read(engine.entries.list(c.id))))
        def tagged(e: Entry): Boolean =
          right(engine.db.read(engine.triage.of(Vector(e.id)))).nonEmpty
        def first(ts: String): Entry = entries(ts).headOption.getOrElse(sys.error(s"no $ts"))

        // T, then the root R, each placed and tagged before the burst.
        hear("1.0", "1.0", "anyone seen the staging logs?", 320)
        assert(eventually(tagged(first("1.0"))))
        hear("2.0", "2.0", "where did we land on the Engine contract term?", 300)
        assert(eventually(tagged(first("2.0"))))
        // The burst: A follows R, but its placement is held; B is heard at once after it.
        hear("3.0", "3.0", "~hold a twelve month term ~back:exchange 1", 200)
        hear("4.0", "4.0", "lunch is here", 150)
        aFirst.set(Some(first("3.0")))
        assert(holding.held.await(30, TimeUnit.SECONDS))
        // While A is held: a reply in T, and every period's close made due.
        hear("1.0", "1.1", "found them", 100)
        val reply = entries("1.0").find(_.turnSeq != first("1.0").turnSeq)
        assert(eventually(reply.exists(tagged)))
        val replyTaggedWhileHeld = !holding.released.get()
        right(engine.sweep(Now.plus(java.time.Duration.ofDays(3))))

        val (r, a, b) = (first("2.0"), first("3.0"), first("4.0"))
        assert(eventually(right(engine.db.read(engine.stitches.placed(b.id))).nonEmpty))
        val offered = right(engine.db.read(engine.stitches.placed(b.id))).toVector.flatMap {
          case f: Placed.Follows => f.seen.offered.map(_.root)
          case n: Placed.Begins => n.seen.offered.map(_.root)
          case u: Placed.Unread => u.seen.offered.map(_.root)
        }
        // B was shown A under its root R, not as an exchange of its own.
        (offered.contains(r.conversationId), offered.contains(a.conversationId)) ==> (true, false)
        assert(replyTaggedWhileHeld)
        assert(eventually(closed.containsKey(a.conversationId)))
        closed.get(a.conversationId) ==> java.lang.Boolean.TRUE
      } finally engine.close()
    }
  }

  /** `engine` launched with the triage over `classifier`, never speaking, and `close` as
    * every close's body.
    */
  private def launch(
      engine: Engine^,
      classifier: Classifier^,
      close: WorkflowId => Durable^ ?=> String
  ): Unit =
    engine.launch(
      nothing,
      close,
      nothing,
      nothing,
      Triage.body(
        TriageEnv(
          TriageRecords(
            engine.entries,
            engine.triage,
            engine.principals,
            engine.conversations,
            engine.speech,
            engine.spending,
            engine.acknowledgements,
            engine.stitches,
            engine.search,
            engine.lifecycle,
            engine.rooms
          ),
          classifier,
          engine.db,
          Clock.system(),
          TriageSpeech(Speaking.Off, Budget(java.time.ZoneOffset.UTC, None), _ => Right(())),
          Tuning.Default,
          engine.placements,
          grit.core.triage.KnowledgeSources.Empty,
          grit.lifecycle.triage.TriageQuestions.shipped(grit.core.persona.Persona.Grit)
        )
      ),
      Stitch.body(
        StitchEnv(
          StitchReads(
            engine.entries,
            engine.conversations,
            engine.lifecycle,
            engine.stitches,
            engine.search,
            engine.principals
          ),
          classifier,
          engine.db,
          Clock.system(),
          Tuning.Default
        )
      ),
      Vector.empty
    )
}
