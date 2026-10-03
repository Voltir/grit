package grit.eval.harness.pull

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.clock.Clock
import grit.core.durable.Durable
import grit.core.id.{PrincipalId, ShadowName, SourceId, WorkflowId}
import grit.core.speech.{Reach, Speaking}
import grit.core.spend.DailyCap
import grit.core.stitch.{StitchReads, Tuning}
import grit.core.store.{Origin, StoreError}
import grit.core.triage.Shadowing
import grit.dbos.engine.{Build, LiveEngine, Reader}
import grit.dbos.sql.{LiveDb, TestPostgres}
import grit.eval.harness.corpus.{Capture, CaseId, Corpus, Digest, Dump}
import grit.eval.harness.score.Answers
import grit.lifecycle.shadow.{Shadow, ShadowAsking, ShadowEnv}
import grit.lifecycle.triage.{Triage, TriageEnv, TriageQuestion, TriageRecords, TriageSpeech}
import grit.models.StubClassifier

import utest.*

/** A database's live tags and shadows pulled as run logs, over a live engine that triaged and
  * shadowed with the stub classifier. Every message is synthetic.
  */
object PullTests extends TestSuite {

  private val nothing = (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id)

  private def eventually(done: => Boolean): Boolean = {
    val until = System.nanoTime() + 30.seconds.toNanos
    var held = done
    while (!held && System.nanoTime() < until) { Thread.sleep(50); held = done }
    held
  }

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private val Far = Instant.parse("2100-01-01T00:00:00Z")

  private def named(n: String): ShadowName =
    ShadowName.of(n).getOrElse(throw new java.lang.AssertionError(n))

  val tests = Tests {
    test(
      "pull writes the corpus's cases' live tags and each shadow's answers as rows, lists the messages no corpus holds, and counts shadows that ended keeping nothing"
    ) {
      val (words, ghost) = (named("words"), named("ghost"))
      val config = TestPostgres.freshDatabase("harness_pull")
      val engine = LiveEngine.open(config, "test")
      try {
        engine.launch(
          nothing,
          nothing,
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
                engine.stitches,
                engine.search,
                engine.lifecycle,
                engine.rooms
              ),
              new StubClassifier,
              engine.db,
              Clock.system(),
              TriageSpeech(Speaking.Off, engine.budget, _ => Right(())),
              Tuning.Default
            )
          ),
          Vector.empty,
          Shadow.body(
            ShadowEnv(
              StitchReads(
                engine.entries,
                engine.conversations,
                engine.lifecycle,
                engine.stitches,
                engine.search,
                engine.principals
              ),
              engine.rooms,
              engine.shadows,
              // `ghost` is swept but not declared here: its shadows ask nothing, keep nothing.
              Map(
                words -> ShadowAsking(TriageQuestion.Wording.Shipped, "stub", new StubClassifier)
              ),
              engine.db,
              Clock.system(),
              Tuning.Default
            )
          ),
          Vector(words, ghost).map(
            Shadowing(_, Instant.EPOCH, DailyCap.of("1").getOrElse(sys.error("a cap")))
          )
        )
        val start = Instant.now().minusSeconds(3_600)
        val heard = Vector("2000.1", "2000.2", "2000.3")
        heard.zipWithIndex.foreach { (ts: String, i: Int) =>
          engine.inbox.hear(
            Origin.Slack("T1", "C1", ts),
            SourceId(ts),
            s"is the release on day $i? ~back:question ~0.3",
            PrincipalId.Local,
            start.plusSeconds(60L * i),
            Reach.Nowhere
          ) ==> Right(())
          assert(
            eventually(
              LiveDb
                .transaction(config)(engine.triage.tagged(Instant.EPOCH, Far))
                .map(_.size) == Right[StoreError, Int](i + 1)
            )
          )
        }
        val tagged = LiveDb.transaction(config)(engine.triage.tagged(Instant.EPOCH, Far))
        // The corpus holds the first two: captured before the third was tagged.
        val dumped = tagged.toOption.flatMap(_.lift(2)).map(_.at).getOrElse(sys.error("tagged"))
        assert(engine.sweep(Instant.now()).map(_.shadowed.size) == Right(6))
        assert(eventually(engine.unfinished() == Right(0)))
        val reader = Reader.open(config)
        val (corpus, pulled) =
          try {
            val c: Corpus =
              right(
                Capture(reader, "source", "restored", Dump(Digest.text("d"), dumped), Build.Unknown)
              )
            (
              c,
              right(
                Pull(
                  reader,
                  "c",
                  Digest.text("c"),
                  c.cases,
                  Vector(words, ghost),
                  Instant.EPOCH,
                  Instant.now()
                )
              )
            )
          } finally reader.close()
        val ids = heard.map(ts => right(CaseId.read(s"C1/$ts")))
        corpus.cases.map(_.id) ==> ids.take(2)
        (
          pulled.live.rows.map(_.id),
          pulled.live.header.variant,
          pulled.shadows.map(s =>
            (s.name, s.log.header.variant, s.log.rows.map(_.id), s.ended, s.waiting)
          ),
          pulled.uncaptured,
          pulled.unbuilt
        ) ==> (
          ids.take(2),
          Pull.Kept,
          Vector(
            (words, "shadow-words", ids.take(2), 0, 0),
            (ghost, "shadow-ghost", Vector.empty, 3, 0)
          ),
          ids.drop(2),
          0
        )
        // The stub answers alike live and in the shipped wording: the kept row keeps the
        // likeliest kind's probability and every yes/no exactly.
        val kept = Answers.of(pulled.live.rows).triage
        val shadowed = pulled.shadows.headOption
          .fold(Answers.of(Vector.empty))(s => Answers.of(s.log.rows))
          .triage
        ids
          .take(2)
          .map(id =>
            kept
              .get(id)
              .map(a => (a.mean.likeliest, a.mean.kinds.get(a.mean.likeliest), a.mean.waiting))
          ) ==> ids
          .take(2)
          .map(id =>
            shadowed
              .get(id)
              .map(a => (a.mean.likeliest, a.mean.kinds.get(a.mean.likeliest), a.mean.waiting))
          )
        assert(ids.headOption.flatMap(kept.get).map(_.mean.waiting) == Some(0.3))
      } finally engine.close()
    }
  }
}
