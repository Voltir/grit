package grit.eval.harness.corpus

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.clock.Clock
import grit.core.durable.Durable
import grit.core.id.{PrincipalId, QuestionName, SourceId, WorkflowId}
import grit.core.period.Probability
import grit.core.speech.{Reach, Speaking}
import grit.core.stitch.{StitchReads, Tuning}
import grit.core.store.{Origin, StoreError}
import grit.dbos.engine.{Build, LiveEngine, Reader}
import grit.dbos.sql.{LiveDb, TestPostgres}
import grit.lifecycle.stitch.{Stitch, StitchEnv}
import grit.lifecycle.triage.{Triage, TriageEnv, TriageRecords, TriageSpeech}
import grit.models.StubClassifier

import utest.*

/** A corpus captured from a database a live engine triaged, with the stub classifier. Every
  * message is synthetic.
  */
object CaptureTests extends TestSuite {

  private val nothing = (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id)

  /** The system clock's now. */
  private def now(): Instant = Clock.system().now()

  private def eventually(done: => Boolean): Boolean = {
    val clock = Clock.system()
    val until = clock.millis() + 30.seconds.toMillis
    var held = done
    while (!held && clock.millis() < until) { clock.sleep(50.millis); held = done }
    held
  }

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private val dump = Dump(Digest.text("synthetic dump"), Instant.parse("2100-01-01T00:00:00Z"))

  private val beforeV2 = Set("lunch at noon tomorrow?", "the deploy moves to friday")

  /** A case's tags by their form: v1's, a question set's names, or unanswered. */
  private def form(l: Live): String = l match {
    case Live.Weighed(_, _, _, _, _, _, _) => "v1"
    case Live.Named(answers, _, _) => answers.keys.map(QuestionName.value).mkString(",")
    case Live.Unanswered(f) => Failure.written(f)
  }

  val tests = Tests {
    test(
      "every placement rebuilds to the state it was shown live, each case keeps its tags in its era's form, and a recapture writes the same bytes"
    ) {
      val config = TestPostgres.freshDatabase("harness_capture")
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
                // The first two heard before live triage asked V2.
                new KeptAsV1(engine.triage, engine.entries, beforeV2.contains),
                engine.principals,
                engine.conversations,
                engine.speech,
                engine.spending,
                engine.acknowledgements,
                engine.deliveries,
                engine.stitches,
                engine.search,
                engine.lifecycle,
                engine.rooms
              ),
              new StubClassifier,
              engine.db,
              Clock.system(),
              TriageSpeech(Speaking.Off, engine.budget, _ => Right(())),
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
              new StubClassifier,
              engine.db,
              Clock.system(),
              Tuning.Default
            )
          ),
          Vector.empty
        )
        val start = now().minusSeconds(3_600)
        def thread(ts: String) = Origin.Slack("T1", "C1", ts)
        // Four threads begun in one channel and a reply in the first: each heard only once
        // the one before it is tagged, so every placement was made over every earlier row.
        val heard = Vector(
          (thread("1000.1"), "1000.1", "lunch at noon tomorrow?"),
          (thread("1000.2"), "1000.2", "the deploy moves to friday"),
          (thread("1000.3"), "1000.3", "who has the release notes"),
          (thread("1000.1"), "1000.4", "noon works for me"),
          (thread("1000.5"), "1000.5", "back to lunch: noon it is ~back:exchange 1")
        )
        heard.zipWithIndex.foreach { (h: (Origin, String, String), i: Int) =>
          val (origin, ts, text) = h
          engine.inbox.hear(
            origin,
            SourceId(ts),
            text,
            PrincipalId.Local,
            start.plusSeconds(60L * i),
            Reach.Nowhere
          ) ==> Right(())
          assert(
            eventually(
              LiveDb
                .transaction(config)(engine.triage.tagged(Instant.EPOCH, dump.at))
                .map(_.size) == Right[StoreError, Int](i + 1)
            )
          )
        }
        def captured(): Corpus = {
          val reader = Reader.open(config)
          try right(Capture(reader, "source", "restored", dump, Build.Unknown))
          finally reader.close()
        }
        val corpus = captured()
        corpus.cases.map(_.id.written) ==>
          Vector("C1/1000.1", "C1/1000.2", "C1/1000.3", "C1/1000.4", "C1/1000.5")
        val v4 = "gap,open,to,to-grit,durable,anchor,anchor-record"
        corpus.cases.map(c => form(c.tags)) ==> Vector("v1", "v1", v4, v4, v4)
        corpus.cases.map(_.stitch.map(_.seen)) ==>
          Vector(None, Some(SeenCheck.Match), Some(SeenCheck.Match), None, Some(SeenCheck.Match))
        corpus.cases.lastOption.flatMap(_.stitch).map(_.placed) ==> Some(
          Placement.Follows(right(CaseId.read("C1/1000.1")), Probability.clamped(0.9))
        )
        corpus.cases.lastOption.map(_.clusters.exchange.written) ==> Some("C1/1000.1")
        def bytes(c: Corpus) =
          CorpusJson.writeManifest(c.manifest).render() +
            c.cases.map(CorpusJson.writeCase(_).render()).mkString("\n")
        bytes(captured()) ==> bytes(corpus)
      } finally engine.close()
    }
  }

}
