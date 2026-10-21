package grit.eval.harness.pull

import java.time.Instant

import scala.collection.immutable.VectorMap
import scala.concurrent.duration.*

import grit.core.classify.Answer
import grit.core.clock.Clock
import grit.core.durable.Durable
import grit.core.id.{QuestionName, ShadowName, SourceId, WorkflowId}
import grit.core.identity.Account
import grit.core.message.{Tokens, Usage}
import grit.core.speech.{Limits, Reach, Speaking}
import grit.core.spend.DailyCap
import grit.core.stitch.{StitchReads, Tuning}
import grit.core.store.{Focus, Origin, StoreError}
import grit.core.triage.{Corpora, Cues, Gate, ShadowAnswers, Shadowed, Shadowing}
import grit.dbos.engine.{Build, Engine, LiveEngine}
import grit.dbos.internal.Reader
import grit.dbos.sql.{LiveDb, TestPostgres}
import grit.eval.harness.capture.{Capture, Captured, CaseId, Digest, Dump}
import grit.eval.harness.log.{Outcome, Row, Weights}
import grit.lifecycle.shadow.{Shadow, ShadowAsking, ShadowEnv}
import grit.lifecycle.stitch.{Stitch, StitchEnv}
import grit.lifecycle.triage.{Triage, TriageEnv, TriageQuestions, TriageRecords, TriageSpeech}
import grit.models.StubClassifier

import utest.*

/** A database's live tags and shadows pulled as run logs, over a live engine that triaged and
  * shadowed with the stub classifier. Every message is synthetic.
  */
object PullTests extends TestSuite {

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

  private val Far = Instant.parse("2100-01-01T00:00:00Z")

  private def named(n: String): ShadowName =
    ShadowName.of(n).getOrElse(throw new java.lang.AssertionError(n))

  private def qn(n: String): QuestionName =
    QuestionName.read(n).getOrElse(throw new java.lang.AssertionError(n))

  private val (words, ghost, set) = (named("words"), named("ghost"), named("set"))

  private val foci: Vector[Option[Focus]] = Vector(Some(Focus.Open), Some(Focus.Focused))

  /** A pulled shadow's form, variant, questions, and each row's case and focus. */
  private def shape(
      log: ShadowLog
  ): (String, String, Option[Vector[QuestionName]], Vector[(CaseId, Option[Focus])]) =
    log match {
      case ShadowLog.Worded(l) =>
        (
          "worded",
          l.header.variant,
          l.header.questions,
          l.rows.map((r: Row[Vector[Weights]]) => (r.id, r.focus))
        )
      case ShadowLog.Named(l) =>
        ("named", l.header.variant, l.header.questions, l.rows.map(r => (r.id, r.focus)))
      case ShadowLog.Mixed(worded, named) =>
        (s"mixed $worded worded, $named named", "", None, Vector.empty)
    }

  /** Starts `engine`'s triage under `speaking`, its stitching, and the shadows `words` and
    * `set` (V1's and V2's), with `ghost` swept but undeclared, each over the stub classifier.
    */
  private def launch(engine: Engine^, speaking: Speaking): Unit = {
    // The triage's and placement's asks; each shadow's asks its variant's classifier
    // through these.
    val moves = grit.kit.run.Launch.moves(
      engine,
      engine.db,
      new grit.models.StubModels,
      grit.core.classify.ClearedClassifier
        .of(grit.models.StubClassifier.Service, new StubClassifier),
      Clock.system()
    )
    engine.launch(
      nothing,
      nothing,
      nothing,
      nothing,
      Triage.body(
        TriageEnv(
          TriageRecords(
            engine.entries,
            // Kept as before V2: the harness reads v1-era tags only.
            // The first heard before live triage asked V2.
            new grit.eval.harness.capture.KeptAsV1(
              engine.triage,
              engine.entries,
              _.contains("day 0")
            ),
            engine.principals,
            engine.conversations,
            engine.speech,
            engine.spending,
            engine.acknowledgements,
            engine.deliveries,
            engine.stitches,
            engine.search,
            engine.lifecycle,
            engine.rooms,
            engine.periods,
            engine.documents
          ),
          moves,
          engine.budget,
          engine.db,
          Clock.system(),
          TriageSpeech(speaking, engine.budget, _ => Right(())),
          Tuning.Default,
          grit.act.phase.Placing
            .Env(
              engine.entries,
              engine.conversations,
              engine.db,
              engine.placements,
              grit.core.clock.Clock.system()
            ),
          Corpora.Empty,
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
          moves,
          engine.budget,
          engine.db,
          Clock.system(),
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
          engine.documents,
          engine.shadows,
          // `ghost` is swept but not declared here: its shadows ask nothing, keep nothing.
          Map(
            words -> ShadowAsking(
              TriageQuestions.V1,
              "stub",
              grit.core.classify.ClearedClassifier
                .of(grit.models.StubClassifier.Service, new StubClassifier)
            ),
            set -> ShadowAsking(
              TriageQuestions.V2,
              "stub",
              grit.core.classify.ClearedClassifier
                .of(grit.models.StubClassifier.Service, new StubClassifier)
            )
          ),
          Corpora.Empty,
          engine.db,
          Clock.system(),
          Tuning.Default,
          moves,
          engine.budget
        )
      ),
      Vector(words, ghost, set).map(
        Shadowing(_, Instant.EPOCH, DailyCap.of("1").getOrElse(sys.error("a cap")))
      )
    )
  }

  val tests = Tests {
    test(
      "pull writes the capture's cases' live tags under the names its first answered row asked, leaving out and counting those of other names, and each shadow's answers, as rows at the focus each was said at, a question set's under its names, a wording's in order and one of both forms refused, lists the messages no capture holds, and counts shadows that ended keeping nothing"
    ) {
      val (mixed, legacy) = (named("mixed"), named("legacy"))
      val config = TestPostgres.freshDatabase("harness_pull")
      val engine = LiveEngine.open(config, "test")
      try {
        launch(engine, Speaking.Off)
        val start = now().minusSeconds(3_600)
        // The second is a reply in the first's thread; the others open their own.
        val heard = Vector("2000.1" -> "2000.1", "2000.1" -> "2000.2", "2000.3" -> "2000.3")
        heard.zipWithIndex.foreach { case ((thread, ts), i) =>
          engine.inbox.hear(
            Origin.Slack("T1", "C1", thread),
            SourceId(ts),
            s"is the release on day $i? ~back:question ~0.3",
            Account.Local,
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
        // The capture holds the first two: captured before the third was tagged.
        val dumped = tagged.toOption.flatMap(_.lift(2)).map(_.at).getOrElse(sys.error("tagged"))
        assert(engine.sweep(now()).map(_.shadowed.size) == Right(9))
        assert(eventually(engine.unfinished() == Right(0)))
        // `mixed` is never declared: its rows are written here, one a wording's and one a
        // question set's, as a name redeclared from one to the other would leave them; and
        // `legacy`'s, a wording's each, as a shadow kept them before a wording was a set.
        val entries = tagged.toOption.getOrElse(Vector.empty).map(_.entry)
        val usage = Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, Some(BigDecimal(0)))
        def answered(answers: ShadowAnswers) =
          Shadowed.Answered("ab" * 32, answers, usage, "stub", "stub", 1.milli)
        entries
          .zip(
            Vector(
              answered(ShadowAnswers.Worded(Vector(Answer.YesNo(0.5)))),
              answered(ShadowAnswers.Named(VectorMap(qn("open") -> Answer.YesNo(0.5))))
            )
          )
          .foreach((entry, row) =>
            LiveDb.transaction(config)(engine.shadows.record(entry, mixed, row, now())) ==>
              Right(true)
          )
        val chosen = Answer
          .choice(
            Vector("question" -> 0.75, "answer" -> 0.0, "decision" -> 0.25, "announcement" -> 0.0)
              .appended("chatter" -> 0.0)
              .map(Answer.Weight(_, _))
          )
          .getOrElse(sys.error("a choice"))
        val worded = Vector(chosen, Answer.YesNo(0.5), Answer.YesNo(0.25), Answer.YesNo(0.125))
        entries
          .take(2)
          .foreach(entry =>
            LiveDb.transaction(config)(
              engine.shadows
                .record(entry, legacy, answered(ShadowAnswers.Worded(worded)), now())
            ) ==> Right(true)
          )
        // When the second, the first triaged by V2, was tagged: a pull since the switch.
        val switched =
          tagged.toOption.flatMap(_.lift(1)).map(_.at).getOrElse(sys.error("tagged"))
        val reader = Reader.open(config)
        val (capture, pulled, since) =
          try {
            val c: Captured =
              right(
                Capture(reader, "source", "restored", Dump(Digest.text("d"), dumped), Build.Unknown)
              )
            def pull(from: Instant, shadows: Vector[ShadowName]) =
              right(Pull(reader, "c", Digest.text("c"), c.cases, shadows, from, now()))
            (
              c,
              pull(Instant.EPOCH, Vector(words, ghost, set, mixed, legacy)),
              pull(switched, Vector.empty)
            )
          } finally reader.close()
        val ids = heard.map((_, ts) => right(CaseId.read(s"C1/$ts")))
        capture.cases.map(_.id) ==> ids.take(2)
        val (v1Names, v5Names) = (
          Vector("kind", "waiting", "durable", "helps").map(qn),
          Vector("gap", "open", "to", "to-grit", "durable", "anchor-record").map(qn)
        )
        (
          pulled.live.rows.map(r => (r.id, r.focus)),
          pulled.live.header.questions,
          pulled.renamed,
          since.live.rows.map(r => (r.id, r.focus)),
          since.live.header.questions,
          since.renamed
        ) ==> (
          Vector(ids(0) -> Some(Focus.Open)),
          Some(v1Names),
          1,
          Vector(ids(1) -> Some(Focus.Focused)),
          Some(v5Names),
          0
        )
        (
          pulled.live.header.variant,
          pulled.shadows.map(s => (s.name, shape(s.log), s.ended, s.waiting)),
          pulled.uncaptured,
          pulled.unbuilt
        ) ==> (
          Pull.Kept,
          Vector(
            (
              words,
              (
                "named",
                "shadow-words",
                Some(Vector("kind", "waiting", "durable", "helps").map(qn)),
                ids.take(2).zip(foci)
              ),
              0,
              0
            ),
            (ghost, ("worded", "shadow-ghost", None, Vector.empty), 3, 0),
            (
              set,
              (
                "named",
                "shadow-set",
                Some(Vector("gap", "open", "to", "durable", "anchor").map(qn)),
                ids.take(2).zip(foci)
              ),
              0,
              0
            ),
            (mixed, ("mixed 1 worded, 1 named", "", None, Vector.empty), 0, 1),
            (legacy, ("worded", "shadow-legacy", None, ids.take(2).zip(foci)), 0, 1)
          ),
          ids.drop(2),
          0
        )
        // V1 is live's question as a set: the case triaged before the switch sends the
        // request the capture rebuilt as live's, and the stub answers it alike, so the v1
        // shadow's answers are live's kept answers exactly.
        val v1 = pulled.shadows
          .collectFirst { case Pull.Pulled.Shadow(`words`, ShadowLog.Named(log), _, _) =>
            log.rows
          }
          .getOrElse(Vector.empty)
        v1.filter(_.id == ids(0)).map(r => (r.id, r.request, r.outcome)) ==>
          pulled.live.rows.map(r => (r.id, r.request, r.outcome))
        assert(
          pulled.live.rows.map(_.outcome).collect { case Outcome.Answered(a) =>
            a.get(qn("waiting"))
          } == Vector(Some(Answer.YesNo(0.3)))
        )
      } finally engine.close()
    }

    test("pull reads each case's cues as consider kept them") {
      val config = TestPostgres.freshDatabase("harness_pull_cues")
      val engine = LiveEngine.open(config, "test")
      try {
        val cap = DailyCap.of("1").getOrElse(sys.error("a cap"))
        launch(engine, Speaking.Shadow(Limits.suggested(cap, Gate.Open)))
        val someone = Account.read("slack:T1/U2").getOrElse(sys.error("an account"))
        // Each opens its own thread in its own room: neither is the other's next message.
        val heard = Vector(
          ("C1", "3000.1", Reach(Some("C1/3000.1"), Set(someone))),
          ("C2", "3000.2", Reach(Some("C2/3000.2"), Set.empty))
        )
        heard.zipWithIndex.foreach { case ((channel, ts, reach), i) =>
          engine.inbox.hear(
            Origin.Slack("T1", channel, ts),
            SourceId(ts),
            s"is the release on day $i? ~back:question ~0.3",
            Account.Local,
            now().minusSeconds(60L - i),
            reach
          ) ==> Right(())
          assert(
            eventually(
              LiveDb
                .transaction(config)(engine.triage.tagged(Instant.EPOCH, Far))
                .map(_.size) == Right[StoreError, Int](i + 1)
            )
          )
        }
        assert(eventually(engine.unfinished() == Right(0)))
        val reader = Reader.open(config)
        val pulled =
          try {
            val c: Captured =
              right(
                Capture(reader, "source", "restored", Dump(Digest.text("d"), Far), Build.Unknown)
              )
            right(Pull(reader, "c", Digest.text("c"), c.cases, Vector.empty, Instant.EPOCH, now()))
          } finally reader.close()
        pulled.heard.toVector.map((id, h) => (id.written, h.reach, h.cues)) ==> Vector(
          ("C1/3000.1", heard(0)._3, Cues(named = true, None, None, None, None)),
          ("C2/3000.2", heard(1)._3, Cues.Empty)
        )
      } finally engine.close()
    }
  }
}
