package grit.app.main

import java.time.Instant

import scala.collection.mutable
import scala.concurrent.duration.*
import scala.util.Using

import grit.core.classify.{Answers, Classifier, ClassifierError, ClearedClassifier, Question}
import grit.core.clock.SetClock
import grit.core.files.PagedText
import grit.core.id.{CloseRef, PluginName, SourceId, TurnRef, TurnSeq}
import grit.core.identity.Account
import grit.core.message.{Message, Tokens}
import grit.core.model.{Assignment, Catalog, ModelId, ModelRef, Pinned, Policy}
import grit.core.period.{
  Change,
  Cited,
  CloseReason,
  Closing,
  Flows,
  Ground,
  LifecycleSettings,
  Probability,
  Section,
  TestClosings
}
import grit.core.plugin.Plugin
import grit.core.provider.{ModelRequest, Models, Provider, ProviderError}
import grit.core.speech.{Limits, Reach, Speaking}
import grit.core.spend.{Budget, DailyCap}
import grit.core.stitch.{StrandShelf, Tuning}
import grit.core.store.{Origin, StoreError, Tx}
import grit.core.visibility.{Integrity, Label, Subject, Visibility}
import grit.dbos.engine.{Engine, LiveEngine}
import grit.dbos.sql.{DbConfig, LiveDb, TestPostgres}
import grit.kit.deployment.{Assembly, Deployment, Offer, Offered, Placer}
import grit.kit.rehearse.{RehearsalRunner, Rehearse, Rehearsed, Situation, StandIns, Taps}
import grit.kit.run.Launch
import grit.knowledge.card.CardSummary
import grit.knowledge.plugin.{StandingDocuments, StrandDocuments}
import grit.knowledge.strand.StrandDocument
import grit.models.{KeptCatalog, StubClassifier, StubProvider}
import grit.turn.{Turn, TurnJudge, TurnLoop, TurnRelevance}

import utest.*

/** A dry rehearsal against the live engine it stands in for: what each call the rehearsal shows
  * was sent is what the engine's own run of the same message sends, on the same database at
  * the same time, its models answering as the rehearsal's stand-ins do.
  */
object RehearseLiveTests extends TestSuite {

  private val Now = Instant.parse("2026-10-17T09:00:00Z")

  private val assigned =
    Assignment(
      ModelRef(ModelId.of("openai/gpt-oss-120b").getOrElse(sys.error("id")), None),
      100,
      None
    )

  /** A deployment assembling by `assembly`, speaking unaddressed as `speaking`, with `plugins`. */
  private def deployed(
      assembly: Assembly,
      speaking: Speaking,
      plugins: Vector[Plugin] = Vector.empty
  ): Deployment =
    Deployment
      .of(
        edges = Vector.empty,
        worksIn = Vector.empty,
        plugins = plugins,
        policy = Policy.one(assigned, assigned, assigned, assigned),
        offer = Offer(Offered.Read, TurnLoop.Rounds.of(3).getOrElse(sys.error("rounds"))),
        assembly = assembly,
        placer = Placer.Stub,
        lifecycle = LifecycleSettings.Default,
        budget = Budget(java.time.ZoneOffset.UTC, None),
        speaking = speaking,
        sweep = 30.seconds,
        persona = grit.core.persona.Persona.Grit,
        formats = PagedText.formats
      )
      .fold(r => sys.error(r.message), identity)

  private val deployment: Deployment =
    deployed(Assembly.Retrieval(Tokens(8000), Tokens(2000)), Speaking.Off)

  /** The strands plugin at public. */
  private val strands: StrandDocuments =
    StrandDocuments
      .of(
        PluginName.of("strands").fold(sys.error, identity),
        Vector(Label.Public),
        java.time.LocalTime.of(4, 30),
        java.time.ZoneOffset.UTC,
        Visibility.Shipped
      )
      .fold(sys.error, identity)

  /** [[deployment]], with the strands plugin's documents. */
  private val dreaming: Deployment =
    deployed(Assembly.Retrieval(Tokens(8000), Tokens(2000)), Speaking.Off, Vector(strands))

  /** Drafting where it is not addressed, in shadow: a draft is judged and never posted. Its gate
    * reads `gap` alone, which the stand-in answers `asks`; the stub answers every yes/no alike,
    * so a message marked `~0.4` is not read as put to grit, and its draft is judged.
    */
  private val drafting: Deployment =
    deployed(
      Assembly.Linear(Tokens(4000)),
      Speaking.Shadow(
        Limits.suggested(
          DailyCap.of("0.25").getOrElse(sys.error("a cap")),
          grit.core.triage.Tags.V2.asks(Probability.clamped(0.5))
        )
      )
    )

  /** How a standing document's message begins. */
  private val Header = "[doc] what a conversation settled"

  /** Retrieval with what each closing settled kept as a document ([[StandingDocuments]]). */
  private val standing: Deployment =
    deployed(
      Assembly.Retrieval(Tokens(8000), Tokens(2000)),
      Speaking.Off,
      Vector(new StandingDocuments(PluginName.of("standing").fold(sys.error, identity), Set.empty))
    )

  /** In `origin`'s conversation, heard through `engine` at [[Now]] and closed resolved, its
    * closing adding the Standing line "Staging deploys from main" as a person said it; whether
    * a document was kept within 30 s, as the engine's posting keeps one.
    */
  private def settledLine(engine: Engine^, origin: Origin, config: DbConfig): Boolean = {
    def right[A](e: Either[StoreError, A]): A = e.fold(x => sys.error(x.toString), identity)
    engine.inbox.hear(
      origin,
      SourceId("1.0"),
      "staging deploys from main",
      Account.Local,
      Now,
      Reach.Nowhere,
      None
    ) ==>
      Right(())
    within(() => settled(config)) ==> true
    val c = right(engine.db.read(Subject.Public)(engine.conversations.find(origin)))
      .getOrElse(sys.error("heard"))
    val first = TurnRef(c.id, TurnSeq.First)
    val line =
      TestClosings.line(Section.Standing, "Staging deploys from main", 1, 1, ground = Ground.Person)
    val closing = Closing(
      Flows
        .of(
          "We settled deploys.",
          Some("Staging deploys from main."),
          Vector(Change.Added(line, Cited.Empty))
        )
        .getOrElse(sys.error("flows")),
      TestClosings.balance(line)
    )
    // Closed a minute before the rehearsed message: a turn draws documents written before it.
    val closedAt = Now.minusSeconds(60)
    right(engine.jot.write(Subject.Public)(engine.periods.of(first))).foreach { p =>
      right(
        engine.jot.write(Subject.Public)(
          engine.periods.seal(
            CloseRef(p.ref, first.turnSeq, closedAt),
            CloseReason.Resolved(Probability.One),
            closing,
            closedAt
          )
        )
      )
    }
    // The engine posts on its sweep, which a launch alone does not run.
    right(engine.sweep(Now))
    within(() => kept(config) > 0)
  }

  /** How many document versions `config`'s database keeps. */
  private def kept(config: DbConfig): Int =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(conn.prepareStatement("SELECT count(*) FROM grit.documents")) { ps =>
        Using.resource(ps.executeQuery()) { rs => if (rs.next()) rs.getInt(1) else 0 }
      }
    }

  /** In `origin`'s conversation, heard through `engine` at [[Now]]: a remark longer than the
    * judge's thread shows, its period closed with a record, so the record is what a later turn
    * recalls.
    */
  private def remarked(engine: Engine^, origin: Origin, config: DbConfig): Unit = {
    def right[A](e: Either[StoreError, A]): A = e.fold(x => sys.error(x.toString), identity)
    engine.inbox.hear(
      origin,
      SourceId("3.0:0"),
      "morning " * (TurnJudge.ThreadChars / 8 + 1),
      Account.Local,
      Now,
      Reach.Nowhere,
      None
    ) ==> Right(())
    within(() => settled(config)) ==> true
    val c = right(engine.db.read(Subject.Public)(engine.conversations.find(origin)))
      .getOrElse(sys.error("heard"))
    val first = TurnRef(c.id, TurnSeq.First)
    right(engine.jot.write(Subject.Public)(engine.periods.of(first))).foreach { p =>
      right(
        engine.jot.write(Subject.Public)(
          engine.periods.seal(
            CloseRef(p.ref, first.turnSeq, Now),
            CloseReason.Lapsed,
            TestClosings.prose("The refi page's byline moves to the bottom."),
            Now
          )
        )
      )
    }
  }

  /** The stub provider, keeping each request it is sent: the stand-in a dry rehearsal answers
    * with by default ([[StandIns.Stubbed]]), so the engine's chain of calls is the rehearsal's.
    */
  final class Capturing extends Provider {
    private val stub = new StubProvider()
    private val kept = mutable.ArrayBuffer[ModelRequest]()
    def sent: Vector[ModelRequest] = synchronized(kept.toVector)
    def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] = {
      synchronized(kept += request)
      stub.complete(request)
    }
  }

  /** The stub classifier, keeping each state and its questions. */
  final class Asked extends Classifier {
    private val kept = mutable.ArrayBuffer[(ujson.Value, Vector[Question])]()
    def sent: Vector[(ujson.Value, Vector[Question])] = synchronized(kept.toVector)
    protected def answer(
        state: ujson.Value,
        questions: Vector[Question]
    ): Either[ClassifierError, Answers] = {
      synchronized(kept += (state -> questions))
      Right(StubClassifier.answers(state, questions))
    }
  }

  /** The catalog the rehearsal's models read ([[Taps.seed]], [[KeptCatalog.read]]), every call
    * made through `model`.
    */
  final class Through(seed: Catalog, engine: Engine^, model: Provider^) extends Models {
    def catalog(): Either[String, Catalog] =
      KeptCatalog.read(seed, engine.db, engine.modelSettings)
    def provider(pinned: Pinned): Provider^{model} = model
  }

  /** Whether `done` holds within 30 s. */
  private def within(done: () => Boolean): Boolean = {
    val until = System.nanoTime() + 30.seconds.toNanos
    while (!done() && System.nanoTime() < until) Thread.sleep(100)
    done()
  }

  /** Whether every workflow `config`'s database holds has ended, and one has. */
  private def settled(config: DbConfig): Boolean = {
    val statuses = LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(conn.prepareStatement("SELECT status FROM dbos.workflow_status")) { ps =>
        Using.resource(ps.executeQuery()) { rs =>
          val rows = Vector.newBuilder[String]
          while (rs.next()) rows += rs.getString(1)
          rows.result()
        }
      }
    }
    statuses.nonEmpty && statuses.forall(s => s != "PENDING" && s != "ENQUEUED")
  }

  val tests = Tests {
    test(
      "what a dry rehearsal shows each call sent is what the engine's run sends, up to and including the turn's own call"
    ) {
      val config = TestPostgres.freshDatabase("rehearse_equal")
      val origin = Origin.Slack("T1", "C1", "2.0")
      val text = "@grit where should we get lunch?"
      val clock = new SetClock(Now)
      val seed = Taps.seed(deployment.policy).fold(sys.error, identity)
      val model = new Capturing
      val classifier = new Asked
      val engine = LiveEngine.open(config, Turn.Epoch, clock = clock)
      try {
        val cleared = ClearedClassifier.of(StubClassifier.Service, classifier)
        Launch.asking(
          engine,
          deployment,
          Map.empty,
          new Through(seed, engine, model),
          cleared,
          cleared,
          Launch.area(
            engine,
            deployment,
            Launch
              .filesDir(None, config, java.nio.file.Path.of(System.getProperty("java.io.tmpdir")))
          ),
          _ => ()
        )
        // Another thread of the channel, heard and triaged first: what the turn may search. Said
        // before the stitch horizon, so the opening is offered nothing to follow (its placement
        // runs beside the engine's turn, a race the rehearsal does not have), and long past its
        // triage's quiet, which so waits for nothing and leaves the clock at Now.
        engine.inbox.hear(
          Origin.Slack("T1", "C1", "1.0"),
          SourceId("1.0"),
          "the noodle place on 5th is good for lunch",
          Account.Local,
          Now.minusNanos((Tuning.Default.horizon + 1.day).toNanos),
          Reach.Nowhere,
          None
        ) ==> Right(())
        within(() => settled(config)) ==> true
        val before = (model.sent.size, classifier.sent.size)
        // Rehearsed beside the running engine, on the database as the engine's run will find it.
        val shown = Rehearse
          .dry(
            config,
            deployment,
            Situation.Said(origin, Account.Local, text),
            StandIns.Stubbed,
            clock
          )
          .fold(sys.error, identity)
        engine.inbox
          .ingest(origin, SourceId("2.0"), Message.User(text), Account.Local, None)
          .flatMap(engine.inbox.startTurn)
          .fold(e => sys.error(s"inbox: $e"), identity)
        val requests = shown.calls.collect {
          case Rehearsed.Call(_, _, _, Rehearsed.Input.Model(r), _) => r
        }
        val questions = shown.calls.collect {
          case Rehearsed.Call(_, _, _, Rehearsed.Input.Classifier(s, q), _) => s -> q
        }
        def sent = (model.sent.drop(before._1), classifier.sent.drop(before._2))
        within(() => sent._1.size >= requests.size && sent._2.size >= questions.size) ==> true
        // By membership, not order: the engine places the opening and runs the turn at once.
        (
          shown.calls.map(c => (c.step, c.input.getClass.getSimpleName)),
          shown.calls.lastOption.map(_.answered),
          requests.filterNot(sent._1.contains),
          questions.filterNot(sent._2.contains)
        ) ==> (
          Vector(
            (Some("ask:needs"), "Classifier"),
            (Some("ask:query"), "Model"),
            (Some("ask:relevance-0"), "Classifier"),
            (Some("ask:0"), "Model")
          ),
          Some(Rehearsed.Answered.Unsent),
          Vector(),
          Vector()
        )
      } finally engine.close()
    }

    test(
      "a Standing line another thread of the channel settled is shown to the relevance judge, and to the turn, as its document"
    ) {
      val config = TestPostgres.freshDatabase("rehearse_standing")
      val clock = new SetClock(Now)
      val seed = Taps.seed(standing.policy).fold(sys.error, identity)
      val engine = LiveEngine.open(config, Turn.Epoch, clock = clock)
      val (posted, shown) =
        try {
          val cleared = ClearedClassifier.of(StubClassifier.Service, new Asked)
          Launch.asking(
            engine,
            standing,
            Map.empty,
            new Through(seed, engine, new Capturing),
            cleared,
            cleared,
            Launch.area(
              engine,
              standing,
              Launch
                .filesDir(None, config, java.nio.file.Path.of(System.getProperty("java.io.tmpdir")))
            ),
            _ => ()
          )
          val posted = settledLine(engine, Origin.Slack("T1", "C1", "1.0"), config)
          posted -> Rehearse
            .dry(
              config,
              standing,
              Situation.Said(
                Origin.Slack("T1", "C1", "2.0"),
                Account.Local,
                "@grit where does staging deploy from?"
              ),
              StandIns.Stubbed,
              clock
            )
            .fold(sys.error, identity)
        } finally engine.close()
      val document =
        "[doc] what a conversation settled, at slack:T1/C1/1.0, 2026-10-17:\n" +
          "Staging deploys from main.\n- Staging deploys from main"
      val judged = shown.calls.collect {
        case Rehearsed.Call(Some(step), _, _, Rehearsed.Input.Classifier(state, _), _)
            if step.startsWith("ask:relevance") =>
          state("candidates").arr.map(_.str).toVector
      }.flatten
      val window = shown.calls.collect {
        case Rehearsed.Call(_, _, _, Rehearsed.Input.Model(r), _) =>
          r.messages.collect { case Message.User(t) if t.startsWith(Header) => t }
      }.lastOption
      (posted, judged.filter(_.startsWith(Header)), window) ==> (
        true,
        Vector(document.take(TurnRelevance.Chars)),
        Some(Vector(document))
      )
    }

    test(
      "a heard message nobody addressed, its draft supplied, is judged, and the rehearsal stops before the draft is posted"
    ) {
      val config = TestPostgres.freshDatabase("rehearse_judged")
      val origin = Origin.Slack("T1", "C1", "3.0")
      val clock = new SetClock(Now)
      val seed = Taps.seed(drafting.policy).fold(sys.error, identity)
      val engine = LiveEngine.open(config, Turn.Epoch, clock = clock)
      val shown =
        try {
          val cleared = ClearedClassifier.of(StubClassifier.Service, new Asked)
          Launch.asking(
            engine,
            drafting,
            Map.empty,
            new Through(seed, engine, new Capturing),
            cleared,
            cleared,
            Launch.area(
              engine,
              drafting,
              Launch
                .filesDir(None, config, java.nio.file.Path.of(System.getProperty("java.io.tmpdir")))
            ),
            _ => ()
          )
          remarked(engine, origin, config)
          Rehearse
            .dry(
              config,
              drafting,
              Situation.Heard(
                origin,
                Account.Local,
                "where does the refi page's byline go? ~0.4",
                Reach(Some("C1/3.0/3.0:1"), Set.empty)
              ),
              StandIns.supplied(Map("ask:0" -> "At the bottom of the page.")),
              clock
            )
            .fold(sys.error, identity)
        } finally engine.close()
      (
        shown.calls.collect {
          case Rehearsed.Call(Some(step), _, _, Rehearsed.Input.Classifier(_, _), _)
              if step == Turn.Step.Judge =>
            step
        },
        shown.workflows.lastOption.map(_.came)
      ) ==> (
        Vector(Turn.Step.Judge),
        Some(RehearsalRunner.Came.Stopped(Turn.Step.RecordSpeech))
      )
    }

    test(
      "a strand's document in its room is shown to the relevance judge and in the turn's window, under its label"
    ) {
      val config = TestPostgres.freshDatabase("rehearse_strand")
      val clock = new SetClock(Now)
      val seed = Taps.seed(dreaming.policy).fold(sys.error, identity)
      val engine = LiveEngine.open(config, Turn.Epoch, clock = clock)
      val summary = CardSummary
        .of(
          "Ana and Bo traced the flaky deploy pipeline to the shared test database; they agreed " +
            "to give each suite its own.",
          Vector("deploy pipeline", "shared database")
        )
        .fold(sys.error, identity)
      val shown =
        try {
          val cleared = ClearedClassifier.of(StubClassifier.Service, new Asked)
          Launch.asking(
            engine,
            dreaming,
            Map.empty,
            new Through(seed, engine, new Capturing),
            cleared,
            cleared,
            Launch.area(
              engine,
              dreaming,
              Launch
                .filesDir(None, config, java.nio.file.Path.of(System.getProperty("java.io.tmpdir")))
            ),
            _ => ()
          )
          val opened = Origin.Slack("T1", "C1", "1.0")
          engine.inbox.hear(
            opened,
            SourceId("1.0"),
            "the deploy pipeline failed again on the integration suite",
            Account.Local,
            Now,
            Reach.Nowhere,
            None
          ) ==> Right(())
          within(() => settled(config)) ==> true
          def right[A](e: Either[StoreError, A]): A = e.fold(x => sys.error(x.toString), identity)
          val root = right(engine.db.read(Subject.Public)(engine.conversations.find(opened)))
            .getOrElse(sys.error("heard"))
            .id
          // The document a dream run writes for this strand, as its keep writes it, before the
          // turn's message, as a nightly run's is: a document is drawn on once current.
          right(engine.jot.write(Subject.Public) {
            StrandShelf
              .over(engine.stitches, engine.conversations)
              .of(Vector(root), Now.plusSeconds(1))
              .flatMap(_.headOption.toRight(StoreError.Invalid("no strand")))
              .flatMap(seen =>
                strands.documents
                  .fold[Either[StoreError, Any]](Left(StoreError.Invalid("no documents")))(d =>
                    engine
                      .keeper(strands.name, d.terms)
                      .write(
                        StrandDocument.key(root),
                        Label.Public,
                        Integrity.Member,
                        seen.room,
                        StrandDocument.text(seen, summary),
                        StrandDocument.data(seen),
                        None,
                        Now.minusSeconds(60)
                      )
                  )
              )
              .map(_ => ())
          })
          Rehearse
            .dry(
              config,
              dreaming,
              Situation.Said(
                Origin.Slack("T1", "C1", "2.0"),
                Account.Local,
                "@grit what did we decide about the flaky deploy pipeline?"
              ),
              StandIns.Stubbed,
              clock
            )
            .fold(sys.error, identity)
        } finally engine.close()
      val judged = shown.calls.collect {
        case Rehearsed.Call(Some(step), _, _, Rehearsed.Input.Classifier(state, _), _)
            if step.startsWith("ask:relevance") =>
          state.objOpt
            .flatMap(_.get("candidates"))
            .flatMap(_.arrOpt)
            .toVector
            .flatten
            .flatMap(_.strOpt)
      }
      val asked = shown.calls.collect {
        case Rehearsed.Call(Some("ask:0"), _, _, Rehearsed.Input.Model(r), _) =>
          r.messages.collect { case Message.User(t) => t }
      }
      val block = "[doc] room discussion, at slack:T1/C1, 2026-10-17:\n" +
        "1 thread in this room, 1 message, from 2026-10-17 to 2026-10-17.\n\n" +
        s"${summary.summary}\n\nTerms: deploy pipeline, shared database"
      (
        judged.flatten.filter(_.startsWith("[doc]")),
        asked.flatten.filter(_.startsWith("[doc]"))
      ) ==>
        (Vector(block), Vector(block))
    }
  }
}
