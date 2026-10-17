package grit.app.main

import java.time.Instant

import scala.collection.mutable
import scala.concurrent.duration.*
import scala.util.Using

import grit.core.classify.{Answers, Classifier, ClassifierError, ClearedClassifier, Question}
import grit.core.clock.SetClock
import grit.core.id.SourceId
import grit.core.identity.Account
import grit.core.message.{Message, Tokens}
import grit.core.model.{Assignment, Catalog, ModelId, ModelRef, Pinned, Policy}
import grit.core.period.LifecycleSettings
import grit.core.provider.{ModelRequest, Models, Provider, ProviderError}
import grit.core.speech.{Reach, Speaking}
import grit.core.spend.Budget
import grit.core.store.{Origin, Tx}
import grit.dbos.engine.{Engine, LiveEngine}
import grit.dbos.sql.{DbConfig, LiveDb, TestPostgres}
import grit.kit.deployment.{Assembly, Deployment, Offer, Offered, Topics}
import grit.kit.rehearse.{Answers as StandIns, Rehearse, Rehearsed, Situation, Taps}
import grit.kit.run.Launch
import grit.models.{KeptCatalog, StubClassifier, StubProvider}
import grit.turn.{Turn, TurnLoop}

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

  private val deployment: Deployment =
    Deployment
      .of(
        edges = Vector.empty,
        worksIn = Vector.empty,
        plugins = Vector.empty,
        policy = Policy.one(assigned, assigned, assigned, assigned),
        offer = Offer(Offered.Read, TurnLoop.Rounds.of(3).getOrElse(sys.error("rounds"))),
        assembly = Assembly.Retrieval(Tokens(8000), Tokens(2000)),
        topics = Topics.Stub,
        lifecycle = LifecycleSettings.Default,
        budget = Budget(java.time.ZoneOffset.UTC, None),
        speaking = Speaking.Off,
        sweep = 30.seconds,
        persona = grit.core.persona.Persona.Grit
      )
      .fold(r => sys.error(r.message), identity)

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
          _ => ()
        )
        // Another thread of the channel, heard and triaged first: what the turn may search.
        engine.inbox.hear(
          Origin.Slack("T1", "C1", "1.0"),
          SourceId("1.0"),
          "the noodle place on 5th is good for lunch",
          Account.Local,
          Now,
          Reach.Nowhere
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
          .ingest(origin, SourceId("2.0"), Message.User(text), Account.Local)
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
  }
}
