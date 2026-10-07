package grit.app.main

import java.time.Instant

import scala.concurrent.duration.*
import scala.util.Using

import grit.core.id.{
  CorpusName,
  PeriodRef,
  PeriodSeq,
  QuestionName,
  ShadowName,
  SourceId,
  TriageRef
}
import grit.core.identity.Account
import grit.core.message.Tokens
import grit.core.model.{Assignment, ModelId, ModelRef, Policy}
import grit.core.period.LifecycleSettings
import grit.core.place.{Namespace, Place}
import grit.core.speech.{Reach, Speaking}
import grit.core.spend.{Budget, DailyCap}
import grit.core.store.{Entry, Origin, StoreError, Tx}
import grit.core.triage.{Corpora, Corpus, ShadowAnswers, Shadowed}
import grit.core.visibility.Subject
import grit.dbos.engine.{Engine, LiveEngine}
import grit.dbos.sql.{DbConfig, LiveDb, TestPostgres}
import grit.kit.deployment.{Assembly, Deployment, Offer, Offered, Topics}
import grit.kit.environment.Secrets
import grit.kit.run.Launch
import grit.lifecycle.shadow.ShadowVariant
import grit.lifecycle.triage.{TriageQuestion, TriageQuestions}
import grit.models.StubClassifier
import grit.turn.{Turn, TurnLoop}

import utest.*

/** A shadow declared in a deployment, over a live engine launched as the kit launches one,
  * with the stub classifier: what its sweep makes of a heard message beside live triage.
  */
object ShadowLaunchLiveTests extends TestSuite {

  private val Words: ShadowName = ShadowName.of("words").getOrElse(sys.error("a name"))

  private val assigned =
    Assignment(
      ModelRef(ModelId.of("openai/gpt-oss-120b").getOrElse(sys.error("id")), None),
      100,
      None
    )

  private def deployment(
      shadows: Vector[ShadowVariant],
      knowledge: Corpora = Corpora.Empty
  ): Deployment =
    Deployment
      .of(
        edges = Vector.empty,
        worksIn = Vector.empty,
        plugins = Vector.empty,
        policy = Policy(assigned, assigned, assigned, assigned),
        offer = Offer(Offered.Read, TurnLoop.Budget.of(2).getOrElse(sys.error("rounds"))),
        assembly = Assembly.Linear(Tokens(8000)),
        topics = Topics.Stub,
        lifecycle = LifecycleSettings.Default,
        budget = Budget(java.time.ZoneOffset.UTC, None),
        speaking = Speaking.Off,
        sweep = 30.seconds,
        persona = grit.core.persona.Persona.Grit,
        shadows = shadows,
        knowledge = knowledge
      )
      .fold(r => sys.error(r.message), identity)

  private def secrets(c: DbConfig, d: Deployment): Secrets =
    Secrets
      .of(
        Map(
          DbConfig.UrlVar -> c.jdbcUrl,
          DbConfig.UserVar -> c.user,
          DbConfig.PasswordVar -> c.password
        ),
        d
      )
      .fold(r => sys.error(r.message), identity)

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

  private val here: Origin = Origin.Slack("T1", "C1", "1.0")

  /** A message heard in `here` on an engine launched for `d` over `config`, once its triage
    * has kept its tags: its entry, the names of its triage's steps as DBOS recorded them, and
    * what shadow `name` made of it once a sweep has enqueued its shadow and it has ended.
    */
  private def heard(
      config: DbConfig,
      d: Deployment,
      name: ShadowName = Words
  ): (Entry, Vector[String], Option[Shadowed]) = {
    val engine = LiveEngine.open(config, Turn.Epoch)
    try {
      Launch(engine, d, secrets(config, d), Launch.Run.Served, sweeping = false, _ => ())
      engine.inbox.hear(
        here,
        SourceId("1.0"),
        "is the release on Thursday? ~back:question ~0.8",
        Account.Local,
        Instant.now(),
        Reach.Nowhere
      ) ==> Right(())
      val c = right(engine.db.read(Subject.Public)(engine.conversations.find(here)))
        .getOrElse(sys.error("heard"))
      val entry =
        right(engine.db.read(Subject.Public)(engine.entries.list(c.id))).headOption
          .getOrElse(sys.error("no entry"))
      assert(
        eventually(
          right(engine.db.read(Subject.Public)(engine.triage.of(Vector(entry.id)))).nonEmpty
        )
      )
      assert(eventually(engine.unfinished() == Right(0)))
      val _ = right(engine.sweep(Instant.now()))
      assert(eventually(engine.unfinished() == Right(0)))
      val triage = TriageRef(PeriodRef(c.id, PeriodSeq.First), entry.turnSeq).workflowId
      (
        entry,
        steps(config, grit.core.id.WorkflowId.value(triage)),
        right(engine.db.read(Subject.Public)(engine.shadows.of(name, Vector(entry.id))))
          .get(entry.id)
      )
    } finally engine.close()
  }

  /** The names of `workflow`'s steps, in order, as DBOS recorded them. Read in SQL because
    * only `grit.dbos` names DBOS's client.
    */
  private def steps(config: DbConfig, workflow: String): Vector[String] =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(
        conn.prepareStatement(
          "SELECT function_name FROM dbos.operation_outputs WHERE workflow_uuid = ? ORDER BY function_id"
        )
      ) { ps =>
        ps.setString(1, workflow)
        Using.resource(ps.executeQuery()) { rs =>
          val names = Vector.newBuilder[String]
          while (rs.next()) names += rs.getString(1)
          names.result()
        }
      }
    }

  val tests = Tests {
    test(
      "a heard message gets its triage and a declared shadow's row, and its triage runs the steps it runs with no shadow"
    ) {
      val variant = ShadowVariant(
        Words,
        TriageQuestions.v1(
          TriageQuestion.Wording.Shipped.copy(waiting = "Is anyone waiting on new_message?")
        ),
        Some("jev-variant"),
        DailyCap.of("0.01").getOrElse(sys.error("a cap")),
        Instant.EPOCH
      )
      val (_, plain, none) =
        heard(TestPostgres.freshDatabase("shadow_none"), deployment(Vector.empty))
      val (_, shadowed, kept) =
        heard(TestPostgres.freshDatabase("shadow_launch"), deployment(Vector(variant)))
      (none, shadowed) ==> (None, plain)
      assert(plain.nonEmpty)
      kept match {
        // The stub answers the marked kind, and the variant's model is the one it names.
        case Some(Shadowed.Answered(_, answers, _, requested, answered, _)) =>
          val names = answers match {
            case ShadowAnswers.Named(as) => Some(as.keys.map(QuestionName.value).toVector)
            case ShadowAnswers.Worded(_) => None
          }
          (requested, answered, names) ==> (
            "jev-variant",
            StubClassifier.Model,
            Some(Vector("kind", "waiting", "durable", "helps"))
          )
        case other => throw new java.lang.AssertionError(s"not answered: $other")
      }
    }

    test(
      "a declared question set's shadow asks the deployment's corpora covering the conversation, each answer under its name"
    ) {
      val asks = ShadowName.of("asks").getOrElse(sys.error("a name"))
      def source(name: String, team: String) = Corpus(
        CorpusName.of(name).getOrElse(sys.error("a name")),
        s"the $name of team $team",
        Place.under(Namespace.Slack, Vector(team))
      )
      val catalog = Corpora
        .of(Vector(source("wiki", "T2"), source("github", "T1")))
        .getOrElse(sys.error("a catalog"))
      val variant = ShadowVariant(
        asks,
        TriageQuestions.V2,
        None,
        DailyCap.of("0.01").getOrElse(sys.error("a cap")),
        Instant.EPOCH
      )
      val (_, _, kept) = heard(
        TestPostgres.freshDatabase("shadow_named"),
        deployment(Vector(variant), catalog),
        asks
      )
      kept.map {
        case Shadowed.Answered(_, ShadowAnswers.Named(answers), _, requested, _, _) =>
          Right((answers.keys.toVector.map(QuestionName.value), requested))
        case other => Left(other)
      } ==> Some(
        Right(
          (
            Vector("gap", "open", "to", "durable", "anchor", "source:github"),
            StubClassifier.Model
          )
        )
      )
    }
  }
}
