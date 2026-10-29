package grit.app.main

import java.time.Instant
import java.util.concurrent.ConcurrentLinkedQueue

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import grit.core.clock.Clock
import grit.core.edge.{EdgeRefusal, EdgeSessions, EdgeStores, ServedEdge, Variable}
import grit.core.files.PagedText
import grit.core.id.{EdgeName, Harness, SessionId}
import grit.core.identity.{Account, TestAccounts}
import grit.core.message.Tokens
import grit.core.model.{Assignment, ModelId, ModelRef, Policy}
import grit.core.period.LifecycleSettings
import grit.core.place.{Namespace, Place}
import grit.core.session.Event
import grit.core.speech.Speaking
import grit.core.spend.Budget
import grit.core.store.StoreError
import grit.core.visibility.Subject
import grit.dbos.engine.LiveEngine
import grit.dbos.sql.{DbConfig, LiveDb, TestPostgres}
import grit.kit.deployment.{Assembly, Deployment, Offer, Offered, Placer}
import grit.kit.environment.Secrets
import grit.kit.run.Kit
import grit.turn.{Turn, TurnLoop}

import utest.*

/** Against Postgres and DBOS: the stores a deployment opens an edge over record the sessions
  * the edge hands them in the engine's session record.
  */
object ServedSessionsLiveTests extends TestSuite {

  private val assigned =
    Assignment(
      ModelRef(ModelId.of("openai/gpt-oss-120b").getOrElse(sys.error("id")), None),
      100,
      None
    )

  private val Ann: Account = TestAccounts.account("workshop:local/ann")

  /** An edge that, as it opens, names `Ann` (making her a person of her own) and records one
    * session's event twice through its stores, keeping what each recording answered.
    */
  private final class Recording(answers: ConcurrentLinkedQueue[Either[StoreError, Int]])
      extends ServedEdge {
    def name: EdgeName = EdgeName("recording")
    def needs: Vector[Variable] = Vector.empty
    def asks: ServedEdge.Asks = ServedEdge.Asks.Never
    def open(
        stores: EdgeStores^,
        env: Map[String, String],
        clock: Clock^,
        log: String => Unit
    ): Either[EdgeRefusal, ServedEdge.Open^{stores, clock, log, caps.any}] = {
      val batch = for {
        harness <- Harness.of("claude-code")
        id <- SessionId.of(harness, "s-served")
        event <- Event.of(
          1,
          Instant.parse("2026-10-28T10:00:00Z"),
          Event.Kind.Edited,
          Event.Shape.Empty
        )
      } yield EdgeSessions.Batch(
        id,
        None,
        Place.under(Namespace.Fs, Vector("/work/grit")),
        None,
        Vector(event)
      )
      val room = Place.under(Namespace.Task, Vector("served"))
      batch match {
        case Left(why) => Left(EdgeRefusal.Failed(why))
        case Right(b) =>
          stores.jot.write(Subject.Public)(stores.principals.name(Ann, "Ann")) match {
            case Left(e) => Left(EdgeRefusal.Failed(e.toString))
            case Right(_) =>
              answers.add(stores.sessions.record(Ann, room, Vector(b)))
              answers.add(stores.sessions.record(Ann, room, Vector(b)))
              Right(new ServedEdge.Open {
                def deliver(): Either[StoreError, Int] = Right(0)
                def close(): Unit = ()
              })
          }
      }
    }
  }

  private def deployment(edge: ServedEdge): Deployment =
    Deployment
      .of(
        edges = Vector(edge),
        worksIn = Vector.empty,
        plugins = Vector.empty,
        policy = Policy.one(assigned, assigned, assigned, assigned),
        offer = Offer(Offered.Read, TurnLoop.Rounds.of(2).getOrElse(sys.error("rounds"))),
        assembly = Assembly.Linear(Tokens(4000)),
        placer = Placer.Off("no classifier here"),
        lifecycle = LifecycleSettings.Default,
        budget = Budget(java.time.ZoneOffset.UTC, None),
        speaking = Speaking.Off,
        sweep = 30.seconds,
        persona = grit.core.persona.Persona.Grit,
        formats = PagedText.formats
      )
      .fold(r => sys.error(r.message), identity)

  val tests = Tests {

    test("an edge's stores record its sessions in the engine's session record, a repeat once") {
      val config = TestPostgres.freshDatabase("served_sessions")
      val engine = LiveEngine.open(config, Turn.Epoch)
      try {
        val answers = new ConcurrentLinkedQueue[Either[StoreError, Int]]()
        val d = deployment(new Recording(answers))
        val env = Map(
          DbConfig.UrlVar -> config.jdbcUrl,
          DbConfig.UserVar -> config.user,
          DbConfig.PasswordVar -> config.password
        )
        val secrets = Secrets.of(env, d).fold(r => sys.error(r.message), identity)
        val log = org.slf4j.LoggerFactory.getLogger("grit.serve")
        Kit.opened(engine, d, secrets, env, Clock.system(), _ => (), log) match {
          case Left(failure) => sys.error(failure.toString)
          case Right(opened) =>
            try {
              val read = for {
                harness <- Harness.of("claude-code")
                id <- SessionId.of(harness, "s-served")
              } yield LiveDb.session(config, id)
              (
                answers.asScala.toVector,
                read.map(_.map(_.map(s => (s.by.accounts.map(_.account), s.lastEvent))))
              ) ==> (
                (
                  Vector(Right(1), Right(0)),
                  Right(Right(Some((Set(Ann), Instant.parse("2026-10-28T10:00:00Z")))))
                )
              )
            } finally opened.close()
        }
      } finally engine.close()
    }
  }
}
