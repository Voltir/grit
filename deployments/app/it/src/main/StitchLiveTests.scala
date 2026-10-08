package grit.app.main

import java.time.Instant

import scala.concurrent.duration.*
import scala.util.Using

import grit.core.clock.Clock
import grit.core.id.{ConversationId, EntryId, SourceId, TurnRef, TurnSeq}
import grit.core.identity.TestAccounts
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.{Assignment, ModelId, ModelRef, Policy}
import grit.core.period.LifecycleSettings
import grit.core.speech.{Limits, Reach, Speaking, Stage}
import grit.core.spend.{Budget, DailyCap}
import grit.core.stitch.{Link, Placed}
import grit.core.store.{Entry, Nearby, Origin, Payload, StoreError, Tx}
import grit.core.visibility.Subject
import grit.dbos.engine.{Engine, LiveEngine}
import grit.dbos.sql.{DbConfig, LiveDb, TestPostgres}
import grit.kit.deployment.{Assembly, Deployment, Offer, Offered, Topics}
import grit.kit.environment.Secrets
import grit.kit.run.Launch
import grit.turn.{Turn, TurnLoop}

import utest.*

/** Topic stitching end to end (ADR 0023), over a live engine launched as the kit launches a
  * deployment, with the stub classifier: the Engine question and David's top-level replies
  * from actualbest's channel, as they were said, stitched to it; a draft held because grit
  * already replied in the strand; and a top-level mention whose window shows the strand.
  */
object StitchLiveTests extends TestSuite {

  private lazy val config = TestPostgres.freshDatabase("stitch")

  private val assigned =
    Assignment(
      ModelRef(ModelId.of("openai/gpt-oss-120b").getOrElse(sys.error("id")), None),
      100,
      None
    )

  /** A gate the stub classifier can pass: V2's `gap` choosing `asks` (the stub answers every
    * yes/no alike, so V2's own gate, which needs `open` high and `to` low, holds everything).
    */
  private val asks = grit.core.triage.Tags.V2.asks(grit.core.period.Probability.clamped(0.5))

  private val deployment: Deployment =
    Deployment
      .of(
        edges = Vector.empty,
        worksIn = Vector.empty,
        plugins = Vector.empty,
        policy = Policy(assigned, assigned, assigned, assigned),
        offer = Offer(Offered.Read, TurnLoop.Budget.of(2).getOrElse(sys.error("rounds"))),
        assembly = Assembly.Retrieval(Tokens(8000), Tokens(2000)),
        topics = Topics.Stub,
        lifecycle = LifecycleSettings.Default,
        budget = Budget(java.time.ZoneOffset.UTC, None),
        speaking = Speaking.Within(
          Limits.suggested(DailyCap.of("0.25").getOrElse(sys.error("cap")), asks)
        ),
        sweep = 30.seconds,
        persona = grit.core.persona.Persona.Grit
      )
      .fold(r => sys.error(r.message), identity)

  private def secrets(c: DbConfig): Secrets =
    Secrets
      .of(
        Map(
          DbConfig.UrlVar -> c.jdbcUrl,
          DbConfig.UserVar -> c.user,
          DbConfig.PasswordVar -> c.password
        ),
        deployment
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

  private val Now = Instant.now()

  private def thread(ts: String): Origin = Origin.Slack("T1", "C1", ts)

  /** `text` heard from `who` in thread `ts`, said `ago` seconds ago, with a reply address;
    * its first entry, once its triage has kept its tags.
    */
  private def hear(engine: Engine^, ts: String, who: String, text: String, ago: Long): Entry = {
    right(
      engine.jot.write(Subject.Public)(
        engine.principals.name(TestAccounts.account(s"slack:T1/$who"), who)
      )
    )
    engine.inbox.hear(
      thread(ts),
      SourceId(ts),
      text,
      TestAccounts.account(s"slack:T1/$who"),
      Now.minusSeconds(ago),
      Reach(Some(s"C1/$ts/$ts"), Set.empty)
    ) ==> Right(())
    val c =
      right(engine.db.read(Subject.Public)(engine.conversations.find(thread(ts))))
        .getOrElse(sys.error("heard"))
    val first =
      right(engine.db.read(Subject.Public)(engine.entries.list(c.id))).headOption
        .getOrElse(sys.error("no entry"))
    assert(
      eventually(right(engine.db.read(Subject.Public)(engine.triage.of(Vector(first.id)))).nonEmpty)
    )
    first
  }

  private def links(engine: Engine^, c: ConversationId): Vector[Link] =
    right(engine.db.read(Subject.Public)(engine.stitches.links(Vector(c))))
      .filter(_.conversation == c)

  /** The kind of what became of `turn`'s draft, and whose entry settled it, from grit.speech.
    * Read in SQL because no store reads an outcome back (SpeechStore only writes it), and
    * kept as the stored JSON on purpose: the test pins its form, `kind` and `by`, which
    * `SpeechJson.writeOutcome` gives the row and the record-speech step's output alike.
    */
  private def outcome(turn: TurnRef): Option[String] =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(
        conn.prepareStatement(
          "SELECT outcome::text FROM grit.speech WHERE conversation_id = ?::uuid AND turn_seq = ?"
        )
      ) { ps =>
        ps.setString(1, ConversationId.value(turn.conversationId))
        ps.setLong(2, TurnSeq.value(turn.turnSeq))
        Using.resource(ps.executeQuery())(rs => if (rs.next()) Option(rs.getString(1)) else None)
      }
    }

  val tests = Tests {
    test(
      "David's top-level replies follow the Engine question, a draft among them is held by grit's reply in the strand, and a mention is shown the strand"
    ) {
      val engine = LiveEngine.open(config, Turn.Epoch)
      try {
        Launch(
          engine,
          deployment,
          secrets(config),
          Launch.Run.Served,
          Clock.system(),
          sweeping = false,
          _ => ()
        )
        // The channel as it was: a sandwich question, then the Engine question 95 s later.
        val sandwich =
          hear(engine, "1.0", "U0NICK", "roast beef sandwiches still cut diagonally?", 200)
        val asked =
          hear(engine, "2.0", "U0NICK", "where did we land on the Engine contract term?", 150)
        // grit replied in the Engine thread after David's first reply was said.
        val replied = {
          val a = asked.conversationId
          right(engine.jot.write(Subject.Public) {
            for {
              next <- engine.entries.lockNext(a)
              e = Entry(
                EntryId("engine:reply"),
                a,
                next.turnSeq,
                None,
                next.seq,
                Payload.Message(
                  Message.Assistant(
                    Vector(AssistantBlock.Text("A 12-month term.")),
                    StopReason.EndTurn,
                    Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, None),
                    "m"
                  )
                ),
                Now.minusSeconds(100)
              )
              _ <- engine.entries.insert(e)
            } yield e
          })
        }
        // The stub reads each choice's first marker that names one of its keys, and every
        // yes/no at 0.1: an unprompted draft, not a message put to grit.
        val real = hear(
          engine,
          "3.0",
          "U0DAVID",
          "Is this a real question ~0.1 ~back:exchange 1 ~back:asks",
          120
        )
        val lol = hear(engine, "4.0", "U0DAVID", "lol ~back:exchange 1", 118)
        val testing =
          hear(engine, "5.0", "U0DAVID", "Or are you testing bots ~back:exchange 1", 110)

        val engineRoot = asked.conversationId
        Vector(real, lol, testing).map(e => links(engine, e.conversationId)) ==>
          Vector(real, lol, testing).map(e => Vector(Link(e.conversationId, engineRoot)))
        (
          links(engine, sandwich.conversationId),
          links(engine, engineRoot).filter(_.conversation == engineRoot)
        ) ==>
          (Vector.empty, Vector.empty)
        // What the first reply's placement saw: the Engine exchange offered first, and taken.
        right(engine.db.read(Subject.Public)(engine.stitches.placed(real.id))) match {
          case Some(f: Placed.Follows) =>
            f.seen.offered.headOption.map(_.root) ==> Some(engineRoot)
          case other => throw new java.lang.AssertionError(s"not follows: $other")
        }

        // The first reply was drafted in its own turn, and held: grit had replied in the strand.
        val drafted = TurnRef(real.conversationId, real.turnSeq)
        assert(eventually(outcome(drafted).nonEmpty))
        // A pin of the stored form (see outcome): its key names and the spoken kind.
        outcome(drafted).map(ujson.read(_)) ==>
          Some(ujson.Obj("kind" -> "spoken", "by" -> EntryId.value(replied.id)))
        right(engine.db.read(Subject.Public)(engine.speech.spoken(Instant.EPOCH)))
          .find(_.turn == drafted)
          .map(_.stage) ==> Some(Stage.Settled)

        // A top-level mention: stitched in its own turn, and shown the strand.
        right(
          engine.jot.write(Subject.Public)(
            engine.principals.name(TestAccounts.account("slack:T1/U0DAVID"), "David")
          )
        )
        val mention = engine.inbox
          .ingest(
            thread("6.0"),
            SourceId("6.0"),
            Message.User("@grit is this a real question? ~back:exchange 1"),
            TestAccounts.account("slack:T1/U0DAVID")
          )
          .fold(e => sys.error(e.toString), identity)
        engine.inbox.startTurn(mention) ==> Right(())
        def window =
          right(engine.db.read(Subject.Public)(engine.entries.get(Turn.windowId(mention))))
            .map(_.payload)
        assert(eventually(window.nonEmpty))
        links(engine, mention.conversationId) ==> Vector(Link(mention.conversationId, engineRoot))
        val along = window.toVector.flatMap {
          case Payload.Window(_, _, nearby, _) => nearby.collect { case a: Nearby.Along => a }
          case _ => Vector.empty
        }
        assert(along.exists(a => a.conversation == engineRoot && a.entries.contains(asked.seq)))
      } finally engine.close()
    }
  }
}
