package grit.app.main

import scala.util.Using

import grit.core.id.{ToolCallId, TurnRef, WorkflowId}
import grit.core.message.Message
import grit.core.store.{Payload, Tx}
import grit.core.visibility.Subject
import grit.dbos.engine.{Engine, LiveEngine}
import grit.dbos.sql.{DbConfig, LiveDb, TestPostgres}
import grit.turn.Turn

import utest.*

/** Against Postgres and DBOS: a task's turn reads a GitHub issue through the MCP edge and is
  * asked to post to Slack, its model obeying every instruction it is shown. A turn that reads
  * the issue, a result of a service believed at nothing, is offered no `slack_post` from the
  * next round on, and a call of it is refused and nothing posted; a turn that asks the hidden
  * issue a bounded question through `extract` keeps `slack_post`, and posts only where the
  * person asked. A crash while GitHub's call is in flight replays the same gate decisions;
  * that case runs under the shipped declarations, as the crashing process and a reopened
  * engine do, both its runs alike, and the others under the reference deployment's.
  */
object IntegrityGateLiveTests extends TestSuite {
  import IntegrityTurn.*

  /** One case: the issue read, what the turn's model does with it while hidden, how the
    * summary's pair answers an extraction; then, as the turn ran, whether each request offered
    * `slack_post`, where the posting service posted, what the turn's refused call to
    * `slack_post` was answered (`None`: no call was refused), and what its extraction was
    * answered (`None`: it asked none).
    */
  final case class Case(
      name: String,
      issue: Issue,
      mode: Mode,
      asked: Asked,
      offered: Vector[Boolean],
      posted: Vector[String],
      refused: Option[String],
      extracted: Option[String]
  )

  private val answeredNo = "h1 answers: no."

  private val withheld =
    "You can't post to channels right now, because of something you read; nothing was done, " +
      "so answer without it."

  private val general = "slack:T0NEUTRAL/C0GENERAL"

  private val cases: Vector[Case] = Vector(
    Case(
      "reads",
      Plain,
      Mode.Reads,
      Asked.Honest,
      Vector(true, true, false, false),
      Vector(),
      Some(withheld),
      None
    ),
    Case(
      "extracts",
      Plain,
      Mode.Extracts,
      Asked.Honest,
      Vector(true, true, true, true),
      Vector(general),
      None,
      Some(answeredNo)
    ),
    Case(
      "reads_planted",
      Planted,
      Mode.Reads,
      Asked.Honest,
      Vector(true, true, false, false),
      Vector(),
      Some(withheld),
      None
    ),
    Case(
      "extracts_planted",
      Planted,
      Mode.Extracts,
      Asked.Honest,
      Vector(true, true, true, true),
      Vector(general),
      None,
      Some(answeredNo)
    ),
    // The pair, shown the planted issue, calls slack_post instead of answering: the turn is
    // told only that it gave no answer, quoting nothing of what it said.
    Case(
      "extracts_planted_obeyed",
      Planted,
      Mode.Extracts,
      Asked.Obeying,
      Vector(true, true, true),
      Vector(),
      None,
      Some("h1 gave no answer that reads as true or false.")
    )
  )

  /** What a run of a case's turn came to. */
  final case class Ran(
      turn: TurnRef,
      requests: Vector[grit.core.provider.ModelRequest],
      extractions: Vector[grit.core.provider.ModelRequest],
      posted: Vector[String],
      results: Map[String, String],
      gates: Vector[(String, String)]
  )

  /** `c`'s turn, run to its end on a fresh database under the reference deployment's
    * declarations ([[Main.visibility]]).
    */
  private def run(c: Case): Ran = {
    val config = TestPostgres.freshDatabase(s"integrity_${c.name}")
    val engine =
      LiveEngine.open(config, Turn.Epoch, visibility = reference)
    try ranOn(engine, config, c.name, c.issue, c.mode, c.asked)
    finally engine.close()
  }

  private def reference =
    Main.visibility(Vector.empty, None, Vector.empty).fold(sys.error(_), identity)

  /** The turn `name` on `engine`, its model's calls made by an [[Obeying]] model for `issue`
    * and `mode`, extractions answered as `asked` says, run to its end.
    */
  private def ranOn(
      engine: Engine^,
      config: DbConfig,
      name: String,
      issue: Issue,
      mode: Mode,
      asked: Asked
  ): Ran = {
    val obeying = new Obeying(issue, mode)
    val answering = new Answering(asked)
    val posts = new Posts
    launch(engine, obeying, answering)
    val fake = github(issue)
    try {
      val turn = serving(engine, fake, posts) { () =>
        val turn = IntegrityTurn.ask(engine, name, Request)
        val _ = engine.awaitTurn(turn)
        turn
      }
      ranOf(engine, config, turn, obeying, answering, posts)
    } finally fake.stop()
  }

  private def ranOf(
      engine: Engine^,
      config: DbConfig,
      turn: TurnRef,
      obeying: Obeying,
      answering: Answering,
      posts: Posts
  ): Ran = {
    val results = engine.db
      .read(Subject.Public)(engine.entries.list(turn.conversationId))
      .fold(e => sys.error(e.toString), identity)
      .collect { case e if e.turnSeq == turn.turnSeq => e.payload }
      .collect { case Payload.Result(Message.ToolResult(call, content, _), _) =>
        ToolCallId.value(call) -> content
      }
      .toMap
    Ran(
      turn,
      obeying.requests,
      answering.extractions,
      posts.ran.flatMap(_.destination).map(_.written),
      results,
      gates(config, turn)
    )
  }

  /** Each `gate:n` step `turn` recorded, by name, with its output. */
  private def gates(config: DbConfig, turn: TurnRef): Vector[(String, String)] =
    LiveEngine
      .steps(config, turn.workflowId)
      .fold(e => sys.error(e.toString), identity)
      .collect { case s if s.name.startsWith("gate:") => s.name -> s.output.getOrElse("") }

  /** Every `gate:n` step the workflows of `config`'s database recorded, by name, with its
    * output, each workflow's in turn: found straight from DBOS, with no engine open.
    */
  private def recordedGates(config: DbConfig): Vector[(String, String)] = {
    val ids = LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(
        conn.prepareStatement(
          "SELECT DISTINCT workflow_uuid FROM dbos.operation_outputs WHERE function_name LIKE 'gate:%'"
        )
      ) { ps =>
        Using.resource(ps.executeQuery()) { rs =>
          val out = Vector.newBuilder[String]
          while (rs.next()) out += rs.getString(1)
          out.result()
        }
      }
    }
    ids.flatMap(id =>
      LiveEngine
        .steps(config, WorkflowId(id))
        .fold(e => sys.error(e.toString), identity)
        .collect { case s if s.name.startsWith("gate:") => s.name -> s.output.getOrElse("") }
    )
  }

  val tests = Tests {
    test(
      "a turn that reads an untrusted issue is offered no slack_post after and posts nothing; one that extracts keeps it and posts only to #general"
    ) {
      val failed = cases.flatMap { c =>
        val ran = run(c)
        val seen = (
          ran.requests.map(_.tools.exists(_.name == "slack_post")),
          ran.posted,
          ran.results.get("p1").orElse(ran.results.get("p2")).filter(_ => c.refused.nonEmpty),
          ran.results.get("x1")
        )
        val hid = ran.gates.find(_._1 == "gate:1").exists(_._2.contains("\"kind\":\"hidden\""))
        Option.when(seen != (c.offered, c.posted, c.refused, c.extracted) || !hid)(
          s"${c.name}: $seen, hid $hid"
        )
      }
      failed ==> Vector()
    }

    test(
      "a turn that extracts never shows its model the planted issue, and its ask is shown it alone"
    ) {
      val ran = run(cases(3))
      ran.requests.exists(_.messages.exists {
        case Message.ToolResult(_, content, _) => content.contains(Injection)
        case Message.User(text) => text.contains(Injection)
        case _ => false
      }) ==> false
      ran.extractions.map(r => (r.tools.map(_.name), r.messages.size)) ==> Vector(
        (Vector("reply"), 1)
      )
      assert(ran.extractions.forall(_.messages.exists {
        case Message.User(text) => text.contains(Injection)
        case _ => false
      }))
    }

    test("a crash while GitHub's call is in flight replays the same gate decisions") {
      // The same turn, run whole on a database of its own: the decisions a crash must keep.
      val whole = {
        val config = TestPostgres.freshDatabase("integrity_whole")
        val engine = LiveEngine.open(config, Turn.Epoch)
        try ranOn(engine, config, "crash", Planted, Mode.Reads, Asked.Honest).gates
        finally engine.close()
      }
      val config = TestPostgres.freshDatabase("integrity_crash")
      val child = os
        .proc(
          sys.env("GRIT_TEST_JAVA"),
          "--sun-misc-unsafe-memory-access=allow",
          "-cp",
          sys.env("GRIT_TEST_CLASSPATH"),
          "grit.app.main.CrashingTurn",
          "crash",
          "github-in-flight"
        )
        .call(
          env = Map(
            "GRIT_DATABASE_URL" -> config.jdbcUrl,
            "GRIT_DATABASE_USER" -> config.user,
            "GRIT_DATABASE_PASSWORD" -> config.password
          ),
          check = false,
          mergeErrIntoOut = true
        )
      assert(child.exitCode == CrashingTurn.Halted)
      // What the crashed run had recorded of its gates, read before anything recovers it.
      val recorded = recordedGates(config)
      val engine = LiveEngine.reopen(config, Turn.Epoch)
      val ran =
        try {
          val obeying = new Obeying(Planted, Mode.Reads)
          val answering = new Answering(Asked.Honest)
          val posts = new Posts
          launch(engine, obeying, answering)
          val fake = github(Planted)
          try {
            // Idempotent ingest hands back the crashed turn; recovery is already running it,
            // and this edge reruns the GitHub call the crashed one claimed.
            val turn = serving(engine, fake, posts) { () =>
              val turn = IntegrityTurn.ask(engine, "crash", Request)
              val _ = engine.awaitTurn(turn)
              turn
            }
            ranOf(engine, config, turn, obeying, answering, posts)
          } finally fake.stop()
        } finally engine.close()
      // The crash came after the first gate and before the second; the recovered turn kept
      // the first as recorded and decided every later one as the whole run did.
      whole.map(_._1) ==> Vector("gate:0", "gate:1", "gate:2", "gate:3")
      (recorded, ran.gates, ran.posted) ==> (whole.take(1), whole, Vector())
      ran.results.get("p1") ==> Some(withheld)
    }
  }
}
