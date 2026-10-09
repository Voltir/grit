package grit.app.main

import java.time.temporal.ChronoUnit
import java.time.{Instant, ZoneOffset}

import scala.concurrent.duration.*
import scala.util.Using

import grit.core.act.{Keeping, MoveLimits, MoveName, Moves, Posed}
import grit.core.classify.{Ask, StateJson}
import grit.core.clock.SetClock
import grit.core.document.{DocLabel, DocText, DocWeight, DocumentTerms}
import grit.core.edge.{Desk, Route, ToolRequest}
import grit.core.id.{
  Declarer,
  DocKey,
  JobName,
  PluginName,
  PrincipalId,
  ScheduleId,
  ScheduleKey,
  TurnRef,
  WorkflowId
}
import grit.core.inbox.Slotted
import grit.core.job.{Declared, Grace, JobRun, KeepingJob, PlainJob, SlotRule}
import grit.core.message.{AssistantBlock, Message, Tokens}
import grit.core.model.{Assignment, ModelId, ModelRef, Policy}
import grit.core.period.LifecycleSettings
import grit.core.place.{Namespace, Place, Service}
import grit.core.plugin.{DocumentPosting, Documents, Plugin}
import grit.core.provider.ModelRequest
import grit.core.schema.{JsonSchema, Typed}
import grit.core.speech.Speaking
import grit.core.spend.Budget
import grit.core.store.{Payload, StoreError, Tx}
import grit.core.tool.{Outcome, Retry, ToolName, ToolSet}
import grit.core.visibility.{Label, Subject}
import grit.dbos.engine.{Engine, LiveEngine}
import grit.dbos.sql.{DbConfig, LiveDb, TestPostgres}
import grit.edge.{Server, Tools}
import grit.job.clock.ClockEdge
import grit.kit.deployment.{Assembly, Deployment, Offer, Offered, Topics}
import grit.kit.environment.Secrets
import grit.kit.run.Launch
import grit.turn.{Turn, TurnLoop}

import utest.*

/** Against Postgres and DBOS, with the stub model: a declared job whose run calls a tool an
  * in-process edge serves at `service:probe`, then asks the model about its answer, launched as
  * the kit launches a deployment. Its request is made for the schedule's principal with the
  * retry the edge advertised, the edge rings the run, the ask's cost is recorded once, and a
  * run whose process stops between its request and the answer comes back to the same reply and
  * rows. And a plugin's keeping job, whose keep that refuses leaves nothing in its plugin's
  * documents, and whose next keep writes there.
  */
object JobMovesLiveTests extends TestSuite {

  private val probe: Service = Service.of("probe").fold(sys.error, identity)

  private val read: ToolName = ToolName("probe_read")

  /** One free tool, `probe_read`, answering "read {path}"; cut short, it is interrupted. */
  private object Reading extends Tools {
    val offered: ToolSet = ToolSet
      .of(
        Vector(
          ToolSet.Entry(
            read,
            "Reads a path.",
            ujson.Obj("type" -> "object"),
            asks = false,
            Retry.Interrupt
          )
        )
      )
      .fold(d => sys.error(d.toString), identity)

    def run(route: Route, request: ToolRequest): Outcome =
      Outcome.Done(s"read ${request.arguments.obj.get("path").flatMap(_.strOpt).getOrElse("")}")
  }

  private case object NoParams extends caps.Pure

  private def name(n: String): MoveName = MoveName.of(n).fold(sys.error, identity)

  /** `{"answer": string}`, read as the answer. */
  private val answered: Typed[String] = Typed(
    JsonSchema
      .read(
        ujson.Obj(
          "type" -> "object",
          "properties" -> ujson.Obj("answer" -> ujson.Obj("type" -> "string")),
          "required" -> ujson.Arr("answer"),
          "additionalProperties" -> false
        )
      )
      .fold(e => sys.error(e.message), identity),
    c => c.json.objOpt.flatMap(_.get("answer")).flatMap(_.strOpt).toRight("no answer")
  )

  /** A judgment's state: a message, as the stub classifier reads its markers. */
  private given StateJson[String] = StateJson.instance(s => ujson.Obj("new_message" -> s))

  /** Calls `probe_read` at `service:probe`, then asks the model about the answer, asks it for
    * `{"answer": string}` (the stub answering what its message marks), and judges whether a
    * message is urgent (the stub classifier answering the probability it marks); replies each
    * move's result, a line each.
    */
  private object Probing extends PlainJob[NoParams.type] {
    val name: JobName = JobName.of("probing").fold(sys.error, identity)
    val version: Int = 1
    override val limits: MoveLimits = MoveLimits.of(2, 1).fold(sys.error, identity)
    def write(params: NoParams.type): ujson.Value = ujson.Obj()
    def read(params: ujson.Value): Either[String, NoParams.type] = Right(NoParams)
    def run(run: JobRun[NoParams.type], moves: Moves^): String = {
      val called = moves
        .call(
          JobMovesLiveTests.name("look"),
          probe,
          JobMovesLiveTests.read,
          ujson.Obj("path" -> "a")
        )
        .fold(_.toString, _.toString)
      val asked = moves
        .ask(
          JobMovesLiveTests.name("think"),
          Posed.Text(ModelRequest("Say.", Vector(Message.User(called))))
        )
        .fold(_.toString, _.reply.blocks.collect { case AssistantBlock.Text(t) => t }.mkString)
      val shaped = moves
        .ask(
          JobMovesLiveTests.name("shape"),
          Posed.Json("Say.", Vector(Message.User("""#call:{"answer":"yes"}""")), answered)
        )
        .fold(_.toString, _.reply)
      val weighed = moves
        .ask(
          JobMovesLiveTests.name("weigh"),
          Posed.judge("~0.25", Ask.yesNo[String]("Is `new_message` urgent?", None, None))
        )
        .fold(_.toString, _.reply.toString)
      s"$called\n$asked\n$shaped\n$weighed"
    }
  }

  private val assigned =
    Assignment(
      ModelRef(ModelId.of("openai/gpt-oss-120b").getOrElse(sys.error("id")), None),
      100,
      None
    )

  /** The plugin whose documents [[Tallying]] keeps; nothing posts them. */
  private final class Tallies(at: Instant) extends Plugin {
    val name: PluginName = PluginName.of("tallies").fold(sys.error, identity)
    val version: Int = 1
    override val documents: Option[Documents] = Some(new Documents {
      val terms: DocumentTerms =
        DocLabel
          .of("tallies")
          .flatMap(DocumentTerms.of(_, DocWeight.Unscaled, 1.day, 10))
          .fold(sys.error, identity)
      val posting: Option[DocumentPosting] = None
    })
    override val jobs: Vector[grit.core.job.Job[?]] = Vector(Tallying)
    override val schedules: Vector[Declared[?]] = Vector(
      Declared(
        key,
        Tallying,
        SlotRule.Once(at, Grace.of(5.minutes).getOrElse(sys.error("grace"))),
        NoParams
      )
    )
  }

  /** A keeping job whose run keeps `undone` under `k` and refuses, then keeps `kept` there:
    * its reply, each keep's result, a line each.
    */
  private object Tallying extends KeepingJob[NoParams.type] {
    val name: JobName = JobName.of("tally").fold(sys.error, identity)
    val version: Int = 1
    def write(params: NoParams.type): ujson.Value = ujson.Obj()
    def read(params: ujson.Value): Either[String, NoParams.type] = Right(NoParams)
    def run(run: JobRun[NoParams.type], moves: Keeping^): String = {
      def keep(n: String, text: String, refuse: Boolean) =
        moves
          .keep[Tallied](JobMovesLiveTests.name(n)) { (keeper, at) =>
            keeper
              .write(
                DocKey.of("k").fold(sys.error, identity),
                Label.Public,
                Place.under(Namespace.Task, Vector("tallies")),
                DocText.of(text).fold(sys.error, identity),
                ujson.Obj(),
                at
              )
              .flatMap(w =>
                if (refuse) Left(StoreError.Invalid("refused"))
                else Right(Tallied(Label.written(w.kept)))
              )
          }
          .fold(_.toString, _.label)
      s"${keep("undone", "undone", refuse = true)}\n${keep("kept", "kept", refuse = false)}"
    }
  }

  /** The label a keep was kept at, in its written form. */
  private final case class Tallied(label: String) extends caps.Pure
  private object Tallied {
    given grit.core.durable.Journaled[Tallied] = grit.core.durable.Journaled
      .json(t => ujson.Str(t.label), v => v.strOpt.map(Tallied(_)).toRight("no label"))
  }

  private val key: ScheduleKey = ScheduleKey.of("probe").fold(sys.error, identity)

  private val schedule: ScheduleId = ScheduleId.declared(Declarer.Deployment, key)

  /** The deployment of [[Probing]], its once slot at `at`, its classifier the stub. */
  private def deployment(at: Instant): Deployment =
    Deployment
      .of(
        edges = Vector.empty,
        worksIn = Vector.empty,
        plugins = Vector.empty,
        policy = Policy(assigned, assigned, assigned, assigned),
        offer = Offer(Offered.Read, TurnLoop.Budget.of(2).getOrElse(sys.error("rounds"))),
        assembly = Assembly.Linear(Tokens(4000)),
        topics = Topics.Stub,
        lifecycle = LifecycleSettings.Default,
        budget = Budget(ZoneOffset.UTC, None),
        speaking = Speaking.Off,
        sweep = 30.seconds,
        persona = grit.core.persona.Persona.Grit,
        jobs = Vector(Probing),
        schedules = Vector(
          Declared(
            key,
            Probing,
            SlotRule.Once(at, Grace.of(5.minutes).getOrElse(sys.error("grace"))),
            NoParams
          )
        )
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

  private def right[A](e: Either[StoreError, A]): A = e.fold(x => sys.error(x.toString), identity)

  /** A desk on `engine` serving `service:probe` for grit, advertising [[Reading]]. */
  private def desk(engine: Engine^): Desk^{engine} = {
    val d = engine.register(PrincipalId.Grit, Set(probe.place)) match {
      case Right(d) => d
      case Left(e) => sys.error(e.why)
    }
    d.advertise(probe.place, Reading.offered, Vector.empty).fold(e => sys.error(e.why), identity)
    d
  }

  /** An edge serving [[Reading]] through `desk`, until closed. */
  private def serving(desk: Desk^): Server^{desk} = {
    val server =
      new Server(desk, Reading, run => { val _ = Thread.ofVirtual().start(() => run()) }, _ => ())
    server.serve()
    server
  }

  /** The run the clock edge starts on `engine` for `d`'s slot of `id` at `at`. */
  private def started(
      engine: Engine^,
      d: Deployment,
      at: Instant,
      id: ScheduleId = schedule
  ): TurnRef = {
    val ticked =
      right(
        new ClockEdge(engine.inbox, engine.schedules, engine.db, new SetClock(at), d.allJobs).tick()
      )
    ticked.slotted
      .collectFirst { case (`id`, Slotted.Started(turn, _)) =>
        turn
      }
      .getOrElse(sys.error(s"not started: $ticked"))
  }

  /** The rows of `sql`, its one parameter `param`, each column as text. */
  private def rows(config: DbConfig, sql: String, param: String): Vector[Vector[String]] =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(conn.prepareStatement(sql)) { ps =>
        ps.setString(1, param)
        Using.resource(ps.executeQuery()) { rs =>
          val n = rs.getMetaData.getColumnCount
          val out = Vector.newBuilder[Vector[String]]
          while (rs.next()) out += (1 to n).toVector.map(i => rs.getString(i))
          out.result()
        }
      }
    }

  /** What `turn` left, by what a run's moves write: its request (principal, tool, retry,
    * arguments, state, outcome), its ring, its ledger rows (by id, the workflow's own part
    * dropped), its reply, and its move steps.
    */
  private def left(config: DbConfig, engine: Engine^, turn: TurnRef): Vector[Vector[String]] = {
    val id = WorkflowId.value(turn.workflowId)
    rows(
      config,
      "SELECT principal, tool, retry, arguments::text, state, outcome::text FROM grit.tool_requests WHERE workflow_id = ?",
      id
    ) ++
      rows(config, "SELECT message FROM dbos.notifications WHERE destination_uuid = ?", id) ++
      // No cost column: the stub provider and classifier answer at no cost, so it would read 0
      // whatever a move recorded. The unit tier pins each move's priced cost (RunTests).
      rows(
        config,
        "SELECT replace(entry_id, ?, '{workflow}'), model FROM grit.usage_ledger ORDER BY entry_id",
        id
      ) ++
      Vector(Vector(replied(engine, turn))) ++
      rows(
        config,
        "SELECT function_name FROM dbos.operation_outputs WHERE workflow_uuid = ? AND function_name LIKE 'move:%' ORDER BY function_id",
        id
      )
  }

  /** `turn`'s reply's text; "no reply" when it has none. */
  private def replied(engine: Engine^, turn: TurnRef): String =
    right(engine.db.read(Subject.Public)(engine.entries.get(turn.replyId))).map(_.payload) match {
      case Some(Payload.Message(Message.Assistant(blocks, _, _, _, _))) =>
        blocks.collect { case AssistantBlock.Text(t) => t }.mkString
      case other => s"no reply: $other"
    }

  /** Waits up to 30 s for `turn`'s request to be written. */
  private def requested(config: DbConfig, turn: TurnRef): Boolean = {
    val id = WorkflowId.value(turn.workflowId)
    within(30.seconds)(
      rows(config, "SELECT key FROM grit.tool_requests WHERE workflow_id = ?", id).nonEmpty
    )
  }

  /** `turn`'s output once its workflow has ended, waiting up to 60 s; fails naming what it had
    * left when it has not ended by then, so a run that never comes back fails the suite rather
    * than hanging it.
    */
  private def awaited(config: DbConfig, engine: Engine^, turn: TurnRef): String = {
    val id = WorkflowId.value(turn.workflowId)
    def ended =
      rows(config, "SELECT status FROM dbos.workflow_status WHERE workflow_uuid = ?", id).exists(
        _.exists(s => s != "PENDING" && s != "ENQUEUED")
      )
    if (within(60.seconds)(ended)) engine.awaitTurn(turn)
    else sys.error(s"$id has not ended in 60 s; it left ${left(config, engine, turn)}")
  }

  /** Whether `done` holds within `limit`, polled every 20 ms. */
  private def within(limit: FiniteDuration)(done: => Boolean): Boolean = {
    val until = System.nanoTime() + limit.toNanos
    var held = done
    while (!held && System.nanoTime() < until) {
      Thread.sleep(20)
      held = done
    }
    held
  }

  private def launch(engine: Engine^, config: DbConfig, d: Deployment): Unit =
    Launch(engine, d, secrets(config, d), Launch.Run.Served, sweeping = false, _ => ())

  /** What a run of [[Probing]] left in a fresh database `name`, its edge serving throughout. */
  private def uninterrupted(name: String): (String, Vector[Vector[String]]) = {
    val config = TestPostgres.freshDatabase(name)
    val at = Instant.now().truncatedTo(ChronoUnit.SECONDS)
    val d = deployment(at)
    val engine = LiveEngine.open(config, Turn.Epoch)
    try {
      launch(engine, config, d)
      val server = serving(desk(engine))
      try {
        val turn = started(engine, d, at)
        val said = awaited(config, engine, turn)
        (said.replace(replyOf(turn), "{reply}"), left(config, engine, turn))
      } finally server.close()
    } finally engine.close()
  }

  private def replyOf(turn: TurnRef): String = grit.core.id.EntryId.value(turn.replyId)

  val tests = Tests {
    test(
      "a run's call is made for its schedule's principal with the advertised retry, rung, and its text, JSON and judgment asks' costs recorded once each"
    ) {
      // A deployment's schedule acts for grit, who also registered the desk, so the request's
      // principal cannot tell the schedule's from a hard-coded grit; CallingTests' gone schedule,
      // made for no one, does.
      val (said, rows) = uninterrupted("job_moves")
      (said, rows) ==> (
        "replied: {reply}",
        Vector(
          Vector(
            "grit",
            "probe_read",
            "interrupt",
            """{"path": "a"}""",
            "answered",
            """{"kind": "done", "text": "read a"}"""
          ),
          Vector(ujson.write(ujson.Str(Desk.Doorbell))),
          Vector("move:{workflow}:shape", "grit/stub"),
          Vector("move:{workflow}:think", "grit/stub"),
          Vector("move:{workflow}:weigh", "grit/stub-classifier"),
          Vector("Done(read a,public)\nstub reply to: Done(read a,public)\nyes\n0.25"),
          Vector("move:look"),
          Vector("move:look:answer"),
          Vector("move:think"),
          Vector("move:think:record"),
          Vector("move:shape"),
          Vector("move:shape:record"),
          Vector("move:weigh"),
          Vector("move:weigh:record")
        )
      )
    }

    test(
      "a keeping job's keep that refuses leaves nothing in its plugin's documents; its next keep writes there"
    ) {
      val config = TestPostgres.freshDatabase("job_keeps")
      val at = Instant.now().truncatedTo(ChronoUnit.SECONDS)
      val tallies = new Tallies(at)
      val d = Deployment
        .of(
          edges = Vector.empty,
          worksIn = Vector.empty,
          plugins = Vector(tallies),
          policy = Policy(assigned, assigned, assigned, assigned),
          offer = Offer(Offered.Read, TurnLoop.Budget.of(2).getOrElse(sys.error("rounds"))),
          assembly = Assembly.Linear(Tokens(4000)),
          topics = Topics.Off("no classifier here"),
          lifecycle = LifecycleSettings.Default,
          budget = Budget(ZoneOffset.UTC, None),
          speaking = Speaking.Off,
          sweep = 30.seconds,
          persona = grit.core.persona.Persona.Grit
        )
        .fold(r => sys.error(r.message), identity)
      val engine = LiveEngine.open(config, Turn.Epoch)
      try {
        launch(engine, config, d)
        val turn = started(engine, d, at, ScheduleId.declared(Declarer.Plugin(tallies.name), key))
        val _ = awaited(config, engine, turn)
        val id = WorkflowId.value(turn.workflowId)
        (
          replied(engine, turn),
          rows(
            config,
            "SELECT key, body FROM grit.documents WHERE plugin = ? ORDER BY version",
            "tallies"
          ),
          rows(
            config,
            "SELECT function_name FROM dbos.operation_outputs WHERE workflow_uuid = ? AND function_name LIKE 'move:%' ORDER BY function_id",
            id
          )
        ) ==> (
          s"${grit.core.act.MoveError.Store("refused")}\npublic",
          Vector(Vector("k", "kept")),
          Vector(Vector("move:undone"), Vector("move:kept"))
        )
      } finally engine.close()
    }

    test(
      "a run whose process stops between its request and the answer comes back to the same reply and rows"
    ) {
      val expected = uninterrupted("job_moves_whole")
      val config = TestPostgres.freshDatabase("job_moves_restart")
      val at = Instant.now().truncatedTo(ChronoUnit.SECONDS)
      val d = deployment(at)
      // Its edge advertises and serves nothing: the request waits, unclaimed, when it stops.
      val before = LiveEngine.open(config, Turn.Epoch)
      val turn =
        try {
          launch(before, config, d)
          val _ = desk(before)
          val t = started(before, d, at)
          assert(requested(config, t))
          t
        } finally before.close()
      val after = LiveEngine.open(config, Turn.Epoch)
      try {
        // Served before the run is recovered, so the ring is waiting for its wait.
        val server = serving(desk(after))
        try {
          launch(after, config, d)
          val said = awaited(config, after, turn).replace(replyOf(turn), "{reply}")
          (said, left(config, after, turn)) ==> expected
        } finally server.close()
      } finally after.close()
    }
  }
}
