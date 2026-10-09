package grit.job.run

import java.time.{Instant, ZoneOffset}

import scala.concurrent.duration.*

import grit.act.moves.{MoveRecords, MovesEnv}
import grit.core.act.MovesFixtures.{Answering, FakeDb, PerChar}
import grit.core.act.{Asked, Keeping, MoveError, MoveLimits, MoveName, Moves, Posed}
import grit.core.clock.SetClock
import grit.core.document.{DocLabel, DocText, DocWeight, DocumentTerms, InMemoryDocuments}
import grit.core.durable.{InMemoryDurable, Journaled}
import grit.core.edge.{
  Advert,
  EdgeDirectory,
  InMemoryDeliveries,
  InMemoryEdges,
  Pending,
  RequestState,
  ToolRequest,
  ToolRequests
}
import grit.core.id.{
  CallSlot,
  ConversationId,
  Declarer,
  DocKey,
  EntryId,
  PluginName,
  PrincipalId,
  ScheduleId,
  ScheduleKey,
  TestCallSlots,
  TurnRef,
  TurnSeq,
  WorkflowId
}
import grit.core.identity.{Principal, TestAccounts}
import grit.core.inbox.{InMemoryInbox, Slotted}
import grit.core.job.JobTests.{Count, Counting}
import grit.core.job.ScheduleContract.{booking, hour}
import grit.core.job.{Declared, JobRun, Jobs, KeepingJob, Owned, PlainJob, Schedule, SlotRule, When}
import grit.core.message.{AssistantBlock, Message, Tokens, Usage}
import grit.core.place.{Namespace, Place, Service}
import grit.core.provider.ModelRequest
import grit.core.spend.{Budget, DailyCap}
import grit.core.store.{
  Askers,
  InMemoryToolSets,
  InMemoryUsageLedger,
  Jot,
  Origin,
  Payload,
  StoreError,
  Tx,
  UsageLedger
}
import grit.core.tool.{Outcome, Retry, ToolName, ToolSet}
import grit.core.visibility.{Clearance, Compartments, Label, RoomLabels, Subject, Trust, Visibility}
import grit.dbos.sql.TestTx

/** A run's world over core's in-memory fakes: an inbox that starts slots as the SQL one does,
  * its schedules, the deliveries an edge reads, and a clock.
  */
object RunFixtures {

  /** When the slots below are due. */
  val Due: Instant = Instant.parse("2026-10-07T09:00:00Z")

  /** `remind` at version 1, the job every slot below is started at. */
  val remind = new Counting("remind")

  /** The deployment of `remind` at `version`. */
  def jobs(version: Int): Jobs = jobsOf(new Counting("remind", version))

  def jobsOf(job: PlainJob[?]*): Jobs =
    Jobs
      .of(job.toVector.map(Owned.Deployments(_)))
      .fold(n => sys.error(s"two jobs named $n"), identity)

  /** The plugin whose keeping job [[noting]] is. */
  val Notes: PluginName = PluginName.of("notes").fold(sys.error, identity)

  val NotesTerms: DocumentTerms =
    DocLabel
      .of("notes")
      .flatMap(DocumentTerms.of(_, DocWeight.Unscaled, 1.day, 10))
      .fold(sys.error, identity)

  /** The deployment of `job`, [[Notes]]' keeping job. */
  def keepingJobs(job: KeepingJob[?]): Jobs =
    Jobs
      .of(Vector(Owned.Keeps(Notes, NotesTerms, job)))
      .fold(n => sys.error(s"two jobs named $n"), identity)

  /** `remind` at `version` as [[Notes]]' keeping job: its run keeps `counted {n}` under the key
    * `count`, publicly, in its keep `note`, and replies the label it was kept at, or the keep's
    * error.
    */
  final class Noting(val version: Int) extends KeepingJob[Count] {
    val name = remind.name
    def write(params: Count): ujson.Value = remind.write(params)
    def read(params: ujson.Value): Either[String, Count] = remind.read(params)
    def run(run: JobRun[Count], moves: Keeping^): String =
      moves
        .keep[Noted](move("note")) { (keeper, at) =>
          (for {
            key <- DocKey.of("count")
            text <- DocText.of(s"counted ${run.params.n}")
          } yield (key, text)) match {
            case Left(why) => Left(StoreError.Invalid(why))
            case Right((key, text)) =>
              keeper
                .write(
                  key,
                  Label.Public,
                  Place.under(Namespace.Task, Vector("notes")),
                  text,
                  ujson.Obj(),
                  at
                )
                .map(w => Noted(Label.written(w.kept)))
          }
        }
        .fold(_.toString, _.label)
  }

  /** The label a keep was kept at, in its written form. */
  final case class Noted(label: String) extends caps.Pure
  object Noted {
    given Journaled[Noted] =
      Journaled.json(n => ujson.Str(n.label), v => v.strOpt.map(Noted(_)).toRight("no label"))
  }

  /** A job named `remind` whose parameters are text: it cannot read a count. */
  final class Texting extends PlainJob[Text] {
    val name = remind.name
    val version = 1
    def write(params: Text): ujson.Value = ujson.Str(params.text)
    def read(params: ujson.Value): Either[String, Text] =
      params.strOpt.map(Text(_)).toRight(s"not text: $params")
    def run(run: JobRun[Text], moves: Moves^): String = run.params.text
  }

  final case class Text(text: String) extends caps.Pure

  /** Writes as its transaction is committed, nothing rolled back; with `crashAfter`, the
    * process dies once just after the first write commits.
    */
  final class FakeJot(crashAfter: Boolean = false) extends Jot {
    @caps.unsafe.untrackedCaptures
    private var armed = crashAfter
    def write[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] = {
      val written = body(using TestTx.fake)
      if (armed) { armed = false; throw new InMemoryDurable.Crash }
      written
    }
  }

  /** A run's world. Its moves: a model whose every answer is [[Answer]], counting its calls; a
    * ledger, recording at [[Due]]; a day's cap of `cap` dollars, if any; and `durable`, whose
    * transactions, like every read, are opened at `floor`, with [[Probe]] trusted with `trust`. With `crashRecord`, the process dies once inside the
    * first ask's record; with `failRecord`, every record fails; with `crashServing`, the process
    * dies once inside the first call's step, after it read whom the call is for; with
    * `crashModel`, inside the first model call.
    */
  final class World(
      cap: Option[String] = None,
      floor: Label = Label.Public,
      trust: Label = Label.Public,
      crashRecord: Boolean = false,
      failRecord: Boolean = false,
      crashServing: Boolean = false,
      crashModel: Boolean = false
  ) extends caps.SharedCapability {
    val inbox: InMemoryInbox = InMemoryInbox.fresh()
    val deliveries: InMemoryDeliveries = new InMemoryDeliveries
    val clock: SetClock = new SetClock(Due.plusSeconds(30))
    private val visibility: Visibility =
      Visibility
        .of(
          Compartments.Shipped,
          RoomLabels.Public,
          Vector.empty,
          Vector.empty,
          Vector(Trust(Probe, trust))
        )
        .fold(r => sys.error(r.toString), identity)
    private val db = new FakeDb(Clearance.of(floor), visibility)
    val durable: InMemoryDurable =
      new InMemoryDurable(resolve = _ => Clearance.of(floor), visibility = visibility)
    val ledger: InMemoryUsageLedger = {
      val l = new InMemoryUsageLedger
      l.now = Due
      l
    }
    val edges: InMemoryEdges = new InMemoryEdges
    val toolSets: InMemoryToolSets = new InMemoryToolSets
    val documents: InMemoryDocuments = new InMemoryDocuments
    val models: Answering = new Answering(Answer, crashModel)

    private val switches = new Switches(crashRecord, crashServing)
    private val requests = new AnsweringEdges(edges, durable, switches)
    private val recording = new RecordingLedger(ledger, switches, failRecord)

    /** An edge serving [[Probe]], advertising [[Read]] (free, interrupted if cut short) and
      * [[Asks]] (asking first); when `answers`, it claims and answers each request
      * `Done("read")` and rings its run as it is written.
      */
    def serve(answers: Boolean = true): Unit = {
      val edge = edges.register(Set(Probe.place), PrincipalId.Grit)
      val set = ToolSet
        .of(
          Vector(
            ToolSet.Entry(Read, "Reads a path.", Schema, asks = false, Retry.Interrupt),
            ToolSet.Entry(Asks, "Asks first.", Schema, asks = true, Retry.Rerun)
          )
        )
        .fold(d => sys.error(d.toString), identity)
      val _ = toolSets.keep(set)(using TestTx.fake)
      edges.advertiseAs(edge, Probe.place, set, Vector.empty)
      switches.answering = Option.when(answers)(edge)
    }

    def env(jot: Jot^ = new FakeJot()): RunEnv^ =
      RunEnv(
        RunRecords(inbox.entries, inbox.conversations, inbox.schedules, deliveries),
        db,
        jot,
        clock,
        MovesEnv(
          MoveRecords(
            recording,
            ledger,
            requests,
            requests,
            toolSets,
            inbox.schedules,
            NoAskers,
            PerChar,
            documents.savepoints
          ),
          models,
          db,
          clock
        ),
        Budget(ZoneOffset.UTC, cap.flatMap(DailyCap.of(_).toOption)),
        (plugin, terms) => documents.keeper(plugin, terms)
      )

    /** The requests written, in order. */
    def sent: Vector[ToolRequest] = edges.rows.map(_.request)

    /** Where each request written stands. */
    def standing: Vector[RequestState | InMemoryEdges.Open.type] = edges.rows.map(_.state)

    /** A declared once schedule of `remind`, keyed `key`, with the count `n`, due at [[Due]]. */
    def declared(key: String, n: Int = 1): ScheduleId = {
      val k = ScheduleKey.of(key).fold(sys.error, identity)
      ok(
        inbox.schedules.declare(
          Vector(Declarer.Deployment -> Declared(k, remind, SlotRule.Once(Due, hour), Count(n))),
          Due.minusSeconds(60)
        )(using TestTx.fake)
      )
      ScheduleId.declared(Declarer.Deployment, k)
    }

    /** A schedule of `remind` with the count `n`, asked in `thread`, a Slack thread whose
      * reply was posted at `address`, due at [[Due]].
      */
    def asked(thread: String, address: String, n: Int = 1): ScheduleId = {
      val origin = Origin.Slack("T1", "C1", thread)
      val turn = TurnRef(ConversationId(thread), TurnSeq.First)
      inbox.schedules.asking(turn, origin, TestAccounts.account("test:ann"), Some(address))
      val plugin = PluginName.of("reminders").fold(sys.error, identity)
      val desk =
        inbox.schedules.desk(plugin, Vector(remind.name), new SetClock(Due.minusSeconds(3600)))
      desk
        .ask(TestCallSlots.at(turn), booking(remind), When.At(Due), hour, Count(n))
        .fold(r => sys.error(s"$r"), _.id)
    }

    /** `schedule`'s due slot started at `version`, as the clock edge starts it: its run's turn. */
    def started(schedule: ScheduleId, version: Int = 1): TurnRef =
      inbox.startSlot(schedule, Some(version), Due) match {
        case Right(Slotted.Started(turn, _)) => turn
        case Right(Slotted.Superseding(turn, _)) => turn
        case other => sys.error(s"not started: $other")
      }

    def schedule(id: ScheduleId): Option[Schedule] = ok(inbox.schedules.read(id)(using TestTx.fake))

    /** `turn`'s reply, as its entry holds it. */
    def reply(turn: TurnRef): Option[String] =
      ok(inbox.entries.get(turn.replyId)(using TestTx.fake)).collect {
        case e if e.turnSeq == turn.turnSeq =>
          e.payload match {
            case Payload.Message(Message.Assistant(blocks, _, _, _, _)) =>
              blocks.collect { case AssistantBlock.Text(t) => t }.mkString
            case other => s"not a reply: $other"
          }
      }

    /** How many entries `turn` has. */
    def entries(turn: TurnRef): Int = ok(inbox.entries.ofTurn(turn)(using TestTx.fake)).size

    def pending: Vector[Pending] = ok(deliveries.pending()(using TestTx.fake))

    /** `slot`'s schedule's run in flight, as the inbox reads it: the slot and its version. */
    def running(id: ScheduleId): Option[(Instant, Option[Int])] =
      inbox.schedules.held(id).flatMap(_._4)
  }

  def ok[A](e: Either[StoreError, A]): A = e.fold(err => sys.error(s"$err"), identity)

  /** What a world's fakes do next: the edge that answers each request, if any; whether the
    * next record, or the next read of who serves, dies.
    */
  final class Switches(crashRecord: Boolean, crashServing: Boolean) {
    // Each holds an immutable value; one test's, read on its one thread.
    @caps.unsafe.untrackedCaptures
    var answering: Option[grit.core.edge.Registration] = None
    @caps.unsafe.untrackedCaptures
    var recordArmed: Boolean = crashRecord
    @caps.unsafe.untrackedCaptures
    var servingArmed: Boolean = crashServing
  }

  /** `edges`, but the edge `switches` names claims, answers `Done("read")` and rings each
    * request as it is written; and a read of who serves dies once when armed.
    */
  final class AnsweringEdges(edges: InMemoryEdges, durable: InMemoryDurable, switches: Switches)
      extends ToolRequests,
        EdgeDirectory {
    def dispatch(sent: Vector[ToolRequest])(using Tx^): Either[StoreError, Unit] = {
      val written = edges.dispatch(sent)
      for (q <- sent; edge <- switches.answering) {
        if (edges.claimAs(edge, q) && edges.answerAs(edge, q.slot, Outcome.Done("read")))
          durable.send(q.slot.turn.workflowId, q.slot.key, "rang")
      }
      written
    }
    def settle(slot: CallSlot)(using Tx^): Either[StoreError, RequestState] = edges.settle(slot)
    def abandon(slot: CallSlot)(using Tx^): Either[StoreError, RequestState] = edges.abandon(slot)
    def answered(slot: CallSlot)(using Tx^): Either[StoreError, Option[Outcome]] =
      edges.answered(slot)
    def serving(place: Place)(using Tx^): Either[StoreError, Option[Advert]] = {
      if (switches.servingArmed) { switches.servingArmed = false; throw new InMemoryDurable.Crash }
      edges.serving(place)
    }
  }

  /** `ledger`, but its next record dies once when armed, and every record fails when `failing`. */
  final class RecordingLedger(ledger: InMemoryUsageLedger, switches: Switches, failing: Boolean)
      extends UsageLedger {
    def record(
        entry: EntryId,
        turn: TurnRef,
        workflow: WorkflowId,
        model: String,
        usage: Usage,
        estimatedInput: Tokens
    )(using Tx^): Either[StoreError, Unit] =
      if (switches.recordArmed) { switches.recordArmed = false; throw new InMemoryDurable.Crash }
      else if (failing) Left(StoreError.DatabaseError("the ledger is down"))
      else ledger.record(entry, turn, workflow, model, usage, estimatedInput)
    def of(workflow: WorkflowId)(using Tx^): Either[StoreError, Vector[UsageLedger.Row]] =
      ledger.of(workflow)
    def forget(conversation: ConversationId, from: TurnSeq, to: TurnSeq)(using
        Tx^
    ): Either[StoreError, Unit] = ledger.forget(conversation, from, to)
  }

  /** What every model asked answers. */
  val Answer = "hello"

  val Probe: Service = Service.of("probe").fold(sys.error, identity)

  /** A tool the edge at [[Probe]] runs freely; cut short, it is interrupted. */
  val Read: ToolName = ToolName("probe_read")

  /** A tool the edge at [[Probe]] runs only once a person approves. */
  val Asks: ToolName = ToolName("probe_ask")

  val Schema: ujson.Value =
    ujson.Obj(
      "type" -> "object",
      "properties" -> ujson.Obj("path" -> ujson.Obj("type" -> "string"))
    )

  /** A request asking the model to count to `n`. */
  def request(n: Int): ModelRequest = ModelRequest("Count.", Vector(Message.User(s"to $n")))

  /** A move's name. */
  def move(n: String): MoveName = MoveName.of(n).fold(sys.error, identity)

  def limits(asks: Int, calls: Int): MoveLimits =
    MoveLimits.of(asks, calls).fold(sys.error, identity)

  /** What a move came to, as a job's reply says it: an ask's text, a call's result, or the
    * error.
    */
  def said(asked: Either[MoveError, Asked[Message.Assistant]]): String =
    asked.fold(_.toString, _.reply.blocks.collect { case AssistantBlock.Text(t) => t }.mkString)

  /** `remind` at `version`, its parameters a count, replying what `plan` makes of its count and
    * moves, within `limits`.
    */
  final class Moving(
      val version: Int,
      override val limits: MoveLimits,
      plan: (Count, Moves^) -> String
  ) extends PlainJob[Count] {
    val name = remind.name
    def write(params: Count): ujson.Value = remind.write(params)
    def read(params: ujson.Value): Either[String, Count] = remind.read(params)
    def run(run: JobRun[Count], moves: Moves^): String = plan(run.params, moves)
  }

  /** `remind` at `version`, within an ask and a call, its plan [[probes]]. */
  def probing(version: Int): Moving = new Moving(version, limits(1, 1), probes)

  /** The moves the recorded run histories make: with the count 1 it asks; with 2 it calls
    * [[Read]] at [[Probe]]; with 3 it calls, then asks about the answer. Its reply is each
    * move's result, a line each.
    */
  val probes: (Count, Moves^) -> String =
    (n, m) => {
      def call() = m.call(move("c"), Probe, Read, ujson.Obj("path" -> "a"))
      def ask(about: String) =
        said(m.ask(move("a"), Posed.Text(ModelRequest("Count.", Vector(Message.User(about))))))
      n.n match {
        case 1 => ask("to 1")
        case 2 => call().fold(_.toString, _.toString)
        case _ =>
          val called = call().fold(_.toString, _.toString)
          s"$called\n${ask(called)}"
      }
    }

  /** A run has no asker: its calls are made for its schedule's principal. */
  object NoAskers extends Askers {
    def of(turn: TurnRef)(using Tx^): Either[StoreError, Option[Principal]] = Right(None)
  }
}
