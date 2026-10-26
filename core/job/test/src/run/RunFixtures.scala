package grit.job.run

import java.time.{Instant, ZoneOffset}

import scala.concurrent.duration.*

import grit.act.moves.{MoveRecords, MovesEnv}
import grit.core.act.MovesFixtures.{Answering, FakeDb, Judging, PerChar, replied, shown}
import grit.core.act.{Asked, Dreaming, Keeping, MoveError, MoveLimits, MoveName, Moves, Posed}
import grit.core.classify.{Answers, Ask, ClassifierError, Criterion, Decision, StateJson}
import grit.core.clock.SetClock
import grit.core.document.{
  DocLabel,
  DocText,
  DocWeight,
  DocumentKeeper,
  DocumentTerms,
  InMemoryDocuments,
  Written
}
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
import grit.core.files.{InMemoryPieces, NoFiles, PieceKeeper}
import grit.core.flow.{Tainted, Trusted}
import grit.core.id.{
  CallSlot,
  ConversationId,
  Declarer,
  DocKey,
  DocumentVersionId,
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
import grit.core.job.{
  Declared,
  DreamingJob,
  JobRun,
  Jobs,
  KeepingJob,
  Owned,
  PlainJob,
  Schedule,
  SlotRule,
  When
}
import grit.core.message.{AssistantBlock, Message, Tokens, Usage}
import grit.core.model.Grade
import grit.core.need.Needs
import grit.core.place.{Namespace, Place, Service}
import grit.core.provider.ModelRequest
import grit.core.schema.{JsonSchema, Typed}
import grit.core.spend.{Budget, DailyCap}
import grit.core.stitch.{Strand, StrandWorld}
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
import grit.core.visibility.Integrity
import grit.core.visibility.{
  Clearance,
  Cleared,
  Compartments,
  Label,
  Party,
  RoomLabels,
  Subject,
  Visibility
}
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
    def run(run: Trusted[JobRun[Count]], moves: Keeping^): String =
      moves
        .keep(move("note"), run.tainted) { (_, keeper, at) =>
          (for {
            key <- DocKey.of("count")
            text <- DocText.of(s"counted ${run.extract.params.n}")
          } yield (key, text)) match {
            case Left(why) => Left(StoreError.Invalid(why))
            case Right((key, text)) =>
              keeper
                .write(
                  key,
                  Label.Public,
                  Integrity.Member,
                  Place.under(Namespace.Task, Vector("notes")),
                  text,
                  ujson.Obj(),
                  None,
                  at
                )
                .map(w => Noted(Label.written(w.kept)))
          }
        }
        .fold(_.toString, _.label)
  }

  /** The deployment of `job`, [[Notes]]' dreaming job. */
  def dreamingJobs(job: DreamingJob[?]): Jobs =
    Jobs
      .of(Vector(Owned.Dreams(Notes, NotesTerms, job)))
      .fold(n => sys.error(s"two jobs named $n"), identity)

  /** `remind` at `version` as [[Notes]]' dreaming job: its run scans, in its keep `scan`, the
    * strands spoken in before it started; summarises the first, as [[Summarised]] asks it, in
    * its ask `summary`; and keeps the answer under the key `dreamt`, publicly, in its keep
    * `dreamt`; it replies the label kept at, or why it stopped.
    */
  final class Dreamer(val version: Int) extends DreamingJob[Count] {
    val name = remind.name
    def write(params: Count): ujson.Value = remind.write(params)
    def read(params: ujson.Value): Either[String, Count] = remind.read(params)
    override def limits: MoveLimits = MoveLimits.of(1, 0).fold(sys.error, identity)
    def run(run: Trusted[JobRun[Count]], moves: Dreaming^): String =
      moves
        .scan(move("scan"), run.tainted)((_, _, strands, _, at) =>
          strands.touched(Instant.EPOCH, at).map(Scanned(_))
        )
        .left
        .map(_.toString)
        .flatMap(_.seen.headOption.toRight("no strand"))
        .flatMap(seen =>
          moves
            .summarise(
              move("summary"),
              Vector(seen),
              Vector.empty,
              run.tainted.map(_ => Summarised)
            )
            .left
            .map(_.toString)
            .flatMap(_.toRight("its strand changed"))
        )
        .flatMap(summarised => dreamt(moves, summarised.asked.reply))
        .fold(identity, _.label)
  }

  /** `remind` at `version` as [[Notes]]' dreaming job over many sources: its run, in its keep
    * `gather`, keeps a note under the key `seen` and reads the strands spoken in before it
    * started; summarises them all with that note, as [[Summarised]] asks it, in its ask
    * `summary`; and keeps the answer as [[Dreamer]] does; it replies the label kept at and how
    * many documents the summary was shown, or why it stopped.
    */
  final class Gatherer(val version: Int) extends DreamingJob[Count] {
    val name = remind.name
    def write(params: Count): ujson.Value = remind.write(params)
    def read(params: ujson.Value): Either[String, Count] = remind.read(params)
    override def limits: MoveLimits = MoveLimits.of(1, 0).fold(sys.error, identity)
    def run(run: Trusted[JobRun[Count]], moves: Dreaming^): String =
      moves
        .scan(move("gather"), run.tainted)((_, keeper, strands, _, at) =>
          for {
            note <- noted(keeper, "seen", "the deploy is on hold", at)
            seen <- strands.touched(Instant.EPOCH, at)
          } yield Gathered(seen, Vector(note))
        )
        .left
        .map(_.toString)
        .flatMap(gathered =>
          moves
            .summarise(
              move("summary"),
              gathered.seen,
              gathered.shown,
              run.tainted.map(_ => Summarised)
            )
            .left
            .map(_.toString)
            .flatMap(_.toRight("a strand changed"))
        )
        .flatMap(summarised =>
          dreamt(moves, summarised.asked.reply)
            .map(n => s"${n.label} shown ${summarised.shown.size}")
        )
        .merge
  }

  /** `text` kept under `key` at [[Notes]]' place, publicly, believed a member's: its version. */
  private def noted(keeper: DocumentKeeper, key: String, text: String, at: Instant)(using
      Tx^
  ): Either[StoreError, DocumentVersionId] =
    (for {
      k <- DocKey.of(key)
      t <- DocText.of(text)
    } yield (k, t)) match {
      case Left(why) => Left(StoreError.Invalid(why))
      case Right((k, t)) =>
        keeper
          .write(k, Label.Public, Integrity.Member, NotesPlace, t, ujson.Obj(), None, at)
          .map {
            case Written.Versioned(v, _, _) => v
            case Written.Unchanged(v, _) => v
          }
    }

  /** Where [[Notes]]' documents are kept. */
  private val NotesPlace: Place = Place.under(Namespace.Task, Vector("notes"))

  /** `answer` kept under the key `dreamt` in the keep `dreamt` of `moves`: the label kept at, or
    * why not.
    */
  private def dreamt(moves: Dreaming^, answer: Tainted[String]): Either[String, Noted] =
    moves
      .keep(move("dreamt"), answer) { (answer, keeper, at) =>
        (for {
          key <- DocKey.of("dreamt")
          text <- DocText.of(answer)
        } yield (key, text)) match {
          case Left(why) => Left(StoreError.Invalid(why))
          case Right((key, text)) =>
            keeper
              .write(key, Label.Public, Integrity.Member, NotesPlace, text, ujson.Obj(), None, at)
              .map(w => Noted(Label.written(w.kept)))
        }
      }
      .left
      .map(_.toString)

  /** What [[Gatherer]]'s scan gathered, as it records them: the strands as
    * [[Strand.Digested.write]] writes them, and the versions to show.
    */
  final case class Gathered(seen: Vector[Strand.Digested], shown: Vector[DocumentVersionId])
      extends caps.Pure
  object Gathered {
    given Journaled[Gathered] = Journaled.json(
      g =>
        ujson.Obj(
          "seen" -> ujson.Arr.from(g.seen.map(Strand.Digested.write)),
          "shown" -> ujson.Arr.from(
            g.shown.map(v => ujson.Num(DocumentVersionId.value(v).toDouble))
          )
        ),
      v =>
        for {
          o <- v.objOpt.toRight("not an object")
          seen <- o
            .get("seen")
            .flatMap(_.arrOpt)
            .toRight("no strands")
            .flatMap(
              _.toVector.foldLeft[Either[String, Vector[Strand.Digested]]](Right(Vector())) {
                (acc, x) => acc.flatMap(done => Strand.Digested.read(x).map(done :+ _))
              }
            )
          shown <- o
            .get("shown")
            .flatMap(_.arrOpt)
            .toRight("no versions")
            .flatMap(
              _.toVector.foldLeft[Either[String, Vector[DocumentVersionId]]](Right(Vector())) {
                (acc, x) =>
                  acc.flatMap(done =>
                    x.numOpt
                      .flatMap(n => DocumentVersionId.of(n.toLong))
                      .toRight("no version")
                      .map(done :+ _)
                  )
              }
            )
        } yield Gathered(seen, shown)
    )
  }

  /** What [[Dreamer]] asks of a strand: its answer, within 1,000 characters. */
  def Summarised: Dreaming.Summary[String] = Dreaming.Summary("Summarise.", 1000, 1000, Answered)

  /** The strands a scan read, as it records them ([[Strand.Digested.write]]). */
  final case class Scanned(seen: Vector[Strand.Digested]) extends caps.Pure
  object Scanned {
    given Journaled[Scanned] = Journaled.json(
      s => ujson.Arr.from(s.seen.map(Strand.Digested.write)),
      v =>
        v.arrOpt
          .toRight("no strands")
          .flatMap(_.toVector.foldLeft[Either[String, Vector[Strand.Digested]]](Right(Vector())) {
            (acc, x) => acc.flatMap(done => Strand.Digested.read(x).map(done :+ _))
          })
          .map(Scanned(_))
    )
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
    def run(run: Trusted[JobRun[Text]], moves: Moves^): String = run.extract.params.text
  }

  final case class Text(text: String) extends caps.Pure

  /** Writes as its transaction is committed, nothing rolled back; with `crashAfter`, the
    * process dies once just after the first write commits.
    */
  final class FakeJot(crashAfter: Boolean = false) extends Jot {
    // Cleared by the first write that dies, on the test's one thread; nothing else reads it.
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
    * `crashModel`, inside the first model call. Required to call a tool, the model calls it
    * `calling` times, each with the next of `shapes` ([[Answering]]). Its classifier answers with the next of `judged`, or
    * each question of its kind when there are none ([[Judging]]). Its model's replies report
    * `upstream` as who served them.
    */
  final class World(
      cap: Option[String] = None,
      floor: Label = Label.Public,
      trust: Label = Label.Public,
      crashRecord: Boolean = false,
      failRecord: Boolean = false,
      crashServing: Boolean = false,
      crashModel: Boolean = false,
      shapes: Vector[ujson.Value] = Vector(),
      judged: Vector[Either[ClassifierError, Answers]] = Vector(),
      calling: Int = 1,
      upstream: Option[String] = None
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
          Vector(Cleared(Party.Service(Probe), trust))
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

    /** Where its keeping jobs' piece moves keep pieces. */
    val pieces: InMemoryPieces = new InMemoryPieces

    /** Its rooms' strands, which a dreaming job reads. */
    val strands: StrandWorld = new StrandWorld(Due.minusSeconds(3600))

    /** A strand of two threads in `room`, said an hour before [[Due]]: "the deploy is stuck",
      * then "still stuck?" a minute later.
      */
    def strand(room: String = "C1"): Unit = {
      val a = strands.thread(room, "1.0")
      val b = strands.thread(room, "2.0")
      val _ = strands.say(a, "the deploy is stuck", 0)
      strands.follow(strands.say(b, "still stuck?", 60), a)
    }

    /** [[Notes]]' terms in force, so a summary reads its documents. */
    def declareNotes(): Unit = {
      val _ = documents.declare(Vector(Notes -> NotesTerms))(using TestTx.fake)
    }
    val models: Answering = new Answering(Answer, crashModel, shapes, calling, upstream)
    val classifier: Judging = new Judging(judged)

    private val switches = new Switches(crashRecord, crashServing)
    private val requests = new AnsweringEdges(edges, durable, switches)
    private val recording = new RecordingLedger(ledger, switches, failRecord)

    /** An edge serving [[Probe]], advertising [[Read]] (free, interrupted if cut short) and
      * [[Asks]] (asking first), and, when `guarded`, [[Guarded]] (requiring `Member`, done as
      * "read guarded paths"); when `answers`, it claims and answers each request
      * `Done("read")` and rings its run as it is written.
      */
    def serve(answers: Boolean = true, guarded: Boolean = false): Unit = {
      val edge = edges.register(Set(Probe.place), PrincipalId.Grit)
      val guard = ToolSet.Entry(
        Guarded,
        "Reads a guarded path.",
        Schema,
        false,
        Retry.Rerun,
        None,
        Integrity.Member,
        Some("read guarded paths")
      )
      val set = ToolSet
        .of(
          Vector(
            ToolSet.Entry(Read, "Reads a path.", Schema, asks = false, Retry.Interrupt),
            ToolSet.Entry(Asks, "Asks first.", Schema, asks = true, Retry.Rerun)
          ) ++ Option.when(guarded)(guard)
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
          grit.core.classify.TestClassifiers.cleared(classifier),
          db,
          clock
        ),
        new NoFiles,
        Budget(ZoneOffset.UTC, cap.flatMap(DailyCap.of(_).toOption)),
        (plugin, terms) => documents.keeper(plugin, terms),
        plugin => pieces.keeper(plugin, PieceKeeper.Bound),
        Dreaming.Reads(
          strands.shelf,
          plugin => Needs.over(plugin, Vector.empty),
          strands.excerpts,
          documents,
          pieces
        )
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
      * reply was posted at `address`, by a turn `believed` at that, due at [[Due]].
      */
    def asked(
        thread: String,
        address: String,
        n: Int = 1,
        believed: Integrity = Integrity.Member
    ): ScheduleId = {
      val origin = Origin.Slack("T1", "C1", thread)
      val turn = TurnRef(ConversationId(thread), TurnSeq.First)
      inbox.schedules.asking(
        turn,
        origin,
        TestAccounts.account("test:ann"),
        Some(address),
        believed = believed
      )
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
    def dispatch(sent: Vector[ToolRequest], at: grit.core.visibility.Classification)(using
        Tx^
    ): Either[StoreError, Unit] = {
      val written = edges.dispatch(sent, at)
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
        upstream: Option[String],
        grade: Option[Grade],
        usage: Usage,
        estimatedInput: Tokens
    )(using Tx^): Either[StoreError, Unit] =
      if (switches.recordArmed) { switches.recordArmed = false; throw new InMemoryDurable.Crash }
      else if (failing) Left(StoreError.DatabaseError("the ledger is down"))
      else ledger.record(entry, turn, workflow, model, upstream, grade, usage, estimatedInput)
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
  val Guarded: ToolName = ToolName("probe_guarded")

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
    asked.fold(_.toString, replied(_).blocks.collect { case AssistantBlock.Text(t) => t }.mkString)

  /** `remind` at `version`, its parameters a count, replying what `plan` makes of its count,
    * believed as its run is, and moves, within `limits`.
    */
  final class Moving(
      val version: Int,
      override val limits: MoveLimits,
      plan: (Tainted[Count], Moves^) -> String
  ) extends PlainJob[Count] {
    val name = remind.name
    def write(params: Count): ujson.Value = remind.write(params)
    def read(params: ujson.Value): Either[String, Count] = remind.read(params)
    def run(run: Trusted[JobRun[Count]], moves: Moves^): String =
      plan(run.tainted.map(_.params), moves)
  }

  /** `remind` at `version`, within an ask and a call, its plan [[probes]], telling `seen` what
    * it replied.
    */
  def probing(version: Int, seen: Seen = new Seen): Moving =
    new Moving(
      version,
      limits(1, 1),
      (n, m) => {
        val r = probes(n, m)
        seen.reply = Some(r)
        r
      }
    )

  /** What a job replied, each time it ran. */
  final class Seen {
    // caps.unsafe: it holds an immutable value; each is one test's own, written by its job on
    // the run's one thread and read only after the run has returned.
    @caps.unsafe.untrackedCaptures
    var reply: Option[String] = None
  }

  /** `{"answer": string}`, read as the answer. */
  val Answered: Typed[String] = Typed(
    schema(ujson.Obj("answer" -> ujson.Obj("type" -> "string"))),
    c => c.json.objOpt.flatMap(_.get("answer")).flatMap(_.strOpt).toRight("no answer")
  )

  /** A schema of `properties`, each required. */
  def schema(properties: ujson.Obj): JsonSchema =
    JsonSchema
      .read(
        ujson.Obj(
          "type" -> "object",
          "properties" -> properties,
          "required" -> ujson.Arr.from(properties.value.keys.map(ujson.Str(_))),
          "additionalProperties" -> false
        )
      )
      .fold(e => sys.error(e.message), identity)

  /** Whether a text is urgent, and which team owns it, asked together. */
  val Weighed: Ask[String, (Double, Decision[String])] = {
    val owner = Ask
      .choice[String, String](
        "Which team owns `text`?",
        Criterion("build", "build", None),
        Criterion("docs", "docs", None)
      )
      .fold(d => sys.error(d.toString), identity)
    Ask.yesNo[String]("Is `text` urgent?", None, None).zip(owner)
  }

  /** How a judgment's state, a text, is sent: `{"text"}`. */
  given texts: StateJson[String] = StateJson.instance(s => ujson.Obj("text" -> s))

  /** The moves the recorded run histories make: with the count 1 it asks; with 2 it calls
    * [[Read]] at [[Probe]]; with 3 it calls, then asks about the answer; with 4 it asks for
    * [[Answered]]; with 5 it judges [[Weighed]] of a text. Its reply is each move's result, a
    * line each.
    */
  val probes: (Tainted[Count], Moves^) -> String =
    (n, m) => {
      def call() = m.call(move("c"), Probe, Read, n.map(_ => ujson.Obj("path" -> "a")))
      def ask(about: String) =
        said(
          m.ask(
            move("a"),
            n.map(_ => Posed.Text(ModelRequest("Count.", Vector(Message.User(about)))))
          )
        )
      Trusted.asIs(n).extract.n match {
        case 1 => ask("to 1")
        case 2 => call().fold(_.toString, shown)
        case 4 =>
          m.ask(
            move("a"),
            n.map(_ => Posed.Json("Count.", Vector(Message.User("to 4")), Answered))
          ).fold(_.toString, replied)
        case 5 =>
          m.ask(move("j"), n.map(_ => Posed.judge("the build is red", Weighed)))
            .fold(_.toString, a => s"${replied(a)._1} ${replied(a)._2.choice}")
        case _ =>
          val called = call().fold(_.toString, shown)
          s"$called\n${ask(called)}"
      }
    }

  /** A run has no asker: its calls are made for its schedule's principal. */
  object NoAskers extends Askers {
    def of(turn: TurnRef)(using Tx^): Either[StoreError, Option[Principal]] = Right(None)
  }
}
