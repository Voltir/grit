package grit.act.moves

import java.io.{ByteArrayInputStream, InputStream}
import java.time.{Instant, ZoneOffset}

import grit.core.act.MovesFixtures.{
  Answering,
  Dream,
  FakeDb,
  Judged as Spent,
  Judging,
  PerChar,
  Summary
}
import grit.core.act.{Acting, ActsFor, Allowance, Dreaming, Gates, Keeping, MoveLimits, Moves}
import grit.core.classify.{Answer as Judged, Answers, Classifier, ClassifierError, Question}
import grit.core.clock.SetClock
import grit.core.document.{
  DocLabel,
  DocWeight,
  Document,
  DocumentKeeper,
  DocumentSearch,
  DocumentTerms,
  InMemoryDocuments
}
import grit.core.durable.{Flow, Flows, InMemoryDurable}
import grit.core.edge.Advert
import grit.core.edge.{
  EdgeDirectory,
  InMemoryEdges,
  Registration,
  RequestState,
  ToolRequest,
  ToolRequests
}
import grit.core.files.{
  FileArea,
  FileItem,
  FileOrigin,
  Files,
  Formats,
  InMemoryFileCatalog,
  InMemoryPieces,
  Incoming,
  KeptPiece,
  NoFiles,
  PagedText,
  PieceKeeper,
  PutKey
}
import grit.core.host.TempBlobs
import grit.core.id.{CallSlot, ConversationId, EdgeName, FileHandle, PluginName, TurnRef, TurnSeq}
import grit.core.identity.Principal
import grit.core.job.InMemorySchedules
import grit.core.message.Message
import grit.core.need.Needs
import grit.core.place.Place
import grit.core.provider.{ModelRequest, Models}
import grit.core.spend.{Budget, DailyCap, Day, Spend, Spending, TurnSpend}
import grit.core.stitch.StrandWorld
import grit.core.store.{Askers, InMemoryToolSets, InMemoryUsageLedger, Jot, StoreError, Tx}
import grit.core.tool.{Outcome, Retry, ToolSet}
import grit.core.visibility.{
  Cap,
  Clearance,
  Cleared,
  Compartments,
  Integrity,
  Label,
  Party,
  Provenance,
  Readers,
  RoomLabels,
  Subject,
  Visibility
}
import grit.dbos.sql.TestTx

/** [[MovesContract]] against [[DurableMoves]], over [[InMemoryDurable]] and core's in-memory
  * stores, with a live edge at [[MovesContract.Probe]] that answers each request as it is sent.
  */
object DurableMovesTests extends MovesContract {

  def within[A <: caps.Pure](
      limits: MoveLimits,
      broken: Boolean,
      shapes: Vector[ujson.Value],
      judgment: Either[ClassifierError, Judged],
      integrity: Integrity
  )(use: Moves^ -> A): A = {
    val w = new MovesWorld(broken, MovesContract.Floor, integrity)
    w.run(limits, shapes, new Each(judgment))(use)
  }

  /** A classifier answering `judgment` to every question it is asked, at [[Spent]]'s cost, or
    * failing with its `Left`.
    */
  private final class Each(judgment: Either[ClassifierError, Judged]) extends Classifier {
    protected def answer(
        state: ujson.Value,
        questions: Vector[Question]
    ): Either[ClassifierError, Answers] =
      judgment.map(j => Answers(questions.map(_ => j), Spent, "test/judge"))
  }

  def keeping[A <: caps.Pure](limits: MoveLimits)(use: Keeping^ -> A): (A, Vector[Document]) = {
    val w = new MovesWorld(broken = false, MovesContract.Floor)
    val got = w.keeping(limits)(use)
    val docs = w.keeper
      .newest(Int.MaxValue)(using TestTx.fake(Clearance.of(MovesContract.Floor)))
      .fold(e => throw new java.lang.AssertionError(s"$e"), identity)
    (got, docs)
  }

  def piecing[A <: caps.Pure](limits: MoveLimits, formats: Formats)(
      use: (Keeping^, FileHandle) -> A
  ): (A, Vector[KeptPiece]) = {
    val w = new MovesWorld(broken = false, MovesContract.Floor)
    TempBlobs.around { blobs =>
      val area = new FileArea(new InMemoryFileCatalog, blobs, w.jot, w.clock, formats)
      val file = area
        .put(
          PutKey.read("tool:test:0:1"),
          MovesContract.Policy,
          Some("application/pdf"),
          FileOrigin(EdgeName("drive"), "policy", Some("7"), MovesWorld.Start),
          FileItem(Provenance(MovesContract.Here, Integrity.Member), Readers.AtPlace),
          new Given(PagedText.bytes(MovesContract.Pages*))
        )(_ => ())
        .fold(e => throw new java.lang.AssertionError(e.message), _._1.handle)
      val got = w.keeping(limits, w.keeper, area)(m => use(m, file))
      (
        got,
        w.pieces.current(MovesWorld.Notes)(using TestTx.fake(Clearance.of(MovesContract.Floor)))
      )
    }
  }

  /** `bytes`, as a source gives them. */
  private final class Given(bytes: Vector[Byte]) extends Incoming {
    // Each byte as the one character ISO-8859-1 encodes back to it.
    private val latin = bytes.map(b => (b & 0xff).toChar).mkString
    def declared: Option[Long] = Some(bytes.size.toLong)
    def read[A](body: InputStream^ => A): Either[String, A] =
      Right(body(new ByteArrayInputStream(latin.getBytes("ISO-8859-1"))))
  }

  def dreaming[A <: caps.Pure](
      limits: MoveLimits,
      world: StrandWorld,
      integrity: Integrity,
      shelves: MovesContract.Shelves
  )(
      use: Dreaming^ -> A
  ): (A, Vector[(String, Vector[String])]) = {
    val w =
      new MovesWorld(broken = false, MovesContract.Floor, integrity, documents = shelves.documents)
    val (got, requests) = w.dreaming(limits, world, shelves.needs)(use)
    (got, requests.map(r => (r.system, r.messages.collect { case Message.User(t) => t })))
  }

  def declassifying[A <: caps.Pure](
      limits: MoveLimits,
      shelves: MovesContract.Shelves,
      rewrite: String,
      leaks: Vector[Vector[Double]]
  )(use: Dreaming^ -> A): (A, Vector[(String, Vector[String])]) = {
    val w = new MovesWorld(
      broken = false,
      MovesContract.Declassing,
      Integrity.Authority,
      documents = shelves.documents,
      compartments = MovesContract.Declared
    )
    val judging = new Judging(
      leaks.map(ls => Right(Answers(ls.map(Judged.YesNo(_)), Spent, "test/judge")))
    )
    val (got, requests) =
      w.answering(limits, new StrandWorld, shelves.needs, rewrite, judging, DurableMoves.Within)(
        use
      )
    (got, requests.map(r => (r.system, r.messages.collect { case Message.User(t) => t })))
  }
}

/** A world for [[DurableMoves]]: a turn whose asker is grit, a model answering
  * [[MovesContract.Answer]], a classifier answering each question of its kind
  * (`grit.core.act.MovesFixtures.Judging`), an edge at [[MovesContract.Probe]] advertising
  * [[MovesContract.Tool]] that claims, answers `answer` (done, [[MovesContract.Read]]) and rings
  * each request as it is written, and the documents of [[MovesWorld.Notes]]. Its every
  * transaction and read is opened at `floor`, and [[MovesContract.Probe]], its summary model and
  * its classifier are cleared for `floor`; what is said at its places is capped by `capped`
  * ([[MovesContract.Probe]]'s at [[MovesContract.ProbeOutput]]). When `broken`, the day's spend
  * cannot be read, and the acting's allowance is daily, so every ask's admission fails. Its run
  * starts believed at `integrity`, and its places are labelled by `rooms`.
  */
final class MovesWorld(
    broken: Boolean,
    floor: Label = Label.Public,
    integrity: Integrity = Integrity.Authority,
    capped: Vector[Cap] = Vector(Cap(MovesContract.Probe.place, MovesContract.ProbeOutput)),
    answer: Outcome = Outcome.Done(MovesContract.Read),
    rooms: RoomLabels = RoomLabels.Public,
    val documents: InMemoryDocuments = new InMemoryDocuments,
    compartments: Compartments = Compartments.Shipped
) extends caps.SharedCapability {
  import MovesContract.*
  import MovesWorld.*

  private val visibility: Visibility =
    Visibility
      .of(
        compartments,
        rooms,
        Vector.empty,
        Vector.empty,
        Vector(
          Cleared(Party.Service(Probe), floor),
          // The world's summary model is cleared for what it knows.
          Cleared(Party.of(Summary).getOrElse(sys.error("no upstream")), floor),
          Cleared(Party.of(Dream).getOrElse(sys.error("no upstream")), floor),
          // The world's classifier is cleared for what it knows.
          Cleared(Party.Service(grit.core.classify.TestClassifiers.Service), floor)
        ),
        caps = capped
      )
      .fold(r => throw new java.lang.AssertionError(r.toString), identity)
  val durable: InMemoryDurable =
    new InMemoryDurable(resolve = _ => Clearance.of(floor), visibility = visibility)
  val edges: InMemoryEdges = new InMemoryEdges
  val ledger: InMemoryUsageLedger = new InMemoryUsageLedger
  val toolSets: InMemoryToolSets = new InMemoryToolSets
  val clock: SetClock = new SetClock(Start)
  private val edge: Registration = edges.register(Set(Probe.place))

  {
    val set = ToolSet
      .of(Vector(ToolSet.Entry(Tool, "Reads a path.", Schema, asks = false, Retry.Rerun)))
      .fold(d => throw new java.lang.AssertionError(d.toString), identity)
    val _ = toolSets.keep(set)(using TestTx.fake)
    edges.advertiseAs(edge, Probe.place, set, Vector.empty)
  }

  /** `edges`, but each request is claimed, answered [[MovesContract.Read]] and its workflow
    * rung as it is written.
    */
  private object answering extends ToolRequests, EdgeDirectory {
    def dispatch(requests: Vector[ToolRequest], at: grit.core.visibility.Classification)(using
        Tx^
    ): Either[StoreError, Unit] = {
      val sent = edges.dispatch(requests, at)
      requests.foreach { q =>
        if (edges.claimAs(edge, q) && edges.answerAs(edge, q.slot, answer))
          durable.send(q.slot.turn.workflowId, q.slot.key, "rang")
      }
      sent
    }
    def settle(slot: CallSlot)(using Tx^): Either[StoreError, RequestState] = edges.settle(slot)
    def abandon(slot: CallSlot)(using Tx^): Either[StoreError, RequestState] = edges.abandon(slot)
    def answered(slot: CallSlot)(using Tx^): Either[StoreError, Option[Outcome]] =
      edges.answered(slot)
    def serving(place: Place)(using Tx^): Either[StoreError, Option[Advert]] = edges.serving(place)
  }

  private val spending: Spending = if (broken) Unreadable else ledger

  def env(models: Models^, classifier: Classifier^ = new Judging()): MovesEnv^ =
    MovesEnv(
      MoveRecords(
        ledger,
        spending,
        answering,
        answering,
        toolSets,
        new InMemorySchedules(),
        GritAsks,
        PerChar,
        documents.savepoints
      ),
      models,
      grit.core.classify.TestClassifiers.cleared(classifier),
      new FakeDb(Clearance.of(floor), visibility),
      clock
    )

  /** The acting of [[Turn]]: its asker's, closed, admitted, or daily under a cap when
    * `broken`, starting at `integrity`.
    */
  val acting: Acting = Acting(
    Turn,
    ActsFor.Asker,
    if (broken) Allowance.Daily(Budget(ZoneOffset.UTC, DailyCap.of("1").toOption))
    else Allowance.Admitted,
    Gates.Closed,
    integrity
  )

  /** What `use` returns over [[Turn]]'s moves within `limits`, its workflow run once, a JSON
    * ask's model replying each of `shapes` in turn (`Answering`'s), a judgment asked of
    * `classifier`, and its program declaring `declared`.
    */
  def run[A <: caps.Pure](
      limits: MoveLimits,
      shapes: Vector[ujson.Value] = Vector(),
      classifier: Classifier^ = new Judging(),
      declared: Vector[Flow.Node] = DurableMoves.Within
  )(use: Moves^ -> A): A = {
    val models = new Answering(Answer, shapes = shapes)
    val moving = MovesWorld.moves(acting, limits, declared)(use)
    var out: Option[A] = None
    durable.run(Turn.workflowId) { _ =>
      out = Some(Flows.run(moving, env(models, classifier), ()).merge)
      "done"
    }
    out.getOrElse(throw new java.lang.AssertionError("the workflow did not run"))
  }

  /** [[Notes]]' documents as it writes them. */
  val keeper: DocumentKeeper = documents.keeper(Notes, NotesTerms)

  /** Where [[Notes]]' piece moves keep its pieces. */
  val pieces: InMemoryPieces = new InMemoryPieces

  /** Short write transactions, each opened at `floor`. */
  val jot: Jot = new Jot {
    def write[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using TestTx.fake(Clearance.of(floor), visibility))
  }

  /** [[env]], and a keeping job's: piece moves reading `files` and writing through [[jot]]. */
  def kept(models: Models^, files: Files^ = MovesWorld.Fileless): KeepingEnv^ =
    KeepingEnv(env(models), files, jot)

  /** What `use` returns over [[Turn]]'s moves within `limits`, keeping [[Notes]]' documents
    * through `keeper` and its pieces in [[pieces]], read from `files`, its workflow run once.
    */
  def keeping[A <: caps.Pure](
      limits: MoveLimits,
      keeper: DocumentKeeper = keeper,
      files: Files^ = MovesWorld.Fileless
  )(
      use: Keeping^ -> A
  ): A = {
    val models = new Answering(Answer)
    val keeps =
      MovesWorld.keeps(acting, limits, keeper, pieces.keeper(Notes, PieceKeeper.Bound))(use)
    var out: Option[A] = None
    durable.run(Turn.workflowId) { _ =>
      out = Some(Flows.run(keeps, kept(models, files), ()).merge)
      "done"
    }
    out.getOrElse(throw new java.lang.AssertionError("the workflow did not run"))
  }

  /** As [[keeping]], its moves also scanning `world`'s strands and `needs` and summarising
    * what they said; and every request its model was sent, in order.
    */
  def dreaming[A <: caps.Pure](
      limits: MoveLimits,
      world: StrandWorld,
      needs: Needs = Needs.over(MovesWorld.Notes, Vector.empty)
  )(
      use: Dreaming^ -> A
  ): (A, Vector[ModelRequest]) =
    answering(limits, world, needs, Answer, new Judging(), DurableMoves.Within)(use)

  /** As [[dreaming]], its text asks answered `answer`, its judgments by `classifier`, and its
    * program declaring `declared`.
    */
  def answering[A <: caps.Pure](
      limits: MoveLimits,
      world: StrandWorld,
      needs: Needs,
      answer: String,
      classifier: Classifier^,
      declared: Vector[Flow.Node]
  )(
      use: Dreaming^ -> A
  ): (A, Vector[ModelRequest]) = {
    val models = new Answering(answer)
    val dreams = MovesWorld.dreams(acting, limits, keeper, world, needs, documents, declared)(use)
    var out: Option[A] = None
    durable.run(Turn.workflowId) { _ =>
      out = Some(
        Flows.run(dreams, KeepingEnv(env(models, classifier), MovesWorld.Fileless, jot), ()).merge
      )
      "done"
    }
    (out.getOrElse(throw new java.lang.AssertionError("the workflow did not run")), models.requests)
  }
}

object MovesWorld {

  /** As [[keeps]], its moves also scanning `world`'s strands and `needs` and summarising what
    * they said.
    */
  def dreams[A <: caps.Pure](
      acting: Acting,
      limits: MoveLimits,
      keeper: DocumentKeeper,
      world: StrandWorld,
      needs: Needs = Needs.over(Notes, Vector.empty),
      documents: DocumentSearch = new InMemoryDocuments,
      declared: Vector[Flow.Node] = DurableMoves.Within
  )(use: Dreaming^ -> A): Flow[KeepingEnv, Unit, Nothing, A] =
    Flow.program[KeepingEnv, Unit, A]("moves", declared)(steps =>
      _ =>
        DurableMoves.dreaming(
          acting,
          limits,
          Notes,
          keeper,
          new InMemoryPieces().keeper(Notes, PieceKeeper.Bound),
          Dreaming.Reads(world.shelf, _ => needs, world.excerpts, documents)
        )(use)(using steps)
    )

  /** What `use` returns over the moves of `acting` within `limits`, as a job's program makes
    * them, declaring `declared`.
    */
  def moves[A <: caps.Pure](
      acting: Acting,
      limits: MoveLimits,
      declared: Vector[Flow.Node] = DurableMoves.Within
  )(use: Moves^ -> A): Flow[MovesEnv, Unit, Nothing, A] =
    Flow.program[MovesEnv, Unit, A]("moves", declared)(steps =>
      _ => DurableMoves.plain[MovesEnv, A](acting, limits, env => env)(use)(using steps)
    )

  /** An area that keeps no file. */
  object Fileless extends NoFiles

  /** As [[moves]], its moves also keeping `keeper`'s documents and `pieces`' pieces. */
  def keeps[A <: caps.Pure](
      acting: Acting,
      limits: MoveLimits,
      keeper: DocumentKeeper,
      pieces: PieceKeeper = new InMemoryPieces().keeper(Notes, PieceKeeper.Bound)
  )(
      use: Keeping^ -> A
  ): Flow[KeepingEnv, Unit, Nothing, A] =
    Flow.program[KeepingEnv, Unit, A]("moves", DurableMoves.Within)(steps =>
      _ => DurableMoves.keeping(acting, limits, keeper, pieces)(use)(using steps)
    )

  val Start: Instant = Instant.parse("2026-10-08T12:00:00Z")

  val Turn: TurnRef = TurnRef(ConversationId("c"), TurnSeq.First)

  /** The plugin whose documents a keep writes. */
  val Notes: PluginName =
    PluginName.of("notes").fold(e => throw new java.lang.AssertionError(e), identity)

  val NotesTerms: DocumentTerms =
    DocLabel
      .of("notes")
      .flatMap(
        DocumentTerms.of(_, DocWeight.Unscaled, scala.concurrent.duration.Duration(1, "day"), 10)
      )
      .fold(e => throw new java.lang.AssertionError(e), identity)

  val Schema: ujson.Value =
    ujson.Obj(
      "type" -> "object",
      "properties" -> ujson.Obj("path" -> ujson.Obj("type" -> "string"))
    )

  /** Every turn's asker is grit. */
  object GritAsks extends Askers {
    def of(turn: TurnRef)(using Tx^): Either[StoreError, Option[Principal]] =
      Right(Some(Principal.Grit))
  }

  object Unreadable extends Spending {
    def on(day: Day)(using Tx^): Either[StoreError, Spend] = Left(StoreError.DatabaseError("down"))
    def conversation(id: ConversationId)(using Tx^): Either[StoreError, Spend] =
      Left(StoreError.DatabaseError("down"))
    def turn(turn: TurnRef)(using Tx^): Either[StoreError, TurnSpend] =
      Left(StoreError.DatabaseError("down"))
  }
}
