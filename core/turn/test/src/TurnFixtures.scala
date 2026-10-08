package grit.turn

import java.time.Instant

import scala.concurrent.duration.FiniteDuration

import grit.assembly.estimate.CharEstimate
import grit.assembly.linear.LinearAssembler
import grit.core.classify.Classifier
import grit.core.classify.{Answer, Answers, ClassifierError, Question}
import grit.core.clock.{Clock, Fresh}
import grit.core.context.{AssemblyError, AssemblyNote, AssemblyRequest, ContextAssembler, Window}
import grit.core.document.{DocumentStore, InMemoryDocuments}
import grit.core.durable.{Durable, InMemoryDurable}
import grit.core.edge.InMemoryDeliveries
import grit.core.edge.{InMemoryEdges, Registration, ToolRequest}
import grit.core.host.{
  Clipped,
  EditError,
  Edited,
  Edits,
  HostError,
  Lines,
  Ran,
  RelPath,
  Replace,
  Shell,
  Workspace
}
import grit.core.id.{CallSlot, ConversationId, EntryId, ToolCallId, TurnRef, WorkflowId}
import grit.core.id.{EntrySeq, PeriodSeq, TurnSeq}
import grit.core.identity.Account
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.{Assignment, Catalog, ModelId, ModelRef, Pinned, Policy}
import grit.core.period.{CloseReason, Probability, TestClosings}
import grit.core.place.{Directory, Place, Reaches, WorksIn}
import grit.core.prompt.{Fragment, SystemPrompt, Voice}
import grit.core.provider.{Delta, ModelRequest, Models, Provider, ProviderError}
import grit.core.speech.{Decision, Heard, InMemorySpeechStore, Limits, Reach}
import grit.core.spend.DailyCap
import grit.core.stitch.{InMemoryStitchStore, Tuning}
import grit.core.store.{
  Db,
  Entry,
  EntryStore,
  InMemoryConversationStore,
  InMemoryEntryStore,
  InMemoryModelProfileStore,
  InMemoryPeriodStore,
  InMemoryPrincipals,
  InMemoryPromptStore,
  InMemoryToolSets,
  InMemoryUsageLedger,
  InMemoryVoiceStore,
  Jot,
  ModelProfileStore,
  Origin,
  Payload,
  PeriodStore,
  Principals,
  StoreError,
  Tx,
  UsageLedger,
  VoiceStore
}
import grit.core.tool.{
  Args,
  Field,
  Gate,
  Hosted,
  Outcome,
  Retry,
  Tool,
  ToolName,
  ToolSet,
  ToolSpec,
  Toolbox
}
import grit.core.triage.{Kind, Tags}
import grit.core.visibility.{
  Compartment,
  Compartments,
  Label,
  Labelled,
  Labeller,
  Subject,
  TestLabels,
  Trust,
  Visibility
}
import grit.dbos.sql.TestTx
import grit.models.StubProvider

/** The turn's test world: the stub provider, the linear assembler, and core's in-memory
  * store and durability fakes. Shared by the turn tests, the replay gate and the recorder.
  */
object TurnFixtures {

  val conversation = ConversationId("c1")

  /** The directory the fixture conversation is a TUI session in. */
  val checkout: Directory =
    Directory.of("/checkout").fold(e => throw new java.lang.AssertionError(e), identity)

  /** The fixture conversation's origin: its id is `c1` in a fresh [[hosting]]. */
  val origin: Origin = Origin.Tui(checkout, "test")

  /** The compartments [[labelling]] declares. */
  val trial: Compartment = TestLabels.compartment("trial")
  val finance: Compartment = TestLabels.compartment("finance")

  /** A deployment's visibility declaring [[trial]] and [[finance]]: each of `places` labelled
    * as given, any other place unplaced, and each service trusted as `trusts` says.
    */
  def labelling(
      places: Vector[(Place, Label)],
      trusts: Vector[Trust] = Vector.empty
  ): Visibility = {
    val rooms: Labeller[Place] = new Labeller[Place] {
      def label(item: Place): Labelled =
        places
          .collectFirst { case (p, l) if p == item => Labelled.Mapped(l) }
          .getOrElse(Labelled.Unmapped(Label.Public))
      def requires: Vector[Compartment] = Vector(trial, finance)
    }
    (for {
      compartments <- Compartments.of(Vector(trial, finance)).left.map(_.toString)
      v <- Visibility
        .of(compartments, rooms, Vector.empty, Vector.empty, trusts)
        .left
        .map(_.toString)
    } yield v).fold(e => throw new java.lang.AssertionError(e), identity)
  }

  /** What a fixture turn is offered when no edge serves its directory: its conversations,
    * the fixture one created first, and in-memory prompts, tool sets, `edges` and `voices`
    * (none set: the default).
    */
  def hosting(
      edges: InMemoryEdges = new InMemoryEdges,
      voices: VoiceStore = new InMemoryVoiceStore
  ): TurnHosting = {
    val conversations = new InMemoryConversationStore
    conversations.findOrCreate(origin, Account.Local, Label.Public)(using TestTx.fake)
    TurnHosting(conversations, Prompts, ToolSets, edges, edges, voices)
  }

  /** A search that finds nothing: what a turn whose conversation is never stitched reads. */
  object NoSearch extends grit.core.store.EntrySearch {
    def search(
        conversation: ConversationId,
        from: TurnSeq,
        before: TurnSeq,
        query: String,
        limit: Int
    )(using Tx^): Either[StoreError, Vector[grit.core.store.EntrySearch.Hit]] = Right(Vector.empty)
    def nearby(open: Vector[grit.core.store.OpenPeriod], query: String, limit: Int)(using
        Tx^
    ): Either[StoreError, Vector[grit.core.store.EntrySearch.Hit]] = Right(Vector.empty)
    def closings(conversations: Vector[ConversationId], query: String, limit: Int)(using
        Tx^
    ): Either[StoreError, Vector[grit.core.store.EntrySearch.Hit]] = Right(Vector.empty)
    def room(
        room: grit.core.place.Place,
        from: Instant,
        until: Instant,
        query: String,
        limit: Int
    )(using Tx^): Either[StoreError, Vector[grit.core.store.EntrySearch.Hit]] = Right(Vector.empty)
  }

  /** Stitching over stores of its own: what a fixture turn at [[origin]], a TUI session, which
    * is never stitched, is given.
    */
  def unstitched(): TurnStitching^ =
    TurnStitching(
      new grit.core.stitch.InMemoryStitchStore(new InMemoryEntryStore, _ => origin),
      NoSearch,
      new grit.core.store.InMemoryLifecycleStore,
      grit.core.stitch.Tuning.Default,
      new Unplaced
    )

  /** No triage kept: every heard root is weighed as unanswered; a message said to grit, when
    * asked about, is not weighed.
    */
  def noTriage(): TurnWeighing^ =
    TurnWeighing(
      new grit.core.triage.InMemoryTriageStore(new InMemoryEntryStore, NoPeriods),
      new Weighs(Left(grit.core.triage.Weighing.Unweighed.Unavailable))
    )

  /** How many times a [[Weighs]] was asked. */
  final class Calls {
    // Read only by the test that owns it.
    @caps.unsafe.untrackedCaptures
    var n = 0
  }

  /** A [[grit.core.triage.Weighing]] that answers every message with `outcome`, as
    * lifecycle's `Mentions` answers one it asked about or could not, counting each call in
    * `calls`.
    */
  final class Weighs(
      outcome: Either[grit.core.triage.Weighing.Unweighed, grit.core.triage.Weighing.Weighed],
      calls: Calls = new Calls
  ) extends grit.core.triage.Weighing {
    def weigh(
        turn: TurnRef
    ): Either[grit.core.triage.Weighing.Unweighed, grit.core.triage.Weighing.Weighed] = {
      calls.n += 1
      outcome
    }
  }

  /** Placements whose every placement ends having asked nothing. */
  final class Unplaced extends grit.core.stitch.Placements {
    def awaited(opening: grit.core.stitch.Opening): Either[String, String] =
      Right("nothing asked")
    def awaitedWithin(
        opening: grit.core.stitch.Opening,
        within: FiniteDuration,
        clock: Clock^
    ): Either[grit.core.stitch.Placements.Unplaced, String] =
      awaited(opening).left.map(grit.core.stitch.Placements.Unplaced.Failed(_))
  }

  /** Every fixture turn's prompts and tool sets, kept by content id as the real stores keep
    * them, forever: shared, so a turn run again over another world reads back what its first
    * run kept.
    */
  val Prompts: InMemoryPromptStore = new InMemoryPromptStore

  val ToolSets: InMemoryToolSets = new InMemoryToolSets

  /** The system prompt a fixture turn is sent when no edge serves its directory, in the
    * default voice.
    */
  val system: String =
    SystemPrompt
      .of(
        Vector(
          TurnPrompt.Base,
          TurnPrompt.Candour,
          TurnPrompt.Answering,
          TurnPrompt.edge(origin),
          TurnPrompt.room(origin, grit.core.visibility.Label.Public),
          TurnPrompt.reach(Some(Place.of(checkout)), TurnPrompt.Serving.Unserved)
        ) ++
          Voice.fragment(Voice.Default)
      )
      .render

  /** The stub provider, keeping every request it was sent. */
  final class RecordingProvider(fail: Boolean = false) extends Provider {
    @caps.unsafe.untrackedCaptures
    var requests = Vector.empty[ModelRequest]

    def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] = {
      requests = requests :+ request
      if (fail) Left(ProviderError.Unavailable("down")) else new StubProvider().complete(request)
    }
  }

  /** The system's time, with waits that return at once, each kept in `waited`. */
  final class NoWait extends Clock {
    @caps.unsafe.untrackedCaptures
    var waited = Vector.empty[scala.concurrent.duration.FiniteDuration]

    def now(): Instant = Instant.now()
    def millis(): Long = System.nanoTime() / 1000000
    def sleep(duration: scala.concurrent.duration.FiniteDuration): Unit = waited =
      waited :+ duration
  }

  /** The stub, streaming; its first `failures` calls each tell a piece of text and then fail
    * with `error`, as an upstream that drops the stream does.
    */
  final class Flaky(failures: Int, error: ProviderError) extends Provider {
    @caps.unsafe.untrackedCaptures
    var calls = 0

    def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] =
      new StubProvider().complete(request)

    override def stream(
        request: ModelRequest,
        onDelta: Delta => Unit
    ): Either[ProviderError, Message.Assistant] = {
      calls += 1
      if (calls > failures) new StubProvider().stream(request, onDelta)
      else { onDelta(Delta.Text("half a rep")); Left(error) }
    }
  }

  /** A provider answering each call with `script(request, call)`, calls counted from 0,
    * keeping every request.
    */
  final class Scripted(script: (ModelRequest, Int) -> Either[ProviderError, Message.Assistant])
      extends Provider {
    @caps.unsafe.untrackedCaptures
    var requests = Vector.empty[ModelRequest]

    def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] = {
      val n = requests.size
      requests = requests :+ request
      script(request, n)
    }
  }

  /** A reply that ends its turn: `text` (no text block when empty), then `calls`; 10 tokens
    * in, 2 out, costing 0.001.
    */
  def said(text: String, calls: AssistantBlock*): Message.Assistant =
    Message.Assistant(
      Vector(AssistantBlock.Text(text)).filter(_ => text.nonEmpty) ++ calls,
      StopReason.EndTurn,
      Usage(Tokens(10), Tokens(2), Tokens.Zero, Some(BigDecimal("0.001"))),
      "m"
    )

  /** A call to the `topic` tool, saying the message is `about` that. */
  def topicCall(about: String): AssistantBlock.ToolCall =
    AssistantBlock.ToolCall(
      ToolCallId("t1"),
      grit.core.tool.ToolName.value(TurnVerdict.Name),
      ujson.Obj("about" -> about)
    )

  /** Entries a crash is aimed at: the reply's insert. */
  val isReply: Entry -> Boolean = _.payload match {
    case Payload.Message(Message.Assistant(_, _, _, _, _)) => true
    case _ => false
  }

  /** The stub, noting as it is called whether `entries` already hold a window record. */
  final class Peeking(entries: EntryStore) extends Provider {
    @caps.unsafe.untrackedCaptures
    var sawWindow: Vector[Boolean] = Vector.empty

    def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] = {
      sawWindow = sawWindow :+ windows(entries).nonEmpty
      new StubProvider().complete(request)
    }
  }

  /** The stub, streaming; its first call tells `before` pieces of its reply and then dies,
    * as a crashed process would, mid-stream.
    */
  final class DiesMidStream(before: Int) extends Provider {
    @caps.unsafe.untrackedCaptures
    var calls = 0

    def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] =
      new StubProvider().complete(request)

    override def stream(
        request: ModelRequest,
        onDelta: Delta => Unit
    ): Either[ProviderError, Message.Assistant] = {
      calls += 1
      var told = 0
      new StubProvider().stream(
        request,
        d => {
          if (calls == 1 && told == before) throw new InMemoryDurable.Crash
          told += 1
          onDelta(d)
        }
      )
    }
  }

  /** What a reader hears of `turn`'s reply stream in `durable`. */
  def heard(
      durable: InMemoryDurable,
      turn: TurnRef
  ): (Vector[TurnStream.Piece], TurnStream.Heard) = {
    val pieces =
      durable.streamed(turn.workflowId, TurnStream.Key).flatMap(TurnStream.decode(_).toOption)
    (pieces, pieces.foldLeft(TurnStream.Heard.nothing)(_ + _))
  }

  /** Reads straight through to the in-memory store. */
  object FakeDb extends Db {
    def read[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using TestTx.fake)
  }

  /** Writes straight through to the in-memory store, never rolled back. */
  final class FakeJot extends Jot {
    def write[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using TestTx.fake)
  }

  /** A checkout with nothing in it: every read fails as not found. */
  object NoCheckout extends Workspace {
    def read(path: RelPath, lines: Lines): Either[HostError, Clipped] =
      Left(HostError.NotFound(path))
    def list(dir: RelPath, depth: Int): Either[HostError, Clipped] = Left(HostError.NotFound(dir))
    def search(pattern: String, under: RelPath): Either[HostError, Clipped] =
      Left(HostError.NotFound(under))
  }

  /** Edits that change nothing: every write and edit fails. */
  object NoEdits extends Edits {
    def write(path: RelPath, text: String): Either[HostError, Unit] =
      Left(HostError.Failed("no edits"))
    def edit(path: RelPath, edits: Seq[Replace]): Either[EditError, Edited] =
      Left(EditError.Host(HostError.Failed("no edits")))
  }

  /** A shell that runs nothing: every command fails to start. */
  object NoShell extends Shell {
    def run(
        command: String,
        timeout: scala.concurrent.duration.FiniteDuration
    ): Either[HostError, Ran] =
      Left(HostError.Failed("no shell"))
  }

  /** A checkout of `files`, by path, counting the reads made of it. It lists and searches
    * nothing.
    */
  final class Files(files: Map[String, String]) extends Workspace {
    @caps.unsafe.untrackedCaptures
    var reads = 0

    def read(path: RelPath, lines: Lines): Either[HostError, Clipped] = {
      reads += 1
      files
        .get(RelPath.value(path))
        .map(Clipped.head(_, _ => None))
        .toRight(HostError.NotFound(path))
    }
    def list(dir: RelPath, depth: Int): Either[HostError, Clipped] =
      Left(HostError.Failed("no listing"))
    def search(pattern: String, under: RelPath): Either[HostError, Clipped] =
      Left(HostError.Failed("no search"))
  }

  private val path: Args[String] = Args.of((path = Field.text("The file."))).map(_.path)

  private def reading(ws: Workspace^, p: String): Outcome =
    RelPath.of(p) match {
      case Left(error) => Outcome.Failed(error.message)
      case Right(at) =>
        ws.read(at, Lines.All).fold(e => Outcome.Failed(e.message), c => Outcome.Done(c.show))
    }

  /** A free tool: `peek` reads a file of `ws`. */
  def peek(ws: Workspace^): Tool[String]^{ws} =
    Tool(ToolSpec(ToolName("peek"), "Reads a file.", path), Gate.Free, p => p, reading(ws, _))

  /** A gated tool, for the tests alone: `poke` reads a file of `ws` once a person approves. */
  def poke(ws: Workspace^): Tool[String]^{ws} =
    Tool(
      ToolSpec(ToolName("poke"), "Reads a file, asking first.", path),
      Gate.Ask(p => s"poke $p"),
      p => p,
      reading(ws, _)
    )

  /** `peek` and `poke` over `ws`. */
  def tools(ws: Workspace^): Toolbox[caps.CapSet^{ws}] =
    Toolbox
      .of[caps.CapSet^{ws}](peek(ws), poke(ws))
      .fold(d => throw new java.lang.AssertionError(d), identity)

  /** `fetch`, hosted: the engine offers it, an edge runs it. Free; reruns. */
  val hostedFetch: Hosted[String] =
    new Hosted(ToolSpec(ToolName("fetch"), "Fetches a file.", path, Retry.Rerun), Gate.Free, p => p)

  /** `prod`, hosted, asking first. */
  val hostedProd: Hosted[String] =
    new Hosted(
      ToolSpec(ToolName("prod"), "Prods a file, asking first.", path),
      Gate.Ask(p => s"prod $p"),
      p => p
    )

  /** Both hosted tools. */
  val hostedTools: Vector[Tool.Offered] = Vector(hostedFetch, hostedProd)

  /** How [[Served]]'s edge treats a request it is sent. */
  enum Serve {

    /** Claimed and answered at once. */
    case Now(outcome: Outcome)

    /** Claimed; answered only when the turn next looks at the row (its `expire` step). */
    case Later(outcome: Outcome)

    /** Claimed, and never answered. */
    case Claimed

    /** Not claimed: no edge takes it. */
    case Never

    /** Claimed, and the turn rung without an answer. */
    case Rung

    /** Claimed, then deleted as a purge would, and the turn rung. */
    case Lost
  }

  /** The edge at `at`, the fixture directory unless given, over `edges`: live, it serves each
    * request the turn sends as `serve` says, keeping its answer and ringing the turn's
    * workflow in `durable` as a desk does. Every request it is sent is kept in [[sent]].
    */
  final class Served(
      val edges: InMemoryEdges,
      durable: InMemoryDurable,
      serve: ToolRequest -> Serve,
      val at: Place = Place.of(checkout)
  ) extends grit.core.edge.ToolRequests {

    val registration: Registration = edges.register(Set(at))

    @caps.unsafe.untrackedCaptures
    var sent = Vector.empty[ToolRequest]

    @caps.unsafe.untrackedCaptures
    private var later = Map.empty[CallSlot, Outcome]

    /** Advertises `hosted` at its place, and `files` as the place's instruction files. */
    def advertise(hosted: Vector[Tool.Offered], files: Vector[Fragment] = Vector.empty): Unit = {
      val set = ToolSet.of(hosted.map(_.entry)).getOrElse(ToolSet.Empty)
      // Kept as a desk keeps them, before it advertises their ids.
      Prompts.keep(files)(using TestTx.fake)
      ToolSets.keep(set)(using TestTx.fake)
      edges.advertiseAs(registration, at, set, files)
    }

    private def tell(q: ToolRequest, outcome: Outcome): Unit =
      if (edges.answerAs(registration, q.slot, outcome)) ring(q)

    private def ring(q: ToolRequest): Unit =
      durable.send(q.slot.turn.workflowId, q.slot.key, grit.core.edge.Desk.Doorbell)

    def dispatch(requests: Vector[ToolRequest])(using Tx^): Either[StoreError, Unit] =
      edges.dispatch(requests).map(_ => requests.foreach(take))

    /** `q`, already dispatched, kept in [[sent]] and served as `serve` says. */
    def take(q: ToolRequest): Unit = {
      sent = sent :+ q
      serve(q) match {
        case Serve.Never => ()
        case Serve.Claimed => val _ = edges.claimAs(registration, q)
        case Serve.Later(o) =>
          if (edges.claimAs(registration, q)) later = later.updated(q.slot, o)
        case Serve.Now(o) => if (edges.claimAs(registration, q)) tell(q, o)
        case Serve.Rung => if (edges.claimAs(registration, q)) ring(q)
        case Serve.Lost =>
          if (edges.claimAs(registration, q)) {
            val t = q.slot.turn
            edges.forget(t.conversationId, t.turnSeq, t.turnSeq)
            ring(q)
          }
      }
    }

    def settle(slot: CallSlot)(using Tx^): Either[StoreError, grit.core.edge.RequestState] =
      edges.settle(slot).map { state =>
        resume(slot)
        state
      }

    /** The answer `serve` held back for `slot`, told now ([[Serve.Later]]). */
    def resume(slot: CallSlot): Unit =
      later.get(slot).foreach { o =>
        later = later - slot
        sent.find(_.slot == slot).foreach(tell(_, o))
      }

    def abandon(slot: CallSlot)(using Tx^): Either[StoreError, grit.core.edge.RequestState] =
      edges.abandon(slot)

    def answered(slot: CallSlot)(using Tx^): Either[StoreError, Option[Outcome]] =
      edges.answered(slot)
  }

  /** The edges `first` and `rest`, over `first`'s [[InMemoryEdges]]: each request dispatched
    * is taken by the one whose place it is addressed to; one at no edge's place is taken by
    * none.
    */
  final class Desks(first: Served, rest: Vector[Served]) extends grit.core.edge.ToolRequests {
    private def all: Vector[Served] = first +: rest

    def dispatch(requests: Vector[ToolRequest])(using Tx^): Either[StoreError, Unit] =
      first.edges.dispatch(requests).map { _ =>
        requests.foreach(q => all.find(_.at == q.workspace).foreach(_.take(q)))
      }

    def settle(slot: CallSlot)(using Tx^): Either[StoreError, grit.core.edge.RequestState] =
      first.edges.settle(slot).map { state =>
        all.foreach(_.resume(slot))
        state
      }

    def abandon(slot: CallSlot)(using Tx^): Either[StoreError, grit.core.edge.RequestState] =
      first.edges.abandon(slot)

    def answered(slot: CallSlot)(using Tx^): Either[StoreError, Option[Outcome]] =
      first.edges.answered(slot)
  }

  /** The turn's workflow body over `entries` and `provider`, its hosted calls sent through
    * `served`, its model offered `hosted` where the edge serves them, its tool sets kept in
    * `toolSets`, for at most `calls` model calls; its conversation is from `from`, the
    * fixture's TUI session unless given, linked to a service by `worksIn`, and to services it
    * reaches by `reaches`, whose edges, over `served`'s, are `reached` ([[Desks]]).
    */
  def hostedBody(
      entries: EntryStore,
      provider: Provider^,
      served: Served,
      hosted: Vector[Tool.Offered] = hostedTools,
      toolSets: InMemoryToolSets = ToolSets,
      calls: Int = 5,
      from: Origin = origin,
      worksIn: Vector[WorksIn] = Vector.empty,
      reaches: Vector[Reaches] = Vector.empty,
      reached: Vector[Served] = Vector.empty,
      recipe: grit.core.recipe.TurnRecipe = grit.core.recipe.TurnRecipe.Shipped,
      knowledge: grit.core.triage.Corpora = grit.core.triage.Corpora.Empty,
      weighing: TurnWeighing^ = noTriage(),
      ledger: UsageLedger = new InMemoryUsageLedger
  )(id: WorkflowId)(using Durable^): String = {
    val requests: grit.core.edge.ToolRequests =
      if (reached.isEmpty) served else new Desks(served, reached)
    val conversations = new InMemoryConversationStore
    conversations.findOrCreate(from, Account.Local, Label.Public)(using TestTx.fake)
    Turn.body(
      TurnEnv(
        TurnRecords(
          entries,
          ledger,
          CharEstimate,
          profilesKept(),
          new InMemoryPrincipals,
          new InMemoryDocuments
        ),
        TurnHosting(
          conversations,
          Prompts,
          toolSets,
          requests,
          served.edges,
          new InMemoryVoiceStore
        ),
        new LinearAssembler(
          entries,
          NoPeriods,
          new InMemoryPrincipals,
          CharEstimate,
          LinearAssembler.DefaultBudget
        ),
        NoClassifier,
        new FixedModels(provider, new StubProvider()),
        FakeDb,
        new NoWait,
        Fresh.random(),
        quiet(),
        unstitched(),
        weighing
      ),
      TurnTooling[{}](
        Toolbox.of[{}]().fold(d => throw new java.lang.AssertionError(d), identity),
        Toolbox.Empty,
        hosted,
        new FakeJot,
        budget(calls),
        worksIn = worksIn,
        reaches = reaches,
        recipe = recipe,
        knowledge = knowledge
      )
    )(id)
  }

  /** The rows the `offer` step among `steps` names, from the fixture stores: its tool set
    * and prompt fragments, as a history keeps them ([[grit.core.durable.History.kept]]).
    */
  def keptBy(steps: Vector[InMemoryDurable.Step]): ujson.Obj = {
    val offered = steps.collectFirst {
      case InMemoryDurable.Step(Turn.Step.Offer, InMemoryDurable.Outcome.Output(out)) =>
        ujson.read(out)
    }
    val ok = offered.flatMap(_.objOpt).flatMap(_.get("ok")).flatMap(_.objOpt)
    val set = ok
      .flatMap(_.get("tools"))
      .flatMap(_.strOpt)
      .flatMap(id => ToolSets.sets.find(s => grit.core.tool.ToolSetId.value(s.id) == id))
    val ids = ok
      .flatMap(_.get("prompt"))
      .flatMap(_.arrOpt)
      .fold(Vector.empty[String])(_.toVector.flatMap(_.strOpt))
    val fragments =
      ids.flatMap(id => Prompts.fragments.find(f => grit.core.prompt.FragmentId.value(f.id) == id))
    if (offered.isEmpty) ujson.Obj()
    else
      ujson.Obj(
        "toolSets" -> ujson.Arr.from(set.toVector.map(ToolSet.write)),
        "fragments" -> ujson.Arr.from(fragments.map(Fragment.write))
      )
  }

  /** Keeps in the fixture stores the rows a history holds ([[keptBy]]'s form). */
  def keepAll(kept: ujson.Obj): Either[String, Unit] = {
    val sets =
      kept.value.get("toolSets").flatMap(_.arrOpt).fold(Vector.empty[ujson.Value])(_.toVector)
    val fragments =
      kept.value.get("fragments").flatMap(_.arrOpt).fold(Vector.empty[ujson.Value])(_.toVector)
    for {
      read <- sets.foldLeft[Either[String, Vector[ToolSet]]](Right(Vector.empty))((acc, v) =>
        acc.flatMap(d => ToolSet.read(v).map(d :+ _))
      )
      frags <- fragments.foldLeft[Either[String, Vector[Fragment]]](Right(Vector.empty)) {
        (acc, v) =>
          acc.flatMap { d =>
            (for {
              o <- v.objOpt
              layer <- o.get("layer").flatMap(_.strOpt).flatMap(grit.core.prompt.Layer.of)
              source <- o.get("source").flatMap(_.strOpt)
              text <- o.get("text").flatMap(_.strOpt)
            } yield Fragment(layer, source, text))
              .map(d :+ _)
              .toRight("a kept fragment does not read")
          }
      }
    } yield {
      read.foreach(ToolSets.keep(_)(using TestTx.fake))
      Prompts.keep(frags)(using TestTx.fake)
      ()
    }
  }

  /** `calls` model calls as a turn's budget. */
  def budget(calls: Int): TurnLoop.Budget =
    TurnLoop.Budget.of(calls).fold(why => throw new java.lang.AssertionError(why), identity)

  /** An entry store that dies, once, on the first insert of an entry `when` picks. */
  final class CrashOnInsert(underlying: EntryStore, when: Entry -> Boolean) extends EntryStore {
    // A Boolean flipped once; one test's store, read and written only on the thread that
    // runs its turn (InMemoryDurable runs a workflow on its caller's thread).
    @caps.unsafe.untrackedCaptures
    var armed = true

    def insert(entry: Entry)(using Tx^): Either[StoreError, Unit] =
      if (armed && when(entry)) { armed = false; throw new InMemoryDurable.Crash }
      else underlying.insert(entry)
    def get(id: EntryId)(using Tx^): Either[StoreError, Option[Entry]] = underlying.get(id)
    def list(c: ConversationId)(using Tx^): Either[StoreError, Vector[Entry]] = underlying.list(c)
    def at(c: ConversationId, seqs: Vector[EntrySeq])(using
        Tx^
    ): Either[StoreError, Vector[Entry]] =
      underlying.at(c, seqs)
    def ofTurn(turn: TurnRef)(using Tx^): Either[StoreError, Vector[Entry]] =
      underlying.ofTurn(turn)
    def lockNext(c: ConversationId)(using Tx^): Either[StoreError, EntryStore.Next] =
      underlying.lockNext(c)
  }

  /** `underlying`, failing `list` from a turn's window being recorded until its reply or
    * draft is: while its model rounds build their requests and record their replies.
    * `windows` counts the windows recorded, so a test can tell the failing span was entered.
    */
  final class UnlistedWhileAnswering(underlying: EntryStore) extends EntryStore {
    // Both a value replaced; one test's store, read and written only on the thread that runs
    // its turn (InMemoryDurable runs a workflow on its caller's thread).
    @caps.unsafe.untrackedCaptures
    private var answering = false

    @caps.unsafe.untrackedCaptures
    var windows = 0

    def insert(entry: Entry)(using Tx^): Either[StoreError, Unit] = {
      val turn = TurnRef(entry.conversationId, entry.turnSeq)
      if (entry.id == turn.replyId || entry.id == turn.draftId) answering = false
      val inserted = underlying.insert(entry)
      entry.payload match {
        case _: Payload.Window => windows += 1; answering = true
        case _ => ()
      }
      inserted
    }
    def get(id: EntryId)(using Tx^): Either[StoreError, Option[Entry]] = underlying.get(id)
    def list(c: ConversationId)(using Tx^): Either[StoreError, Vector[Entry]] =
      if (answering) Left(StoreError.DatabaseError("the conversation is not listed mid-answer"))
      else underlying.list(c)
    def at(c: ConversationId, seqs: Vector[EntrySeq])(using
        Tx^
    ): Either[StoreError, Vector[Entry]] =
      underlying.at(c, seqs)
    def ofTurn(turn: TurnRef)(using Tx^): Either[StoreError, Vector[Entry]] =
      underlying.ofTurn(turn)
    def lockNext(c: ConversationId)(using Tx^): Either[StoreError, EntryStore.Next] =
      underlying.lockNext(c)
  }

  /** `underlying`, dying inside the step that records the turn's first ask. A turn run over
    * it has started, and is resumed at that ask, so a test can answer the ask in between:
    * [[InMemoryDurable.send]] refuses a turn that has not started, as DBOS does.
    */
  def crashingAtAsk(underlying: EntryStore): CrashOnInsert =
    new CrashOnInsert(
      underlying,
      e =>
        e.payload match {
          case _: Payload.Ask => true
          case _ => false
        }
    )

  /** Records `text` as the user message that starts the conversation's next turn. */
  def say(entries: EntryStore, text: String): TurnRef = {
    given Tx = TestTx.fake
    val next = entries.lockNext(conversation).getOrElse(sys.error("in-memory store"))
    entries.insert(
      Entry(
        EntryId(s"in:$text"),
        conversation,
        next.turnSeq,
        None,
        next.seq,
        Payload.Message(Message.User(text)),
        Instant.EPOCH
      )
    )
    TurnRef(conversation, next.turnSeq)
  }

  /** Records `text` as a heard message, the first entry of the conversation's next turn:
    * what an unprompted turn is rooted on.
    */
  def hear(entries: EntryStore, text: String): TurnRef = {
    given Tx = TestTx.fake
    val next = entries.lockNext(conversation).getOrElse(sys.error("in-memory store"))
    entries.insert(
      Entry(
        EntryId(s"heard:$text"),
        conversation,
        next.turnSeq,
        None,
        next.seq,
        Payload.Heard(text),
        Instant.EPOCH
      )
    )
    TurnRef(conversation, next.turnSeq)
  }

  /** The conversation as a transcript: every entry but the windows' records. */
  def texts(entries: EntryStore): Vector[String] =
    all(entries).flatMap {
      _.payload match {
        case Payload.Message(Message.User(text)) => Some(s"user: $text")
        case Payload.Message(Message.Assistant(blocks, _, _, _, _)) =>
          Some(blocks.collect { case AssistantBlock.Text(t) => s"assistant: $t" }.mkString)
        case Payload.Message(other) => Some(other.toString)
        case Payload.Heard(text) => Some(s"heard: $text")
        case Payload.Posted(text) => Some(s"posted: $text")
        case Payload.Summary(text) => Some(s"summary: $text")
        case Payload.Query(text) => Some(s"query: $text")
        case Payload.Exchange(Message.Assistant(blocks, _, _, _, _)) =>
          Some(blocks.collect {
            case AssistantBlock.Text(t) => s"called: $t"
            case AssistantBlock.ToolCall(_, name, args) => s"[$name ${args.render()}]"
          }.mkString)
        case Payload.Result(Message.ToolResult(_, content, isError), _) =>
          Some(s"${if (isError) "error" else "result"}: $content")
        case Payload.Attempt(call) => Some(s"attempt: ${grit.core.id.ToolCallId.value(call)}")
        case Payload.Ask(call, shown) =>
          Some(s"ask: ${grit.core.id.ToolCallId.value(call)}: $shown")
        case Payload.Closed(_, _, closing) => Some(s"closed: ${closing.flows.prose}")
        case Payload.Draft(Message.Assistant(blocks, _, _, _, _)) =>
          Some(blocks.collect { case AssistantBlock.Text(t) => s"draft: $t" }.mkString)
        case Payload.Window(_, _, _, _) | Payload.Topic(_) => None
      }
    }

  /** The records of the turns' windows, with their ids. */
  def windows(entries: EntryStore): Vector[(EntryId, Payload.Window)] =
    all(entries).collect { case Entry(id, _, _, _, _, w: Payload.Window, _) => id -> w }

  private def all(entries: EntryStore): Vector[Entry] =
    entries.list(conversation)(using TestTx.fake).getOrElse(Vector.empty)

  /** The linear window, with `note` added: an assembler that says it wrote a query. */
  final class Noting(entries: EntryStore, note: AssemblyNote) extends ContextAssembler {
    def assemble(request: AssemblyRequest)(using Db^): Either[AssemblyError, Window] =
      new LinearAssembler(
        entries,
        NoPeriods,
        new InMemoryPrincipals,
        CharEstimate,
        LinearAssembler.DefaultBudget
      )
        .assemble(request)
        .map(_.copy(notes = Vector(note)))
  }

  /** A conversation that has never closed a period: an empty period store, over entries of
    * its own.
    */
  val NoPeriods: PeriodStore = new InMemoryPeriodStore(new InMemoryEntryStore)

  /** A query note, as retrieval would write it. */
  val queried: AssemblyNote.Queried =
    AssemblyNote.Queried(
      "hello greeting",
      "writer",
      Usage(Tokens(30), Tokens(4), Tokens.Zero, Some(BigDecimal("0.00001"))),
      Tokens(28)
    )

  /** A catalog whose three roles differ only in their model: what every fixture turn pins. */
  val TestCatalog: Catalog = {
    def role(name: String, budget: Int) =
      Assignment(
        ModelRef(
          ModelId.of(s"test/$name").getOrElse(throw new java.lang.AssertionError(name)),
          None
        ),
        budget,
        None
      )
    Catalog.of(
      Policy(role("turn", 4096), role("summary", 1024), role("query", 1024), role("summary", 1024)),
      Vector.empty
    )
  }

  /** Models for a test: [[TestCatalog]] is in force, and the summary's calls go to
    * `summary`, every other role's to `turn`.
    */
  final class FixedModels(turn: Provider^, summary: Provider^) extends Models {
    def catalog(): Either[String, Catalog] = Right(TestCatalog)
    def provider(pinned: Pinned): Provider^ =
      if (pinned.assignment == TestCatalog.policy.summary) summary else turn
  }

  /** The turn's workflow body over `entries` and `provider`, windowed by `assembler`, showing
    * and counting `documents`, dated by `clock`.
    */
  def turnBodyWith(
      entries: EntryStore,
      provider: Provider^,
      assembler: ContextAssembler^,
      ledger: UsageLedger,
      classifier: Classifier^ = NoClassifier,
      speech: TurnSpeech = quiet(),
      stitching: TurnStitching^ = unstitched(),
      hosted: TurnHosting = hosting(),
      recipe: grit.core.recipe.TurnRecipe = grit.core.recipe.TurnRecipe.Shipped,
      weighing: TurnWeighing^ = noTriage(),
      documents: DocumentStore = new InMemoryDocuments,
      clock: Clock^ = new NoWait
  )(
      id: WorkflowId
  )(using Durable^): String =
    Turn.body(
      TurnEnv(
        TurnRecords(
          entries,
          ledger,
          CharEstimate,
          profilesKept(),
          new InMemoryPrincipals,
          documents
        ),
        hosted,
        assembler,
        classifier,
        new FixedModels(provider, new StubProvider()),
        FakeDb,
        clock,
        Fresh.random(),
        speech,
        stitching,
        weighing
      ),
      TurnTooling[caps.CapSet^{NoCheckout}](
        noTools,
        Toolbox.Empty,
        Vector.empty,
        new FakeJot,
        budget(5),
        recipe = recipe
      )
    )(id)

  /** The turn's workflow body over `entries`, its calls to `turn`'s model and its summary to
    * `summary`'s, in the voice `voices` holds.
    */
  def voicedBody(
      entries: EntryStore,
      turn: Provider^,
      summary: Provider^,
      voices: VoiceStore,
      principals: Principals = new InMemoryPrincipals
  )(
      id: WorkflowId
  )(using Durable^): String =
    Turn.body(
      TurnEnv(
        TurnRecords(
          entries,
          new InMemoryUsageLedger,
          CharEstimate,
          profilesKept(),
          principals,
          new InMemoryDocuments
        ),
        hosting(voices = voices),
        new LinearAssembler(
          entries,
          NoPeriods,
          new InMemoryPrincipals,
          CharEstimate,
          LinearAssembler.DefaultBudget
        ),
        NoClassifier,
        new FixedModels(turn, summary),
        FakeDb,
        new NoWait,
        Fresh.random(),
        quiet(),
        unstitched(),
        noTriage()
      ),
      TurnTooling[caps.CapSet^{NoCheckout}](
        noTools,
        Toolbox.Empty,
        Vector.empty,
        new FakeJot,
        budget(5)
      )
    )(id)

  /** The stub classifier, counting the questions it was asked, call by call. */
  final class CountingClassifier(fail: Boolean = false) extends Classifier {
    @caps.unsafe.untrackedCaptures
    var calls = 0

    /** Every state it was asked about, as sent. */
    @caps.unsafe.untrackedCaptures
    var states = Vector.empty[ujson.Value]

    protected def answer(
        state: ujson.Value,
        questions: Vector[grit.core.classify.Question]
    ): Either[grit.core.classify.ClassifierError, grit.core.classify.Answers] = {
      calls += 1
      states = states :+ state
      if (fail) Left(grit.core.classify.ClassifierError.Unavailable("HTTP 529: overloaded"))
      else Right(grit.models.StubClassifier.answers(state, questions))
    }
  }

  /** The conversation's topics as its entries leave them. */
  def topics(entries: EntryStore): grit.core.topic.Topics =
    grit.core.topic.Topics
      .fold(Vector.empty, all(entries).flatMap(e => grit.core.store.EntryTopics.events(e.payload)))

  /** No classifier: every message after a conversation's first is unclassified. */
  def NoClassifier: Classifier^ = Classifier.none("no classifier")

  /** The turn's workflow body over `entries` and `provider`, summarised by `summarizer`,
    * its messages placed by `classifier`.
    */
  def turnBody(
      entries: EntryStore,
      provider: Provider^,
      ledger: UsageLedger = new InMemoryUsageLedger,
      summarizer: Provider^ = new StubProvider(),
      classifier: Classifier^ = NoClassifier,
      clock: Clock^ = new NoWait,
      speech: TurnSpeech = quiet(),
      stitching: TurnStitching^ = unstitched()
  )(id: WorkflowId)(using Durable^): String =
    tooledBody(
      entries,
      provider,
      ledger,
      summarizer,
      classifier,
      NoCheckout,
      noTools,
      5,
      clock,
      speech = speech,
      stitching = stitching
    )(id)

  /** The limits a speech fixture speaks within: [[grit.core.speech.Limits.suggested]], $0.25,
    * by v1's gate.
    */
  val speechLimits: Limits =
    Limits.suggested(
      DailyCap.of("0.25").getOrElse(throw new java.lang.AssertionError("a cap")),
      grit.core.triage.Tags.V1.gate
    )

  /** Answers the judge's two yes/no questions, in the order asked ([[TurnJudge]] asks
    * grounded, then worth), with `answers`' first and second, counting its calls; unavailable
    * when `answers` is `None`, unreadable when asked any other number of questions.
    */
  final class Judge(answers: Option[(Double, Double)]) extends Classifier {
    @caps.unsafe.untrackedCaptures
    var calls = 0

    protected def answer(
        state: ujson.Value,
        questions: Vector[Question]
    ): Either[ClassifierError, Answers] = {
      calls += 1
      answers match {
        case None => Left(ClassifierError.Unavailable("down"))
        case Some(_) if questions.size != 2 =>
          Left(ClassifierError.Unreadable(s"asked ${questions.size} questions, not 2"))
        case Some((first, second)) =>
          Right(
            Answers(
              Vector(Answer.YesNo(first), Answer.YesNo(second)),
              Usage(Tokens(40), Tokens.Zero, Tokens.Zero, Some(BigDecimal("0.0000017"))),
              "jev"
            )
          )
      }
    }
  }

  /** Answers every yes/no question `yes`, at the judge's usage, keeping the questions asked. */
  final class AnswersYes(yes: Double) extends Classifier {
    @caps.unsafe.untrackedCaptures
    var asked = Vector.empty[Question]

    protected def answer(
        state: ujson.Value,
        questions: Vector[Question]
    ): Either[ClassifierError, Answers] = {
      asked = asked ++ questions
      Right(
        Answers(
          questions.map(_ => Answer.YesNo(yes)),
          Usage(Tokens(40), Tokens.Zero, Tokens.Zero, Some(BigDecimal("0.0000017"))),
          "jev"
        )
      )
    }
  }

  /** What triage kept for `w`'s heard message: a question put to grit by name
    * ([[grit.core.triage.Tags.V3.directed]]), so its turn is rooted `Named`.
    */
  def directed(w: SpeechWorld): grit.core.triage.InMemoryTriageStore = {
    import grit.core.triage.Tags
    val store = new grit.core.triage.InMemoryTriageStore(w.entries, NoPeriods)
    val kept = store.record(
      EntryId("heard:is the freeze still on?"),
      Tags.Weighed(
        scala.collection.immutable.VectorMap(
          Tags.V2.gap -> Answer.Choice("asks", Vector(Answer.Weight("asks", 1.0)), 1.0),
          Tags.V2.open -> Answer.YesNo(0.9),
          Tags.V2.to -> Answer.YesNo(0.9),
          Tags.V3.toGrit -> Answer.YesNo(0.9)
        ),
        "jev",
        Usage.Zero
      ),
      Instant.EPOCH
    )(using TestTx.fake)
    if (kept != Right(true)) throw new java.lang.AssertionError(s"triage not kept: $kept")
    store
  }

  /** A window of the conversation's entries before the turn, the closing among them. */
  final class Before(entries: InMemoryEntryStore) extends ContextAssembler {
    def assemble(request: AssemblyRequest)(using Db^): Either[AssemblyError, Window] =
      Right(
        Window(
          entries
            .list(conversation)(using TestTx.fake)
            .getOrElse(Vector.empty)
            .filter(e => TurnSeq.value(e.turnSeq) < TurnSeq.value(request.turn.turnSeq))
            .map(_.seq)
        )
      )
  }

  final case class SpeechWorld(
      entries: InMemoryEntryStore,
      ledger: InMemoryUsageLedger,
      store: InMemorySpeechStore,
      deliveries: InMemoryDeliveries,
      turn: TurnRef
  )

  /** A conversation that begins with a record of its closed period (turn 0) when
    * `recalled`, else with a person's message, then a heard message (the turn), heard with the
    * reply address `replyTo` and decided on: drafting, or, when `answering`, answered as said
    * to grit at that address.
    */
  def speechWorld(
      recalled: Boolean = true,
      replyTo: Option[String] = Some("C/1"),
      answering: Boolean = false
  ): SpeechWorld = {
    given grit.core.store.Tx = TestTx.fake
    val entries = new InMemoryEntryStore
    val ledger = new InMemoryUsageLedger
    val next = entries.lockNext(conversation).getOrElse(sys.error("store"))
    val first =
      if (recalled)
        Payload.Closed(
          PeriodSeq.First,
          CloseReason.Lapsed,
          TestClosings.prose("The freeze moved to Thursday.")
        )
      else Payload.Heard("morning all")
    entries.insert(
      Entry(EntryId("first"), conversation, next.turnSeq, None, next.seq, first, Instant.EPOCH)
    )
    val turn = hear(entries, "is the freeze still on?")
    val store = new InMemorySpeechStore(entries, ledger)
    store.heard(turn, Reach(replyTo, Set.empty))
    val p = Probability.clamped(0.9)
    store.decided(
      Heard(
        turn,
        EntrySeq(1),
        Place.Everywhere,
        Instant.EPOCH,
        Reach(replyTo, Set.empty),
        Tags.Weighed(Tags.V1.answers(Kind.Question, p, p, p, p), "jev", Usage.Zero)
      ),
      replyTo.filter(_ => answering).fold(Decision.Drafting(turn))(Decision.Answering(turn, _)),
      Instant.EPOCH
    )
    SpeechWorld(entries, ledger, store, new InMemoryDeliveries, turn)
  }

  /** Speaking off, over stores of its own: what a turn not rooted on a heard message never
    * reads.
    */
  def quiet(): TurnSpeech =
    TurnSpeech(
      grit.core.speech.Speaking.Off,
      new grit.core.speech.InMemorySpeechStore(new InMemoryEntryStore, new InMemoryUsageLedger),
      new grit.core.edge.InMemoryDeliveries
    )

  /** No tools offered: the loop's first call is the plain request, and answers. */
  def noTools: Toolbox[caps.CapSet^{NoCheckout}] =
    Toolbox
      .of[caps.CapSet^{NoCheckout}]()
      .fold(d => throw new java.lang.AssertionError(d), identity)

  /** As [[turnBody]], the model offered `tools`, which read `ws`, for at most `calls` model
    * calls.
    */
  def tooledBody(
      entries: EntryStore,
      provider: Provider^,
      ledger: UsageLedger,
      summarizer: Provider^,
      classifier: Classifier^,
      ws: Workspace^,
      tools: Toolbox[caps.CapSet^{ws}],
      calls: Int,
      clock: Clock^ = new NoWait,
      hosted: Vector[Tool.Offered] = Vector.empty,
      speech: TurnSpeech = quiet(),
      stitching: TurnStitching^ = unstitched()
  )(id: WorkflowId)(using Durable^): String =
    Turn.body(
      TurnEnv(
        TurnRecords(
          entries,
          ledger,
          CharEstimate,
          profilesKept(),
          new InMemoryPrincipals,
          new InMemoryDocuments
        ),
        hosting(),
        new LinearAssembler(
          entries,
          NoPeriods,
          new InMemoryPrincipals,
          CharEstimate,
          LinearAssembler.DefaultBudget
        ),
        classifier,
        new FixedModels(provider, summarizer),
        FakeDb,
        clock,
        Fresh.random(),
        speech,
        stitching,
        noTriage()
      ),
      TurnTooling[caps.CapSet^{ws}](tools, Toolbox.Empty, hosted, new FakeJot, budget(calls))
    )(id)

  /** A profile store already holding [[TestCatalog]]'s profile, as the database holds it
    * across a turn's restarts: a body built afresh for a resumed turn reads its recorded pin
    * back by id.
    */
  def profilesKept(): InMemoryModelProfileStore = {
    val kept = new InMemoryModelProfileStore
    val _ = kept.pin(WorkflowId("kept"), TestCatalog.pin)(using TestTx.fake)
    kept
  }

  /** The turn's workflow body over `entries`, its models `models` and its profile kept in
    * `profiles`, the loop offered `tools` over `ws` for at most `calls` model calls.
    */
  def modelsBody(
      entries: EntryStore,
      models: Models^,
      profiles: ModelProfileStore,
      ws: Workspace^,
      tools: Toolbox[caps.CapSet^{ws}],
      calls: Int = 5
  )(id: WorkflowId)(using Durable^): String =
    Turn.body(
      TurnEnv(
        TurnRecords(
          entries,
          new InMemoryUsageLedger,
          CharEstimate,
          profiles,
          new InMemoryPrincipals,
          new InMemoryDocuments
        ),
        hosting(),
        new LinearAssembler(
          entries,
          NoPeriods,
          new InMemoryPrincipals,
          CharEstimate,
          LinearAssembler.DefaultBudget
        ),
        NoClassifier,
        models,
        FakeDb,
        new NoWait,
        Fresh.random(),
        quiet(),
        unstitched(),
        noTriage()
      ),
      TurnTooling[caps.CapSet^{ws}](tools, Toolbox.Empty, Vector.empty, new FakeJot, budget(calls))
    )(id)

  def runTurn(
      durable: InMemoryDurable,
      entries: EntryStore,
      provider: Provider^,
      turn: TurnRef,
      ledger: UsageLedger = new InMemoryUsageLedger,
      summarizer: Provider^ = new StubProvider(),
      classifier: Classifier^ = NoClassifier
  ): String =
    durable.run(turn.workflowId)(turnBody(entries, provider, ledger, summarizer, classifier))

  /** Answers every choice with its first option at 0.9, counting its calls. */
  final class FirstOption extends Classifier {
    @caps.unsafe.untrackedCaptures
    var calls = 0

    protected def answer(
        state: ujson.Value,
        questions: Vector[Question]
    ): Either[ClassifierError, Answers] = {
      calls += 1
      val answers = questions.map {
        case q: Question.Choice =>
          val keys = q.keys.map(_.name)
          Answer.Choice(
            keys.headOption.getOrElse(""),
            keys.zipWithIndex.map((k, i) => Answer.Weight(k, if (i == 0) 0.9 else 0.1)),
            0.8
          )
        case Question.YesNo(_, _, _) => Answer.YesNo(0.9)
      }
      Right(Answers(answers, Usage(Tokens(900), Tokens.Zero, Tokens.Zero, None), "jev"))
    }
  }

  val StitchedAt = Instant.parse("2026-09-30T22:31:57Z")

  /** Slack channel C's first thread, holding a heard question, and a second, whose first message is
    * `first`; the second thread's turn.
    */
  final class StitchChannel(first: Payload) {
    val entries = new InMemoryEntryStore
    val conversations = new InMemoryConversationStore
    private def originOf(c: ConversationId): Origin =
      conversations.get(c)(using TestTx.fake).toOption.flatten.fold(origin)(_.origin)
    val stitches = new InMemoryStitchStore(entries, originOf)
    val a: ConversationId =
      conversations
        .findOrCreate(Origin.Slack("T", "C", "1.0"), Account.Local, Label.Public)(using
          TestTx.fake
        )
        .fold(e => sys.error(e.toString), _.id)
    val b: ConversationId =
      conversations
        .findOrCreate(Origin.Slack("T", "C", "2.0"), Account.Local, Label.Public)(using
          TestTx.fake
        )
        .fold(e => sys.error(e.toString), _.id)
    private def put(c: ConversationId, id: String, payload: Payload, at: Instant): Entry = {
      given grit.core.store.Tx = TestTx.fake
      val next = entries.lockNext(c).getOrElse(sys.error("store"))
      val e = Entry(EntryId(id), c, next.turnSeq, None, next.seq, payload, at)
      entries.insert(e)
      e
    }
    val asked =
      put(a, "a:asked", Payload.Heard("where did we land on the Engine contract term?"), StitchedAt)
    val root = put(b, "b:first", first, StitchedAt.plusSeconds(29))
    val turn = TurnRef(b, root.turnSeq)

    val lifecycle = new grit.core.store.InMemoryLifecycleStore
    val principals = new grit.core.store.InMemoryPrincipals

    /** The openings whose placements the last [[run]] waited for. */
    @caps.unsafe.untrackedCaptures
    var waited: Vector[grit.core.stitch.Opening] = Vector.empty

    /** The turn run, asking `classifier`, its placements made at once as its own workflow
      * would make them; a patch in `unpatched` is not taken.
      */
    def run(
        classifier: Classifier^,
        unpatched: Set[String] = Set.empty
    ): (InMemoryDurable, String) = {
      val durable = new InMemoryDurable(unpatched)
      val placements = new grit.core.stitch.InMemoryPlacements(
        classifier,
        grit.core.stitch
          .StitchReads(entries, conversations, lifecycle, stitches, NoSearch, principals),
        FakeDb,
        Tuning.Default,
        StitchedAt.plusSeconds(60)
      )
      val hosted = TurnHosting(
        conversations,
        Prompts,
        ToolSets,
        new grit.core.edge.InMemoryEdges,
        new grit.core.edge.InMemoryEdges,
        new grit.core.store.InMemoryVoiceStore
      )
      val done = durable.run(turn.workflowId)(
        turnBodyWith(
          entries,
          new Scripted((_, _) =>
            Right(
              Message.Assistant(
                Vector(AssistantBlock.Text("It is a real question.")),
                StopReason.EndTurn,
                Usage(Tokens(10), Tokens(2), Tokens.Zero, None),
                "m"
              )
            )
          ),
          new Before(entries),
          new InMemoryUsageLedger,
          classifier,
          stitching = TurnStitching(stitches, NoSearch, lifecycle, Tuning.Default, placements),
          hosted = hosted
        )
      )
      waited = placements.waited
      (durable, done)
    }
  }

  /** A Slack thread working in `github`, whose turns a recipe shapes by the `repo` source
    * that `github` supplies: what the `weigh` step and its histories run over.
    */
  object Sourced {
    import scala.collection.immutable.VectorMap

    import grit.core.context.Width
    import grit.core.id.{CorpusName, QuestionName}
    import grit.core.place.Service
    import grit.core.recipe.{ByFocus, Offering, Shaping, TurnRecipe}
    import grit.core.triage.{Corpora, Corpus, InMemoryTriageStore, TriageStore}

    val github: Service =
      Service.of("github").fold(e => throw new java.lang.AssertionError(e), identity)

    val repo: CorpusName =
      CorpusName.of("repo").fold(e => throw new java.lang.AssertionError(e), identity)

    val knowledge: Corpora = Corpora
      .of(Vector(Corpus(repo, "the repository", Place.Everywhere, Some(github))))
      .fold(n => throw new java.lang.AssertionError(n), identity)

    /** A heard turn offered `github`'s tools only when `repo` reads at least 0.2. */
    val recipe = TurnRecipe(
      ByFocus.both(Shaping(Width.Deployed, Offering.BySource(Probability.clamped(0.2)))),
      TurnRecipe.Shipped.addressed
    )

    /** Triage's tags for a root whose `repo` source reads `p`. */
    def repoReads(p: Double): Tags.Weighed =
      Tags.Weighed(
        VectorMap(QuestionName.per(Tags.V2.sourcePrefix, repo) -> Answer.YesNo(p)),
        "jev",
        Usage.Zero
      )

    /** A Slack thread working in `github`, whose edge advertises `github_search`; its turn
      * rooted on a message `heard` or said to grit, triage having kept `kept` for it when heard,
      * and a message said to grit weighed as `asked` says, offered by `addressed`, with the
      * sources of `sources`.
      */
    final class Thread(
        heard: Boolean,
        kept: Option[Tags],
        unpatched: Set[String] = Set.empty,
        addressed: Offering = TurnRecipe.Shipped.addressed.offering,
        asked: Either[grit.core.triage.Weighing.Unweighed, grit.core.triage.Weighing.Weighed] =
          Right(
            grit.core.triage.Weighing.Weighed(repoReads(0.1), grit.core.message.Tokens(321))
          ),
        sources: Corpora = knowledge
    ) {
      val entries = new InMemoryEntryStore
      val durable = new InMemoryDurable(unpatched)
      val turn: TurnRef =
        if (heard) hear(entries, "someone should look at the repo") else say(entries, "look at it")
      val root =
        EntryId(if (heard) "heard:someone should look at the repo" else "in:look at it")
      val triage = new InMemoryTriageStore(entries, NoPeriods)
      val asks = new Calls
      val ledger = new InMemoryUsageLedger
      kept.foreach(t => triage.record(root, t, Instant.EPOCH)(using TestTx.fake))
      val edge = new Served(new InMemoryEdges, durable, _ => Serve.Never, github.place)
      edge.advertise(
        Hosted
          .advertised(
            ToolSet.Entry(
              ToolName("github_search"),
              "Searches GitHub.",
              ujson.Obj("type" -> "object"),
              false,
              Retry.Rerun
            )
          )
          .toVector
      )

      def body(store: TriageStore)(id: WorkflowId)(using grit.core.durable.Durable^): String =
        hostedBody(
          entries,
          new RecordingProvider,
          edge,
          hosted = Vector.empty,
          from = Origin.Slack("T1", "C1", "1.0"),
          worksIn = Vector(WorksIn(Place.Everywhere, github)),
          recipe = recipe.copy(addressed = Shaping(Width.Deployed, addressed)),
          knowledge = sources,
          weighing = TurnWeighing(store, new Weighs(asked, asks)),
          ledger = ledger
        )(id)

      def run(store: TriageStore = triage): String = durable.run(turn.workflowId)(body(store))

      /** The tools its `offer` step recorded the turn was offered, by name. */
      def offered: Vector[String] =
        durable
          .history(turn.workflowId)
          .collectFirst {
            case InMemoryDurable.Step("offer", InMemoryDurable.Outcome.Output(text)) =>
              text
          }
          .flatMap(TurnJournal.recordedOffer.decode(_).toOption)
          .flatMap(_.toOption)
          .flatMap(r => ToolSets.get(r.tools)(using TestTx.fake).toOption)
          .toVector
          .flatMap(_.tools.map(t => ToolName.value(t.name)))

      /** The `weigh` step's recorded output, when it ran. */
      def weighed: Option[String] = durable.history(turn.workflowId).collectFirst {
        case InMemoryDurable.Step("weigh", InMemoryDurable.Outcome.Output(text)) => text
      }
    }
  }

}
