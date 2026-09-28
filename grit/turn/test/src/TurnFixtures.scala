package grit.turn

import java.time.Instant

import grit.assembly.estimate.CharEstimate
import grit.assembly.linear.LinearAssembler
import grit.core.classify.Classifier
import grit.core.clock.{Clock, Fresh}
import grit.core.context.{AssemblyError, AssemblyNote, AssemblyRequest, ContextAssembler, Window}
import grit.core.durable.{Durable, InMemoryDurable}
import grit.core.edge.{InMemoryEdges, OutcomeJson, Registration, ToolRequest}
import grit.core.id.{CallSlot, ConversationId, EntryId, PrincipalId, ToolCallId, TurnRef, WorkflowId}
import grit.core.place.{Directory, Place}
import grit.core.prompt.{Fragment, SystemPrompt, Voice}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.{Assignment, Catalog, ModelId, ModelRef, Pinned, Policy}
import grit.core.provider.{Delta, ModelRequest, Models, Provider, ProviderError}
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
import grit.core.store.{
  Db,
  Entry,
  EntryStore,
  InMemoryEntryStore,
  InMemoryPeriodStore,
  PeriodStore,
  InMemoryConversationStore,
  InMemoryModelProfileStore,
  InMemoryPrincipals,
  InMemoryPromptStore,
  InMemoryToolSets,
  InMemoryUsageLedger,
  InMemoryVoiceStore,
  Origin,
  Jot,
  ModelProfileStore,
  Payload,
  Principals,
  StoreError,
  Tx,
  UsageLedger,
  VoiceStore
}
import grit.core.tool.{Args, Field, Gate, Hosted, Outcome, Retry, Tool, ToolName, ToolSet, ToolSpec, Toolbox}
import grit.dbos.sql.TestTx
import grit.models.StubProvider

/** The turn's test world: the stub provider, the linear assembler, and core's in-memory
  * store and durability fakes. Shared by the turn tests, the replay gate and the recorder.
  */
object TurnFixtures {

  val conversation = ConversationId("c1")

  /** The directory the fixture conversation is a TUI session in. */
  val checkout: Directory = Directory.of("/checkout").fold(e => throw new java.lang.AssertionError(e), identity)

  /** The fixture conversation's origin: its id is `c1` in a fresh [[hosting]]. */
  val origin: Origin = Origin.Tui(checkout, "test")

  /** What a fixture turn is offered when no edge serves its directory: its conversations,
    * the fixture one created first, and in-memory prompts, tool sets, `edges` and `voices`
    * (none set: the default).
    */
  def hosting(
      edges: InMemoryEdges = new InMemoryEdges,
      voices: VoiceStore = new InMemoryVoiceStore
  ): TurnHosting = {
    val conversations = new InMemoryConversationStore
    conversations.findOrCreate(origin, PrincipalId.Local)(using TestTx.fake)
    TurnHosting(conversations, Prompts, ToolSets, edges, edges, voices)
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
        Vector(TurnPrompt.Base, TurnPrompt.edge(origin), TurnPrompt.reach(Some(checkout), ToolSet.Empty)) ++
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
    def sleep(duration: scala.concurrent.duration.FiniteDuration): Unit = waited = waited :+ duration
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
    AssistantBlock.ToolCall(ToolCallId("t1"), grit.core.tool.ToolName.value(TurnVerdict.Name), ujson.Obj("about" -> about))

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
    def read[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using TestTx.fake)
  }

  /** Writes straight through to the in-memory store, never rolled back. */
  final class FakeJot extends Jot {
    def write[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
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
    def run(command: String, timeout: scala.concurrent.duration.FiniteDuration): Either[HostError, Ran] =
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
      files.get(RelPath.value(path)).map(Clipped.head(_, _ => None)).toRight(HostError.NotFound(path))
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
      case Right(at) => ws.read(at, Lines.All).fold(e => Outcome.Failed(e.message), c => Outcome.Done(c.show))
    }

  /** A free tool: `peek` reads a file of `ws`. */
  def peek(ws: Workspace^): Tool[String]^{ws} =
    new Tool(ToolSpec(ToolName("peek"), "Reads a file.", path), Gate.Free, p => p, reading(ws, _))

  /** A gated tool, for the tests alone: `poke` reads a file of `ws` once a person approves. */
  def poke(ws: Workspace^): Tool[String]^{ws} =
    new Tool(ToolSpec(ToolName("poke"), "Reads a file, asking first.", path), Gate.Ask(p => s"poke $p"), p => p, reading(ws, _))

  /** `peek` and `poke` over `ws`. */
  def tools(ws: Workspace^): Toolbox[{ws}] =
    Toolbox.of[{ws}](peek(ws), poke(ws)).fold(d => throw new java.lang.AssertionError(d), identity)

  /** `fetch`, hosted: the engine offers it, an edge runs it. Free; reruns. */
  val hostedFetch: Hosted[String] =
    new Hosted(ToolSpec(ToolName("fetch"), "Fetches a file.", path, Retry.Rerun), Gate.Free, p => p)

  /** `prod`, hosted, asking first. */
  val hostedProd: Hosted[String] =
    new Hosted(ToolSpec(ToolName("prod"), "Prods a file, asking first.", path), Gate.Ask(p => s"prod $p"), p => p)

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
  }

  /** The fixture directory's edge, over `edges`: live, it serves each request the turn sends
    * as `serve` says, telling the turn's workflow in `durable` its answer as a desk does.
    * Every request it is sent is kept in [[sent]].
    */
  final class Served(val edges: InMemoryEdges, durable: InMemoryDurable, serve: ToolRequest -> Serve)
      extends grit.core.edge.ToolRequests {

    val registration: Registration = edges.register(Set(Place.of(checkout)))

    @caps.unsafe.untrackedCaptures
    var sent = Vector.empty[ToolRequest]

    @caps.unsafe.untrackedCaptures
    private var later = Map.empty[CallSlot, Outcome]

    /** Advertises `hosted`, and `files` as the directory's instruction files. */
    def advertise(hosted: Vector[Tool.Offered], files: Vector[Fragment] = Vector.empty): Unit = {
      val set = ToolSet.of(hosted.map(_.entry)).getOrElse(ToolSet.Empty)
      // Kept as a desk keeps them, before it advertises their ids.
      Prompts.keep(files)(using TestTx.fake)
      ToolSets.keep(set)(using TestTx.fake)
      edges.advertiseAs(registration, Place.of(checkout), set, files)
    }

    private def tell(q: ToolRequest, outcome: Outcome): Unit =
      if (edges.answerAs(registration, q.slot, outcome))
        durable.send(q.slot.turn.workflowId, q.slot.key, ujson.write(OutcomeJson.write(outcome)))

    def dispatch(requests: Vector[ToolRequest])(using Tx^): Either[StoreError, Unit] =
      edges.dispatch(requests).map { _ =>
        requests.foreach { q =>
          sent = sent :+ q
          serve(q) match {
            case Serve.Never => ()
            case Serve.Claimed => val _ = edges.claimAs(registration, q)
            case Serve.Later(o) =>
              if (edges.claimAs(registration, q)) later = later.updated(q.slot, o)
            case Serve.Now(o) => if (edges.claimAs(registration, q)) tell(q, o)
          }
        }
      }

    def settle(slot: CallSlot)(using Tx^): Either[StoreError, grit.core.edge.RequestState] =
      edges.settle(slot).map { state =>
        later.get(slot).foreach { o =>
          later = later - slot
          sent.find(_.slot == slot).foreach(tell(_, o))
        }
        state
      }

    def abandon(slot: CallSlot)(using Tx^): Either[StoreError, grit.core.edge.RequestState] =
      edges.abandon(slot)
  }

  /** The turn's workflow body over `entries` and `provider`, its hosted calls sent through
    * `served`, its model offered `hosted` where the edge serves them, its tool sets kept in
    * `toolSets`, for at most `calls` model calls.
    */
  def hostedBody(
      entries: EntryStore,
      provider: Provider^,
      served: Served,
      hosted: Vector[Tool.Offered] = hostedTools,
      toolSets: InMemoryToolSets = ToolSets,
      calls: Int = 5
  )(id: WorkflowId)(using Durable^): String = {
    val conversations = new InMemoryConversationStore
    conversations.findOrCreate(origin, PrincipalId.Local)(using TestTx.fake)
    Turn.body(
      TurnEnv(
        TurnRecords(entries, new InMemoryUsageLedger, CharEstimate, new InMemoryModelProfileStore, new InMemoryPrincipals),
        TurnHosting(conversations, Prompts, toolSets, served, served.edges, new InMemoryVoiceStore),
        new LinearAssembler(entries, NoPeriods, new InMemoryPrincipals, CharEstimate, LinearAssembler.DefaultBudget),
        NoClassifier,
        new FixedModels(provider, new StubProvider()),
        FakeDb,
        new NoWait,
        Fresh.random()
      ),
      TurnTooling[{}](
        Toolbox.of[{}]().fold(d => throw new java.lang.AssertionError(d), identity),
        hosted,
        new FakeJot,
        budget(calls)
      )
    )(id)
  }

  /** The rows the `offer` step among `steps` names, from the fixture stores: its tool set
    * and prompt fragments, as a history keeps them ([[grit.core.durable.History.kept]]).
    */
  def keptBy(steps: Vector[InMemoryDurable.Step]): ujson.Obj = {
    val offered = steps.collectFirst {
      case InMemoryDurable.Step(Turn.Step.Offer, InMemoryDurable.Outcome.Output(out)) => ujson.read(out)
    }
    val ok = offered.flatMap(_.objOpt).flatMap(_.get("ok")).flatMap(_.objOpt)
    val set = ok.flatMap(_.get("tools")).flatMap(_.strOpt).flatMap(id => ToolSets.sets.find(s => grit.core.tool.ToolSetId.value(s.id) == id))
    val ids = ok.flatMap(_.get("prompt")).flatMap(_.arrOpt).fold(Vector.empty[String])(_.toVector.flatMap(_.strOpt))
    val fragments = ids.flatMap(id => Prompts.fragments.find(f => grit.core.prompt.FragmentId.value(f.id) == id))
    if (offered.isEmpty) ujson.Obj()
    else
      ujson.Obj(
        "toolSets" -> ujson.Arr.from(set.toVector.map(ToolSet.write)),
        "fragments" -> ujson.Arr.from(fragments.map(Fragment.write))
      )
  }

  /** Keeps in the fixture stores the rows a history holds ([[keptBy]]'s form). */
  def keepAll(kept: ujson.Obj): Either[String, Unit] = {
    val sets = kept.value.get("toolSets").flatMap(_.arrOpt).fold(Vector.empty[ujson.Value])(_.toVector)
    val fragments = kept.value.get("fragments").flatMap(_.arrOpt).fold(Vector.empty[ujson.Value])(_.toVector)
    for {
      read <- sets.foldLeft[Either[String, Vector[ToolSet]]](Right(Vector.empty))((acc, v) => acc.flatMap(d => ToolSet.read(v).map(d :+ _)))
      frags <- fragments.foldLeft[Either[String, Vector[Fragment]]](Right(Vector.empty)) { (acc, v) =>
        acc.flatMap { d =>
          (for {
            o <- v.objOpt
            layer <- o.get("layer").flatMap(_.strOpt).flatMap(grit.core.prompt.Layer.of)
            source <- o.get("source").flatMap(_.strOpt)
            text <- o.get("text").flatMap(_.strOpt)
          } yield Fragment(layer, source, text)).map(d :+ _).toRight("a kept fragment does not read")
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
    @caps.unsafe.untrackedCaptures
    var armed = true

    def insert(entry: Entry)(using Tx^): Either[StoreError, Unit] =
      if (armed && when(entry)) { armed = false; throw new InMemoryDurable.Crash }
      else underlying.insert(entry)
    def get(id: EntryId)(using Tx^): Either[StoreError, Option[Entry]] = underlying.get(id)
    def list(c: ConversationId)(using Tx^): Either[StoreError, Vector[Entry]] = underlying.list(c)
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

  /** The conversation as a transcript: every entry but the windows' records. */
  def texts(entries: EntryStore): Vector[String] =
    all(entries).flatMap {
      _.payload match {
        case Payload.Message(Message.User(text)) => Some(s"user: $text")
        case Payload.Message(Message.Assistant(blocks, _, _, _, _)) =>
          Some(blocks.collect { case AssistantBlock.Text(t) => s"assistant: $t" }.mkString)
        case Payload.Message(other) => Some(other.toString)
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
        case Payload.Ask(call, shown) => Some(s"ask: ${grit.core.id.ToolCallId.value(call)}: $shown")
        case Payload.Closed(_, _, closing) => Some(s"closed: ${closing.flows.prose}")
        case Payload.Window(_, _, _) | Payload.Topic(_) => None
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
      new LinearAssembler(entries, NoPeriods, new InMemoryPrincipals, CharEstimate, LinearAssembler.DefaultBudget)
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
      Assignment(ModelRef(ModelId.of(s"test/$name").getOrElse(throw new java.lang.AssertionError(name)), None), budget, None)
    Catalog.of(Policy(role("turn", 4096), role("summary", 1024), role("query", 1024)), Vector.empty)
  }

  /** Models for a test: [[TestCatalog]] is in force, and the summary's calls go to
    * `summary`, every other role's to `turn`.
    */
  final class FixedModels(turn: Provider^, summary: Provider^) extends Models {
    def catalog(): Either[String, Catalog] = Right(TestCatalog)
    def provider(pinned: Pinned): Provider^ =
      if (pinned.assignment == TestCatalog.policy.summary) summary else turn
  }

  /** The turn's workflow body over `entries` and `provider`, windowed by `assembler`. */
  def turnBodyWith(
      entries: EntryStore,
      provider: Provider^,
      assembler: ContextAssembler^,
      ledger: UsageLedger
  )(
      id: WorkflowId
  )(using Durable^): String =
    Turn.body(
      TurnEnv(
        TurnRecords(entries, ledger, CharEstimate, new InMemoryModelProfileStore, new InMemoryPrincipals),
        hosting(),
        assembler,
        NoClassifier,
        new FixedModels(provider, new StubProvider()),
        FakeDb,
        new NoWait,
        Fresh.random()
      ),
      TurnTooling[{NoCheckout}](noTools, Vector.empty, new FakeJot, budget(5))
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
          new InMemoryModelProfileStore,
          principals
        ),
        hosting(voices = voices),
        new LinearAssembler(entries, NoPeriods, new InMemoryPrincipals, CharEstimate, LinearAssembler.DefaultBudget),
        NoClassifier,
        new FixedModels(turn, summary),
        FakeDb,
        new NoWait,
        Fresh.random()
      ),
      TurnTooling[{NoCheckout}](noTools, Vector.empty, new FakeJot, budget(5))
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
    grit.core.topic.Topics.fold(Vector.empty, all(entries).flatMap(e => grit.core.store.EntryTopics.events(e.payload)))

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
      clock: Clock^ = new NoWait
  )(id: WorkflowId)(using Durable^): String =
    tooledBody(entries, provider, ledger, summarizer, classifier, NoCheckout, noTools, 5, clock)(
      id
    )

  /** No tools offered: the loop's first call is the plain request, and answers. */
  def noTools: Toolbox[{NoCheckout}] =
    Toolbox.of[{NoCheckout}]().fold(d => throw new java.lang.AssertionError(d), identity)

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
      tools: Toolbox[{ws}],
      calls: Int,
      clock: Clock^ = new NoWait,
      hosted: Vector[Tool.Offered] = Vector.empty
  )(id: WorkflowId)(using Durable^): String =
    Turn.body(
      TurnEnv(
        TurnRecords(entries, ledger, CharEstimate, new InMemoryModelProfileStore, new InMemoryPrincipals),
        hosting(),
        new LinearAssembler(entries, NoPeriods, new InMemoryPrincipals, CharEstimate, LinearAssembler.DefaultBudget),
        classifier,
        new FixedModels(provider, summarizer),
        FakeDb,
        clock,
        Fresh.random()
      ),
      TurnTooling[{ws}](tools, hosted, new FakeJot, budget(calls))
    )(id)

  /** The turn's workflow body over `entries`, its models `models` and its profile kept in
    * `profiles`, the loop offered `tools` over `ws` for at most `calls` model calls.
    */
  def modelsBody(
      entries: EntryStore,
      models: Models^,
      profiles: ModelProfileStore,
      ws: Workspace^,
      tools: Toolbox[{ws}],
      calls: Int = 5
  )(id: WorkflowId)(using Durable^): String =
    Turn.body(
      TurnEnv(
        TurnRecords(entries, new InMemoryUsageLedger, CharEstimate, profiles, new InMemoryPrincipals),
        hosting(),
        new LinearAssembler(entries, NoPeriods, new InMemoryPrincipals, CharEstimate, LinearAssembler.DefaultBudget),
        NoClassifier,
        models,
        FakeDb,
        new NoWait,
        Fresh.random()
      ),
      TurnTooling[{ws}](tools, Vector.empty, new FakeJot, budget(calls))
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
}
