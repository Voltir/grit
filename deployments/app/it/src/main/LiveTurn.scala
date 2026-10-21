package grit.app.main

import scala.concurrent.duration.FiniteDuration

import grit.assembly.estimate.CharEstimate
import grit.assembly.linear.LinearAssembler
import grit.core.clock.{Clock, Fresh}
import grit.core.id.{PrincipalId, SourceId, TurnRef}
import grit.core.identity.Account
import grit.core.message.{AssistantBlock, Message}
import grit.core.model.{Catalog, ModelSetting, ModelSettings, Pinned}
import grit.core.place.{Directory, Reaches}
import grit.core.provider.{Delta, ModelRequest, Models, Provider, ProviderError}
import grit.core.recipe.TurnRecipe
import grit.core.stitch.StitchReads
import grit.core.store.{EntryStore, Origin, Payload}
import grit.core.tool.{ToolSet, Toolbox}
import grit.core.triage.Corpora
import grit.core.visibility.Subject
import grit.dbos.engine.Engine
import grit.edge.Server
import grit.kit.deployment.Offered
import grit.lifecycle.close.{Close, CloseEnv, CloseRecords}
import grit.lifecycle.post.{PostEnv, Posting}
import grit.lifecycle.settle.{Settle, SettleEnv, SettleRecords}
import grit.lifecycle.stitch.{Stitch, StitchEnv}
import grit.lifecycle.triage.{Triage, TriageEnv, TriageRecords}
import grit.models.{StubClassifier, StubModels, StubProvider}
import grit.tools.Coding
import grit.turn.{Turn, TurnEnv, TurnHosting, TurnLoop, TurnRecords, TurnTooling, TurnTools}

/** The real turn over a live engine, with the stub provider, for the end-to-end tests: a
  * TUI session in the checkout the engine was launched over, whose coding tools an edge in
  * this process serves, through requests, the host rule and `Edges.authorize` as grit's
  * own edge does.
  */
object LiveTurn {

  /** The session a launched engine's turns are in: in the checkout it was launched over. */
  // Written once, by the launch, before any turn reads it; volatile, so each turn sees that write.
  @volatile @caps.unsafe.untrackedCaptures
  private var session: Origin = Origin.Task("live", "unlaunched")

  /** The conversation's origin: a TUI session named `gate` in `root`, its links resolved. */
  def origin(root: java.nio.file.Path): Origin =
    Origin.Tui(
      Directory.of(root.toRealPath().toString).fold(e => sys.error(e), identity),
      "gate"
    )

  /** The stub provider, counting its calls through either entry point. */
  final class CountingProvider extends Provider {
    // A counter only the test's assertions read; the provider's two entry points bump it.
    @caps.unsafe.untrackedCaptures
    var calls = 0

    def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] = {
      calls += 1
      new StubProvider().complete(request)
    }

    override def stream(
        request: ModelRequest,
        onDelta: Delta => Unit
    ): Either[ProviderError, Message.Assistant] = {
      calls += 1
      new StubProvider().stream(request, onDelta)
    }
  }

  /** The stub's catalog ([[StubModels.Catalog]]): the turn's calls go to `turn`, every
    * other role's to the stub, uncounted.
    */
  final class LiveModels(turn: Provider^) extends Models {
    private val rest = new StubProvider()
    def catalog(): Either[String, Catalog] = Right(StubModels.Catalog)
    def provider(pinned: Pinned): Provider^ =
      if (pinned.assignment == StubModels.Catalog.policy.turn.first.assignment) turn else rest
  }

  /** A model settings book that keeps nothing: the live tests offer no tool that proposes one. */
  object NoSettings extends ModelSettings {
    def keep(setting: ModelSetting): Either[String, Unit] = Left(
      "the live tests keep no model settings"
    )
  }

  /** Launches `engine` with the turn over `entries` and `provider`, summarised by the
    * stub, which `provider` does not count.
    */
  def launch(engine: Engine^, entries: EntryStore, provider: Provider^): Unit =
    launchIn(engine, entries, provider, java.nio.file.Path.of("").toAbsolutePath)

  /** As [[launch]], the model offered the coding tools over the checkout at `root`: all of
    * them when `all`, each gated call waiting `answerWithin` for its answer; otherwise the
    * read-only ones. The session reaches the services of `reaches`, offered as `recipe`
    * shapes it by the sources of `knowledge`; a message said to grit that the recipe weighs
    * is asked of the stub classifier.
    */
  def launchIn(
      engine: Engine^,
      entries: EntryStore,
      provider: Provider^,
      root: java.nio.file.Path,
      all: Boolean = false,
      answerWithin: FiniteDuration = grit.core.tool.Gate.AnswerWithin,
      reaches: Vector[Reaches] = Vector.empty,
      knowledge: Corpora = Corpora.Empty,
      recipe: TurnRecipe = TurnRecipe.Shipped
  ): Unit =
    launchWith(
      engine,
      entries,
      new LiveModels(provider),
      root,
      all,
      answerWithin,
      reaches,
      knowledge,
      recipe
    )

  /** As [[launchIn]], every model call made through `models`. */
  def launchWith(
      engine: Engine^,
      entries: EntryStore,
      models: Models^,
      root: java.nio.file.Path,
      all: Boolean,
      answerWithin: FiniteDuration,
      reaches: Vector[Reaches],
      knowledge: Corpora,
      recipe: TurnRecipe
  ): Unit = {
    // One Mentions weighs a said root before its offer and asks its task questions in route.
    val mentions =
      new grit.lifecycle.triage.Mentions(
        grit.core.stitch.StitchReads(
          engine.entries,
          engine.conversations,
          engine.stitches,
          engine.search,
          engine.principals
        ),
        engine.rooms,
        engine.documents,
        knowledge,
        grit.lifecycle.triage.TriageQuestions.shipped(grit.core.persona.Persona.Grit),
        grit.core.classify.ClearedClassifier.of(StubClassifier.Service, new StubClassifier),
        engine.placements,
        engine.db,
        Clock.system(),
        CharEstimate,
        grit.core.stitch.Tuning.Default
      )
    // What the close's, settle's, triage's and placement's asks are made with: no classifier,
    // no cap.
    val clock: Clock^ = Clock.system()
    val unasked: grit.core.classify.ClearedClassifier^ =
      grit.core.classify.ClearedClassifier.none("no classifier")
    val moves: grit.act.moves.MovesEnv^{engine, models, unasked, clock} =
      grit.kit.run.Launch.moves(engine, engine.db, models, unasked, clock)
    val uncapped = grit.core.spend.Budget(java.time.ZoneOffset.UTC, None)
    def launch[C^](tooling: TurnTooling[C]^): Unit =
      engine.launch(
        Turn.body(
          TurnEnv(
            TurnRecords(
              entries,
              engine.ledger,
              CharEstimate,
              engine.profiles,
              engine.principals,
              engine.documents
            ),
            TurnHosting(
              engine.conversations,
              engine.prompts,
              engine.toolSets,
              engine.requests,
              engine.edgeDirectory,
              engine.voices,
              engine.askers
            ),
            grit.kit.run.Launch.moveRecords(engine),
            new LinearAssembler(
              entries,
              engine.periods,
              engine.principals,
              CharEstimate,
              LinearAssembler.DefaultBudget
            ),
            grit.core.classify.ClearedClassifier.none("no classifier"),
            models,
            engine.db,
            Clock.system(),
            Fresh.random(),
            grit.turn.TurnSpeech(grit.core.speech.Speaking.Off, engine.speech, engine.deliveries),
            grit.turn.TurnStitching(
              engine.stitches,
              engine.search,
              grit.core.stitch.Tuning.Default,
              grit.act.phase.Placing
                .Env(
                  engine.entries,
                  engine.conversations,
                  engine.db,
                  engine.placements,
                  grit.core.clock.Clock.system()
                )
            ),
            grit.turn.TurnWeighing(engine.triage, mentions),
            grit.turn.TurnRouting(mentions, engine.budget, engine.spending)
          ),
          tooling
        ),
        Close.body(
          CloseEnv(
            CloseRecords(
              entries,
              engine.periods,
              engine.lifecycle,
              engine.tombstones,
              engine.principals,
              engine.triage,
              engine.conversations
            ),
            moves,
            uncapped,
            engine.db,
            Clock.system(),
            grit.core.persona.Persona.Grit
          )
        ),
        Settle.body(
          SettleEnv(
            SettleRecords(entries, engine.periods, engine.lifecycle, engine.principals),
            engine.db,
            Clock.system(),
            moves,
            uncapped
          )
        ),
        Posting
          .body(
            Vector.empty,
            PostEnv(
              engine.periods,
              engine.cursors,
              engine.cache,
              engine.keeper,
              engine.tombstones,
              engine.jot,
              Clock.system()
            )
          ),
        Triage.body(
          TriageEnv(
            TriageRecords(
              entries,
              engine.triage,
              engine.principals,
              engine.conversations,
              engine.speech,
              engine.spending,
              engine.acknowledgements,
              engine.deliveries,
              engine.stitches,
              engine.search,
              engine.rooms,
              engine.periods,
              engine.documents
            ),
            moves,
            uncapped,
            engine.db,
            Clock.system(),
            grit.lifecycle.triage.TriageSpeech(
              grit.core.speech.Speaking.Off,
              engine.budget,
              turn => engine.inbox.startTurn(turn).left.map(_.toString)
            ),
            grit.core.stitch.Tuning.Default,
            grit.act.phase.Placing
              .Env(
                engine.entries,
                engine.conversations,
                engine.db,
                engine.placements,
                grit.core.clock.Clock.system()
              ),
            grit.core.triage.Corpora.Empty,
            grit.lifecycle.triage.TriageQuestions.shipped(grit.core.persona.Persona.Grit)
          )
        ),
        Stitch.body(
          StitchEnv(
            StitchReads(
              entries,
              engine.conversations,
              engine.stitches,
              engine.search,
              engine.principals
            ),
            moves,
            uncapped,
            engine.db,
            Clock.system(),
            grit.core.stitch.Tuning.Default
          )
        ),
        Vector.empty
      )
    val budget = TurnLoop.Rounds.of(5).fold(why => sys.error(why), identity)
    val hosted = if (all) Coding.hosted else Coding.readOnlyHosted
    val none = Toolbox.of[{}]().fold(d => sys.error(d.toString), identity)
    launch(
      TurnTooling[{}](
        none,
        Toolbox.Empty,
        hosted,
        engine.jot,
        budget,
        answerWithin,
        reaches = reaches,
        recipe = recipe,
        knowledge = knowledge
      )
    )
    // This process's edge, serving the checkout as grit's own does.
    val here = origin(root)
    session = here
    val place = here.place
    engine.register(PrincipalId.Local, Set(place)) match {
      case Left(e) => sys.error(s"no edge: ${e.why}")
      case Right(desk) =>
        desk
          .advertise(
            place,
            ToolSet.of(hosted.map(_.entry)).fold(d => sys.error(d.toString), identity),
            Vector.empty
          )
          .left
          .foreach(e => sys.error(s"not advertised: ${e.why}"))
        new Server(
          desk,
          new LocalTools(if (all) Offered.All else Offered.Read),
          run => { val _ = Thread.ofVirtual().start(() => run()) },
          _ => ()
        ).serve()
    }
  }

  /** Ingests `source` and starts its turn. */
  def say(engine: Engine^, source: String): TurnRef = {
    val started = for {
      turn <- engine.inbox.ingest(
        session,
        SourceId(source),
        Message.User(s"message $source"),
        Account.Local,
        None
      )
      _ <- engine.inbox.startTurn(turn)
    } yield turn
    started.fold(e => sys.error(s"inbox: $e"), identity)
  }

  /** The text of `turn`'s recorded reply, if it has one. */
  def reply(engine: Engine^, turn: TurnRef): Option[String] =
    engine.db.read(Subject.Public)(engine.entries.get(turn.replyId)).toOption.flatten.map {
      _.payload match {
        case Payload.Message(Message.Assistant(blocks, _, _, _, _)) =>
          blocks.collect { case AssistantBlock.Text(t) => t }.mkString
        case other => s"not a reply: $other"
      }
    }
}
