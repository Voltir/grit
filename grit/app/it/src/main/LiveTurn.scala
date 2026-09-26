package grit.app.main

import scala.concurrent.duration.FiniteDuration

import grit.assembly.estimate.CharEstimate
import grit.assembly.linear.LinearAssembler
import grit.core.clock.{Clock, Fresh}
import grit.core.id.{SourceId, TurnRef}
import grit.core.message.{AssistantBlock, Message}
import grit.core.model.{Catalog, Fact, FactBook, Pinned}
import grit.core.provider.{Delta, ModelRequest, Models, Provider, ProviderError}
import grit.core.store.{EntryStore, Origin, Payload}
import grit.dbos.engine.Engine
import grit.host.{LocalEdits, LocalShell, LocalWorkspace}
import grit.lifecycle.close.{Close, CloseEnv, CloseRecords}
import grit.lifecycle.post.{PostEnv, Posting}
import grit.lifecycle.settle.{Settle, SettleEnv, SettleRecords}
import grit.models.{StubModels, StubProvider}
import grit.tools.Coding
import grit.turn.{Turn, TurnEnv, TurnLoop, TurnRecords, TurnTooling, TurnTools}

/** The real turn over a live engine, with the stub provider, for the end-to-end tests. */
object LiveTurn {

  val Origin: grit.core.store.Origin = grit.core.store.Origin.Task("live", "gate")

  /** The stub provider, counting its calls through either entry point. */
  final class CountingProvider extends Provider {
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
      if (pinned.assignment == StubModels.Catalog.policy.turn) turn else rest
  }

  /** A fact book that keeps nothing: the live tests offer no tool that proposes one. */
  object NoFacts extends FactBook {
    def keep(fact: Fact): Either[String, Unit] = Left("the live tests keep no facts")
  }

  /** Launches `engine` with the turn over `entries` and `provider`, summarised by the
    * stub, which `provider` does not count.
    */
  def launch(engine: Engine^, entries: EntryStore, provider: Provider^): Unit =
    launchIn(engine, entries, provider, java.nio.file.Path.of("").toAbsolutePath)

  /** As [[launch]], the model offered the coding tools over the checkout at `root`: all of
    * them when `all`, each gated call waiting `answerWithin` for its answer; otherwise the
    * read-only ones.
    */
  def launchIn(
      engine: Engine^,
      entries: EntryStore,
      provider: Provider^,
      root: java.nio.file.Path,
      all: Boolean = false,
      answerWithin: FiniteDuration = TurnTools.AnswerWithin
  ): Unit = {
    val checkout = new LocalWorkspace(root)
    val models = new LiveModels(provider)
    def launch(tooling: TurnTooling^): Unit =
      engine.launch(
        Turn.body(
          TurnEnv(
            "You are grit.",
            TurnRecords(entries, engine.ledger, CharEstimate, engine.profiles),
            new LinearAssembler(
              entries,
              engine.periods,
              CharEstimate,
              LinearAssembler.DefaultBudget
            ),
            grit.core.classify.Classifier.none("no classifier"),
            models,
            engine.db,
            Clock.system(),
            Fresh.random()
          ),
          tooling
        ),
        Close.body(
          CloseEnv(
            CloseRecords(
              entries,
              engine.periods,
              engine.lifecycle,
              engine.ledger,
              engine.tombstones,
              CharEstimate
            ),
            grit.core.classify.Classifier.none("no classifier"),
            models,
            engine.db,
            Clock.system()
          )
        ),
        Settle.body(
          SettleEnv(
            SettleRecords(entries, engine.periods, engine.lifecycle),
            grit.core.classify.Classifier.none("no classifier"),
            engine.db,
            Clock.system()
          )
        ),
        Posting
          .body(
            Vector.empty,
            PostEnv(engine.periods, engine.cursors, engine.cache, engine.jot, Clock.system())
          ),
        Vector.empty
      )
    val budget = TurnLoop.Budget.of(5).fold(why => sys.error(why), identity)
    if (all) {
      val edits = new LocalEdits(root)
      val shell = new LocalShell(root, sys.env)
      launch(
        TurnTooling.Full(
          checkout,
          edits,
          shell,
          NoFacts,
          models,
          engine.db,
          Coding.all(checkout, edits, shell).fold(d => sys.error(d.toString), identity),
          engine.jot,
          budget,
          answerWithin
        )
      )
    } else
      launch(
        TurnTooling
          .ReadOnly(
            checkout,
            engine.db,
            Coding.readOnly(checkout).fold(d => sys.error(d.toString), identity),
            engine.jot,
            budget
          )
      )
  }

  /** Ingests `source` and starts its turn. */
  def say(engine: Engine^, source: String): TurnRef = {
    val started = for {
      turn <- engine.inbox.ingest(Origin, SourceId(source), Message.User(s"message $source"))
      _ <- engine.inbox.startTurn(turn)
    } yield turn
    started.fold(e => sys.error(s"inbox: $e"), identity)
  }

  /** The text of `turn`'s recorded reply, if it has one. */
  def reply(engine: Engine^, turn: TurnRef): Option[String] =
    engine.db.read(engine.entries.get(Turn.replyId(turn))).toOption.flatten.map {
      _.payload match {
        case Payload.Message(Message.Assistant(blocks, _, _, _, _)) =>
          blocks.collect { case AssistantBlock.Text(t) => t }.mkString
        case other => s"not a reply: $other"
      }
    }
}
