package grit.app.main

import scala.concurrent.duration.FiniteDuration

import grit.assembly.estimate.CharEstimate
import grit.assembly.linear.LinearAssembler
import grit.core.clock.{Clock, Fresh}
import grit.core.id.{SourceId, TurnRef}
import grit.core.message.{AssistantBlock, Message}
import grit.core.provider.{ModelRequest, Provider, ProviderError}
import grit.core.store.{EntryStore, Origin, Payload}
import grit.dbos.engine.Engine
import grit.host.{LocalEdits, LocalShell, LocalWorkspace}
import grit.models.StubProvider
import grit.tools.Coding
import grit.turn.{Turn, TurnEnv, TurnLoop, TurnRecords, TurnTooling, TurnTools}

/** The real turn over a live engine, with the stub provider, for the end-to-end tests. */
object LiveTurn {

  val Origin: grit.core.store.Origin = grit.core.store.Origin.Task("live", "gate")

  /** The stub provider, counting its calls. */
  final class CountingProvider extends Provider {
    @caps.unsafe.untrackedCaptures
    var calls = 0

    def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] = {
      calls += 1
      new StubProvider().complete(request)
    }
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
    val edits = new LocalEdits(root)
    val shell = new LocalShell(root, sys.env)
    engine.launch(
      Turn.body(
        TurnEnv(
          "You are grit.",
          TurnRecords(entries, engine.ledger, CharEstimate),
          new LinearAssembler(entries, CharEstimate, LinearAssembler.DefaultBudget),
          grit.core.classify.Classifier.none("no classifier"),
          provider,
          new StubProvider(),
          engine.db,
          engine.jot,
          Clock.system(),
          Fresh.random(),
          TurnTooling(
            checkout,
            edits,
            shell,
            if (all) Coding.all(checkout, edits, shell) else Coding.readOnly(checkout),
            TurnLoop.Budget.of(5).fold(why => sys.error(why), identity),
            strict = false,
            answerWithin
          )
        )
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
        case Payload.Message(Message.Assistant(blocks, _, _, _)) =>
          blocks.collect { case AssistantBlock.Text(t) => t }.mkString
        case other => s"not a reply: $other"
      }
    }
}
