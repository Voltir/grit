package grit.app.main

import scala.concurrent.duration.FiniteDuration

import grit.assembly.estimate.CharEstimate
import grit.assembly.linear.LinearAssembler
import grit.assembly.retrieval.RetrievalAssembler
import grit.core.clock.{Clock, Fresh}
import grit.core.context.ContextAssembler
import grit.core.message.Tokens
import grit.core.model.Catalog
import grit.core.plugin.Plugin
import grit.core.provider.Models
import grit.core.store.Db
import grit.core.tool.{DuplicateName, Tool, ToolName, Toolbox}
import grit.dbos.engine.Engine
import grit.digest.Digest
import grit.lifecycle.close.{Close, CloseEnv, CloseRecords}
import grit.lifecycle.post.{PostEnv, Posting}
import grit.lifecycle.settle.{Settle, SettleEnv, SettleRecords}
import grit.models.{OpenRouterModels, StubModels}
import grit.tools.{About, Coding, Probes, Tuning}
import grit.turn.{Turn, TurnEnv, TurnHosting, TurnLoop, TurnRecords, TurnTooling}

import grit.core.period.LifecycleSettings

/** The engine's workflows, launched on an open engine the same way by every way grit runs:
  * the chat, a run with arguments, and `grit serve`.
  */
object Launch {

  /** What a run chose, as [[Main]] read it from the settings: the lifecycle's first
    * settings (`seeded`), OpenRouter's key and catalog (the stub, answering after
    * `stubDelay` ms, without one), whether this is the chat (`tui`: a run with arguments
    * prints each model call), the assembler (`retrieving`, within `budget`, keeping `tail`),
    * the topic classifier, the plugins on, how often to sweep, the tools offered and the
    * turn's model calls (`rounds`).
    */
  final case class Settings(
      seeded: LifecycleSettings,
      openRouter: Option[(String, Catalog)],
      stubDelay: Long,
      tui: Boolean,
      retrieving: Boolean,
      budget: Tokens,
      tail: Tokens,
      topics: Main.ClassifierChoice,
      plugins: Vector[Plugin],
      sweep: FiniteDuration,
      offered: Main.ToolChoice,
      rounds: TurnLoop.Budget
  )

  /** `engine` with its lifecycle's settings seeded, and its workflows launched on
    * it: the assembler reads its stores, and the models its kept model settings. Throws when the
    * settings cannot be seeded, when the coding tools repeat a name, a fault in `grit.tools`
    * that no setting can cause, or when the kept model settings cannot be read.
    */
  def apply(engine: Engine^, s: Settings): Engine^{engine} = {
    import s.*
    engine.jot.write(engine.lifecycle.seed(seeded)).left.foreach { error =>
      throw new IllegalStateException(s"the lifecycle's settings could not be seeded: $error")
    }
    val reached: Models = openRouter match {
      case None => new StubModels(stubDelay)
      case Some((key, seed)) => new OpenRouterModels(key, seed, engine.db, engine.modelSettings)
    }
    val models: Models = if (tui) reached else Main.announced(reached)
    // The query writer is built with the assembler, from the catalog as the engine opens.
    val startup = models.catalog().fold(why => throw new IllegalStateException(why), _.pin)
    val writer = models.provider(startup.query)
    val assembler: ContextAssembler^ =
      if (retrieving)
        new RetrievalAssembler(
          engine.entries,
          engine.conversations,
          engine.periods,
          engine.principals,
          engine.lifecycle,
          engine.search,
          writer,
          CharEstimate,
          budget,
          tail
        )
      else new LinearAssembler(engine.entries, engine.periods, engine.principals, CharEstimate, budget)
    def launch[C^](tooling: TurnTooling[C]^): Unit = {
      engine.launch(
        Turn.body(
          TurnEnv(
            TurnRecords(engine.entries, engine.ledger, CharEstimate, engine.profiles, engine.principals),
            TurnHosting(
              engine.conversations,
              engine.prompts,
              engine.toolSets,
              engine.requests,
              engine.edgeDirectory,
              engine.voices,
              engine.principals
            ),
            assembler,
            Main.classifier(topics),
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
              engine.entries,
              engine.periods,
              engine.lifecycle,
              engine.ledger,
              engine.tombstones,
              CharEstimate,
              engine.principals
            ),
            Main.classifier(topics),
            models,
            engine.db,
            Clock.system()
          )
        ),
        Settle.body(
          SettleEnv(
            SettleRecords(engine.entries, engine.periods, engine.lifecycle, engine.principals),
            Main.classifier(topics),
            engine.db,
            Clock.system()
          )
        ),
        Posting.body(
          plugins,
          PostEnv(
            engine.periods,
            engine.cursors,
            engine.cache,
            engine.tombstones,
            engine.jot,
            Clock.system()
          )
        ),
        plugins
      )
      engine.sweepEvery(sweep, Clock.system())
    }
    val store: Db^ = engine.db
    // Digest's recent_activity, offered when Digest is on.
    val digest = plugins.collectFirst { case d: Digest => engine.docs(d.name) }
    // The engine's own tools touch no file: they read grit's store, keep a model setting, probe a
    // model. The coding tools are hosted: offered here, run by the edge serving the
    // conversation's directory (ADR 0017).
    // What grit is, from the docs grit.tools ships; offered under either choice.
    val about: Tool[Option[About.Subject]] =
      About.load().fold(why => throw new IllegalStateException(why), t => t)
    // Offered everywhere: what grit is, and Digest's recent_activity when it is on.
    val everyone: Either[DuplicateName, Toolbox[{store}]] = digest match {
      case None => Toolbox.of[{store}](about)
      case Some(docs) => Toolbox.of[{store}](about, Digest.recentActivity(store, docs))
    }
    val launching = offered match {
      case Main.ToolChoice.Read =>
        everyone.map(tools =>
          launch(TurnTooling[{store}](tools, Toolbox.Empty, Coding.readOnlyHosted, engine.jot, rounds))
        )
      case Main.ToolChoice.All =>
        // Offered only to the operator (Origin.operator): they tune grit, and ask first.
        val tuned = new KeptModelSettings(engine.jot, engine.modelSettings, Clock.system())
        (everyone, Toolbox.of[{tuned, models}](Tuning.propose(tuned), Probes.probe(models))) match {
          case (Right(tools), Right(operator)) =>
            // Refused here, at start, rather than as a failed offer on some turn.
            Toolbox.joined[{tuned, models, store}](tools, operator).map { _ =>
              launch(
                TurnTooling[{tuned, models, store}](tools, operator, Coding.hosted, engine.jot, rounds)
              )
            }
          case (Left(repeated), _) => Left(repeated)
          case (_, Left(repeated)) => Left(repeated)
        }
    }
    // Its caller closes the engine and reports the throw: in the chat, as the engine that
    // could not open.
    launching.left.foreach { case DuplicateName(name) =>
      throw new IllegalStateException(s"grit's tools offer ${ToolName.value(name)} twice")
    }
    engine
  }
}
