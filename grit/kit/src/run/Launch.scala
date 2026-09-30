package grit.kit.run

import grit.assembly.estimate.CharEstimate
import grit.assembly.linear.LinearAssembler
import grit.assembly.retrieval.RetrievalAssembler
import grit.core.classify.Classifier
import grit.core.clock.{Clock, Fresh}
import grit.core.context.ContextAssembler
import grit.core.message.Message
import grit.core.model.{Catalog, Pinned}
import grit.core.provider.{ModelRequest, Models, Provider, ProviderError}
import grit.core.period.{LifecycleSettings, Probability}
import grit.core.place.Weight
import grit.core.store.{Db, Jot, LifecycleStore, StoreError}
import grit.core.tool.{DuplicateName, Tool, ToolName, Toolbox}
import grit.dbos.engine.Engine
import grit.digest.Digest
import grit.kit.deployment.{Assembly, Deployment, Offered, Topics}
import grit.kit.environment.Secrets
import grit.lifecycle.close.{Close, CloseEnv, CloseRecords}
import grit.lifecycle.post.{PostEnv, Posting}
import grit.lifecycle.settle.{Settle, SettleEnv, SettleRecords}
import grit.lifecycle.triage.{Triage, TriageEnv, TriageRecords, TriageSpeech}
import grit.models.{JevClassifier, OpenRouterModels, Seed, StubClassifier, StubModels}
import grit.tools.{About, Coding, Probes, Tuning}
import grit.turn.{Turn, TurnEnv, TurnHosting, TurnRecords, TurnTooling}

/** The engine's workflows, launched on an open engine the same way by every way grit runs:
  * the chat, a run with arguments, `grit serve` and a catch-up.
  */
private[grit] object Launch {

  /** What a process wants beside its deployment: whether each model call is printed with
    * the model it goes to (`announced`: a run with arguments, where a replayed turn is then
    * visibly one that did not call), and how long the stub model waits before it answers, in
    * milliseconds (`stubDelay`; the stub answers only without OpenRouter's key).
    */
  final case class Run(announced: Boolean, stubDelay: Long)

  object Run {

    /** A served deployment's: nothing printed, the stub answering at once. */
    val Served: Run = Run(announced = false, stubDelay = 0)
  }

  /** `engine` with `d`'s lifecycle settings written over those its database keeps (logged as
    * they stand after), and its workflows launched on it, sweeping every
    * `d.sweep` when `sweeping` (otherwise its caller sweeps it): the assembler reads its
    * stores, and the models its kept model settings, OpenRouter's under `d.policy` when `s`
    * holds its key, the stub's otherwise. Throws when the settings cannot be written, when the
    * seed catalog cannot be read or the tools repeat a name (faults of the build no setting
    * can cause), or when the kept model settings cannot be read.
    */
  def apply(engine: Engine^, d: Deployment, s: Secrets, run: Run, sweeping: Boolean): Engine^{engine} = {
    declare(engine.lifecycle, engine.jot, d.lifecycle) match {
      case Left(error) =>
        throw new IllegalStateException(s"the lifecycle's settings could not be written: $error")
      case Right(inForce) =>
        org.slf4j.LoggerFactory.getLogger("grit.launch").info(s"lifecycle settings: ${written(inForce)}")
    }
    val reached: Models = s.openRouter match {
      case None => new StubModels(run.stubDelay)
      case Some(key) =>
        val seed = Seed.catalog.fold(why => throw new IllegalStateException(why), identity)
        new OpenRouterModels(key, seed.withPolicy(d.policy), engine.db, engine.modelSettings)
    }
    val models: Models = if (run.announced) announced(reached) else reached
    // The query writer is built with the assembler, from the catalog as the engine opens.
    val startup = models.catalog().fold(why => throw new IllegalStateException(why), _.pin)
    val writer = models.provider(startup.query)
    val assembler: ContextAssembler^ = d.assembly match {
      case Assembly.Retrieval(budget, tail) =>
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
      case Assembly.Linear(budget) =>
        new LinearAssembler(engine.entries, engine.periods, engine.principals, CharEstimate, budget)
    }
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
            classifier(d, s),
            models,
            engine.db,
            Clock.system(),
            Fresh.random(),
            grit.turn.TurnSpeech(d.speaking, engine.speech, engine.deliveries)
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
              engine.principals,
              engine.triage
            ),
            classifier(d, s),
            models,
            engine.db,
            Clock.system()
          )
        ),
        Settle.body(
          SettleEnv(
            SettleRecords(engine.entries, engine.periods, engine.lifecycle, engine.principals),
            classifier(d, s),
            engine.db,
            Clock.system()
          )
        ),
        Posting.body(
          d.plugins,
          PostEnv(
            engine.periods,
            engine.cursors,
            engine.cache,
            engine.tombstones,
            engine.jot,
            Clock.system()
          )
        ),
        Triage.body(
          TriageEnv(
            TriageRecords(
              engine.entries,
              engine.triage,
              engine.principals,
              engine.conversations,
              engine.speech,
              engine.spending,
              engine.stitches,
              engine.search,
              engine.lifecycle
            ),
            classifier(d, s),
            engine.db,
            Clock.system(),
            TriageSpeech(
              d.speaking,
              d.budget,
              turn => engine.inbox.startTurn(turn).left.map(_.toString)
            ),
            grit.core.stitch.Tuning.Default
          )
        ),
        d.plugins
      )
      if (sweeping) engine.sweepEvery(d.sweep, Clock.system())
    }
    val store: Db^ = engine.db
    // Digest's recent_activity, offered when Digest is on.
    val digest = d.plugins.collectFirst { case d: Digest => engine.docs(d.name) }
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
    val launching = d.offer.tools match {
      case Offered.Read =>
        everyone.map(tools =>
          launch(TurnTooling[{store}](tools, Toolbox.Empty, Coding.readOnlyHosted, engine.jot, d.offer.rounds))
        )
      case Offered.All =>
        // Offered only to the operator (Audience.operator): they tune grit, and ask first.
        val tuned = new KeptModelSettings(engine.jot, engine.modelSettings, Clock.system())
        (everyone, Toolbox.of[{tuned, models}](Tuning.propose(tuned), Probes.probe(models))) match {
          case (Right(tools), Right(operator)) =>
            // Refused here, at start, rather than as a failed offer on some turn.
            Toolbox.joined[{tuned, models, store}](tools, operator).map { _ =>
              launch(
                TurnTooling[{tuned, models, store}](tools, operator, Coding.hosted, engine.jot, d.offer.rounds)
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

  /** `declared` written over the settings `lifecycle` keeps, through `jot`; the settings in
    * force after, read back.
    */
  private[run] def declare(
      lifecycle: LifecycleStore,
      jot: Jot,
      declared: LifecycleSettings
  ): Either[StoreError, LifecycleSettings] =
    jot.write(lifecycle.set(declared).flatMap(_ => lifecycle.current()))

  /** `s` in one line, for the log. */
  private def written(s: LifecycleSettings): String = {
    val w = s.windows
    s"idle ${w.idle.toCoarsest}, retention ${w.retention.toCoarsest}, ledger ${w.ledger.toCoarsest}, balance ${s.balance}, " +
      s"settle ${s.settle.toCoarsest}, resolve at ${Probability.value(s.resolveAt)}, asks ${s.asks}, " +
      s"scope ${s.locality.scope.written}, weight ${Weight.value(s.locality.weight)}"
  }

  /** The classifier `d` places topics with: Jev over `s`'s settings, which [[Secrets.of]]
    * holds for [[Topics.Jev]].
    */
  private def classifier(d: Deployment, s: Secrets): Classifier^ = (d.topics, s.jev) match {
    case (Topics.Jev, Some(config)) => new JevClassifier(config)
    case (Topics.Jev, None) => Classifier.none("JEV_API_KEY is not set")
    case (Topics.Stub, _) => new StubClassifier
    case (Topics.Off(reason), _) => Classifier.none(reason)
  }

  /** `models`, each call printed with the model it goes to. */
  private def announced(models: Models): Models =
    new Models {
      def catalog(): Either[String, Catalog] = models.catalog()
      def provider(pinned: Pinned): Provider^ = {
        val model = models.provider(pinned)
        new Provider {
          def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] = {
            println(
              s"[provider] ${pinned.assignment.ref} called with ${request.messages.size} message(s)"
            )
            model.complete(request)
          }
        }
      }
    }
}
