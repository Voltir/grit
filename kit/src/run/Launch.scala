package grit.kit.run

import java.time.Instant

import grit.act.moves.{MoveRecords, MovesEnv}
import grit.assembly.estimate.CharEstimate
import grit.assembly.linear.LinearAssembler
import grit.assembly.retrieval.RetrievalAssembler
import grit.core.classify.Classifier
import grit.core.clock.{Clock, Fresh}
import grit.core.context.ContextAssembler
import grit.core.id.{JobName, PluginName, ShadowName, TurnRef, WorkflowId}
import grit.core.job.{NotOwn, ScheduleDesk, ScheduleStore}
import grit.core.message.Message
import grit.core.model.{Catalog, ModelSettings, Pinned}
import grit.core.period.{LifecycleSettings, Probability}
import grit.core.persona.Persona
import grit.core.place.Weight
import grit.core.plugin.Unneeded
import grit.core.provider.{ModelRequest, Models, Provider, ProviderError}
import grit.core.stitch.StitchReads
import grit.core.store.{Askers, Db, Jot, LifecycleStore, StoreError}
import grit.core.tool.{DuplicateName, Tool, ToolName, Toolbox}
import grit.core.visibility.Subject
import grit.dbos.engine.Engine
import grit.job.clock.ClockEdge
import grit.job.run.{RunEnv, RunRecords}
import grit.kit.deployment.{Assembly, Deployment, Desks, Offered, PluginBinding, Topics}
import grit.kit.environment.Secrets
import grit.lifecycle.close.{Close, CloseEnv, CloseRecords}
import grit.lifecycle.post.{PostEnv, Posting}
import grit.lifecycle.settle.{Settle, SettleEnv, SettleRecords}
import grit.lifecycle.shadow.{Shadow, ShadowAsking, ShadowEnv, ShadowVariant}
import grit.lifecycle.stitch.{Stitch, StitchEnv}
import grit.lifecycle.triage.{Mentions, Triage, TriageEnv, TriageRecords, TriageSpeech}
import grit.models.{JevClassifier, JevConfig, OpenRouterModels, Seed, StubClassifier, StubModels}
import grit.tools.{About, Cleared, Coding, Probes, Tuning}
import grit.turn.{Turn, TurnEnv, TurnHosting, TurnRecords, TurnTally, TurnTooling}

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
    * they stand after), its declared schedules made the stored ones ([[reconcile]]), and its
    * workflows launched on it, each job's run by `d.allJobs`, sweeping every `d.sweep` and
    * starting what the schedules have waiting every [[ClockEdge.Every]] when `sweeping`
    * (otherwise its caller sweeps it, and nothing starts a schedule's slot): the assembler reads
    * its stores, and the models its kept model settings, OpenRouter's under `d.policy` when `s`
    * holds its key, the stub's otherwise. Each plugin's tools write its schedules through a
    * desk of its own jobs. Throws when the settings or the declared schedules cannot be
    * written, when the
    * seed catalog cannot be read or the tools repeat a name (faults of the build no setting
    * can cause), or when the kept model settings cannot be read. `finished` is told each
    * turn's [[TurnTally.line]] when its workflow's body returns (again if a recovered turn's
    * body returns again), or, when the tally cannot be read, what the body returned and why.
    * Its workflows, the clock edge and the declared schedules tell the time by
    * [[Engine.clock]], as the sweep and the plugins' desks do.
    */
  def apply(
      engine: Engine^,
      d: Deployment,
      s: Secrets,
      run: Run,
      sweeping: Boolean,
      finished: String => Unit
  ): Engine^{engine} = {
    val reached: Models = s.openRouter match {
      case None => new StubModels(run.stubDelay)
      case Some(key) =>
        val seed = Seed.catalog.fold(why => throw new IllegalStateException(why), identity)
        new OpenRouterModels(key, seed.withPolicy(d.policy), engine.db, engine.modelSettings)
    }
    asking(
      engine,
      d,
      s,
      if (run.announced) announced(reached) else reached,
      classifier(d, s),
      weighing(d, s),
      sweeping,
      finished
    )
  }

  /** As [[apply]], but every model call is made through `models`, and every question the
    * topics and the turn's weighing ask goes to `classifier` and `weighing`: a launch whose calls
    * a test counts, and whose time it sets through the clock it opened `engine` with. A shadow still asks the classifier `s` and `d` name.
    */
  private[grit] def asking(
      engine: Engine^,
      d: Deployment,
      s: Secrets,
      models: Models,
      classifier: Classifier^,
      weighing: Classifier^,
      sweeping: Boolean,
      finished: String => Unit
  ): Engine^{engine} = {
    val clock: Clock^{engine} = engine.clock
    declare(engine.lifecycle, engine.jot, d.lifecycle) match {
      case Left(error) =>
        throw new IllegalStateException(s"the lifecycle's settings could not be written: $error")
      case Right(inForce) =>
        org.slf4j.LoggerFactory
          .getLogger("grit.launch")
          .info(s"lifecycle settings: ${written(inForce)}")
    }
    // Before anything runs, so the clock edge's first pass sees the schedules as declared.
    reconcile(engine.schedules, engine.jot, d, clock.now()).left.foreach { error =>
      throw new IllegalStateException(s"the declared schedules could not be written: $error")
    }
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
          engine.documents,
          engine.stitches,
          writer,
          CharEstimate,
          budget,
          tail,
          grit.core.stitch.Tuning.Default
        )
      case Assembly.Linear(budget) =>
        new LinearAssembler(engine.entries, engine.periods, engine.principals, CharEstimate, budget)
    }
    def launch[C^](tooling: TurnTooling[C]^): Unit = {
      val turnEnv =
        TurnEnv(
          TurnRecords(
            engine.entries,
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
          assembler,
          classifier,
          models,
          engine.db,
          clock,
          Fresh.random(),
          grit.turn.TurnSpeech(d.speaking, engine.speech, engine.deliveries),
          grit.turn.TurnStitching(
            engine.stitches,
            engine.search,
            engine.lifecycle,
            grit.core.stitch.Tuning.Default,
            engine.placements
          ),
          grit.turn.TurnWeighing(
            engine.triage,
            new Mentions(
              grit.core.stitch.StitchReads(
                engine.entries,
                engine.conversations,
                engine.lifecycle,
                engine.stitches,
                engine.search,
                engine.principals
              ),
              engine.rooms,
              d.knowledge,
              d.triage,
              weighing,
              engine.placements,
              engine.db,
              clock,
              CharEstimate,
              grit.core.stitch.Tuning.Default
            )
          )
        )
      engine.launch(
        // Told after the body returns, outside any step: the turn's steps are unchanged.
        id => d ?=> told(engine, id, Turn.body(turnEnv, tooling)(id)(using d), finished),
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
              engine.triage,
              engine.conversations
            ),
            classifier,
            models,
            engine.db,
            clock
          )
        ),
        Settle.body(
          SettleEnv(
            SettleRecords(engine.entries, engine.periods, engine.lifecycle, engine.principals),
            classifier,
            engine.db,
            clock
          )
        ),
        Posting.body(
          d.plugins,
          PostEnv(
            engine.periods,
            engine.cursors,
            engine.cache,
            engine.keeper,
            engine.tombstones,
            engine.jot,
            clock
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
              engine.acknowledgements,
              engine.deliveries,
              engine.stitches,
              engine.search,
              engine.lifecycle,
              engine.rooms
            ),
            classifier,
            engine.db,
            clock,
            TriageSpeech(
              d.speaking,
              d.budget,
              turn => engine.inbox.startTurn(turn).left.map(_.toString)
            ),
            grit.core.stitch.Tuning.Default,
            engine.placements,
            d.knowledge,
            d.triage
          )
        ),
        Stitch.body(
          StitchEnv(
            StitchReads(
              engine.entries,
              engine.conversations,
              engine.lifecycle,
              engine.stitches,
              engine.search,
              engine.principals
            ),
            classifier,
            engine.db,
            clock,
            grit.core.stitch.Tuning.Default
          )
        ),
        d.plugins,
        Shadow.body(
          ShadowEnv(
            StitchReads(
              engine.entries,
              engine.conversations,
              engine.lifecycle,
              engine.stitches,
              engine.search,
              engine.principals
            ),
            engine.rooms,
            engine.shadows,
            variants(d, s),
            d.knowledge,
            engine.db,
            clock,
            grit.core.stitch.Tuning.Default
          )
        ),
        d.shadows.map(_.shadowing),
        grit.job.run.Run.body(
          RunEnv(
            RunRecords(engine.entries, engine.conversations, engine.schedules, engine.deliveries),
            engine.db,
            engine.jot,
            clock,
            MovesEnv(
              MoveRecords(
                engine.ledger,
                engine.spending,
                engine.requests,
                engine.edgeDirectory,
                engine.toolSets,
                engine.schedules,
                engine.askers,
                CharEstimate,
                engine.savepoints
              ),
              models,
              engine.db,
              clock
            ),
            d.budget
          ),
          d.allJobs
        )
      )
      if (sweeping) {
        engine.sweepEvery(d.sweep)
        ticking(engine, new ClockEdge(engine.inbox, engine.schedules, engine.db, clock, d.allJobs))
      }
    }
    val store: Db^ = engine.db
    // Each plugin's desk, of its own jobs: built per call, since it keeps nothing but the
    // engine's transactions and its clock.
    val desks: Desks^{engine} = new Desks {
      def of(plugin: PluginName, jobs: Vector[JobName]): ScheduleDesk^ =
        engine.desk(plugin, jobs)
    }
    // Every plugin's tools, each bound over its own documents and its needs' services.
    val plugged = PluginBinding
      .bound(d.plugins, engine.reads)
      .fold(
        {
          // Deployment.of refuses a tool asking for a plugin its own does not list, or booking
          // a job not its plugin's.
          case u: Unneeded =>
            throw new IllegalStateException(
              s"plugin ${PluginName.value(u.plugin)} asks for ${PluginName.value(u.dependency)}, which it does not need"
            )
          case n: NotOwn =>
            throw new IllegalStateException(
              s"plugin ${PluginName.value(n.plugin)} books ${JobName.value(n.job)}, which is not its job"
            )
        },
        identity
      )
    // The engine's own tools touch no file: they read grit's store, keep a model setting, probe a
    // model. The coding tools are hosted: offered here, run by the edge serving the
    // conversation's directory (ADR 0017).
    // Offered everywhere: grit's own, and every plugin's tools.
    val everyone: Either[DuplicateName, Toolbox[caps.CapSet^{store, desks}]] =
      Toolbox.of[caps.CapSet^{store, desks}](
        (Vector[Tool.Offered^{store, desks}](
          own(d.persona, engine.askers, store)*
        ) ++
          plugged.map(_.over(store, desks)))*
      )
    val launching = d.offer.tools match {
      case Offered.Read =>
        everyone.map(tools =>
          launch(
            TurnTooling[caps.CapSet^{store, desks}](
              tools,
              Toolbox.Empty,
              Coding.readOnlyHosted,
              engine.jot,
              d.offer.rounds,
              worksIn = d.worksIn,
              reaches = d.reaches,
              recipe = d.recipe,
              knowledge = d.knowledge,
              persona = d.persona
            )
          )
        )
      case Offered.All =>
        // Offered only to the operator (Audience.operator): they tune grit, and ask first.
        val tuned = new KeptModelSettings(engine.jot, engine.modelSettings, clock)
        (
          everyone,
          Toolbox.of[caps.CapSet^{tuned, models}](operator(tuned, models)*)
        ) match {
          case (Right(tools), Right(operator)) =>
            // Refused here, at start, rather than as a failed offer on some turn.
            Toolbox.joined[caps.CapSet^{tuned, models, store, desks}](tools, operator).map { _ =>
              launch(
                TurnTooling[caps.CapSet^{tuned, models, store, desks}](
                  tools,
                  operator,
                  Coding.hosted,
                  engine.jot,
                  d.offer.rounds,
                  worksIn = d.worksIn,
                  reaches = d.reaches,
                  recipe = d.recipe,
                  knowledge = d.knowledge,
                  persona = d.persona
                )
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

  /** grit's own tools, offered in every conversation: what grit is, as `persona` is told it
    * ([[About]]), and what the person asking is cleared for, its turn's asker as `askers`
    * resolves them, explained as read through `store` ([[Cleared]]); the
    * kit, and the turn it launches, which reads them in its dispatch, alone hold the engine's
    * askers, so no plugin's tool can read who asked. Throws when
    * grit's shipped docs cannot be read, a fault of the build.
    */
  private[run] def own(
      persona: Persona,
      askers: Askers,
      store: Db^
  ): Vector[Tool.Offered^{store}] = {
    val about: Tool[Option[About.Subject]] =
      About.load(persona).fold(why => throw new IllegalStateException(why), t => t)
    Vector[Tool.Offered^{store}](about, Cleared.tool(askers, store))
  }

  /** The operator's tools: a measured model setting proposed, kept through `tuned` once a person
    * approves it ([[Tuning]]), and a battery of calls measuring a model through `models`
    * ([[Probes]]).
    */
  private[run] def operator(
      tuned: ModelSettings^,
      models: Models^
  ): Vector[Tool.Offered^{tuned, models}] =
    Vector[Tool.Offered^{tuned, models}](Tuning.propose(tuned), Probes.probe(models))

  /** `said`, what the body of the workflow `id` returned, once `finished` is told its
    * [[TurnTally.line]], or `said` and why the tally could not be read.
    */
  private def told(
      engine: Engine^,
      id: WorkflowId,
      said: String,
      finished: String => Unit
  ): String = {
    finished(TurnRef.fromWorkflowId(id) match {
      case None => said
      case Some(turn) =>
        engine.db
          .read(Subject.Turn(turn))(
            TurnTally.read(engine.conversations, engine.entries, engine.ledger, turn, said)
          )
          .fold(why => s"turn ${WorkflowId.value(id)}: $said; not tallied: $why", _.line)
    })
    said
  }

  /** `declared` written over the settings `lifecycle` keeps, through `jot`; the settings in
    * force after, read back.
    */
  private[run] def declare(
      lifecycle: LifecycleStore,
      jot: Jot,
      declared: LifecycleSettings
  ): Either[StoreError, LifecycleSettings] =
    jot.write(Subject.Public)(lifecycle.set(declared).flatMap(_ => lifecycle.current()))

  /** `d`'s declared schedules made the stored ones as of `now` ([[ScheduleStore.declare]]),
    * through `jot`.
    */
  private[grit] def reconcile(
      schedules: ScheduleStore,
      jot: Jot,
      d: Deployment,
      now: Instant
  ): Either[StoreError, Unit] =
    jot.write(Subject.Public)(schedules.declare(d.declared, now))

  /** `edge`'s passes run every [[ClockEdge.Every]] on `engine` ([[Engine.every]], as
    * `grit.clock`): a pass that cannot read the schedules is logged, and so is each schedule
    * the inbox failed to start, which the next pass tries again.
    */
  private def ticking(engine: Engine^, edge: ClockEdge^): Unit = {
    val log = org.slf4j.LoggerFactory.getLogger("grit.clock")
    engine.every("grit.clock", ClockEdge.Every) { () =>
      edge.tick() match {
        case Left(e) => log.warn(s"the schedules could not be read: $e")
        case Right(ticked) =>
          ticked.failed.foreach((id, e) =>
            log.warn(s"schedule ${grit.core.id.ScheduleId.value(id)} not started: $e")
          )
      }
    }
  }

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

  /** [[classifier]] for a mention's weighing: Jev's requests time out at
    * [[Mentions.AskWithin]], so a call the weighing gave up on ends there too.
    */
  private def weighing(d: Deployment, s: Secrets): Classifier^ = (d.topics, s.jev) match {
    case (Topics.Jev, Some(config)) =>
      new JevClassifier(
        config.copy(timeout = java.time.Duration.ofMillis(Mentions.AskWithin.toMillis))
      )
    case (Topics.Jev, None) => Classifier.none("JEV_API_KEY is not set")
    case (Topics.Stub, _) => new StubClassifier
    case (Topics.Off(reason), _) => Classifier.none(reason)
  }

  /** Each of `d`'s shadows by name, asking its question of the classifier `d` places topics
    * with ([[classifier]]): Jev's of the shadow's model when it names one.
    */
  private def variants(d: Deployment, s: Secrets): Map[ShadowName, ShadowAsking^] =
    asked(d.topics, s.jev, d.shadows.toList)

  // Recursive rather than a `map`: each classifier is a capability made per variant, which
  // a lambda's result cannot carry out into the map.
  private def asked(
      topics: Topics,
      jev: Option[JevConfig],
      shadows: List[ShadowVariant]
  ): Map[ShadowName, ShadowAsking^] =
    shadows match {
      case Nil => Map.empty
      case v :: rest =>
        val others = asked(topics, jev, rest)
        (topics, jev) match {
          case (Topics.Jev, Some(config)) =>
            val model = v.model.getOrElse(config.model)
            others.updated(
              v.name,
              ShadowAsking(v.questions, model, new JevClassifier(config.copy(model = model)))
            )
          case (Topics.Stub, _) =>
            others.updated(
              v.name,
              ShadowAsking(v.questions, v.model.getOrElse(StubClassifier.Model), new StubClassifier)
            )
          case (Topics.Jev, None) =>
            others.updated(
              v.name,
              ShadowAsking(
                v.questions,
                v.model.getOrElse(JevConfig.DefaultModel),
                Classifier.none("JEV_API_KEY is not set")
              )
            )
          // Deployment.of refuses shadows with topics off.
          case (Topics.Off(reason), _) =>
            others.updated(v.name, ShadowAsking(v.questions, "none", Classifier.none(reason)))
        }
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
