package grit.kit.run

import java.nio.file.Path
import java.time.Instant

import grit.act.moves.{MoveRecords, MovesEnv}
import grit.act.phase.Placing
import grit.assembly.estimate.CharEstimate
import grit.assembly.linear.LinearAssembler
import grit.assembly.retrieval.RetrievalAssembler
import grit.core.act.Dreaming
import grit.core.admin.Administration
import grit.core.classify.ClearedClassifier
import grit.core.clock.{Clock, Fresh}
import grit.core.context.ContextAssembler
import grit.core.durable.Durable
import grit.core.edge.ServedEdge
import grit.core.files.{FileArea, Files, PieceKeeper}
import grit.core.id.{EdgeName, JobName, PluginName, ShadowName, TurnRef, WorkflowId}
import grit.core.job.{NotOwn, ScheduleDesk, ScheduleStore}
import grit.core.message.Message
import grit.core.model.{Catalog, ModelSettings, Pinned}
import grit.core.need.{Needs, Unneeded}
import grit.core.period.{LifecycleSettings, Probability}
import grit.core.persona.Persona
import grit.core.place.Weight
import grit.core.plugin.Plugin
import grit.core.provider.{ModelRequest, Models, Provider, ProviderError}
import grit.core.stitch.{StitchReads, StrandExcerpts, StrandShelf, Tuning as StitchTuning}
import grit.core.store.{Askers, Db, Jot, LifecycleStore, StoreError}
import grit.core.tool.{DuplicateName, Tool, ToolName, Toolbox}
import grit.core.visibility.Subject
import grit.dbos.engine.{Engine, Runtime, Stores}
import grit.dbos.sql.DbConfig
import grit.host.blobs.LocalBlobs
import grit.job.clock.ClockEdge
import grit.job.run.{Run as JobRun, RunEnv, RunRecords}
import grit.kit.deployment.{Assembly, Deployment, Desks, Offered, Placer, PluginBinding}
import grit.kit.environment.Secrets
import grit.lifecycle.close.{Close, CloseEnv, CloseRecords}
import grit.lifecycle.post.{PostEnv, Posting}
import grit.lifecycle.settle.{Settle, SettleEnv, SettleRecords}
import grit.lifecycle.shadow.{Shadow, ShadowAsking, ShadowEnv, ShadowVariant}
import grit.lifecycle.stitch.{Stitch, StitchEnv}
import grit.lifecycle.triage.{Mentions, Triage, TriageEnv, TriageRecords, TriageSpeech}
import grit.models.{JevClassifier, JevConfig, OpenRouterModels, Seed, StubClassifier, StubModels}
import grit.tools.{About, Administering, Cleared, Coding, FileReading, MessageIds, Probes, Tuning}
import grit.turn.{
  Turn,
  TurnEnv,
  TurnHosting,
  TurnRecords,
  TurnRouting,
  TurnSpeech,
  TurnStitching,
  TurnTally,
  TurnTooling,
  TurnWeighing
}

/** The engine's workflows, launched on an open engine the same way by every way grit runs:
  * the chat, a run with arguments, `grit serve` and a catch-up.
  */
private[grit] object Launch {

  private val Log = org.slf4j.LoggerFactory.getLogger("grit.launch")

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

  /** `engine`, opened on `s.database`, logged first as `database: <host:port/name>` (its
    * [[grit.dbos.sql.DbConfig.endpoint]], never its login), with `d`'s lifecycle settings written
    * over those its database keeps (logged as they stand after), its declared schedules made the stored ones ([[reconcile]]), and its
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
    * [[Engine.clock]], as the sweep and the plugins' desks do. Its answer is the file area its
    * turns read files from ([[area]]), for its edges to keep files in.
    */
  def apply(
      engine: Engine^,
      d: Deployment,
      s: Secrets,
      run: Run,
      sweeping: Boolean,
      finished: String => Unit
  ): FileArea^ = {
    val area = files(engine, d, s)
    launched(engine, d, s, run, sweeping, finished, area)
    area
  }

  /** `engine`'s file area for `d`, as [[apply]] makes it, after logging `database: <endpoint>`;
    * nothing launched, so an edge can keep files in it before [[launched]] runs anything.
    * Throws when its directory cannot be made ([[area]]).
    */
  private[grit] def files(engine: Engine^, d: Deployment, s: Secrets): FileArea^ = {
    Log.info(s"database: ${s.database.endpoint}")
    val temp = Path.of(System.getProperty("java.io.tmpdir"))
    area(engine, d, filesDir(s.files, s.database, temp))
  }

  /** `d`'s workflows launched on `engine` as [[apply]] launches them, over `files`, the area
    * [[files]] made for it. Throws as [[apply]] does.
    */
  private[grit] def launched(
      engine: Engine^,
      d: Deployment,
      s: Secrets,
      run: Run,
      sweeping: Boolean,
      finished: String => Unit,
      files: FileArea^
  ): Unit = {
    val reached: Models = s.openRouter match {
      case None => new StubModels(run.stubDelay)
      case Some(key) =>
        val seed = Seed.catalog.fold(why => throw new IllegalStateException(why), identity)
        new OpenRouterModels(key, seed.withPolicy(d.policy), engine.db, engine.modelSettings)
    }
    val temp = Path.of(System.getProperty("java.io.tmpdir"))
    asking(
      engine,
      d,
      variants(d, s),
      if (run.announced) announced(reached) else reached,
      classifier(d, s),
      weighing(d, s),
      files,
      finished
    )
    if (sweeping) {
      engine.sweepEvery(d.sweep)
      ticking(
        engine,
        new ClockEdge(engine.inbox, engine.schedules, engine.db, engine.clock, d.allJobs)
      )
      LocalBlobs.leftovers(temp) match {
        case Left(e) =>
          Log.warn(s"files: an earlier Drive edge's scratch directories were not removed: $e")
        case Right(0) => ()
        case Right(n) =>
          Log.info(s"files: $n scratch directories an earlier Drive edge left removed")
      }
      val tombstones = engine.tombstones
      engine.every("grit.files", d.sweep) { () =>
        files.sweep(tombstones) match {
          case Left(why) => Log.warn(s"files: $why")
          case Right(FileArea.Swept(Vector(), 0, 0)) => ()
          case Right(swept) =>
            Log.info(
              s"files: ${swept.marked.size} marked for deletion, ${swept.removed} blobs removed, " +
                s"${swept.cleared} pending cleared"
            )
        }
      }
    }
  }

  /** `runtime`'s file area (ADR 0045): its rows in `runtime`'s store, written in its short
    * transactions and dated by its clock; its bytes in `dir` ([[filesDir]]); kept as `d` says.
    * Throws when `dir` cannot be made: nothing a turn reads a file with, or an edge keeps one in,
    * can work.
    */
  private[grit] def area(runtime: Runtime^, d: Deployment, dir: Path): FileArea^ =
    LocalBlobs.open(dir) match {
      case Left(e) =>
        throw new IllegalStateException(s"the file area's directory $dir could not be made: $e")
      case Right(blobs) =>
        new FileArea(runtime.files, blobs, runtime.jot, runtime.clock, d.formats, d.files)
    }

  /** Where the file area of `database` keeps its bytes: `set`, or else a directory in `temp` named
    * for the database (`grit-files/<host_port_database>`).
    */
  private[grit] def filesDir(set: Option[Path], database: DbConfig, temp: Path): Path =
    set.getOrElse(
      temp
        .resolve("grit-files")
        .resolve(database.endpoint.toString.replaceAll("[^A-Za-z0-9._-]", "_"))
    )

  /** `d`'s workflows launched on `runtime` as [[apply]] launches them on an engine, but sweeping
    * nothing, every model call made through `models`, and every question the placer and a job's
    * judgments ask going to `classifier`, and the turn's weighing's to `weighing`, each as its
    * clearance allows: a launch whose calls a test counts, or a rehearsal's. `d`'s lifecycle
    * settings and schedules are written through `runtime.jot` first. Each shadow asks the
    * variant of its name in `variants`, and one it lacks asks nothing. Its turns are offered
    * the administering tools through `runtime`'s administration ([[administering]]), whatever
    * `d`'s [[Offered]]. Throws as [[apply]] does.
    */
  private[grit] def asking(
      runtime: Runtime^,
      d: Deployment,
      variants: Map[ShadowName, ShadowAsking^],
      models: Models,
      classifier: ClearedClassifier^,
      weighing: ClearedClassifier^,
      files: Files^,
      finished: String => Unit
  ): Unit = {
    val clock: Clock^{runtime} = runtime.clock
    declare(runtime.lifecycle, runtime.jot, d.lifecycle) match {
      case Left(error) =>
        throw new IllegalStateException(s"the lifecycle's settings could not be written: $error")
      case Right(inForce) =>
        Log.info(s"lifecycle settings: ${written(inForce)}")
    }
    // Before anything runs, so the clock edge's first pass sees the schedules as declared.
    reconcile(runtime.schedules, runtime.jot, d, clock.now()).left.foreach { error =>
      throw new IllegalStateException(s"the declared schedules could not be written: $error")
    }
    val db = runtime.db
    val jot = runtime.jot
    val assembler: ContextAssembler = d.assembly match {
      case Assembly.Retrieval(budget, tail) =>
        new RetrievalAssembler(
          runtime.entries,
          runtime.conversations,
          runtime.periods,
          runtime.principals,
          runtime.lifecycle,
          runtime.search,
          runtime.documents,
          runtime.pieces,
          runtime.stitches,
          CharEstimate,
          budget,
          tail,
          StitchTuning.Default
        )
      case Assembly.Linear(budget) =>
        new LinearAssembler(
          runtime.entries,
          runtime.periods,
          runtime.principals,
          CharEstimate,
          budget
        )
    }
    // What stitching reads, the placement's and a mention's.
    val reads =
      StitchReads(
        runtime.entries,
        runtime.conversations,
        runtime.stitches,
        runtime.search,
        runtime.principals
      )
    // The one wait for an opening's placement, the turn's and triage's.
    val placing = Placing.Env(runtime.entries, runtime.conversations, db, runtime.placements, clock)
    // What every move is made with: a job's, the lifecycle's and the turn's asks.
    val moves: MovesEnv^{runtime, models, classifier, clock} =
      Launch.moves(runtime, db, models, classifier, clock)
    // One Mentions both weighs a said root before its offer and asks its task questions in route.
    val mentions =
      new Mentions(
        reads,
        runtime.rooms,
        runtime.documents,
        d.knowledge,
        d.triage,
        weighing,
        runtime.placements,
        db,
        clock,
        CharEstimate,
        StitchTuning.Default
      )
    val turnEnv =
      TurnEnv(
        TurnRecords(
          runtime.entries,
          runtime.ledger,
          CharEstimate,
          runtime.profiles,
          runtime.principals,
          runtime.documents,
          runtime.pieces
        ),
        TurnHosting(
          runtime.conversations,
          runtime.prompts,
          runtime.toolSets,
          runtime.requests,
          runtime.edgeDirectory,
          runtime.voices,
          runtime.askers
        ),
        moves.records,
        assembler,
        classifier,
        models,
        db,
        clock,
        Fresh.random(),
        TurnSpeech(d.speaking, runtime.speech, runtime.deliveries),
        TurnStitching(
          runtime.stitches,
          runtime.search,
          StitchTuning.Default,
          placing
        ),
        TurnWeighing(runtime.triage, mentions),
        TurnRouting(mentions, d.budget, runtime.spending)
      )
    val close: WorkflowId => Durable^ ?=> String =
      Close.body(
        CloseEnv(
          CloseRecords(
            runtime.entries,
            runtime.periods,
            runtime.lifecycle,
            runtime.tombstones,
            runtime.principals,
            runtime.triage,
            runtime.conversations
          ),
          moves,
          d.budget,
          db,
          clock,
          d.persona
        )
      )
    val settle: WorkflowId => Durable^ ?=> String =
      Settle.body(
        SettleEnv(
          SettleRecords(runtime.entries, runtime.periods, runtime.lifecycle, runtime.principals),
          db,
          clock,
          moves,
          d.budget
        )
      )
    val post: WorkflowId => Durable^ ?=> String =
      Posting.body(
        d.plugins,
        PostEnv(
          runtime.periods,
          runtime.cursors,
          runtime.cache,
          runtime.keeper,
          runtime.tombstones,
          jot,
          clock
        )
      )
    val triage: WorkflowId => Durable^ ?=> String =
      Triage.body(
        TriageEnv(
          TriageRecords(
            runtime.entries,
            runtime.triage,
            runtime.principals,
            runtime.conversations,
            runtime.speech,
            runtime.spending,
            runtime.acknowledgements,
            runtime.deliveries,
            runtime.stitches,
            runtime.search,
            runtime.rooms,
            runtime.periods,
            runtime.documents
          ),
          moves,
          d.budget,
          db,
          clock,
          TriageSpeech(
            d.speaking,
            d.budget,
            turn => runtime.startTurn(turn).left.map(_.toString)
          ),
          StitchTuning.Default,
          placing,
          d.knowledge,
          d.triage
        )
      )
    val stitch: WorkflowId => Durable^ ?=> String =
      Stitch.body(StitchEnv(reads, moves, d.budget, db, clock, StitchTuning.Default))
    val shadow: WorkflowId => Durable^ ?=> String =
      Shadow.body(
        ShadowEnv(
          reads,
          runtime.rooms,
          runtime.documents,
          runtime.shadows,
          variants,
          d.knowledge,
          db,
          clock,
          StitchTuning.Default,
          moves,
          d.budget
        )
      )
    val needs: Map[PluginName, Needs] =
      d.plugins.map(p => p.name -> Plugin.needsOf(p, runtime.reads)).toMap
    val runs: WorkflowId => Durable^ ?=> String =
      JobRun.body(
        RunEnv(
          RunRecords(
            runtime.entries,
            runtime.conversations,
            runtime.schedules,
            runtime.deliveries
          ),
          db,
          jot,
          clock,
          moves,
          files,
          d.budget,
          runtime.keeper,
          plugin => runtime.pieceKeeper(plugin, PieceKeeper.Bound),
          Dreaming.Reads(
            StrandShelf.over(runtime.stitches, runtime.conversations),
            // A dreaming job's plugin is always among the deployment's: none other is asked.
            plugin => needs.getOrElse(plugin, Needs.over(plugin, Vector.empty)),
            StrandExcerpts.over(runtime.stitches, runtime.principals),
            runtime.documents,
            runtime.pieces
          )
        ),
        d.allJobs
      )
    def launch[C^](tooling: TurnTooling[C]^): Unit =
      runtime.launch(
        // Told after the body returns, outside any step: the turn's steps are unchanged.
        id => d ?=> told(runtime, id, Turn.body(turnEnv, tooling)(id)(using d), finished),
        close,
        settle,
        post,
        triage,
        stitch,
        d.plugins,
        shadow,
        d.shadows.map(_.shadowing),
        runs
      )
    // Each plugin's desk, of its own jobs: built per call, since it keeps nothing but the
    // runtime's transactions and its clock.
    val desks: Desks^{runtime} = new Desks {
      def of(plugin: PluginName, jobs: Vector[JobName]): ScheduleDesk^ =
        runtime.desk(plugin, jobs)
    }
    // Every plugin's tools, each bound over its own documents and its needs' services.
    val plugged = PluginBinding
      .bound(d.plugins, runtime.reads)
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
    // The engine's own tools touch no file of the machine's: they read grit's store and its file
    // area, keep a model setting, probe a model. The coding tools are hosted: offered here, run
    // by the edge serving the conversation's directory (ADR 0017).
    // Offered everywhere: grit's own, and every plugin's tools.
    val everyone: Either[DuplicateName, Toolbox[caps.CapSet^{desks, files}]] =
      Toolbox.of[caps.CapSet^{desks, files}](
        (Vector[Tool.Offered^{desks, files}](
          own(
            d.persona,
            runtime.askers,
            MessageIds.Stores(runtime.entries, runtime.rooms, runtime.search, runtime.principals),
            files
          )*
        ) ++
          plugged.map(_.over(desks)))*
      )
    // Offered whatever d.offer is, where TurnOffer.decide offers them.
    val administration = runtime.administration
    val administers: Either[DuplicateName, Toolbox[caps.CapSet^{administration, clock}]] =
      Toolbox.of[caps.CapSet^{administration, clock}](administering(d, administration, clock)*)
    val shows = renders(d)
    val launching = d.offer.tools match {
      case Offered.Read =>
        (everyone, administers) match {
          case (Right(tools), Right(admin)) =>
            // Refused here, at start, rather than as a failed offer on some turn.
            Toolbox.joined[caps.CapSet^{desks, files, administration, clock}](tools, admin).map {
              _ =>
                launch(
                  TurnTooling[caps.CapSet^{desks, files, administration, clock}](
                    tools,
                    Toolbox.Empty,
                    Coding.readOnlyHosted,
                    runtime.jot,
                    d.offer.rounds,
                    worksIn = d.worksIn,
                    reaches = d.reaches,
                    recipe = d.recipe,
                    knowledge = d.knowledge,
                    persona = d.persona,
                    administering = admin,
                    renders = shows
                  )
                )
            }
          case (Left(repeated), _) => Left(repeated)
          case (_, Left(repeated)) => Left(repeated)
        }
      case Offered.All =>
        // Offered only to the operator (Audience.operator): they tune grit, and ask first.
        val tuned = new KeptModelSettings(runtime.jot, runtime.modelSettings, clock)
        (
          everyone,
          Toolbox.of[caps.CapSet^{tuned, models}](operator(tuned, models)*),
          administers
        ) match {
          case (Right(tools), Right(operator), Right(admin)) =>
            // Refused here, at start, rather than as a failed offer on some turn.
            Toolbox
              .joined[caps.CapSet^{tuned, models, desks, files}](tools, operator)
              .flatMap(
                Toolbox.joined[caps.CapSet^{tuned, models, desks, files, administration, clock}](
                  _,
                  admin
                )
              )
              .map { _ =>
                launch(
                  TurnTooling[caps.CapSet^{tuned, models, desks, files, administration, clock}](
                    tools,
                    operator,
                    Coding.hosted,
                    runtime.jot,
                    d.offer.rounds,
                    worksIn = d.worksIn,
                    reaches = d.reaches,
                    recipe = d.recipe,
                    knowledge = d.knowledge,
                    persona = d.persona,
                    administering = admin,
                    renders = shows
                  )
                )
              }
          case (Left(repeated), _, _) => Left(repeated)
          case (_, Left(repeated), _) => Left(repeated)
          case (_, _, Left(repeated)) => Left(repeated)
        }
    }
    // Its caller closes the engine and reports the throw: in the chat, as the engine that
    // could not open.
    launching.left.foreach { case DuplicateName(name) =>
      throw new IllegalStateException(s"grit's tools offer ${ToolName.value(name)} twice")
    }
  }

  /** What moves over `stores` are made with: a job's, the lifecycle's and the turn's asks, their
    * costs kept in its ledger and admitted against its spending, read through `db`, `models`
    * answering their asks and `classifier` their judgments, at `clock`. An engine and a
    * rehearsal each pass their own stores and `db`.
    */
  def moves(
      stores: (Stores & caps.SharedCapability)^,
      db: Db^,
      models: Models^,
      classifier: ClearedClassifier^,
      clock: Clock^
  ): MovesEnv^{stores, db, models, classifier, clock} =
    MovesEnv(moveRecords(stores), models, classifier, db, clock)

  /** Where moves over `stores` keep what they did: its ledger and spending, its edges' requests,
    * its schedules and savepoints.
    */
  def moveRecords(stores: (Stores & caps.SharedCapability)^): MoveRecords =
    MoveRecords(
      stores.ledger,
      stores.spending,
      stores.requests,
      stores.edgeDirectory,
      stores.toolSets,
      stores.schedules,
      stores.askers,
      CharEstimate,
      stores.savepoints
    )

  /** grit's own tools, offered in every conversation: what grit is, as `persona` is told it
    * ([[About]]), what the person asking is cleared for, its turn's asker as `askers`
    * resolves them, explained as read through the reads its turn hands it ([[Cleared]]), and
    * the ids of this conversation and of the asker's messages, read from `messages`
    * ([[MessageIds]]), and more of a file `files` keeps ([[FileReading]]);
    * the kit, and the turn it launches, which reads them in its dispatch,
    * alone hold the engine's askers, so no plugin's tool can read who asked. Throws when
    * grit's shipped docs cannot be read, a fault of the build.
    */
  private[run] def own(
      persona: Persona,
      askers: Askers,
      messages: MessageIds.Stores,
      files: Files^
  ): Vector[Tool.Offered^{files}] = {
    val about: Tool[Option[About.Section]] =
      About.load(persona).fold(why => throw new IllegalStateException(why), t => t)
    Vector[Tool.Offered^{files}](
      about,
      Cleared.tool(askers),
      MessageIds.tool(messages, askers),
      FileReading.tool(files)
    )
  }

  /** The administering tools ([[Administering]]), acting through `administration` at the time
    * `clock` tells, unless some edge `d` serves is [[ServedEdge.Asks.Unanswered]]: each asks
    * first, and there a call would wait its whole time; none then. Whatever `d`'s
    * [[Offered]], which governs only the coding and operator tools.
    */
  private[run] def administering(
      d: Deployment,
      administration: Administration^,
      clock: Clock^
  ): Vector[Tool.Offered^{administration, clock}] =
    if (d.edges.exists(_.asks == ServedEdge.Asks.Unanswered)) Vector.empty
    else Administering.tools(administration, clock)

  /** What each edge `d` serves says its replies show ([[ServedEdge.renders]]), by its name. */
  private[run] def renders(d: Deployment): Map[EdgeName, String] =
    d.edges.flatMap(e => e.renders.map(e.name -> _)).toMap

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
      runtime: Runtime^,
      id: WorkflowId,
      said: String,
      finished: String => Unit
  ): String = {
    finished(TurnRef.fromWorkflowId(id) match {
      case None => said
      case Some(turn) =>
        runtime.db
          .read(Subject.Turn(turn))(
            TurnTally.read(runtime.conversations, runtime.entries, runtime.ledger, turn, said)
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
      s"weight ${Weight.value(s.weight)}"
  }

  /** The classifier `d` places messages with: Jev over `s`'s settings, which [[Secrets.of]]
    * holds for [[Placer.Jev]], as the party [[JevClassifier.Service]]; the stub as
    * [[StubClassifier.Service]].
    */
  private def classifier(d: Deployment, s: Secrets): ClearedClassifier^ =
    (d.placer, s.jev) match {
      case (Placer.Jev, Some(config)) =>
        ClearedClassifier.of(JevClassifier.Service, new JevClassifier(config))
      case (Placer.Jev, None) => ClearedClassifier.none("JEV_API_KEY is not set")
      case (Placer.Stub, _) => ClearedClassifier.of(StubClassifier.Service, new StubClassifier)
      case (Placer.Off(reason), _) => ClearedClassifier.none(reason)
    }

  /** [[classifier]] for a mention's weighing: Jev's requests time out at
    * [[Mentions.AskWithin]], so a call the weighing gave up on ends there too.
    */
  private def weighing(d: Deployment, s: Secrets): ClearedClassifier^ =
    (d.placer, s.jev) match {
      case (Placer.Jev, Some(config)) =>
        ClearedClassifier.of(
          JevClassifier.Service,
          new JevClassifier(
            config.copy(timeout = java.time.Duration.ofMillis(Mentions.AskWithin.toMillis))
          )
        )
      case (Placer.Jev, None) => ClearedClassifier.none("JEV_API_KEY is not set")
      case (Placer.Stub, _) => ClearedClassifier.of(StubClassifier.Service, new StubClassifier)
      case (Placer.Off(reason), _) => ClearedClassifier.none(reason)
    }

  /** Each of `d`'s shadows by name, asking its question of the model [[shadowModel]] names,
    * through the classifier `d` places messages with ([[classifier]]) made for that model.
    */
  private def variants(d: Deployment, s: Secrets): Map[ShadowName, ShadowAsking^] = {
    // Recursive rather than a `map`: each classifier is a capability made per variant, which
    // a lambda's result cannot carry out into the map.
    def asked(shadows: List[ShadowVariant]): Map[ShadowName, ShadowAsking^] = shadows match {
      case Nil => Map.empty
      case v :: rest =>
        val model = shadowModel(d.placer, v)
        asked(rest).updated(v.name, ShadowAsking(v.questions, model, classifying(model)))
    }
    def classifying(model: String): ClearedClassifier^ =
      (d.placer, s.jev) match {
        case (Placer.Jev, Some(config)) =>
          ClearedClassifier.of(
            JevClassifier.Service,
            new JevClassifier(config.copy(model = model))
          )
        case (Placer.Stub, _) => ClearedClassifier.of(StubClassifier.Service, new StubClassifier)
        case (Placer.Jev, None) => ClearedClassifier.none("JEV_API_KEY is not set")
        // Deployment.of refuses shadows with the placer off.
        case (Placer.Off(reason), _) => ClearedClassifier.none(reason)
      }
    asked(d.shadows.toList)
  }

  /** The model the shadow `v` asks of, under a deployment whose placer is `placer`: the one it
    * names, else Jev's [[JevConfig.DefaultModel]] or the stub's [[StubClassifier.Model]];
    * `"none"` with the placer off, which [[Deployment.of]] refuses beside a shadow.
    */
  private[grit] def shadowModel(placer: Placer, v: ShadowVariant): String = placer match {
    case Placer.Jev => v.model.getOrElse(JevConfig.DefaultModel)
    case Placer.Stub => v.model.getOrElse(StubClassifier.Model)
    case Placer.Off(_) => "none"
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
