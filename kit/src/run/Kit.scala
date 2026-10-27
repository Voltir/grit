package grit.kit.run

import java.time.{Instant, LocalDate}
import java.util.concurrent.{CountDownLatch, TimeUnit}

import scala.concurrent.duration.FiniteDuration

import grit.core.clock.Clock
import grit.core.edge.{Attesting, CatchUp, EdgeRefusal, EdgeStores}
import grit.core.files.Intake
import grit.core.id.{AttesterName, Declarer, EdgeName, PluginName, ScheduleKey}
import grit.core.identity.{Domain, Identities, Realm}
import grit.core.job.{Declared, Ending}
import grit.core.message.Cost
import grit.core.place.Place
import grit.core.spend.Budget
import grit.core.store.{Linking, StoreError, Tx}
import grit.core.visibility.{Label, Subject}
import grit.dbos.engine.{Engine, Link, Unopened}
import grit.dbos.sql.DbConfig
import grit.host.machine.LocalMachine
import grit.job.clock.ClockEdge
import grit.kit.deployment.Deployment
import grit.kit.environment.{Secrets, SecretsRefusal}

/** Why [[Kit.serve]], [[Kit.catchUp]] or [[Kit.runOnce]] could not run, or stopped. */
enum KitFailure {

  /** The environment names no usable database, or a key the deployment needs. */
  case Unconfigured(refusal: SecretsRefusal)

  /** Edge `edge` would not open, or stopped partway. */
  case Edge(edge: EdgeName, refusal: EdgeRefusal)

  /** The engine would not open: another grit holds the database's engine (never attached
    * to: two processes would each post the same replies), or the database failed; `why`
    * says which, for a person.
    */
  case Engine(why: String)

  /** The database could not be read or swept partway through a catch-up. */
  case Store(why: String)

  /** Runs [[Kit.runOnce]] was asked for that the deployment cannot run, and why. */
  case Unrunnable(why: String)

  def message: String = this match {
    case Unconfigured(refusal) => refusal.message
    case Edge(edge, refusal) => s"${EdgeName.value(edge)}: ${refusal.message}"
    case Engine(why) => why
    case Store(why) => why
    case Unrunnable(why) => why
  }
}

/** The kit's entry: a [[Deployment]] run beside its engine (ADR 0021). */
object Kit {

  /** How often the edges are asked to post finished turns' replies: 500 ms. */
  val DeliverEvery: scala.concurrent.duration.FiniteDuration =
    scala.concurrent.duration.DurationInt(500).millis

  /** How often [[serve]] picks heard messages for a deployment's review, between deliveries:
    * a minute.
    */
  val PickEvery: scala.concurrent.duration.FiniteDuration =
    scala.concurrent.duration.DurationInt(1).minute

  /** The logger [[serve]] and [[catchUp]] write each finished turn's line to: `grit.turn`. */
  val TurnLog: String = "grit.turn"

  /** How long the shutdown hook holds the process open for the edges and the engine to close:
    * 45 s, over the engine's own 30 s wait for running turns.
    */
  val ClosedWithin: scala.concurrent.duration.FiniteDuration =
    scala.concurrent.duration.DurationInt(45).seconds

  /** Serves `deployment`: its engine over the database `env` names, and each of its edges,
    * blocking the calling thread until the process is stopped. It installs a shutdown hook
    * that stops serving, closes each edge, then the engine (which waits up to 30 s for running
    * turns), holding the process open [[ClosedWithin]] for them. Refused before the engine
    * opens when a secret or an edge's variable is missing; an edge refusing to open closes
    * those already opened. Once the engine opens, and before any edge does, what the deployment's
    * identities no longer trust is ended ([[Engine.untrust]]); its edges open before any of its
    * workflows runs ([[opened]]). Each finished turn is logged in one line at INFO under [[TurnLog]].
    * When `deployment` declares a review, heard messages are picked for it every
    * [[PickEvery]]; a round the database fails is logged as a warning and run again next time.
    * Each edge that is an attester the deployment trusts is asked for a look once its edges
    * open and every [[Attesting.Every]] after; what it reports is logged as [[reported]] says.
    */
  def serve(deployment: Deployment, env: Map[String, String]): Either[KitFailure, Unit] = {
    // The process's own time, read here at the serving loop's root and passed down.
    val clock: Clock^ = Clock.system() // clock-check: the composition root of a serving process
    val log = org.slf4j.LoggerFactory.getLogger("grit.serve")
    val turns = org.slf4j.LoggerFactory.getLogger(TurnLog)
    preflight(deployment, env, deployment.edges.toList.map(e => (e.name, e.needs))) match {
      case Left(failure) => Left(failure)
      case Right(secrets) =>
        trusted(deployment, secrets, clock) match {
          case Left(failure) => Left(failure)
          case Right(engine) =>
            val stopped = new CountDownLatch(1)
            val closed = new CountDownLatch(1)
            try {
              opened(engine, deployment, secrets, env, clock, said => turns.info(said), log) match {
                case Left(failure) => Left(failure)
                case Right(opened) =>
                  // On ctrl-c or SIGTERM: stop the loop, then hold the JVM open until the edges
                  // and the engine are closed.
                  Runtime.getRuntime.addShutdownHook(Thread.ofPlatform().unstarted { () =>
                    stopped.countDown()
                    val _ = closed.await(ClosedWithin.toMillis, TimeUnit.MILLISECONDS)
                  })
                  // Looks and pick rounds, each run between deliveries when due.
                  val looking = cadence(Attesting.Every, clock)
                  val picking = cadence(PickEvery, clock)
                  try
                    Serving.deliver(
                      opened,
                      () => stopped.getCount == 0,
                      () => looking().nonEmpty,
                      () => {
                        deployment.review.foreach { review =>
                          picking().foreach { now =>
                            Picking.round(
                              review,
                              engine.reviews,
                              engine.jot,
                              deployment.budget,
                              now
                            ) match {
                              case Left(e) => log.warn(s"review: nothing picked: $e")
                              case Right(0) => ()
                              case Right(n) => log.info(s"review: $n picked")
                            }
                          }
                        }
                        val _ = stopped.await(DeliverEvery.toMillis, TimeUnit.MILLISECONDS)
                      },
                      said => log.warn(said)
                    )
                  finally opened.close()
                  Right(())
              }
            } finally {
              engine.close()
              closed.countDown()
            }
        }
    }
  }

  /** `deployment`'s edges opened on `engine` in order ([[Serving.open]]), each over its stores and
    * `engine`'s file area, and only then its workflows launched, sweeping, each finished turn's
    * line told to `finished` ([[Launch.apply]]): what DBOS starts at launch, a turn recovered
    * mid-round among it, finds every edge this process serves registered, so its calls to their
    * places are sent. An edge refusing to open is the failure: those opened are closed and
    * nothing is launched. Throws as [[Launch.apply]] does, having closed the edges. What the
    * spend so far is, and each edge's opening, is logged to `log` at INFO; each edge that is an
    * attester the deployment trusts reports to `log` as [[reported]] says.
    */
  private[grit] def opened(
      engine: Engine^,
      deployment: Deployment,
      secrets: Secrets,
      env: Map[String, String],
      clock: Clock^,
      finished: String => Unit,
      log: org.slf4j.Logger
  ): Either[KitFailure, Serving.Opened^] = {
    val files = Launch.files(engine, deployment, secrets)
    val link: Link^{engine} = engine
    metered(link, deployment.budget, clock.now()).foreach(log.info)
    val report: Attesting.Report => Unit =
      reported(_, said => log.info(said), said => log.warn(said), said => log.error(said))
    // The edges first: DBOS runs what it recovers as soon as it launches, and a turn's call to a
    // place no live edge serves is answered as not served.
    Serving.open(
      deployment.edges.toList,
      edge =>
        stores(
          link,
          new Attesting(
            voucherOf(engine, deployment.identities, edge.attester),
            link.jot,
            report
          ),
          files
        ),
      env,
      clock,
      said => log.info(said)
    ) match {
      case Left(failure) => Left(failure)
      case Right(opened) =>
        try
          Launch.launched(
            engine,
            deployment,
            secrets,
            Launch.Run.Served,
            sweeping = true,
            finished,
            files
          )
        catch {
          case e: Throwable =>
            opened.close()
            throw e
        }
        Right(opened)
    }
  }

  /** Opens `deployment`'s engine with no sweep of its own and `catchUp` over it; shows `say`
    * what it would hear from each source, estimated, with the label its conversations are
    * created at (as the engine's visibility labels the source's room), and, before asking, that a
    * label declared later does not relabel what it hears now; and that nothing caps a catch-up when
    * the estimate is more than today's cap leaves; hears it once `agree` accepts, sweeps
    * until nothing is left to close or ask, says what the day's recorded spend rose by, and
    * closes. Nothing is heard when `agree` declines or nothing is unheard. Refused as
    * [[serve]] is, before the engine opens; once it opens, what the deployment's identities no
    * longer trust is ended ([[Engine.untrust]]). Each finished turn is logged as [[serve]] logs it.
    */
  def catchUp(
      deployment: Deployment,
      catchUp: CatchUp,
      env: Map[String, String],
      agree: String => Boolean,
      say: String => Unit
  ): Either[KitFailure, Unit] =
    preflight(deployment, env, List((catchUp.name, catchUp.needs))) match {
      case Left(failure) => Left(failure)
      case Right(secrets) =>
        // The process's own time, read here at the catch-up's root and passed down.
        val clock: Clock^ = Clock.system() // clock-check: the composition root of a catch-up
        trusted(deployment, secrets, clock) match {
          case Left(failure) => Left(failure)
          case Right(engine) =>
            val turns = org.slf4j.LoggerFactory.getLogger(TurnLog)
            try {
              val files = Launch(
                engine,
                deployment,
                secrets,
                Launch.Run.Served,
                sweeping = false,
                said => turns.info(said)
              )
              val link: Link^{engine} = engine
              val log = org.slf4j.LoggerFactory.getLogger("grit.serve")
              CatchingUp.run(
                catchUp,
                stores(
                  link,
                  new Attesting(
                    voucherOf(engine, deployment.identities, catchUp.attester),
                    link.jot,
                    reported(
                      _,
                      said => log.info(said),
                      said => log.warn(said),
                      said => log.error(said)
                    )
                  ),
                  files
                ),
                env,
                deployment.budget,
                place => roomLabel(link, place),
                clock,
                () => spentToday(link, deployment.budget, clock.now()),
                () => engine.sweep(clock.now()).left.map(_.toString),
                () => engine.unfinished().left.map(_.toString),
                agree,
                say
              )
            } finally engine.close()
        }
    }

  /** What one of [[runOnce]]'s runs came to: its schedule's `key`, how it `ended` (`None` when
    * it had not ended within the wait), the last `reply` its job wrote (`None` when none), and
    * what the day's recorded spend rose by while it ran, in USD.
    */
  final case class Ran(
      key: ScheduleKey,
      ended: Option[Ending],
      reply: Option[String],
      spent: BigDecimal
  ) extends caps.Pure {

    /** `{key}: {ended} ({not ended}); {reply} ({no reply}); spent {cents}`. */
    def line: String =
      s"${ScheduleKey.value(key)}: ${ended.fold("not ended")(e => s"ended ${e.word}")}; " +
        s"${reply.getOrElse("no reply")}; spent ${Cost.cents(spent)}"
  }

  /** Opens `deployment`'s engine over the database `env` names, with no sweep and no edge,
    * says through `say` the database's endpoint (`database: host:port/name`), and once `agree`
    * accepts `what`, launches its workflows and runs `runs` one at a time, in order: each
    * declared as a schedule of `plugin` beside the deployment's own, its slot a once slot due
    * when it is reached (its own rule is not used), started by a pass of the clock edge, and
    * waited for, a pass made every [[OnceRuns.Every]], until it ends or `longest` passes (a
    * slot of the deployment's own that comes due meanwhile runs too). Says each [[Ran]]'s
    * line as it ends; a run not ended within `longest` finishes when the engine next opens.
    * Refused before the engine opens ([[KitFailure.Unrunnable]]) when `plugin` is not the
    * deployment's, a run's job is not `plugin`'s, a run's key is one the deployment declares,
    * or a run's clearance is none of `plugin`'s own schedules'; and as [[serve]] is refused.
    * Once it opens, refused while another process holds the database's engine, and when none
    * of `plugin`'s own schedules is stored there (the deployment has never been served on
    * that database). Nothing runs when `agree` declines.
    */
  def runOnce(
      deployment: Deployment,
      plugin: PluginName,
      runs: Vector[Declared[?]],
      what: String,
      longest: FiniteDuration,
      env: Map[String, String],
      agree: String => Boolean,
      say: String => Unit
  ): Either[KitFailure, Vector[Ran]] =
    OnceRuns.unrunnable(deployment, plugin, runs).map(KitFailure.Unrunnable(_)) match {
      case Some(refused) => Left(refused)
      case None =>
        preflight(deployment, env, Nil) match {
          case Left(failure) => Left(failure)
          case Right(secrets) =>
            // The process's own time, read here at the run's root and passed down.
            val clock: Clock^ = Clock.system() // clock-check: the composition root of once runs
            trusted(deployment, secrets, clock) match {
              case Left(failure) => Left(failure)
              case Right(engine) =>
                try {
                  OnceRuns.served(engine, deployment, plugin) match {
                    case Left(e) => Left(KitFailure.Store(s"the schedules are unread: $e"))
                    case Right(false) =>
                      Left(
                        KitFailure.Unrunnable(
                          s"the plugin ${PluginName.value(plugin)} has never been served on " +
                            s"${secrets.database.endpoint}: run grit serve on it first"
                        )
                      )
                    case Right(true) =>
                      say(s"database: ${secrets.database.endpoint}")
                      if (!agree(what)) Right(Vector.empty)
                      else {
                        val turns = org.slf4j.LoggerFactory.getLogger(TurnLog)
                        Launch(
                          engine,
                          deployment,
                          secrets,
                          Launch.Run.Served,
                          sweeping = false,
                          said => turns.info(said)
                        )
                        val edge = new ClockEdge(
                          engine.inbox,
                          engine.schedules,
                          engine.db,
                          engine.clock,
                          deployment.allJobs
                        )
                        runs.foldLeft[Either[KitFailure, Vector[Ran]]](Right(Vector.empty)) {
                          (acc, run) =>
                            acc.flatMap { done =>
                              val at = clock.now()
                              val dated = OnceRuns.dated(run, at)
                              val declared = deployment.declared :+ (Declarer.Plugin(plugin), dated)
                              (for {
                                before <- spentToday(engine, deployment.budget, at)
                                _ <- engine.jot
                                  .write(Subject.Public)(engine.schedules.declare(declared, at))
                                  .left
                                  .map(e => s"the run could not be declared: $e")
                                ended = OnceRuns.waited(
                                  engine,
                                  plugin,
                                  run.key,
                                  clock,
                                  at.plusMillis(longest.toMillis),
                                  () => { val _ = edge.tick() }
                                )
                                after <- spentToday(engine, deployment.budget, at)
                              } yield {
                                val ran = Ran(
                                  run.key,
                                  ended,
                                  OnceRuns.replied(engine, plugin, run.key, run.job.name, at),
                                  after - before
                                )
                                say(ran.line)
                                done :+ ran
                              }).left.map(KitFailure.Store(_))
                            }
                        }
                      }
                  }
                } finally engine.close()
            }
        }
    }

  /** A round asked for between deliveries, by `clock`: the time now when one is due (the first
    * at once, then each once `every` has passed since the last), `None` otherwise.
    */
  private[run] def cadence(every: FiniteDuration, clock: Clock^): () => Option[Instant] = {
    var next = Instant.EPOCH
    () => {
      val now = clock.now()
      Option.when(!now.isBefore(next)) {
        next = now.plusMillis(every.toMillis)
        now
      }
    }
  }

  /** Ends what `deployment`'s identities no longer support ([[Engine.untrust]]), logging each
    * change at info in the `grit.serve` log; and warns there when realms are trusted and no email
    * domain is claimed, so no account is linked to another by email. `Store` when the database
    * fails. Called through [[trusted]] by [[serve]] and [[catchUp]] alone, both built from the whole
    * deployment and every attester it trusts; a chat start ([[launch]]) never withdraws trust,
    * since a chat may be declared with fewer edges, and so trust fewer realms.
    */
  private def trusting(engine: Engine^, deployment: Deployment): Either[KitFailure, Unit] = {
    val log = org.slf4j.LoggerFactory.getLogger("grit.serve")
    trusted(engine.untrust, deployment.identities, said => log.info(said), said => log.warn(said))
  }

  /** [[trusting]] over `untrust`, saying its changes to `info` and its warning to `warn`. */
  private[run] def trusted(
      untrust: Identities => Either[StoreError, Vector[Linking]],
      identities: Identities,
      info: String => Unit,
      warn: String => Unit
  ): Either[KitFailure, Unit] = {
    if (identities.realms.nonEmpty && identities.domains.isEmpty) {
      val realms = identities.realms.toVector.map(r => s"${r.namespace}:${r.within}/").sorted
      warn(
        s"identity: realms are trusted (${realms.mkString(", ")}) but no email domain is " +
          "claimed, so no account is linked to another by email; each is its own person, and " +
          "membership still counts. Claim the deployment's domains (GRIT_CLAIMED_DOMAINS in " +
          "the reference deployment) to link them."
      )
    }
    untrust(identities) match {
      case Left(e) =>
        Left(KitFailure.Store(s"identity: what the deployment no longer trusts was not ended: $e"))
      case Right(changes) => Right(changes.foreach(l => info(s"identity: ${l.message}")))
    }
  }

  /** `engine` with `deployment`'s workflows launched, for a process that runs its own edge
    * (the chat), sweeping as `deployment` says, its finished turns not logged: a line on the
    * terminal would draw over the chat. It ends no attestation: a process that serves no
    * attester leaves what each attested standing ([[serve]] and [[catchUp]] end what the
    * deployment no longer trusts). Throws as [[Launch.apply]] does.
    */
  private[grit] def launch(
      engine: Engine^,
      deployment: Deployment,
      secrets: Secrets,
      run: Launch.Run
  ): Engine^{engine} = {
    val _ = Launch(engine, deployment, secrets, run, sweeping = true, _ => ())
    engine
  }

  /** `deployment`'s secrets in `env`, once every variable each of `needs` names is set. */
  private[run] def preflight(
      deployment: Deployment,
      env: Map[String, String],
      needs: List[(EdgeName, Vector[grit.core.edge.Variable])]
  ): Either[KitFailure, Secrets] = {
    @scala.annotation.tailrec
    def unset(left: List[(EdgeName, Vector[grit.core.edge.Variable])]): Option[KitFailure] =
      left match {
        case Nil => None
        case (name, vars) :: rest =>
          EdgeRefusal.missing(vars, env) match {
            case Some(refusal) => Some(KitFailure.Edge(name, refusal))
            case None => unset(rest)
          }
      }
    Secrets.of(env, deployment).left.map(KitFailure.Unconfigured(_)).flatMap { secrets =>
      unset(needs).toLeft(secrets)
    }
  }

  /** `deployment`'s engine over `database`, held by this machine's process and opened with
    * `clock`, under the deployment's version, budget and visibility; it ends nothing. Refused
    * as [[Engine.open]] refuses: [[grit.dbos.engine.Unopened.Lock]] while another process holds
    * the database's engine.
    */
  private[grit] def open(
      database: DbConfig,
      deployment: Deployment,
      clock: Clock^
  ): Either[Unopened, Engine^] =
    Engine.open(
      database,
      deployment.runsUnder,
      LocalMachine.identity(),
      deployment.budget,
      deployment.visibility,
      clock
    )

  /** [[open]] over `secrets`' database, with what `deployment`'s identities no longer trust
    * ended; a refusal says when, by `clock`, the database's engine was last seen held.
    */
  private def trusted(
      deployment: Deployment,
      secrets: Secrets,
      clock: Clock^
  ): Either[KitFailure, Engine^] =
    open(secrets.database, deployment, clock) match {
      case Left(refused) => Left(KitFailure.Engine(refused.message(clock.now())))
      case Right(engine) =>
        trusting(engine, deployment) match {
          case Left(failure) =>
            engine.close()
            Left(failure)
          case Right(()) => Right(engine)
        }
    }

  private def stores(
      link: Link^,
      attesting: Attesting^,
      files: Intake^
  ): EdgeStores^{link, attesting, files} =
    EdgeStores(
      link.inbox,
      link.administration,
      link.joins,
      link.principals,
      link.deliveries,
      link.spending,
      link.acknowledgements,
      link.reviews,
      link.origins,
      link.documentReviewsOf,
      link.jot,
      link,
      attesting,
      files
    )

  /** The voucher an edge attesting as `attester` records through: of [[trustOf]]'s realms and
    * domains.
    */
  private def voucherOf(
      engine: Engine^,
      identities: Identities,
      attester: Option[AttesterName]
  ): grit.core.store.Voucher = {
    val (realms, domains) = trustOf(identities, attester)
    engine.voucher(realms, domains)
  }

  /** What an edge attesting as `attester` records for: the realms `identities` trusts that
    * attester for, none when the edge attests nothing, and every domain `identities` claims.
    */
  private[run] def trustOf(
      identities: Identities,
      attester: Option[AttesterName]
  ): (Set[Realm], Set[Domain]) =
    (attester.fold(Set.empty[Realm])(identities.realmsOf), identities.domains)

  /** Tells the log what an edge's check or look reported: a change at `info`, a source that
    * could not be reached at `warn`, and an alarm at `error`, each line beginning `identity: `.
    */
  private[run] def reported(
      report: Attesting.Report,
      info: String => Unit,
      warn: String => Unit,
      error: String => Unit
  ): Unit = report match {
    case Attesting.Report.Changed(_) => info(s"identity: ${report.message}")
    case Attesting.Report.Unreached(_, _, _) => warn(s"identity: ${report.message}")
    case Attesting.Report.Overdue(_, _, _) => error(s"identity: ${report.message}")
  }

  /** What the log says of `budget` as `link`'s ledger stands at `now`: the cap, and, when
    * some of today's calls were not priced, that the cap counts them as nothing (a provider
    * that prices none is never capped). None with no cap.
    */
  private def metered(link: Link^, budget: Budget, now: Instant): Option[String] =
    budget.cap.map { cap =>
      val today = budget.today(now)
      val unpriced = link.db.read(Subject.Public)(link.spending.on(today)) match {
        case Right(spent) =>
          spent.cost match {
            case Cost.AtLeast(_) =>
              s"; today's recorded spend, ${spent.cost.written}, includes calls no provider priced, which the cap counts as nothing"
            case Cost.Exact(_) => ""
          }
        case Left(e) => s"; today's spend could not be read: $e"
      }
      s"daily cap $$${cap.usd}, days from midnight ${budget.zone}$unpriced"
    }

  /** The dollars `link`'s ledger recorded on the day `now` falls on in `budget`'s zone, a call
    * no provider priced counted as nothing.
    */
  /** The label in force for `room` ([[Tx.roomLabel]]), read in a transaction of its own. */
  private def roomLabel(link: Link^, room: Place): Either[String, Label] =
    link.db.read(Subject.Public)(Right(Tx.roomLabel(room))).left.map(_.toString)

  private def spentToday(link: Link^, budget: Budget, now: Instant): Either[String, BigDecimal] =
    link.db
      .read(Subject.Public)(link.spending.on(budget.today(now)))
      .left
      .map(e => s"today's spend could not be read: $e")
      .map(_.cost match {
        case Cost.Exact(usd) => usd
        case Cost.AtLeast(usd) => usd
      })

  /** The first day `since` falls on in `budget`'s zone. */
  private[run] def dayOf(since: Instant, budget: Budget): LocalDate =
    LocalDate.ofInstant(since, budget.zone)
}
