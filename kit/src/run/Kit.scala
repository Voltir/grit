package grit.kit.run

import java.time.{Instant, LocalDate}
import java.util.concurrent.{CountDownLatch, TimeUnit}

import grit.core.edge.{Attesting, CatchUp, EdgeRefusal, EdgeStores}
import grit.core.id.{AttesterName, EdgeName}
import grit.core.identity.{Identities, Realm}
import grit.core.message.Cost
import grit.core.spend.Budget
import grit.core.store.{Linking, StoreError}
import grit.core.visibility.Subject
import grit.dbos.engine.{Engine, Link}
import grit.host.LocalMachine
import grit.kit.deployment.Deployment
import grit.kit.environment.{Secrets, SecretsRefusal}
import grit.turn.Turn

/** Why [[Kit.serve]] or [[Kit.catchUp]] could not run, or stopped. */
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

  def message: String = this match {
    case Unconfigured(refusal) => refusal.message
    case Edge(edge, refusal) => s"${EdgeName.value(edge)}: ${refusal.message}"
    case Engine(why) => why
    case Store(why) => why
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
    * identities no longer trust is ended ([[trusting]]). Each finished turn is logged in one line at INFO under [[TurnLog]].
    * When `deployment` declares a review, heard messages are picked for it every
    * [[PickEvery]]; a round the database fails is logged as a warning and run again next time.
    * What each edge's attesting reports is logged as [[reported]] says.
    */
  def serve(deployment: Deployment, env: Map[String, String]): Either[KitFailure, Unit] = {
    val log = org.slf4j.LoggerFactory.getLogger("grit.serve")
    val turns = org.slf4j.LoggerFactory.getLogger(TurnLog)
    preflight(deployment, env, deployment.edges.toList.map(e => (e.name, e.needs))) match {
      case Left(failure) => Left(failure)
      case Right(secrets) =>
        open(deployment, secrets) match {
          case Left(failure) => Left(failure)
          case Right(engine) =>
            val stopped = new CountDownLatch(1)
            val closed = new CountDownLatch(1)
            try {
              Launch(
                engine,
                deployment,
                secrets,
                Launch.Run.Served,
                sweeping = true,
                said => turns.info(said)
              )
              val link: Link^{engine} = engine
              metered(link, deployment.budget, Instant.now()).foreach(log.info)
              val report: Attesting.Report => Unit =
                reported(_, said => log.info(said), said => log.warn(said), said => log.error(said))
              Serving.open(
                deployment.edges.toList,
                edge =>
                  stores(
                    link,
                    new Attesting(
                      voucherOf(engine, deployment.identities, edge.attester),
                      link.jot,
                      report
                    )
                  ),
                env,
                said => log.info(said)
              ) match {
                case Left(failure) => Left(failure)
                case Right(opened) =>
                  // On ctrl-c or SIGTERM: stop the loop, then hold the JVM open until the edges
                  // and the engine are closed.
                  Runtime.getRuntime.addShutdownHook(Thread.ofPlatform().unstarted { () =>
                    stopped.countDown()
                    val _ = closed.await(ClosedWithin.toMillis, TimeUnit.MILLISECONDS)
                  })
                  // The next pick round is due at `pickAt`, run between deliveries.
                  var pickAt = Instant.EPOCH
                  try
                    Serving.deliver(
                      opened,
                      () => stopped.getCount == 0,
                      () => {
                        deployment.review.foreach { review =>
                          val now = Instant.now()
                          if (!now.isBefore(pickAt)) {
                            pickAt = now.plusMillis(PickEvery.toMillis)
                            Picking.round(
                              review,
                              engine.reviews,
                              link.jot,
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

  /** Opens `deployment`'s engine with no sweep of its own and `catchUp` over it; shows `say`
    * what it would hear from each source, estimated, and that nothing caps a catch-up when
    * the estimate is more than today's cap leaves, once [[trusting]] has run; hears it once `agree` accepts, sweeps
    * until nothing is left to close or ask, says what the day's recorded spend rose by, and
    * closes. Nothing is heard when `agree` declines or nothing is unheard. Refused as
    * [[serve]] is, before the engine opens; once it opens, what the deployment's identities no
    * longer trust is ended ([[trusting]]). Each finished turn is logged as [[serve]] logs it.
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
        open(deployment, secrets) match {
          case Left(failure) => Left(failure)
          case Right(engine) =>
            val turns = org.slf4j.LoggerFactory.getLogger(TurnLog)
            try {
              Launch(
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
                  )
                ),
                env,
                deployment.budget,
                () => spentToday(link, deployment.budget, Instant.now()),
                () => engine.sweep(Instant.now()).left.map(_.toString),
                () => engine.unfinished().left.map(_.toString),
                agree,
                say
              )
            } finally engine.close()
        }
    }

  /** Ends what `deployment`'s identities no longer support ([[Engine.untrust]]), logging each
    * change at info in the `grit.serve` log; and warns there, at every start, when realms are
    * trusted and no email domain is claimed, so no account is linked to another by email: what
    * each start of the deployment's own engine calls before anything serves. `Store` when the
    * database fails.
    */
  def trusting(engine: Engine^, deployment: Deployment): Either[KitFailure, Unit] = {
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
    * terminal would draw over the chat. Throws as [[Launch.apply]] does.
    */
  private[grit] def launch(
      engine: Engine^,
      deployment: Deployment,
      secrets: Secrets,
      run: Launch.Run
  ): Engine^{engine} =
    Launch(engine, deployment, secrets, run, sweeping = true, _ => ())

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

  private def open(deployment: Deployment, secrets: Secrets): Either[KitFailure, Engine^] =
    Engine.open(
      secrets.database,
      Turn.Epoch,
      LocalMachine.identity(),
      deployment.budget,
      deployment.visibility
    ) match {
      case Left(refused) => Left(KitFailure.Engine(refused.message(Instant.now())))
      case Right(engine) =>
        trusting(engine, deployment) match {
          case Left(failure) =>
            engine.close()
            Left(failure)
          case Right(()) => Right(engine)
        }
    }

  private def stores(link: Link^, attesting: Attesting^): EdgeStores^{link, attesting} =
    EdgeStores(
      link.inbox,
      link.principals,
      link.deliveries,
      link.acknowledgements,
      link.reviews,
      link.jot,
      link,
      attesting
    )

  /** The voucher of the realms `identities` trusts `attester` for, none when there is none. */
  private def voucherOf(
      engine: Engine^,
      identities: Identities,
      attester: Option[AttesterName]
  ): grit.core.store.Voucher =
    engine.voucher(attester.fold(Set.empty[Realm])(identities.realmsOf), identities.domains)

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
