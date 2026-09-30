package grit.kit.run

import java.time.{Instant, LocalDate}
import java.util.concurrent.{CountDownLatch, TimeUnit}

import grit.core.edge.{CatchUp, EdgeRefusal, EdgeStores}
import grit.core.id.EdgeName
import grit.core.message.Cost
import grit.core.spend.Budget
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
    * those already opened.
    */
  def serve(deployment: Deployment, env: Map[String, String]): Either[KitFailure, Unit] = {
    val log = org.slf4j.LoggerFactory.getLogger("grit.serve")
    preflight(deployment, env, deployment.edges.toList.map(e => (e.name, e.needs))) match {
      case Left(failure) => Left(failure)
      case Right(secrets) =>
        open(deployment, secrets) match {
          case Left(failure) => Left(failure)
          case Right(engine) =>
            val stopped = new CountDownLatch(1)
            val closed = new CountDownLatch(1)
            try {
              Launch(engine, deployment, secrets, Launch.Run.Served, sweeping = true)
              val link: Link^{engine} = engine
              metered(link, deployment.budget, Instant.now()).foreach(log.info)
              Serving.open(
                deployment.edges.toList,
                stores(link),
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
                  try
                    Serving.deliver(
                      opened,
                      () => stopped.getCount == 0,
                      () => {
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
    * the estimate is more than today's cap leaves; hears it once `agree` accepts, sweeps
    * until nothing is left to close or ask, says what the day's recorded spend rose by, and
    * closes. Nothing is heard when `agree` declines or nothing is unheard. Refused as
    * [[serve]] is, before the engine opens.
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
            try {
              Launch(engine, deployment, secrets, Launch.Run.Served, sweeping = false)
              val link: Link^{engine} = engine
              CatchingUp.run(
                catchUp,
                stores(link),
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

  /** `engine` with `deployment`'s workflows launched, for a process that runs its own edge
    * (the chat), sweeping as `deployment` says. Throws as [[Launch.apply]] does.
    */
  private[grit] def launch(
      engine: Engine^,
      deployment: Deployment,
      secrets: Secrets,
      run: Launch.Run
  ): Engine^{engine} =
    Launch(engine, deployment, secrets, run, sweeping = true)

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
    Engine.open(secrets.database, Turn.Epoch, LocalMachine.identity(), deployment.budget) match {
      case Left(refused) => Left(KitFailure.Engine(refused.message(Instant.now())))
      case Right(engine) => Right(engine)
    }

  private def stores(link: Link^): EdgeStores^{link} =
    EdgeStores(link.inbox, link.principals, link.deliveries, link.jot)

  /** What the log says of `budget` as `link`'s ledger stands at `now`: the cap, and, when
    * some of today's calls were not priced, that the cap counts them as nothing (a provider
    * that prices none is never capped). None with no cap.
    */
  private def metered(link: Link^, budget: Budget, now: Instant): Option[String] =
    budget.cap.map { cap =>
      val today = budget.today(now)
      val unpriced = link.db.read(link.spending.on(today)) match {
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
      .read(link.spending.on(budget.today(now)))
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
