package grit.app.serve

import java.util.concurrent.CountDownLatch

import scala.concurrent.duration.{DurationInt, FiniteDuration}

import grit.core.host.ProcessIdentity
import grit.core.spend.Budget
import grit.dbos.engine.{Engine, Link}
import grit.dbos.sql.DbConfig
import grit.slack.client.{AppToken, BotToken, SocketSlack}
import grit.slack.edge.{EdgeStores, SlackEdge}
import grit.slack.event.ChannelId

/** `grit serve`: the engine of one database, and the Slack edge in its process (ADR 0019). */
object Serve {

  /** The Slack bot token's variable (`xoxb-…`). */
  val BotTokenVar = "SLACK_BOT_TOKEN"

  /** The Slack app-level token's variable (`xapp-…`), which opens Socket Mode. */
  val AppTokenVar = "SLACK_APP_TOKEN"

  /** The variable naming the channels grit listens in: their ids, comma-separated. Unset,
    * it listens in none.
    */
  val ListenVar = "GRIT_SLACK_LISTEN"

  /** The channels `env` says grit listens in ([[ListenVar]]); why not, naming the first entry
    * that is not a channel id.
    */
  def listening(env: Map[String, String]): Either[String, Set[ChannelId]] =
    env
      .get(ListenVar)
      .toVector
      .flatMap(_.split(',').toVector.map(_.trim).filter(_.nonEmpty))
      .foldLeft[Either[String, Set[ChannelId]]](Right(Set.empty)) { (acc, raw) =>
        acc.flatMap(ids =>
          ChannelId
            .read(raw)
            .map(ids + _)
            .toRight(
              s"$ListenVar: $raw is not a channel id (C…, as Slack's channel details show it)"
            )
        )
      }

  /** The tokens and the channels listened in that `env` sets; why not, naming the variable,
    * never quoting a token.
    */
  private[serve] def slackOf(
      env: Map[String, String]
  ): Either[String, (BotToken, AppToken, Set[ChannelId])] =
    for {
      bot <- env
        .get(BotTokenVar)
        .toRight(s"$BotTokenVar is not set")
        .flatMap(BotToken.of(_).left.map(w => s"$BotTokenVar: $w"))
      app <- env
        .get(AppTokenVar)
        .toRight(s"$AppTokenVar is not set")
        .flatMap(AppToken.of(_).left.map(w => s"$AppTokenVar: $w"))
      listen <- listening(env)
    } yield (bot, app, listen)

  /** How often the edge looks for finished turns to post: 500 ms. */
  val DeliverEvery: FiniteDuration = 500.millis

  /** Runs the engine of `config`'s database, launched by `launch`, and the Slack edge over it,
    * connected with the tokens in `env`, taking new messages as `budget` allows, until the
    * process is stopped (its shutdown closes
    * Slack, then the engine). Why it could not start: a token missing or of the wrong kind
    * (never quoted), the database's engine held by another grit (never attached to: two edges
    * would each post the same replies), or Slack refusing the tokens.
    */
  def run(
      env: Map[String, String],
      config: DbConfig,
      epoch: String,
      identity: ProcessIdentity,
      budget: Budget,
      launch: Engine^ => Unit
  ): Option[String] = {
    val log = org.slf4j.LoggerFactory.getLogger("grit.serve")
    slackOf(env) match {
      case Left(why) => Some(why)
      case Right((bot, app, listen)) =>
        Engine.open(config, epoch, identity, budget) match {
          case Left(refused) => Some(refused.message(java.time.Instant.now()))
          case Right(engine) =>
            val slack = new SocketSlack(bot, app)
            val stopped = new CountDownLatch(1)
            val closed = new CountDownLatch(1)
            try {
              launch(engine)
              val link: Link^{engine} = engine
              slack.self() match {
                case Left(e) => Some(s"Slack refused the bot token: $e")
                case Right(self) =>
                  val edge = new SlackEdge(
                    slack,
                    self,
                    EdgeStores(link.inbox, link.principals, link.deliveries, link.jot),
                    listen,
                    said => log.info(said)
                  )
                  Serve.metered(link, budget, java.time.Instant.now()).foreach(log.info)
                  log.info(edge.listened() match {
                    case Vector() => "slack: listening in no channel"
                    case channels => s"slack: listening in ${channels.sorted.mkString(", ")}"
                  })
                  // Unnamed, turns are simply not told a name: worth a warning, not a refusal.
                  edge.introduce() match {
                    case Right(()) => log.info("slack: the assistant is named as grit's bot is")
                    case Left(why) => log.warn(s"slack: the assistant is not named: $why")
                  }
                  slack.listen(edge.receive) match {
                    case Left(e) => Some(s"Socket Mode would not open: $e")
                    case Right(()) =>
                      log.info(
                        s"serving Slack team ${grit.slack.event.TeamId.value(self.team)} as ${grit.slack.event.UserId.value(self.bot)}"
                      )
                      // On ctrl-c or SIGTERM: stop the loop, then hold the JVM open until Slack and
                      // the engine are closed (the engine waits up to 30 s for running turns).
                      Runtime.getRuntime.addShutdownHook(Thread.ofPlatform().unstarted { () =>
                        stopped.countDown()
                        val _ = closed.await(45, java.util.concurrent.TimeUnit.SECONDS)
                      })
                      while (stopped.getCount > 0) {
                        edge
                          .deliver()
                          .left
                          .foreach(why => log.warn(s"slack: replies not read: $why"))
                        val _ = stopped.await(
                          DeliverEvery.toMillis,
                          java.util.concurrent.TimeUnit.MILLISECONDS
                        )
                      }
                      None
                  }
              }
            } finally {
              slack.close()
              engine.close()
              closed.countDown()
            }
        }
    }
  }

  /** What the log says of `budget` as `link`'s ledger stands at `now`: the cap, and, when
    * some of today's calls were not priced, that the cap counts them as nothing (a provider
    * that prices none is never capped). None with no cap.
    */
  private def metered(link: Link^, budget: Budget, now: java.time.Instant): Option[String] =
    budget.cap.map { cap =>
      val today = budget.today(now)
      val unpriced = link.db.read(link.spending.on(today)) match {
        case Right(spent) =>
          spent.cost match {
            case grit.core.message.Cost.AtLeast(_) =>
              s"; today's recorded spend, ${spent.cost.written}, includes calls no provider priced, which the cap counts as nothing"
            case grit.core.message.Cost.Exact(_) => ""
          }
        case Left(e) => s"; today's spend could not be read: $e"
      }
      s"daily cap $$${cap.usd}, days from midnight ${budget.zone}$unpriced"
    }
}
