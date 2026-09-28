package grit.app.serve

import java.util.concurrent.CountDownLatch

import scala.concurrent.duration.{DurationInt, FiniteDuration}

import grit.core.host.ProcessIdentity
import grit.dbos.engine.{Engine, Link}
import grit.dbos.sql.DbConfig
import grit.slack.client.{AppToken, BotToken, SocketSlack}
import grit.slack.edge.{EdgeStores, SlackEdge}

/** `grit serve`: the engine of one database, and the Slack edge in its process (ADR 0019). */
object Serve {

  /** The Slack bot token's variable (`xoxb-…`). */
  val BotTokenVar = "SLACK_BOT_TOKEN"

  /** The Slack app-level token's variable (`xapp-…`), which opens Socket Mode. */
  val AppTokenVar = "SLACK_APP_TOKEN"

  /** How often the edge looks for finished turns to post: 500 ms. */
  val DeliverEvery: FiniteDuration = 500.millis

  /** Runs the engine of `config`'s database, launched by `launch`, and the Slack edge over it,
    * connected with the tokens in `env`, until the process is stopped (its shutdown closes
    * Slack, then the engine). Why it could not start: a token missing or of the wrong kind
    * (never quoted), the database's engine held by another grit (never attached to: two edges
    * would each post the same replies), or Slack refusing the tokens.
    */
  def run(
      env: Map[String, String],
      config: DbConfig,
      epoch: String,
      identity: ProcessIdentity,
      launch: Engine^ => Unit
  ): Option[String] = {
    val log = org.slf4j.LoggerFactory.getLogger("grit.serve")
    val tokens = for {
      bot <- env
        .get(BotTokenVar)
        .toRight(s"$BotTokenVar is not set")
        .flatMap(BotToken.of(_).left.map(w => s"$BotTokenVar: $w"))
      app <- env
        .get(AppTokenVar)
        .toRight(s"$AppTokenVar is not set")
        .flatMap(AppToken.of(_).left.map(w => s"$AppTokenVar: $w"))
    } yield (bot, app)
    tokens match {
      case Left(why) => Some(why)
      case Right((bot, app)) =>
        Engine.open(config, epoch, identity) match {
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
                    said => log.info(said)
                  )
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
}
