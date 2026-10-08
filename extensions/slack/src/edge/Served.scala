package grit.slack.edge

import java.time.{Duration, Instant}

import grit.core.clock.Clock
import grit.core.edge.{CatchUp, EdgeRefusal, EdgeStores, ServedEdge, Unheard, Variable}
import grit.core.id.{AttesterName, EdgeName, PrincipalId}
import grit.core.place.Place
import grit.core.speech.Rate
import grit.core.store.{Origin, StoreError}
import grit.core.tool.ToolSet
import grit.edge.Server
import grit.slack.client.{AppToken, BotToken, Slack}
import grit.slack.event.{ChannelId, Event, TeamId, UserId}

/** The Slack edge as a deployment serves it, and its backfill, over whatever a [[Connect]]
  * makes of the tokens: [[SlackEdge.serving]] and [[SlackEdge.backfill]] connect over Socket
  * Mode.
  */
private[slack] object Served {

  /** Makes a [[Slack]] of the tokens, one per call; it is closed once, last. */
  trait Connect {
    def apply(bot: BotToken, app: AppToken): Slack^
  }

  val Name: EdgeName = EdgeName.Slack

  /** The bot token's variable (`xoxb-…`). */
  val BotTokenVar: Variable = Variable("SLACK_BOT_TOKEN")

  /** The app-level token's variable (`xapp-…`), which opens Socket Mode. */
  val AppTokenVar: Variable = Variable("SLACK_APP_TOKEN")

  val Needs: Vector[Variable] = Vector(BotTokenVar, AppTokenVar)

  /** The tokens `env` sets; why not, naming the variable, never quoting a token. */
  def tokens(env: Map[String, String]): Either[EdgeRefusal, (BotToken, AppToken)] = {
    def read[T](v: Variable, of: String => Either[String, T]): Either[EdgeRefusal, T] =
      env
        .get(Variable.value(v))
        .toRight(EdgeRefusal.Missing(v))
        .flatMap(of(_).left.map(EdgeRefusal.Malformed(v, _)))
    for {
      bot <- read(BotTokenVar, BotToken.of)
      app <- read(AppTokenVar, AppToken.of)
    } yield (bot, app)
  }

  /** The team grit's bot token is installed in, as `env`'s tokens open Slack through `connect`
    * (`auth.test`); refused as the edge's open is refused, Slack closed either way.
    */
  def installedIn(env: Map[String, String], connect: Connect^): Either[EdgeRefusal, TeamId] =
    tokens(env).flatMap { (bot, app) =>
      val slack = connect(bot, app)
      try slack.self().map(_.team).left.map(refusedToken)
      finally slack.close()
    }

  private def refusedToken(e: Any): EdgeRefusal =
    EdgeRefusal.Refused(s"Slack refused the bot token: $e")

  def serving(
      command: SlackCommand,
      backfill: Backfill,
      posting: Option[Rate],
      review: Option[SlackReview],
      connect: Connect^,
      start: Backfilling.Start
  ): ServedEdge^{connect} = new ServedEdge {
    def name: EdgeName = Name
    def needs: Vector[Variable] = Needs
    def answersAsks: Boolean = false
    override def attester: Option[AttesterName] = Some(SlackAccounts.Attester)
    override def reviewsAt: Option[Place] = review.map(_.place)
    def open(
        stores: EdgeStores^,
        env: Map[String, String],
        clock: Clock^,
        log: String => Unit
    ): Either[EdgeRefusal, ServedEdge.Open^{stores, clock, log, caps.any}] =
      tokens(env) match {
        case Left(refused) => Left(refused)
        case Right((bot, app)) =>
          val slack = connect(bot, app)
          slack.self() match {
            case Left(e) =>
              slack.close()
              Left(refusedToken(e))
            case Right(self) if review.exists(_.team != self.team) =>
              slack.close()
              Left(
                EdgeRefusal.Refused(
                  s"the review's place, ${review.fold("")(_.place.written)}, is not in grit's " +
                    s"team, ${TeamId.value(self.team)}"
                )
              )
            case Right(self) =>
              val edge = new SlackEdge(slack, self, stores, review, clock, log)
              edge.reconcile() match {
                case Left(why) =>
                  slack.close()
                  Left(EdgeRefusal.Refused(why))
                case Right(()) =>
                  // The deployment's persona is the assistant's name (ADR 0026); Slack's, logged,
                  // is what people see and may type.
                  edge.displayName() match {
                    case Right(name) => log(s"slack: grit's bot is named $name in Slack")
                    case Left(why) => log(s"slack: grit's bot's name is unread: $why")
                  }
                  slack.listen(edge.receive, edge.command(command)) match {
                    case Left(e) =>
                      slack.close()
                      Left(EdgeRefusal.Refused(s"Socket Mode would not open: $e"))
                    case Right(()) =>
                      log(
                        s"slack: serving team ${TeamId.value(self.team)} as ${UserId.value(self.bot)}, answering ${command.name}"
                      )
                      val posts = posting match {
                        case Some(rate) => served(slack, self.team, rate, stores, clock, log, edge)
                        case None => None
                      }
                      val backfilling = new Backfilling(
                        start,
                        new Backfilling.Work {
                          def wanted(): Boolean = edge.joinsWaiting()
                          def run(stopping: () => Boolean): Unit =
                            edge
                              .backfillJoins(backfill, stopping)
                              .left
                              .foreach(why =>
                                log(
                                  s"slack: $why; it is heard from where it stopped at the next open"
                                )
                              )
                        }
                      )
                      // What a crash or a close left pending, and any channel found at open.
                      backfilling.wake()
                      Right(new ServedEdge.Open {
                        def deliver(): Either[StoreError, Int] = {
                          backfilling.wake()
                          posts.foreach(_.reoffer())
                          // First, so a slow post never holds back a mark.
                          edge
                            .acknowledge()
                            .left
                            .foreach(e => log(s"slack: acknowledgements unread: $e"))
                          val delivered = edge.deliver()
                          edge.prompt().left.foreach(e => log(s"slack: review prompts unread: $e"))
                          delivered
                        }
                        override def attest(): Either[StoreError, Int] = edge.attest()
                        def close(): Unit = {
                          posts.foreach(_.stop())
                          backfilling.close(Backfill.StopWithin)
                          slack.close()
                        }
                      })
                  }
              }
          }
      }
  }

  /** `slack_post` as served: offered again over the channels it may post in now, when they
    * may have changed ([[SlackEdge.reoffer]]), and stopped.
    */
  private trait PostingServed {
    def reoffer(): Unit
    def stop(): Unit
  }

  /** `slack_post` served at [[SlackEdge.PostsAt]] through `slack`, at most `rate` posts,
    * counted on `clock`, over the channels of `team` that `edge` says it may post in, offered
    * at once and again on each [[PostingServed.reoffer]]; each change of them logged. `None`, logged,
    * when the desk would not register, so the edge serves its replies without it.
    */
  private def served(
      slack: Slack^,
      team: TeamId,
      rate: Rate,
      stores: EdgeStores^,
      clock: Clock^,
      log: String => Unit,
      edge: SlackEdge^{slack, stores, clock, log}
  ): Option[PostingServed^{slack, stores, clock, log, caps.any}] = {
    val place = SlackEdge.PostsAt.place
    stores.desks.register(PrincipalId.Grit, Set(place)) match {
      case Left(e) =>
        log(s"slack: posts nowhere: ${place.written} not registered: ${e.why}")
        None
      case Right(desk) =>
        val tool = new Posting(slack, clock, rate, team)
        // The tool set last advertised, so only a change is logged; written and read only by
        // offer, run by the open and then by the kit's delivery rounds, one after another.
        @caps.unsafe.untrackedCaptures
        var last: Option[ToolSet] = None
        def offer(named: Vector[(String, ChannelId)]): Either[String, Unit] =
          for {
            offered <- tool.offer(named)
            _ <- desk
              .advertise(place, offered, Vector.empty)
              .left
              .map(e => s"${place.written} not advertised: ${e.why}")
          } yield {
            if (!last.contains(offered))
              log(named match {
                case Vector() =>
                  "slack: slack_post posts in no channel now"
                case _ =>
                  s"slack: slack_post posts in ${named.map((n, id) => s"#$n (${ChannelId.value(id)})").mkString(", ")}, " +
                    s"at most ${rate.count} per ${rate.per}"
              })
            last = Some(offered)
          }
        val server = new Server(
          desk,
          tool,
          run => { val _ = Thread.ofVirtual().start(() => run()) },
          said => log(s"${place.written}: $said")
        )
        edge.reoffer(offer)
        server.serve()
        Some(new PostingServed {
          def reoffer(): Unit = edge.reoffer(offer)
          def stop(): Unit = server.close()
        })
    }
  }

  def backfill(days: Int, connect: Connect^): CatchUp^{connect} =
    new CatchUp {
      def name: EdgeName = Name
      def needs: Vector[Variable] = Needs
      override def attester: Option[AttesterName] = Some(SlackAccounts.Attester)
      def open(
          stores: EdgeStores^,
          env: Map[String, String],
          clock: Clock^,
          log: String => Unit
      ): Either[EdgeRefusal, CatchUp.Open^{stores, clock, log, caps.any}] =
        tokens(env) match {
          case Left(refused) => Left(refused)
          case Right((bot, app)) =>
            val slack = connect(bot, app)
            slack.self() match {
              case Left(e) =>
                slack.close()
                Left(refusedToken(e))
              case Right(self) =>
                val edge = new SlackEdge(slack, self, stores, None, clock, log)
                val from = clock.now().minus(Duration.ofDays(days.toLong))
                val sorted = edge.reconcile() match {
                  case Left(why) => Left(EdgeRefusal.Refused(why))
                  case Right(()) if edge.channels.isEmpty =>
                    Left(
                      EdgeRefusal.Refused(
                        "grit's bot is a member of no channel: there is nothing to backfill"
                      )
                    )
                  case Right(()) => Right(edge.channels)
                }
                val read =
                  sorted.flatMap(
                    _.foldLeft[Either[EdgeRefusal, Vector[(ChannelId, Vector[Event.Said])]]](
                      Right(Vector.empty)
                    ) { (acc, channel) =>
                      acc.flatMap(done =>
                        edge
                          .unheard(channel, from)
                          .left
                          .map(why =>
                            EdgeRefusal.Refused(s"${ChannelId.value(channel)} not read: $why")
                          )
                          .map(said => done :+ (channel, said))
                      )
                    }
                  )
                read match {
                  case Left(refused) =>
                    slack.close()
                    Left(refused)
                  case Right(each) =>
                    val found = each.map { (channel, said) =>
                      val id = ChannelId.value(channel)
                      val name = slack.channelName(channel).toOption.flatten
                      Unheard(
                        name.fold(id)(n => s"#$n ($id)"),
                        Origin.channel(TeamId.value(self.team), id),
                        threads(said)
                      )
                    }
                    val all = each.flatMap(_._2)
                    Right(new CatchUp.Open {
                      def since: Instant = from
                      def unheard: Vector[Unheard] = found
                      def hear(): Either[EdgeRefusal, Unit] =
                        edge.backfill(all).left.map(EdgeRefusal.Failed(_))
                      def close(): Unit = slack.close()
                    })
                }
            }
        }
    }

  /** `said`'s threads, each its messages' lengths in the order they were said, the threads
    * in the order each began.
    */
  private def threads(said: Vector[Event.Said]): Vector[Vector[Int]] = {
    def key(m: Event.Said) = (m.channel, m.thread)
    said.map(key).distinct.map(k => said.filter(key(_) == k).map(_.text.length))
  }
}
