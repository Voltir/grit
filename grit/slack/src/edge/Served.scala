package grit.slack.edge

import java.time.{Duration, Instant}

import grit.core.edge.{CatchUp, EdgeRefusal, EdgeStores, ServedEdge, Unheard, Variable}
import grit.core.id.EdgeName
import grit.core.store.StoreError
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

  val Name: EdgeName = EdgeName("slack")

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

  private def refusedToken(e: Any): EdgeRefusal =
    EdgeRefusal.Refused(s"Slack refused the bot token: $e")

  def serving(channels: Set[ChannelId], connect: Connect^): ServedEdge^{connect} = new ServedEdge {
    def name: EdgeName = Name
    def needs: Vector[Variable] = Needs
    def answersAsks: Boolean = false
    def open(
        stores: EdgeStores^,
        env: Map[String, String],
        log: String => Unit
    ): Either[EdgeRefusal, ServedEdge.Open^{stores, log, caps.any}] =
      tokens(env) match {
        case Left(refused) => Left(refused)
        case Right((bot, app)) =>
          val slack = connect(bot, app)
          slack.self() match {
            case Left(e) =>
              slack.close()
              Left(refusedToken(e))
            case Right(self) =>
              val edge = new SlackEdge(slack, self, stores, channels, log)
              log(edge.listened() match {
                case Vector() => "slack: listening in no channel"
                case listened => s"slack: listening in ${listened.sorted.mkString(", ")}"
              })
              // Unnamed, turns are simply not told a name: worth a warning, not a refusal.
              edge.introduce() match {
                case Right(()) => log("slack: the assistant is named as grit's bot is")
                case Left(why) => log(s"slack: the assistant is not named: $why")
              }
              slack.listen(edge.receive) match {
                case Left(e) =>
                  slack.close()
                  Left(EdgeRefusal.Refused(s"Socket Mode would not open: $e"))
                case Right(()) =>
                  log(
                    s"slack: serving team ${TeamId.value(self.team)} as ${UserId.value(self.bot)}"
                  )
                  Right(new ServedEdge.Open {
                    def deliver(): Either[StoreError, Int] = edge.deliver()
                    def close(): Unit = slack.close()
                  })
              }
          }
      }
  }

  def backfill(channels: Set[ChannelId], days: Int, connect: Connect^): CatchUp^{connect} =
    new CatchUp {
      def name: EdgeName = Name
      def needs: Vector[Variable] = Needs
      def open(
          stores: EdgeStores^,
          env: Map[String, String],
          now: Instant,
          log: String => Unit
      ): Either[EdgeRefusal, CatchUp.Open^{stores, log, caps.any}] =
        if (channels.isEmpty)
          Left(EdgeRefusal.Refused("Slack listens in no channel: there is nothing to backfill"))
        else
          tokens(env) match {
            case Left(refused) => Left(refused)
            case Right((bot, app)) =>
              val slack = connect(bot, app)
              slack.self() match {
                case Left(e) =>
                  slack.close()
                  Left(refusedToken(e))
                case Right(self) =>
                  val edge = new SlackEdge(slack, self, stores, channels, log)
                  val from = now.minus(Duration.ofDays(days.toLong))
                  val sorted = channels.toVector.sortBy(ChannelId.value)
                  val read =
                    sorted.foldLeft[Either[EdgeRefusal, Vector[(ChannelId, Vector[Event.Said])]]](
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
                  read match {
                    case Left(refused) =>
                      slack.close()
                      Left(refused)
                    case Right(each) =>
                      val found = each.map { (channel, said) =>
                        val id = ChannelId.value(channel)
                        val name = slack.channelName(channel).toOption.flatten
                        Unheard(name.fold(id)(n => s"#$n ($id)"), threads(said))
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
