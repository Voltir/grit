package grit.slack.edge

import java.time.{Duration, Instant}

import grit.core.clock.Clock
import grit.core.edge.{CatchUp, EdgeRefusal, EdgeStores, ServedEdge, Unheard, Variable}
import grit.core.id.{AttesterName, EdgeName, PrincipalId}
import grit.core.place.{Namespace, Place}
import grit.core.store.StoreError
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
      channels: Set[ChannelId],
      posts: Option[Posts],
      review: Option[SlackReview],
      connect: Connect^
  ): ServedEdge^{connect} = new ServedEdge {
    def name: EdgeName = Name
    def needs: Vector[Variable] = Needs
    def answersAsks: Boolean = false
    override def attester: Option[AttesterName] = Some(SlackAccounts.Attester)
    override def reviewsAt: Option[Place] = review.map(_.place)
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
            case Right(self) if review.exists(_.team != self.team) =>
              slack.close()
              Left(
                EdgeRefusal.Refused(
                  s"the review's place, ${review.fold("")(_.place.written)}, is not in grit's " +
                    s"team, ${TeamId.value(self.team)}"
                )
              )
            case Right(self) =>
              val edge = new SlackEdge(slack, self, stores, channels, review, Clock.system(), log)
              log(edge.listened() match {
                case Vector() => "slack: listening in no channel"
                case listened => s"slack: listening in ${listened.sorted.mkString(", ")}"
              })
              // The deployment's persona is the assistant's name (ADR 0026); Slack's, logged,
              // is what people see and may type.
              edge.displayName() match {
                case Right(name) => log(s"slack: grit's bot is named $name in Slack")
                case Left(why) => log(s"slack: grit's bot's name is unread: $why")
              }
              slack.listen(edge.receive) match {
                case Left(e) =>
                  slack.close()
                  Left(EdgeRefusal.Refused(s"Socket Mode would not open: $e"))
                case Right(()) =>
                  log(
                    s"slack: serving team ${TeamId.value(self.team)} as ${UserId.value(self.bot)}"
                  )
                  val stopPosting = posts match {
                    case Some(p) => posting(slack, self.team, p, stores, log)
                    case None => None
                  }
                  Right(new ServedEdge.Open {
                    def deliver(): Either[StoreError, Int] = {
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
                      stopPosting.foreach(_())
                      slack.close()
                    }
                  })
              }
          }
      }
  }

  /** `slack_post` served at [[SlackEdge.PostsAt]] as `posts` allows, over the channels of
    * `posts` Slack gives a name, each left out logged, each written to at its place in `team`;
    * `None`, logged, when none has one or the desk would not register or advertise, so the edge
    * serves its replies without it. What stops serving it.
    */
  private def posting(
      slack: Slack^,
      team: TeamId,
      posts: Posts,
      stores: EdgeStores^,
      log: String => Unit
  ): Option[() ->{slack, stores, log, caps.any} Unit] = {
    val named = posts.to.toVector.sortBy(ChannelId.value).flatMap { id =>
      val shown = ChannelId.value(id)
      slack.channelName(id) match {
        case Right(Some(name)) => Vector((name, id))
        case Right(None) =>
          log(s"slack: not posting to $shown: Slack gives grit no name for it")
          Vector.empty
        case Left(e) =>
          log(s"slack: not posting to $shown: Slack not asked: $e")
          Vector.empty
      }
    }
    val place = SlackEdge.PostsAt.place
    def nowhere(why: String): None.type = {
      log(s"slack: posts nowhere: $why")
      None
    }
    Posting.of(slack, Clock.system(), posts.rate, team, named) match {
      case Left(_) if named.isEmpty => nowhere("no channel it may post to has a name grit can read")
      case Left(why) => nowhere(why)
      case Right(tool) =>
        stores.desks.register(PrincipalId.Grit, Set(place)) match {
          case Left(e) => nowhere(s"${place.written} not registered: ${e.why}")
          case Right(desk) =>
            desk.advertise(place, tool.offered, Vector.empty) match {
              case Left(e) => nowhere(s"${place.written} not advertised: ${e.why}")
              case Right(()) =>
                val server = new Server(
                  desk,
                  tool,
                  run => { val _ = Thread.ofVirtual().start(() => run()) },
                  said => log(s"${place.written}: $said")
                )
                log(
                  s"slack: posts to ${named.map((n, id) => s"#$n (${ChannelId.value(id)})").mkString(", ")}, " +
                    s"at most ${posts.rate.count} per ${posts.rate.per}"
                )
                server.serve()
                Some(() => server.close())
            }
        }
    }
  }

  def backfill(channels: Set[ChannelId], days: Int, connect: Connect^): CatchUp^{connect} =
    new CatchUp {
      def name: EdgeName = Name
      def needs: Vector[Variable] = Needs
      override def attester: Option[AttesterName] = Some(SlackAccounts.Attester)
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
                  val edge = new SlackEdge(slack, self, stores, channels, None, Clock.system(), log)
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
                        Unheard(
                          name.fold(id)(n => s"#$n ($id)"),
                          Place.under(Namespace.Slack, Vector(TeamId.value(self.team), id)),
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
