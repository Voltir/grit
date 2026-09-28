package grit.slack.client

import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

import grit.slack.event.{ChannelId, TeamId, Ts, UserId}
import grit.slack.text.Post

import com.slack.api.methods.request.auth.AuthTestRequest
import com.slack.api.methods.request.chat.ChatPostMessageRequest
import com.slack.api.methods.request.conversations.{
  ConversationsInfoRequest,
  ConversationsRepliesRequest
}
import com.slack.api.methods.request.reactions.{ReactionsAddRequest, ReactionsRemoveRequest}
import com.slack.api.methods.request.users.UsersInfoRequest
import com.slack.api.methods.{MethodsClient, SlackApiException, SlackApiTextResponse}
import com.slack.api.model.Message
import com.slack.api.socket_mode.SocketModeClient
import com.slack.api.socket_mode.request.EventsApiEnvelope
import com.slack.api.socket_mode.response.AckResponse
import com.slack.api.{Slack => Sdk}

/** [[Slack]] over Socket Mode, opened with `app`, calling the Web API with `bot`: the Slack SDK
  * (slack-api-client) behind the trait, and the only file that names it. Closing it closes the
  * socket.
  */
final class SocketSlack(bot: BotToken, app: AppToken) extends Slack, AutoCloseable {
  import SocketSlack.*

  private val sdk: Sdk = Sdk.getInstance()
  private val methods: MethodsClient = sdk.methods(bot.value)

  // Set once by listen, closed by close; only ever replaced whole.
  @caps.unsafe.untrackedCaptures
  @volatile private var socket: Option[SocketModeClient] = None

  def self(): Either[SlackError, Self] =
    call(methods.authTest(AuthTestRequest.builder().build()))
      .map(r => Self(TeamId(r.getTeamId), UserId(r.getUserId)))

  def listen(handle: String => Boolean): Either[SlackError, Unit] =
    try {
      val client = sdk.socketMode(app.value, SocketModeClient.Backend.JavaWebSocket)
      client.setAutoReconnectEnabled(true)
      client.addEventsApiEnvelopeListener { (envelope: EventsApiEnvelope) =>
        if (handle(envelope.getPayload.toString))
          client.sendSocketModeResponse(
            AckResponse.builder().envelopeId(envelope.getEnvelopeId).build()
          )
      }
      client.connect()
      socket = Some(client)
      Right(())
    } catch { case NonFatal(e) => Left(unreachable(e)) }

  def post(channel: ChannelId, thread: Ts, post: Post, tag: Tag): Either[SlackError, Ts] =
    call(methods.chatPostMessage(request(channel, thread, post, tag))).map(r => Ts(r.getTs))

  def tagged(channel: ChannelId, thread: Ts, tag: Tag): Either[SlackError, Vector[Ts]] = {
    def page(cursor: Option[String], found: Vector[Ts]): Either[SlackError, Vector[Ts]] = {
      val req = ConversationsRepliesRequest
        .builder()
        .channel(ChannelId.value(channel))
        .ts(Ts.value(thread))
        .includeAllMetadata(true)
        .limit(200)
      cursor.foreach(c => req.cursor(c))
      call(methods.conversationsReplies(req.build())).flatMap { r =>
        val here = found ++ Option(r.getMessages)
          .fold(Vector.empty[Message])(_.asScala.toVector)
          .filter(carries(_, tag))
          .map(m => Ts(m.getTs))
        val next =
          Option(r.getResponseMetadata).flatMap(m => Option(m.getNextCursor)).filter(_.nonEmpty)
        next match {
          case Some(c) if r.isHasMore => page(Some(c), here)
          case _ => Right(here)
        }
      }
    }
    page(None, Vector.empty)
  }

  def react(channel: ChannelId, ts: Ts, emoji: String): Either[SlackError, Unit] =
    call(
      methods.reactionsAdd(
        ReactionsAddRequest
          .builder()
          .channel(ChannelId.value(channel))
          .timestamp(Ts.value(ts))
          .name(emoji)
          .build()
      )
    ).map(_ => ()).left.flatMap(settled("already_reacted"))

  def unreact(channel: ChannelId, ts: Ts, emoji: String): Either[SlackError, Unit] =
    call(
      methods.reactionsRemove(
        ReactionsRemoveRequest
          .builder()
          .channel(ChannelId.value(channel))
          .timestamp(Ts.value(ts))
          .name(emoji)
          .build()
      )
    ).map(_ => ()).left.flatMap(settled("no_reaction"))

  def name(user: UserId): Either[SlackError, Option[String]] =
    call(methods.usersInfo(UsersInfoRequest.builder().user(UserId.value(user)).build())).map { r =>
      val u = Option(r.getUser)
      val display = u.flatMap(x => Option(x.getProfile)).flatMap(p => Option(p.getDisplayName))
      val real = u.flatMap(x => Option(x.getRealName))
      display.filter(_.trim.nonEmpty).orElse(real.filter(_.trim.nonEmpty))
    }

  def public(channel: ChannelId): Either[SlackError, Boolean] =
    call(
      methods.conversationsInfo(
        ConversationsInfoRequest.builder().channel(ChannelId.value(channel)).build()
      )
    )
      .map(r =>
        Option(r.getChannel).exists(c => c.isChannel && !c.isPrivate && !c.isIm && !c.isMpim)
      )
      .left
      .flatMap {
        // A private channel, or one grit may not look at: not public, never a failure.
        case SlackError.Refused("channel_not_found" | "missing_scope" | "not_in_channel") =>
          Right(false)
        case other => Left(other)
      }

  def channelName(channel: ChannelId): Either[SlackError, Option[String]] =
    call(
      methods.conversationsInfo(
        ConversationsInfoRequest.builder().channel(ChannelId.value(channel)).build()
      )
    )
      .map(r => Option(r.getChannel).flatMap(c => Option(c.getName)).filter(_.nonEmpty))
      .left
      .flatMap {
        // One grit may not look at has no name it can be told.
        case SlackError.Refused("channel_not_found" | "missing_scope" | "not_in_channel") =>
          Right(None)
        case other => Left(other)
      }

  def close(): Unit = socket.foreach { s =>
    try s.close()
    catch { case NonFatal(_) => () }
  }
}

object SocketSlack {

  /** The metadata event type a grit reply carries. */
  val ReplyEvent = "grit_reply"

  /** The metadata event type grit's line refusing a message carries. */
  val RefusalEvent = "grit_refusal"

  /** The request that posts `post` in `thread` of `channel`, carrying `tag` as metadata; link
    * previews off, so a reply is only what grit wrote.
    */
  def request(channel: ChannelId, thread: Ts, post: Post, tag: Tag): ChatPostMessageRequest =
    ChatPostMessageRequest
      .builder()
      .channel(ChannelId.value(channel))
      .threadTs(Ts.value(thread))
      .blocksAsString(post.blocks.render())
      .text(post.fallback)
      .metadata(
        Message.Metadata
          .builder()
          .eventType(event(tag))
          .eventPayload(payload(tag).asJava)
          .build()
      )
      .unfurlLinks(false)
      .unfurlMedia(false)
      .build()

  /** Whether `m` carries `tag`, as [[request]] wrote it. */
  def carries(m: Message, tag: Tag): Boolean =
    Option(m.getMetadata).exists { md =>
      md.getEventType == event(tag) &&
      Option(md.getEventPayload).map(_.asScala).exists { p =>
        payload(tag).forall((k, v) => p.get(k).map(_.toString).contains(v))
      }
    }

  /** The metadata event type `tag` is written under. */
  private def event(tag: Tag): String = tag match {
    case Tag.Reply(_, _) => ReplyEvent
    case Tag.Refused(_) => RefusalEvent
  }

  /** `tag`'s metadata payload. */
  private def payload(tag: Tag): Map[String, String] = tag match {
    case Tag.Reply(turn, part) => Map("turn" -> turn, "part" -> part.toString)
    case Tag.Refused(message) => Map("message" -> Ts.value(message))
  }

  /** A refusal `code` as done, anything else as it is. */
  private def settled(code: String)(e: SlackError): Either[SlackError, Unit] = e match {
    case SlackError.Refused(`code`) => Right(())
    case other => Left(other)
  }

  /** `body`'s response, or what went wrong: Slack's error code, a rate limit, or the
    * network.
    */
  private def call[R <: SlackApiTextResponse](body: => R): Either[SlackError, R] =
    try {
      val r = body
      if (r.isOk) Right(r) else Left(SlackError.Refused(Option(r.getError).getOrElse("unknown")))
    } catch {
      case e: SlackApiException if e.getResponse.code == 429 =>
        val after =
          Option(e.getResponse.header("Retry-After")).flatMap(_.trim.toIntOption).getOrElse(1)
        Left(SlackError.Limited(after.seconds))
      case e: SlackApiException => Left(SlackError.Refused(s"http ${e.getResponse.code}"))
      case NonFatal(e) => Left(unreachable(e))
    }

  private def unreachable(e: Throwable): SlackError =
    SlackError.Unreachable(Option(e.getMessage).getOrElse(e.getClass.getSimpleName))
}
