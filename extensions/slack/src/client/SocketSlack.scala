package grit.slack.client

import java.time.Instant

import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

import grit.slack.event.{ChannelId, Listed, TeamId, Ts, UserId}
import grit.slack.text.Post

import com.slack.api.methods.request.auth.AuthTestRequest
import com.slack.api.methods.request.chat.ChatPostMessageRequest
import com.slack.api.methods.request.conversations.{
  ConversationsHistoryRequest,
  ConversationsInfoRequest,
  ConversationsRepliesRequest
}
import com.slack.api.methods.request.reactions.{ReactionsAddRequest, ReactionsRemoveRequest}
import com.slack.api.methods.request.users.UsersInfoRequest
import com.slack.api.methods.{MethodsClient, SlackApiException, SlackApiTextResponse}
import com.slack.api.model.{Message, ResponseMetadata}
import com.slack.api.socket_mode.SocketModeClient
import com.slack.api.socket_mode.request.EventsApiEnvelope
import com.slack.api.socket_mode.response.AckResponse
import com.slack.api.{Slack => Sdk, SlackConfig}

/** [[Slack]] over Socket Mode, opened with `app`, calling the Web API at `api` (a URL ending
  * `/api/`) with `bot`: the Slack SDK (slack-api-client) behind the trait, and the only file
  * that names it. Closing it closes the socket.
  */
final class SocketSlack private[client] (bot: BotToken, app: AppToken, api: String)
    extends Slack,
      AutoCloseable {
  import SocketSlack.*

  /** At Slack's own Web API. */
  def this(bot: BotToken, app: AppToken) = this(bot, app, SocketSlack.SlackApi)

  private val sdk: Sdk = {
    val config = new SlackConfig()
    config.setMethodsEndpointUrlPrefix(api)
    Sdk.getInstance(config)
  }
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

  def postTopLevel(channel: ChannelId, post: Post, tag: Tag): Either[SlackError, Ts] =
    call(methods.chatPostMessage(topLevel(channel, post, tag))).map(r => Ts(r.getTs))

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

  def root(channel: ChannelId, thread: Ts): Either[SlackError, Option[Root]] = {
    val req = ConversationsRepliesRequest
      .builder()
      .channel(ChannelId.value(channel))
      .ts(Ts.value(thread))
      .includeAllMetadata(true)
      .limit(1)
      .build()
    call(methods.conversationsReplies(req))
      .map { r =>
        Option(r.getMessages)
          .fold(Vector.empty[Message])(_.asScala.toVector)
          .find(_.getTs == Ts.value(thread))
          .map(m =>
            Root(
              Option(m.getUser).map(UserId(_)),
              SocketSlack.tagOf(m),
              Option(m.getText).getOrElse("")
            )
          )
      }
      .left
      .flatMap {
        case SlackError.Refused("thread_not_found") => Right(None)
        case other => Left(other)
      }
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

  def history(channel: ChannelId, since: Instant): Either[SlackError, Vector[Listed]] = {
    val from = oldest(since)
    val id = ChannelId.value(channel)
    def top(cursor: Option[String]): Either[SlackError, Page] = {
      val req =
        ConversationsHistoryRequest.builder().channel(id).oldest(from).inclusive(true).limit(200)
      cursor.foreach(c => req.cursor(c))
      patient(() => methods.conversationsHistory(req.build()))
        .map(r => page(r.getMessages, r.getResponseMetadata, r.isHasMore))
    }
    def replies(root: String)(cursor: Option[String]): Either[SlackError, Page] = {
      val req =
        ConversationsRepliesRequest
          .builder()
          .channel(id)
          .ts(root)
          .oldest(from)
          .inclusive(true)
          .limit(200)
      cursor.foreach(c => req.cursor(c))
      patient(() => methods.conversationsReplies(req.build()))
        .map(r => page(r.getMessages, r.getResponseMetadata, r.isHasMore))
    }
    for {
      roots <- pages(top)
      threads <- roots
        .filter(m => Option(m.getReplyCount).exists(_ > 0))
        .foldLeft[Either[SlackError, Vector[Message]]](Right(Vector.empty)) { (acc, root) =>
          acc.flatMap(found => pages(replies(root.getTs)).map(found ++ _))
        }
    } yield (roots ++ threads)
      .distinctBy(_.getTs)
      .sortBy(m => scala.util.Try(BigDecimal(m.getTs)).getOrElse(BigDecimal(0)))
      .map(listed)
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

  /** Slack's own Web API. */
  private val SlackApi = "https://slack.com/api/"

  /** The metadata event type a grit reply carries. */
  val ReplyEvent = "grit_reply"

  /** The metadata event type grit's line refusing a message carries. */
  val RefusalEvent = "grit_refusal"

  /** The metadata event type a post `slack_post` made carries. */
  val PostEvent = "grit_post"

  /** The request that posts `post` in `thread` of `channel`, carrying `tag` as metadata; link
    * previews off, so a reply is only what grit wrote.
    */
  def request(channel: ChannelId, thread: Ts, post: Post, tag: Tag): ChatPostMessageRequest =
    message(channel, post, tag).threadTs(Ts.value(thread)).build()

  /** The request that posts `post` at `channel`'s top level, as [[request]] posts one in a
    * thread.
    */
  def topLevel(channel: ChannelId, post: Post, tag: Tag): ChatPostMessageRequest =
    message(channel, post, tag).build()

  /** [[request]] and [[topLevel]] alike, but for the thread. */
  private def message(
      channel: ChannelId,
      post: Post,
      tag: Tag
  ): ChatPostMessageRequest.ChatPostMessageRequestBuilder =
    ChatPostMessageRequest
      .builder()
      .channel(ChannelId.value(channel))
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

  /** Whether `m` carries `tag`, as [[request]] wrote it. */
  def carries(m: Message, tag: Tag): Boolean =
    Option(m.getMetadata).exists { md =>
      md.getEventType == event(tag) &&
      Option(md.getEventPayload).map(_.asScala).exists { p =>
        payload(tag).forall((k, v) => p.get(k).map(_.toString).contains(v))
      }
    }

  /** The tag `m` carries, as [[request]] wrote it; `None` for a message carrying none. */
  def tagOf(m: Message): Option[Tag] =
    Option(m.getMetadata).flatMap { md =>
      val p: Map[String, String] = Option(md.getEventPayload)
        .fold(Map.empty[String, String])(_.asScala.toMap.map((k, v) => (k, String.valueOf(v))))
      Option(md.getEventType).flatMap {
        case PostEvent => p.get("request").map(Tag.Sent(_))
        case ReplyEvent =>
          for {
            turn <- p.get("turn")
            part <- p.get("part").flatMap(_.toIntOption)
          } yield Tag.Reply(turn, part)
        case RefusalEvent => p.get("message").map(m => Tag.Refused(Ts(m)))
        case _ => None
      }
    }

  /** The metadata event type `tag` is written under. */
  private def event(tag: Tag): String = tag match {
    case Tag.Reply(_, _) => ReplyEvent
    case Tag.Refused(_) => RefusalEvent
    case Tag.Sent(_) => PostEvent
  }

  /** `tag`'s metadata payload. */
  private def payload(tag: Tag): Map[String, String] = tag match {
    case Tag.Reply(turn, part) => Map("turn" -> turn, "part" -> part.toString)
    case Tag.Refused(message) => Map("message" -> Ts.value(message))
    case Tag.Sent(request) => Map("request" -> request)
  }

  /** How many times a rate-limited call is waited out and made again. */
  val Retries = 5

  /** `since` as a ts, to the microsecond. */
  def oldest(since: Instant): String = {
    val micros = (since.getNano / 1000).toString
    s"${since.getEpochSecond}.${"0" * (6 - micros.length)}$micros"
  }

  /** `m` as a channel's history lists it. */
  def listed(m: Message): Listed =
    Listed(
      Ts(m.getTs),
      Option(m.getThreadTs).map(Ts(_)),
      Option(m.getUser).map(UserId(_)),
      Option(m.getBotId).nonEmpty,
      Option(m.getSubtype),
      Option(m.getText).getOrElse("")
    )

  /** One page of a listing: its messages, and the cursor of the next when there is one. */
  private final case class Page(messages: Vector[Message], next: Option[String])

  private def page(
      messages: java.util.List[Message],
      meta: ResponseMetadata,
      more: Boolean
  ): Page =
    Page(
      Option(messages).fold(Vector.empty[Message])(_.asScala.toVector),
      Option(meta).flatMap(m => Option(m.getNextCursor)).filter(c => more && c.nonEmpty)
    )

  /** Every message of every page `fetch` returns, from the first (no cursor) on. */
  private def pages(
      fetch: Option[String] => Either[SlackError, Page]
  ): Either[SlackError, Vector[Message]] = {
    @scala.annotation.tailrec
    def from(cursor: Option[String], found: Vector[Message]): Either[SlackError, Vector[Message]] =
      fetch(cursor) match {
        case Left(e) => Left(e)
        case Right(p) =>
          p.next match {
            case Some(next) => from(Some(next), found ++ p.messages)
            case None => Right(found ++ p.messages)
          }
      }
    from(None, Vector.empty)
  }

  /** `body`'s response as [[call]] reads it, a rate limit waited out and the call made again,
    * up to [[Retries]] times.
    */
  private def patient[R <: SlackApiTextResponse](body: () => R): Either[SlackError, R] = {
    @scala.annotation.tailrec
    def attempt(left: Int): Either[SlackError, R] = call(body()) match {
      case Left(SlackError.Limited(after)) if left > 0 =>
        Thread.sleep(after.toMillis)
        attempt(left - 1)
      case other => other
    }
    attempt(Retries)
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
