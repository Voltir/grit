package grit.mcp.client

import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.{
  CompletableFuture,
  ExecutionException,
  Flow,
  LinkedBlockingQueue,
  TimeUnit,
  TimeoutException
}

import scala.jdk.DurationConverters.*
import scala.jdk.OptionConverters.*

import grit.core.clock.Clock
import grit.mcp.client.McpClient.Timeout
import grit.mcp.wire.{Headers, McpError, Rpc, Sse}

/** Streamable HTTP under a client: each request a POST of its own (no session) to `transport`'s
  * endpoint carrying the bearer `authorizer` gives, timing out after [[McpClient.Timeout]]. A
  * request for which it gives none is not sent, and is its [[McpError.NoToken]].
  */
private[client] final class HttpConnection(
    transport: McpTransport.Http,
    authorizer: Authorizer,
    clock: Clock
) extends Connection {

  import HttpConnection.*

  private val http = HttpClient.newBuilder().connectTimeout(Timeout.toJava).build()

  // Each request's JSON-RPC id, unique within this connection: a counter written only by its own
  // atomic increment, so no reader sees a torn value.
  @caps.unsafe.untrackedCaptures
  private val ids = new AtomicLong(0L)

  def exchange(call: Rpc.Call): Either[McpError, ujson.Obj] =
    authorizer.bearer(clock).flatMap(exchange(call, _))

  /** Nothing: each request is its own. */
  def close(): Unit = ()

  /** The result of one request making `call`, carrying `bearer`. */
  private def exchange(call: Rpc.Call, bearer: Bearer): Either[McpError, ujson.Obj] = {
    val id = ids.incrementAndGet()
    val request = Headers
      .of(call)
      .foldLeft(HttpRequest.newBuilder(transport.endpoint).timeout(Timeout.toJava)) {
        case (b, (k, v)) => b.header(k, v)
      }
      .header("Authorization", s"Bearer ${bearer.value}")
      .POST(HttpRequest.BodyPublishers.ofString(ujson.write(Rpc.request(id, call))))
      .build()
    val body = new Body(clock, clock.millis() + Timeout.toMillis)
    val head = new CompletableFuture[HttpResponse.ResponseInfo]()
    val sent = http.sendAsync(
      request,
      (info: HttpResponse.ResponseInfo) => {
        head.complete(info)
        HttpResponse.BodySubscribers.fromLineSubscriber(body)
      }
    )
    // A failure before the headers ends the wait for them at once.
    sent.whenComplete((_, failed) => if (failed != null) head.completeExceptionally(failed))
    try {
      val info = head.get(Timeout.toMillis, TimeUnit.MILLISECONDS)
      val status = info.statusCode
      if (status == 200)
        info.headers.firstValue("Content-Type").toScala.map(mediaType) match {
          case Some("text/event-stream") =>
            Sse
              .response(body.lines, id)
              .left
              .map(e => body.broke.fold(e)(McpError.Unreachable(_)))
              .flatMap(Rpc.result(id, _))
          case Some("application/json") =>
            val text = body.lines.mkString("\n")
            body.broke match {
              case Some(why) => Left(McpError.Unreachable(why))
              case None =>
                scala.util
                  .Try(ujson.read(text))
                  .toOption
                  .toRight(McpError.Unreadable("an application/json answer that is not JSON"))
                  .flatMap(Rpc.result(id, _))
            }
          case other =>
            Left(McpError.Unreadable(s"an answer of type ${other.getOrElse("none")}"))
        }
      else
        Left(
          Rpc.failure(
            status,
            // A server that echoes the token in a refusal does not get it into the error.
            body.lines.mkString("\n").replace(bearer.value, "[token]"),
            info.headers.firstValue("WWW-Authenticate").toScala
          )
        )
    } catch {
      case _: TimeoutException => Left(McpError.Unreachable(s"no answer within $Timeout"))
      case e: ExecutionException =>
        val cause = Option(e.getCause).getOrElse(e)
        // The message never includes the request, so never the token.
        Left(McpError.Unreachable(s"${cause.getClass.getSimpleName}: ${cause.getMessage}"))
      case _: InterruptedException =>
        Thread.currentThread().interrupt()
        Left(McpError.Unreachable("interrupted"))
    } finally {
      body.cancel()
      sent.cancel(true)
    }
  }
}

private object HttpConnection {

  /** A `Content-Type`'s media type, lowercase, without its parameters. */
  private def mediaType(contentType: String): String =
    contentType.takeWhile(_ != ';').trim.toLowerCase
}

/** A response body's lines as they arrive, read until `deadline`, a [[Clock.millis]] reading.
  * The JDK client's request timeout ends at the headers; this bounds the body, which an event
  * stream may hold open.
  */
private final class Body(clock: Clock, deadline: Long) extends Flow.Subscriber[String] {

  // Lines as they arrive; `None` marks the end of the body. Filled by the client's thread and
  // drained by the caller's, each through the queue's own locking.
  @caps.unsafe.untrackedCaptures
  private val arrived = new LinkedBlockingQueue[Option[String]]()

  // Written once, when the body is subscribed to, by the client's thread; read by the
  // caller's to cancel it. Volatile, so the caller sees the write.
  @volatile @caps.unsafe.untrackedCaptures
  private var subscription: Option[Flow.Subscription] = None

  // Why the lines stopped before the body ended; written before the end marker is queued (or,
  // on a timeout, by the reader itself), so a reader that has seen the end sees it.
  @volatile @caps.unsafe.untrackedCaptures
  private var stopped: Option[String] = None

  def onSubscribe(s: Flow.Subscription): Unit = {
    subscription = Some(s)
    s.request(Long.MaxValue)
  }

  def onNext(line: String): Unit = arrived.put(Some(line))

  def onError(failed: Throwable): Unit = {
    stopped = Some(s"the answer broke off: ${failed.getClass.getSimpleName}: ${failed.getMessage}")
    arrived.put(None)
  }

  def onComplete(): Unit = arrived.put(None)

  /** Why the lines ended before the body did: it broke off, or the deadline passed. */
  def broke: Option[String] = stopped

  /** The lines, ending with the body, or when it breaks off or the deadline passes. */
  def lines: Iterator[String]^{this} =
    Iterator.unfold(()) { _ =>
      if (stopped.nonEmpty) None
      else
        Option(arrived.poll((deadline - clock.millis()).max(0L), TimeUnit.MILLISECONDS)) match {
          case None =>
            stopped = Some(s"no answer within ${McpClient.Timeout}")
            None
          case Some(None) =>
            arrived.put(None) // the end stays the end for a later read
            None
          case Some(Some(line)) => Some((line, ()))
        }
    }

  /** Stops the body arriving. */
  def cancel(): Unit = subscription.foreach(_.cancel())
}
