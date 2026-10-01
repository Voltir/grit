package grit.mcp.client

import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.util.concurrent.atomic.{AtomicLong, AtomicReference}
import java.util.concurrent.{
  CompletableFuture,
  ExecutionException,
  Flow,
  LinkedBlockingQueue,
  TimeUnit,
  TimeoutException
}

import scala.concurrent.duration.*
import scala.jdk.DurationConverters.*
import scala.jdk.OptionConverters.*

import grit.core.clock.Clock
import grit.mcp.wire.{Headers, McpError, McpTool, Rpc, Skipped, Sse}

/** A server's tools as listed: `tools`, those grit may offer, each name once, in the order
  * listed; `skipped`, the others and why.
  */
final case class Listed private[client] (tools: Vector[McpTool], skipped: Vector[Skipped])

/** `server`'s tools and calls, each request a POST of its own (no session), timing out after
  * [[McpClient.Timeout]].
  */
final class McpClient(server: McpServer, bearer: Bearer, clock: Clock) {
  import McpClient.*

  private val http = HttpClient.newBuilder().connectTimeout(Timeout.toJava).build()

  // Each request's JSON-RPC id, unique within this client: a counter written only by its own
  // atomic increment, so no reader sees a torn value.
  @caps.unsafe.untrackedCaptures
  private val ids = new AtomicLong(0L)

  // The list as last read, and the Clock.millis reading until which it is fresh. Replaced
  // whole through the reference's own atomic operations, so a reader sees one list or the
  // other; two callers that both find it stale both list, and either result is right.
  @caps.unsafe.untrackedCaptures
  private val kept = new AtomicReference[Option[(Listed, Long)]](None)

  /** The tools grit may offer from `server`, every page of its list: the list kept while
    * fresh, else listed again. A list is fresh until the earliest of its pages' receipt plus
    * that page's `ttlMs`, so a page without one makes it stale at once. A re-list that fails
    * returns the kept list when there is one, else its error; [[McpError.Unreadable]] when the
    * list runs past [[McpClient.MaxPages]] pages.
    */
  def tools(): Either[McpError, Listed] = {
    val held = kept.get()
    held match {
      case Some((listed, until)) if clock.millis() < until => Right(listed)
      case _ =>
        list() match {
          case Right((listed, until)) =>
            kept.set(Some((listed, until)))
            Right(listed)
          case Left(e) => held.map(_._1).toRight(e)
        }
    }
  }

  /** The kept list made stale, so the next [[tools]] lists again; it is still served when
    * that fails.
    */
  def stale(): Unit = {
    kept.updateAndGet(_.map((listed, _) => (listed, Long.MinValue)))
    ()
  }

  /** Every page of the list, from the first, and the reading until which it is fresh. */
  private def list(): Either[McpError, (Listed, Long)] = {
    def from(
        cursor: Option[String],
        pages: Int,
        read: Vector[(McpTool.Page, Long)]
    ): Either[McpError, Vector[(McpTool.Page, Long)]] =
      if (pages >= MaxPages)
        Left(McpError.Unreadable(s"tools/list ran past $MaxPages pages"))
      else
        exchange(Rpc.Call.ListTools(cursor)).flatMap(McpTool.page(_, server.name)).flatMap { page =>
          val received = (page, fresh(clock.millis(), page.ttlMs))
          page.next match {
            case None => Right(read :+ received)
            case next => from(next, pages + 1, read :+ received)
          }
        }
    from(None, 0, Vector.empty).map { read =>
      val pages = read.map(_._1)
      val listed = pages.flatMap(_.tools).distinctBy(_.name)
      val (allowed, unallowed) =
        listed.partition(t => server.allow.isEmpty || server.allow.contains(t.name))
      (
        Listed(allowed, pages.flatMap(_.skipped) ++ unallowed.map(t => Skipped.NotAllowed(t.name))),
        read.map(_._2).minOption.getOrElse(Long.MinValue)
      )
    }
  }

  /** The result of one request making `call`. */
  private def exchange(call: Rpc.Call): Either[McpError, ujson.Obj] = {
    val id = ids.incrementAndGet()
    val request = Headers
      .of(call)
      .foldLeft(HttpRequest.newBuilder(server.endpoint).timeout(Timeout.toJava)) {
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
            body.lines.mkString("\n"),
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

object McpClient {

  /** How long one request may take: 60 seconds, well under the 11 minutes a turn waits on a
    * claimed request.
    */
  val Timeout: FiniteDuration = 60.seconds

  /** The most pages one listing reads: 100. */
  val MaxPages: Int = 100

  /** The reading until which a page received at `received` with `ttlMs` is fresh. */
  private def fresh(received: Long, ttlMs: Long): Long =
    if (ttlMs > Long.MaxValue - received) Long.MaxValue else received + ttlMs

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
