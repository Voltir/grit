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
import grit.core.visibility.Itemised
import grit.mcp.wire.{Answer, Headers, McpCalled, McpError, McpTool, Rpc, Skipped, Sse}

/** A server's tools as listed: `tools`, those grit may offer, each name once, in the order
  * listed; `skipped`, the others and why; `unlisted`, the names the server's allowlist gives
  * that no entry of the list has.
  */
final case class Listed private[client] (
    tools: Vector[McpTool],
    skipped: Vector[Skipped],
    unlisted: Set[String]
)

/** `server`'s tools and calls, each request a POST of its own (no session) carrying the bearer
  * `authorizer` gives, timing out after [[McpClient.Timeout]]. A request for which it gives
  * none is not sent, and is its [[McpError.NoToken]].
  */
final class McpClient(val server: McpServer, authorizer: Authorizer, clock: Clock) {
  import McpClient.*

  private val http = HttpClient.newBuilder().connectTimeout(Timeout.toJava).build()

  // Each request's JSON-RPC id, unique within this client: a counter written only by its own
  // atomic increment, so no reader sees a torn value.
  @caps.unsafe.untrackedCaptures
  private val ids = new AtomicLong(0L)

  // The list as last read, the read-only tools it lists that a lookup names, by name, and the
  // Clock.millis reading until which it is fresh. Replaced whole through the reference's own
  // atomic operations, so a reader sees one list or the other; two callers that both find it
  // stale both list, and either result is right.
  @caps.unsafe.untrackedCaptures
  private val kept = new AtomicReference[Option[(Listed, Map[String, McpTool], Long)]](None)

  /** The tools grit may offer from `server`, every page of its list, less those its allowlist
    * leaves out ([[Skipped.NotAllowed]]) and then those its scope excludes
    * ([[Skipped.OutOfScope]]): the list kept while fresh, else listed again. A list is fresh until the earliest of its pages' receipt plus
    * that page's `ttlMs`, so a page without one makes it stale at once. A re-list that fails
    * returns the kept list when there is one, else its error; [[McpError.Unreadable]] when the
    * list runs past [[McpClient.MaxPages]] pages. A re-list blocks on its server for up to
    * [[McpClient.Timeout]] a page, so up to [[McpClient.MaxPages]] times that in all.
    */
  def tools(): Either[McpError, Listed] = {
    val held = kept.get()
    held match {
      case Some((listed, _, until)) if clock.millis() < until => Right(listed)
      case _ =>
        list() match {
          case Right(read @ (listed, _, _)) =>
            kept.set(Some(read))
            Right(listed)
          case Left(e) => held.map(_._1).toRight(e)
        }
    }
  }

  /** The kept list made stale, so the next [[tools]] lists again; it is still served when
    * that fails.
    */
  def stale(): Unit = {
    kept.updateAndGet(_.map((listed, lookable, _) => (listed, lookable, Long.MinValue)))
    ()
  }

  /** `tool` called with `arguments` as the server's scope sends them
    * ([[grit.mcp.scope.McpScope.request]]): its answer as the scope shows it
    * ([[grit.mcp.scope.McpScope.shown]]), an `isError` one included, its items as the server's
    * `accounts` read the shown result, whole; or why none:
    * [[McpError.OutOfScope]], with nothing sent, when the scope refuses the call;
    * [[McpError.Unattributed]], with none of the answer shown, when the scope cannot hold it or
    * `accounts` does not read it whole;
    * [[McpError.NotLooked]], with the call not sent, when a lookup it needs first cannot be
    * made ([[McpServer.lookups]], whose answers the `accounts` read beside it). An
    * [[McpError.Rpc]] with code `-32602` (an unknown tool, or invalid arguments) makes the list
    * [[stale]], since the server's tools may have changed. Blocks on the server for up to
    * [[McpClient.Timeout]] a request: the call and each of its lookups.
    */
  def call(tool: McpTool, arguments: ujson.Obj): Either[McpError, Answer] = {
    val answered = for {
      sent <- server.scope.request(tool, arguments).left.map(McpError.OutOfScope(_))
      looked <- look(tool.name, sent)
      result <- exchange(Rpc.Call.CallTool(tool, sent))
      shown <- server.scope.shown(tool, result).left.map(McpError.Unattributed(_))
      items <- server.accounts.items(McpCalled(tool.name, sent, shown, looked)) match {
        case Itemised.Whole(items) => Right(items)
        // No reader of this client's servers cuts an answer yet, and an answer is shown whole
        // or not at all: one cut or unplaced shows nothing.
        case Itemised.Parts(_, _) => Left(McpError.Unattributed("its answer was cut by item"))
        case Itemised.Unplaced(why) => Left(McpError.Unattributed(why))
      }
      answer <- Answer.of(shown, items)
    } yield answer
    // A -32020 HeaderMismatch is not re-listed and retried, though the spec says a client
    // SHOULD (streamable-http.mdx, Client Behavior): a list with ttlMs 0, as GitHub's is, is
    // re-read on every access, so `tool` already carries the schema the server holds and a
    // retry would mirror the same headers. A server listing with a longer ttlMs may want it.
    answered match {
      case Left(McpError.Rpc(InvalidParams, _)) => stale()
      case _ => ()
    }
    answered
  }

  /** The answers of `tool`'s lookups ([[McpServer.lookups]]) for a call sending `sent`, each by
    * its tool's name; [[McpError.NotLooked]], with none sent, when `sent` lacks an argument one
    * names, and with none after it sent, when one cannot be made. A lookup's tool is read from
    * the kept list, stale or not, and the list is read only when none is kept.
    */
  private def look(tool: String, sent: ujson.Obj): Either[McpError, Map[String, ujson.Obj]] = {
    val lookups = server.lookups.getOrElse(tool, Vector.empty)
    val valued = lookups.foldLeft[Either[McpError, Vector[(Lookup, ujson.Value)]]](
      Right(Vector.empty)
    ) { (read, lookup) =>
      read.flatMap(vs =>
        sent.value
          .get(lookup.argument)
          .map(v => vs :+ (lookup -> v))
          .toRight(
            McpError.NotLooked(
              s"the call names no ${lookup.argument}, which grit looks up with ${lookup.tool} first"
            )
          )
      )
    }
    def lookable(): Either[McpError, Map[String, McpTool]] = kept.get() match {
      case Some((_, held, _)) => Right(held)
      case None => tools().map(_ => kept.get().fold(Map.empty[String, McpTool])(_._2))
    }
    valued.flatMap(
      _.foldLeft[Either[McpError, Map[String, ujson.Obj]]](Right(Map.empty)) {
        case (read, (lookup, value)) =>
          def failed(e: McpError) = McpError.NotLooked(s"${lookup.tool} ${e.message}")
          for {
            looked <- read
            held <- lookable().left.map(failed)
            listed <- held
              .get(lookup.tool)
              .toRight(McpError.NotLooked(s"${lookup.tool} is not a read-only tool it lists"))
            result <- exchange(Rpc.Call.CallTool(listed, ujson.Obj(lookup.argument -> value))).left
              .map(failed)
          } yield looked + (lookup.tool -> result)
      }
    )
  }

  /** Every page of the list, from the first; the read-only tools on it that a lookup names; and
    * the reading until which it is fresh.
    */
  private def list(): Either[McpError, (Listed, Map[String, McpTool], Long)] = {
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
      val skipped = pages.flatMap(_.skipped)
      val (allowed, unallowed) =
        listed.partition(t => server.allow.isEmpty || server.allow.contains(t.name))
      val held = allowed.map(t => (t, server.scope.excludes(t)))
      val looked = server.lookups.values.flatten.map(_.tool).toSet
      (
        Listed(
          held.collect { case (t, None) => t },
          skipped ++ unallowed.map(t => Skipped.NotAllowed(t.name)) ++ held.flatMap(_._2),
          server.allow -- listed.map(_.name) -- skipped.flatMap(named)
        ),
        listed.filter(t => looked.contains(t.name)).map(t => t.name -> t).toMap,
        read.map(_._2).minOption.getOrElse(Long.MinValue)
      )
    }
  }

  /** The result of one request making `call`. */
  private def exchange(call: Rpc.Call): Either[McpError, ujson.Obj] =
    authorizer.bearer(clock).flatMap(exchange(call, _))

  /** The result of one request making `call`, carrying `bearer`. */
  private def exchange(call: Rpc.Call, bearer: Bearer): Either[McpError, ujson.Obj] = {
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

object McpClient {

  /** How long one request may take: 60 seconds, well under the 11 minutes a turn waits on a
    * claimed request.
    */
  val Timeout: FiniteDuration = 60.seconds

  /** JSON-RPC's Invalid params, which a server answers for an unknown tool. */
  private val InvalidParams = -32602

  /** The most pages one listing reads: 100. */
  val MaxPages: Int = 100

  /** The listed tool `skipped` names, `None` for an entry with no name it can read. */
  private def named(skipped: Skipped): Option[String] = skipped match {
    case Skipped.NotReadOnly(tool) => Some(tool)
    case Skipped.BadName(tool, _) => Some(tool)
    case Skipped.BadHeader(tool, _) => Some(tool)
    case Skipped.NotAllowed(tool) => Some(tool)
    case Skipped.OutOfScope(tool) => Some(tool)
    case Skipped.Malformed(_) => None
  }

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
