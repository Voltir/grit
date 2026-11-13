package grit.edge.http

import java.io.IOException
import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.charset.CodingErrorAction
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.{
  ConcurrentHashMap,
  ExecutorService,
  Executors,
  ScheduledThreadPoolExecutor,
  TimeUnit
}

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

import grit.core.edge.EdgeRefusal

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}

/** An address answered over HTTP until [[close]]. */
final class Listener private (
    /** Where it listens: the address it was opened at, with the port it was given when that was 0. */
    val at: Listen,
    server: HttpServer,
    handling: Handling^,
    threads: ExecutorService,
    deadlines: ScheduledThreadPoolExecutor,
    limits: Limits
) extends AutoCloseable {

  private val closed = new AtomicBoolean(false)

  /** Stops it. A request that arrives from now on is answered 503 without `answer`. Those under way
    * are waited for up to [[Listener.CloseWithin]], then their threads are interrupted and given
    * [[Listener.InterruptedWithin]] to answer, and then their connections are closed; the verb and
    * path of each still running is logged at the interrupt and every [[Listener.CloseWithin]] after.
    * Returns once every request has ended, so no call to `answer` is under way or can start: an
    * `answer` that ignores its interrupt holds it until it returns. Closing again does nothing.
    */
  def close(): Unit = if (closed.compareAndSet(false, true)) {
    handling.closing(at)
    if (!handling.idleWithin(limits.closeWithin)) {
      handling.interrupt()
      handling.idleWithin(limits.interruptedWithin)
    }
    server.stop(0)
    threads.shutdownNow()
    deadlines.shutdownNow()
    var interrupted = false
    var ended = false
    while (!ended)
      try {
        ended = threads.awaitTermination(limits.closeWithin.toMillis, TimeUnit.MILLISECONDS)
        if (!ended) handling.stillRunning()
      } catch { case _: InterruptedException => interrupted = true }
    if (interrupted) Thread.currentThread.interrupt()
  }
}

object Listener {

  /** 1 MiB: the longest body read; a longer one is answered 413 without `answer`, refused before
    * it is read when its `Content-Length` says so.
    */
  val BodyMost: Int = 1 << 20

  /** 10 seconds: how long a request may take to send its body once its headers are read; a client
    * slower than that is dropped with no response.
    */
  val ReadWithin: FiniteDuration = 10.seconds

  /** 5 seconds: how long [[Listener.close]] waits for requests under way before interrupting them,
    * and then how often it logs those still running.
    */
  val CloseWithin: FiniteDuration = 5.seconds

  /** 1 second: how long a request [[Listener.close]] interrupted has to answer before its
    * connection is closed.
    */
  val InterruptedWithin: FiniteDuration = 1.second

  /** 200: the most characters of a path or a throwable's message one log line quotes. */
  val LogMost: Int = 200

  /** Listens at `at` and answers each request with `answer`, given the request and `log` lent for
    * it, each on a virtual thread of its own: `answer` is called on many threads at once and may
    * block (an [[grit.core.edge.EdgeWatch]] holds its request up to
    * [[grit.core.edge.EdgeWatch.Longest]]). Answered without `answer`: a body over [[BodyMost]]
    * 413; a body not UTF-8, or a target [[Request.of]] refuses, 400, but `*` 404; a request once
    * closing has begun 503. An `answer` that throws, short of a `VirtualMachineError`, is answered 500, its
    * throwable's class and message and the request's verb and path logged, never its query,
    * headers or body. A response the client has gone before is dropped, `answer` having run. A
    * HEAD is answered with no body. `Refused` when `at` cannot be listened on: its port is taken,
    * its host does not resolve, or it is [[Listen.loopback]] and resolves to an address that is
    * not. A `Right` dropped without [[Listener.close]] goes on answering, with nothing left to
    * close it.
    */
  def open(
      at: Listen,
      answer: (Request, String => Unit) => Response,
      log: String => Unit
  ): Either[EdgeRefusal, Listener^{answer, log}] = opened(at, answer, log, Limits.Default)

  /** [[open]], keeping `limits` in place of the constants. */
  private[http] def opened(
      at: Listen,
      answer: (Request, String => Unit) => Response,
      log: String => Unit,
      limits: Limits
  ): Either[EdgeRefusal, Listener^{answer, log}] = {
    val address = new InetSocketAddress(at.host, at.port)
    if (address.isUnresolved) Left(EdgeRefusal.Refused(s"${at.host} does not resolve"))
    else {
      try {
        val server = HttpServer.create(address, 0)
        val bound = server.getAddress
        if (at.loopback && !bound.getAddress.isLoopbackAddress) {
          server.stop(0)
          Left(EdgeRefusal.Refused(s"${at.host} is not a loopback address here"))
        } else {
          val threads = Executors.newVirtualThreadPerTaskExecutor()
          val deadlines = new ScheduledThreadPoolExecutor(1, Thread.ofVirtual().factory())
          deadlines.setRemoveOnCancelPolicy(true)
          val handling = new Handling(answer, log, limits, deadlines)
          server.setExecutor(threads)
          // The JDK's server takes `handling`, which holds `answer` and `log`, untracked: a Java
          // method's parameter is not capture checked. It cannot be observed outside the
          // Listener's capture set: the Listener holds the same `handling`, so its type carries
          // {answer, log}; `close` returns only once (1) `stop` has joined the server's
          // dispatcher, so no exchange is handed to `threads` after it, (2) the termination of
          // `threads`, which runs every exchange, has been awaited without a bound, and (3) no
          // deadline task calls `answer` (they only interrupt a read). A Listener never closed
          // keeps answering, as `open`'s doc says.
          server.createContext("/", handling)
          server.start()
          Right(new Listener(at.bound(bound.getPort), server, handling, threads, deadlines, limits))
        }
      } catch {
        case e: IOException =>
          Left(EdgeRefusal.Refused(s"cannot listen at ${at.shown}: ${e.getMessage}"))
      }
    }
  }
}

/** The bounds a listener keeps: [[Listener]]'s constants, shortened by tests. */
private[http] final case class Limits(
    bodyMost: Int,
    readWithin: FiniteDuration,
    closeWithin: FiniteDuration,
    interruptedWithin: FiniteDuration
)

private[http] object Limits {
  val Default: Limits =
    Limits(Listener.BodyMost, Listener.ReadWithin, Listener.CloseWithin, Listener.InterruptedWithin)
}

/** What a [[Listener]] does with each exchange the JDK's server hands it, and the requests under
  * way that its `close` waits for.
  */
private final class Handling(
    answer: (Request, String => Unit) => Response,
    log: String => Unit,
    limits: Limits,
    deadlines: ScheduledThreadPoolExecutor
) extends HttpHandler {

  private val shutting = new AtomicBoolean(false)
  private val running = new ConcurrentHashMap[Thread, String]()
  private val lock = new ReentrantLock()
  private val idle = lock.newCondition()

  def handle(exchange: HttpExchange): Unit = {
    val path = Option(exchange.getRequestURI).flatMap(u => Option(u.getRawPath)).getOrElse("")
    val named =
      s"${exchange.getRequestMethod.take(Listener.LogMost)} ${path.take(Listener.LogMost)}"
    running.put(Thread.currentThread, named)
    try serve(exchange, named)
    finally {
      exchange.close()
      running.remove(Thread.currentThread)
      lock.lock()
      try idle.signalAll()
      finally lock.unlock()
    }
  }

  /** Marks it closing: every request from now on is answered 503. */
  def closing(at: Listen): Unit = {
    shutting.set(true)
    log(s"closing ${at.shown} with ${running.size} requests under way")
  }

  /** Whether every request has ended within `limit`. */
  def idleWithin(limit: FiniteDuration): Boolean = {
    lock.lock()
    try {
      var left = limit.toNanos
      var interrupted = false
      while (!running.isEmpty && left > 0)
        try left = idle.awaitNanos(left)
        catch { case _: InterruptedException => interrupted = true; left = 0 }
      if (interrupted) Thread.currentThread.interrupt()
      running.isEmpty
    } finally lock.unlock()
  }

  /** Interrupts every request under way, logging each. */
  def interrupt(): Unit = running.asScala.foreach { (thread, named) =>
    log(s"$named still running at close: interrupted")
    thread.interrupt()
  }

  /** Logs every request under way. */
  def stillRunning(): Unit =
    running.asScala.values.foreach(named => log(s"$named still running at close"))

  private def serve(exchange: HttpExchange, named: String): Unit =
    if (shutting.get)
      send(exchange, Response.text(Status.Unavailable, "the listener is closing"), head = false)
    else {
      val verb = Verb.of(exchange.getRequestMethod)
      val head = verb == Verb.Head
      val declared =
        Option(exchange.getRequestHeaders.getFirst("Content-Length")).flatMap(_.trim.toLongOption)
      if (declared.exists(_ > limits.bodyMost)) send(exchange, tooLarge, head)
      else
        read(exchange) match {
          case None => ()
          case Some(Left(())) => send(exchange, tooLarge, head)
          case Some(Right(body)) =>
            val target = Option(exchange.getRequestURI).map(_.toString).getOrElse("")
            val headers = exchange.getRequestHeaders.asScala.toVector.flatMap { (name, values) =>
              values.asScala.toVector.map(v => (name, v))
            }
            val response = body
              .toRight("the body must be UTF-8")
              .flatMap(text => Request.of(verb, target, headers, text)) match {
              case Left(why) => Response.text(Status.BadRequest, why)
              case Right(request) => answered(request, named)
            }
            send(exchange, response, head)
        }
    }

  private def tooLarge: Response =
    Response.text(Status.TooLarge, s"the body is over ${limits.bodyMost} bytes")

  private def answered(request: Request, named: String): Response =
    try answer(request, log)
    catch {
      case e: Throwable if NonFatal(e) || e.isInstanceOf[InterruptedException] =>
        val message = Option(e.getMessage).getOrElse("").take(Listener.LogMost)
        log(s"$named failed: ${e.getClass.getName}: $message")
        Response.text(Status.Failed, "the request failed")
    }

  /** The body, or Left when it is over the cap, its text None when not UTF-8; None when the client
    * was dropped for sending it too slowly, or went.
    */
  private def read(exchange: HttpExchange): Option[Either[Unit, Option[String]]] = {
    val phase = new AtomicInteger(Reading)
    val reader = Thread.currentThread
    val deadline = deadlines.schedule(
      (() => if (phase.compareAndSet(Reading, Dropped)) reader.interrupt()): Runnable,
      limits.readWithin.toMillis,
      TimeUnit.MILLISECONDS
    )
    val bytes =
      try Some(ByteBuffer.wrap(exchange.getRequestBody.readNBytes(limits.bodyMost + 1)))
      catch { case _: IOException => None }
    deadline.cancel(false)
    if (!phase.compareAndSet(Reading, Answering)) {
      Thread.interrupted()
      log(s"${exchange.getRequestMethod} dropped: its body took longer than ${limits.readWithin}")
      None
    } else
      bytes.map { read =>
        if (read.remaining > limits.bodyMost) Left(())
        else Right(utf8(read))
      }
  }

  private def utf8(bytes: ByteBuffer): Option[String] =
    try
      Some(
        UTF_8
          .newDecoder()
          .onMalformedInput(CodingErrorAction.REPORT)
          .onUnmappableCharacter(CodingErrorAction.REPORT)
          .decode(bytes)
          .toString
      )
    catch { case _: java.nio.charset.CharacterCodingException => None }

  private def send(exchange: HttpExchange, response: Response, head: Boolean): Unit = {
    val bytes = response.body.getBytes(UTF_8)
    val headers = exchange.getResponseHeaders
    if (response.body.nonEmpty) headers.set("Content-Type", response.contentType.header)
    response.status match {
      case Status.NotAllowed(allowed) => headers.set("Allow", allowed.map(_.name).mkString(", "))
      case _ => ()
    }
    try {
      if (head || bytes.isEmpty) exchange.sendResponseHeaders(response.status.code, -1)
      else {
        exchange.sendResponseHeaders(response.status.code, bytes.length.toLong)
        exchange.getResponseBody.write(bytes)
      }
    } catch { case _: IOException => () }
  }

  private val Reading = 0
  private val Dropped = 1
  private val Answering = 2
}
