package grit.mcp.client

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong, AtomicReference}
import java.util.concurrent.{CompletableFuture, LinkedBlockingQueue, TimeUnit}

import scala.concurrent.duration.*
import scala.util.Try

import grit.core.clock.Clock
import grit.core.host.{Processes, RelPath, Started}
import grit.mcp.wire.{McpError, Rpc}

/** stdio under a client: `transport`'s program started by `processes` at their root, by one
  * thread of its own, when a request comes and none runs. Requests are answered in the order
  * asked, one at a time; each must be answered within `timeout` of being asked, or within
  * `coldStart` of its program's start when that ends later, else the program is killed and the
  * request and every one queued behind it are [[McpError.Unreachable]]. A program that ends
  * before it answers fails only the request it was asked ([[McpError.Exited]]); the next request
  * starts it again. Lines answering no request of grit's are passed over, and a request the
  * server sends is answered [[Rpc.refusal]].
  */
private[client] final class StdioConnection(
    transport: McpTransport.Stdio,
    processes: Processes^,
    clock: Clock,
    timeout: FiniteDuration,
    coldStart: FiniteDuration
) extends Connection {

  import StdioConnection.*

  // Requests not yet answered, in the order asked. Filled by callers and drained by the owner,
  // each through the queue's own locking; it holds requests and futures of their pure results,
  // so no capability flows through it.
  @caps.unsafe.untrackedCaptures
  private val queue = new LinkedBlockingQueue[Pending]()

  // Each request's JSON-RPC id (a counter only its own increment writes), and whether `close`
  // was called (set once): each changed only by its own atomic operation, holding no capability.
  @caps.unsafe.untrackedCaptures
  private val ids = new AtomicLong(0L)
  @caps.unsafe.untrackedCaptures
  private val closed = new AtomicBoolean(false)

  // The owner thread, once started: set once by its compare-and-set, and read by `close` to wait
  // for it. A Thread holds no capability this class tracks.
  @caps.unsafe.untrackedCaptures
  private val owner = new AtomicReference[Option[Thread]](None)

  def exchange(call: Rpc.Call): Either[McpError, ujson.Obj] =
    if (closed.get()) Left(Closed)
    else {
      val pending = Pending(call, clock.millis(), new CompletableFuture())
      queue.put(pending)
      ensureOwner()
      // A close between the check above and the put may have drained the queue already.
      if (closed.get()) failQueued(Closed)
      Try(
        pending.answer.get((timeout + coldStart + Grace).toMillis, TimeUnit.MILLISECONDS)
      ).toOption
        .getOrElse(Left(McpError.Unreachable("no answer from its program's thread")))
    }

  /** Kills the program, failing a request in flight and every one queued, and waits for the
    * owner thread to end, at most [[StdioConnection.Grace]].
    */
  def close(): Unit = {
    closed.set(true)
    owner.get().foreach(_.join(Grace.toMillis))
    failQueued(Closed)
  }

  private def ensureOwner(): Unit =
    if (owner.get().isEmpty) {
      val thread = Thread.ofVirtual().name("grit-mcp-stdio").unstarted(() => serve())
      if (owner.compareAndSet(None, Some(thread))) thread.start()
    }

  /** Until closed: the program started for the first request queued, and serving until it ends,
    * stalls or is closed; then the next.
    */
  private def serve(): Unit = {
    while (!closed.get())
      Option(queue.poll(Slice.toMillis, TimeUnit.MILLISECONDS)).filterNot(_ => closed.get()) match {
        case None => ()
        case Some(first) =>
          val start = clock.millis()
          processes.started(transport.argv, RelPath.Root)(s => running(s, start, first)) match {
            case Left(e) =>
              first.answer.complete(
                Left(McpError.Unreachable(s"its program could not start: ${e.message}"))
              )
              ()
            case Right(()) => ()
          }
      }
    failQueued(Closed)
  }

  /** `first` and each request after it sent to `s`, until the program ends, a request's
    * deadline passes, or the connection closes; the program started at `start`.
    */
  private def running(s: Started^, start: Long, first: Pending): Unit = {
    var current: Option[Pending] = Some(first)
    var on = true
    while (on && !closed.get())
      current match {
        case None =>
          Option(queue.poll(Slice.toMillis, TimeUnit.MILLISECONDS)) match {
            case Some(next) => current = Some(next)
            case None =>
              // Between requests: what the program says unasked is passed over; its end ends this.
              on = idle(s)
          }
        case Some(pending) =>
          val deadline = (pending.asked + timeout.toMillis).max(start + coldStart.toMillis)
          answered(s, pending, deadline) match {
            case Answer.Got =>
              current = None
            case Answer.Ended =>
              val (code, errors) = s.await(Grace) match {
                case Right(ran) => (ran.exit, ran.output.text)
                case Left(e) => (-1, e.message)
              }
              pending.answer.complete(Left(McpError.Exited(code, errors.takeRight(ErrorsKept))))
              on = false
            case Answer.Stalled =>
              // Only those queued now are behind it: one asked after this drain is left for the
              // next program. Each hears once the program is stopped, as its error says.
              val behind = new java.util.ArrayList[Pending]()
              val _ = queue.drainTo(behind)
              s.kill()
              pending.answer.complete(Left(Stalled))
              behind.forEach(p => { val _ = p.answer.complete(Left(StalledBehind)) })
              on = false
            case Answer.Closed =>
              pending.answer.complete(Left(Closed))
              on = false
          }
      }
    current.foreach(_.answer.complete(Left(Closed)))
  }

  /** Whether the program still runs, having passed over what it said unasked. */
  private def idle(s: Started^): Boolean = s.next(Duration.Zero) match {
    case Started.Next.Line(_) => idle(s)
    case Started.Next.Quiet => true
    case Started.Next.Ended => false
  }

  /** `pending` sent to `s` and its answer read until `deadline`, a [[Clock.millis]] reading. */
  private def answered(s: Started^, pending: Pending, deadline: Long): Answer = {
    val id = ids.incrementAndGet()
    if (!sent(s, Rpc.request(id, pending.call))) Answer.Ended
    else {
      var got: Option[Answer] = None
      while (got.isEmpty) {
        val left = deadline - clock.millis()
        if (closed.get()) got = Some(Answer.Closed)
        else if (left <= 0) got = Some(Answer.Stalled)
        else
          s.next(left.min(Slice.toMillis).millis) match {
            case Started.Next.Quiet => ()
            case Started.Next.Ended => got = Some(Answer.Ended)
            case Started.Next.Line(text) =>
              Try(ujson.read(text)).toOption.flatMap(_.objOpt) match {
                case Some(o) if o.contains("method") =>
                  // A request of the server's is refused; a notification is passed over.
                  o.get("id")
                    .foreach(asked => if (!sent(s, Rpc.refusal(asked))) got = Some(Answer.Ended))
                case Some(o) if o.get("id").contains(ujson.Num(id.toDouble)) =>
                  pending.answer.complete(Rpc.result(id, o))
                  got = Some(Answer.Got)
                case _ => ()
              }
          }
      }
      got.getOrElse(Answer.Stalled)
    }
  }

  /** Whether `message` was written to `s` as one line; false when its input is closed. */
  private def sent(s: Started^, message: ujson.Value): Boolean =
    try {
      s.input.write((ujson.write(message) + "\n").getBytes(StandardCharsets.UTF_8))
      s.input.flush()
      true
    } catch { case _: IOException => false }

  /** Every request queued, failed with `error`. */
  private def failQueued(error: McpError): Unit =
    Iterator.continually(queue.poll()).takeWhile(_ != null).foreach(_.answer.complete(Left(error)))
}

private object StdioConnection {

  /** A request as asked: `asked`, the [[Clock.millis]] reading when it was. */
  final case class Pending(
      call: Rpc.Call,
      asked: Long,
      answer: CompletableFuture[Either[McpError, ujson.Obj]]
  )

  /** How one request's exchange ended. */
  enum Answer {
    case Got, Ended, Stalled, Closed
  }

  /** 100 ms: how often the owner looks whether the connection was closed while it waits. */
  val Slice: FiniteDuration = 100.millis

  /** 5 seconds: how long an ended program's errors, and `close`'s wait for the owner, are given. */
  val Grace: FiniteDuration = 5.seconds

  /** The most characters of a program's errors kept for the log: the last 200. */
  val ErrorsKept: Int = 200

  val Closed: McpError = McpError.Unreachable("its connection was closed")

  val Stalled: McpError =
    McpError.Unreachable(
      "no answer in time, so its program was stopped; it starts again on the next call"
    )

  val StalledBehind: McpError = McpError.Unreachable(
    "its program was stopped for not answering an earlier call before this one was asked; it starts again on the next call"
  )
}
