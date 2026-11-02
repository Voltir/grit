package grit.mcp.client

import java.io.{ByteArrayOutputStream, OutputStream}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}

import scala.concurrent.duration.FiniteDuration

import grit.core.host.{Argv, Clipped, HostError, Processes, Ran, RelPath, Started}
import grit.mcp.wire.Rpc

/** An MCP server at revision 2026-07-28 over stdio, in this JVM: [[Processes]] whose every
  * started program is this server, reading a JSON-RPC message a line and writing one a line, as
  * `LocalProcesses` gives a program's lines ([[Started.next]]: a line, quiet, or the end; killed,
  * it ends). It answers `tools/list` with the tools [[lists]] gives and `tools/call` with the
  * text [[answers]] gives the tool, each request only when its params' `_meta` declares
  * [[Rpc.Version]] (else error -32602); writes [[before]]'s lines ahead of each answer; never
  * answers a tool [[stalls]] names, answers one [[slow]] names after its delay, and ends (exit 3,
  * "boom" on its errors) on a call of one [[ends]] names. A program whose input closes ends, as
  * a server reading to its input's end does.
  */
final class FakeStdioServer extends Processes {

  // Test scaffolding: the script is set by the test's thread and read by the program's, each
  // under `lock`, and no capability is reached through any of it.
  private val lock = new Object

  // Each var is read and written only inside `lock.synchronized`.
  @caps.unsafe.untrackedCaptures
  private var tools = Vector.empty[ujson.Obj]
  @caps.unsafe.untrackedCaptures
  private var texts = Map.empty[String, String]
  @caps.unsafe.untrackedCaptures
  private var stalled = Set.empty[String]
  @caps.unsafe.untrackedCaptures
  private var delays = Map.empty[String, Long]
  @caps.unsafe.untrackedCaptures
  private var ending = Set.empty[String]
  @caps.unsafe.untrackedCaptures
  private var ahead = Vector.empty[ujson.Value]
  @caps.unsafe.untrackedCaptures
  private var startable = true
  @caps.unsafe.untrackedCaptures
  private var heard = Vector.empty[ujson.Value]

  // Counters each changed only by their own atomic increments.
  private val starting = new AtomicInteger(0)
  private val killing = new AtomicInteger(0)

  def lists(listed: Vector[ujson.Obj]): Unit = lock.synchronized { tools = listed }
  def answers(tool: String, text: String): Unit = lock.synchronized { texts += tool -> text }
  def stalls(tool: String): Unit = lock.synchronized { stalled += tool }
  def slow(tool: String, millis: Long): Unit = lock.synchronized { delays += tool -> millis }
  def ends(tool: String): Unit = lock.synchronized { ending += tool }
  def before(lines: Vector[ujson.Value]): Unit = lock.synchronized { ahead = lines }
  def refusesStart(): Unit = lock.synchronized { startable = false }

  /** Every message the programs read, in order. */
  def received: Vector[ujson.Value] = lock.synchronized(heard)

  /** How many programs were started, and how many killed while running. */
  def starts: Int = starting.get()
  def kills: Int = killing.get()

  def run(argv: Argv, dir: RelPath, timeout: FiniteDuration): Either[HostError, Ran] =
    Left(HostError.Failed("the fake runs no command to its end"))

  def started[A](argv: Argv, dir: RelPath)(body: Started^ => A): Either[HostError, A] =
    if (!lock.synchronized(startable)) Left(HostError.Failed(s"${argv.program}: not found"))
    else {
      val _ = starting.incrementAndGet()
      val program = new Program
      try Right(body(program))
      finally program.kill()
    }

  /** One started program. */
  private final class Program extends Started {

    // Its lines, `None` at the end, which stays: filled by the program's own handling (on the
    // writer's thread, or a delayed answer's) and drained by the reader, through the queue's
    // own locking; strings only.
    @caps.unsafe.untrackedCaptures
    private val out = new LinkedBlockingQueue[Option[String]]()

    // Whether it ended, and its exit code: set once, by their own atomic operations.
    @caps.unsafe.untrackedCaptures
    private val over = new AtomicBoolean(false)
    @caps.unsafe.untrackedCaptures
    private val exit = new AtomicInteger(0)

    def input: OutputStream^{this} = new OutputStream {
      // What has been written since the last newline: the writer's thread alone touches it.
      @caps.unsafe.untrackedCaptures
      private val pending = new ByteArrayOutputStream()
      def write(b: Int): Unit =
        if (over.get()) throw new java.io.IOException("the program has ended")
        else if (b == '\n') {
          val line = new String(pending.toByteArray, UTF_8)
          pending.reset()
          handle(ujson.read(line))
        } else pending.write(b)
      override def close(): Unit = end(0)
    }

    def lines: Iterator[String]^{this} =
      Iterator
        .continually(taken(None).flatten)
        .takeWhile(_.isDefined)
        .collect { case Some(line) => line }

    def next(within: FiniteDuration): Started.Next = taken(Some(within)) match {
      case Some(Some(line)) => Started.Next.Line(line)
      case Some(None) => Started.Next.Ended
      case None => Started.Next.Quiet
    }

    def await(timeout: FiniteDuration): Either[HostError, Ran] =
      Right(Ran(exit.get(), Clipped.tail(if (exit.get() == 3) "boom" else "", _ => None)))

    def kill(): Unit =
      if (!over.get()) {
        val _ = killing.incrementAndGet()
        end(-9)
      }

    private def end(code: Int): Unit =
      if (over.compareAndSet(false, true)) {
        exit.set(code)
        out.put(None)
      }

    private def say(message: ujson.Value): Unit =
      if (!over.get()) out.put(Some(ujson.write(message)))

    private def taken(within: Option[FiniteDuration]): Option[Option[String]] = {
      val got = within match {
        case None => Some(out.take())
        case Some(w) => Option(out.poll(w.toMillis, TimeUnit.MILLISECONDS))
      }
      got.foreach(line => if (line.isEmpty) out.put(None))
      got
    }

    private def handle(message: ujson.Value): Unit = {
      lock.synchronized { heard :+= message }
      val o = message.obj
      val id = o.get("id")
      val versioned = o
        .get("params")
        .flatMap(_.obj.get("_meta"))
        .flatMap(_.obj.get("io.modelcontextprotocol/protocolVersion"))
        .contains(ujson.Str(Rpc.Version))
      (o.get("method").map(_.str), id) match {
        case (None, _) => () // a response to one of its own requests
        case (Some(_), Some(i)) if !versioned =>
          say(
            ujson.Obj(
              "jsonrpc" -> "2.0",
              "id" -> i,
              "error" -> ujson.Obj("code" -> -32602, "message" -> "no version")
            )
          )
        case (Some("tools/list"), Some(i)) =>
          say(
            ujson.Obj(
              "jsonrpc" -> "2.0",
              "id" -> i,
              "result" -> ujson.Obj("tools" -> lock.synchronized(tools))
            )
          )
        case (Some("tools/call"), Some(i)) =>
          val name = o("params")("name").str
          val (stalls, ends, delay, text, lines) = lock.synchronized(
            (stalled(name), ending(name), delays.get(name), texts.getOrElse(name, ""), ahead)
          )
          if (ends) {
            exit.set(3)
            end(3)
          } else if (!stalls) {
            val answer = ujson.Obj(
              "jsonrpc" -> "2.0",
              "id" -> i,
              "result" -> ujson.Obj(
                "content" -> ujson.Arr(ujson.Obj("type" -> "text", "text" -> text)),
                "isError" -> false
              )
            )
            delay match {
              case None =>
                lines.foreach(say)
                say(answer)
              case Some(ms) =>
                val _ = Thread.ofVirtual().start { () =>
                  Thread.sleep(ms)
                  lines.foreach(say)
                  say(answer)
                }
            }
          }
        case _ => ()
      }
    }
  }
}
