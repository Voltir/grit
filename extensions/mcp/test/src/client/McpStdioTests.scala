package grit.mcp.client

import java.util.concurrent.{Callable, Executors}

import scala.concurrent.duration.*

import grit.core.clock.Clock
import grit.core.host.Argv
import grit.mcp.scope.McpScope
import grit.mcp.wire.{McpError, McpTool, Rpc}

import utest.*

/** [[McpClient.stdio]] against [[FakeStdioServer]]: requests sent one a line, answered in turn,
  * held to their deadlines, and the program started again after it ends.
  */
object McpStdioTests extends TestSuite {

  private def entry(name: String): ujson.Obj =
    ujson.Obj(
      "name" -> name,
      "inputSchema" -> ujson.Obj("type" -> "object"),
      "annotations" -> ujson.Obj("readOnlyHint" -> true)
    )

  /** A client of `fake`'s server `fake`, listing `names`, with `timeout` and `coldStart`. */
  private def client(
      fake: FakeStdioServer,
      names: Vector[String],
      timeout: FiniteDuration = 5.seconds,
      coldStart: FiniteDuration = Duration.Zero
  ): McpClient^ = {
    fake.lists(names.map(entry))
    val server = Argv
      .of("/opt/fake/server", "mcp")
      .flatMap(McpServer.stdio("fake", _, McpScope.Open))
      .fold(why => throw new java.lang.AssertionError(why), identity)
    McpClient.stdio(server, fake, Clock.system(), timeout, coldStart)
  }

  /** `name` as `c` lists it. */
  private def listed(c: McpClient^, name: String): McpTool =
    c.tools()
      .toOption
      .flatMap(_.tools.find(_.name == name))
      .getOrElse(throw new java.lang.AssertionError(s"$name is not listed"))

  private def text(answer: Either[McpError, grit.mcp.wire.Answer]): Either[McpError, String] =
    answer.map(_.text)

  /** `made` of 0 to `n - 1` at once, each on a thread of its own, started in order a few
    * ms apart.
    */
  private def together[A](n: Int)(made: Int => A): Vector[A] = {
    val pool = Executors.newVirtualThreadPerTaskExecutor()
    try {
      val futures = (0 until n).toVector.map { i =>
        val f = pool.submit(new Callable[A] { def call(): A = made(i) })
        Thread.sleep(20)
        f
      }
      futures.map(_.get())
    } finally pool.shutdown()
  }

  val tests = Tests {

    test("a call through a stdio server answers its result, its list read from the same program") {
      val fake = new FakeStdioServer
      fake.answers("show", "the definition")
      val c = client(fake, Vector("show", "find"))
      val got = text(c.call(listed(c, "show"), ujson.Obj("symbols" -> ujson.Arr("A"))))
      c.close()
      (got, fake.starts, fake.received.map(_("method").str)) ==>
        (Right("the definition"), 1, Vector("tools/list", "tools/call"))
    }

    test(
      "a program that ends mid-call is Exited for that call, its errors logged, and the next call starts it again"
    ) {
      val fake = new FakeStdioServer
      fake.answers("find", "found")
      fake.ends("show")
      val c = client(fake, Vector("show", "find"))
      val show = listed(c, "show")
      val find = listed(c, "find")
      val ended = c.call(show, ujson.Obj())
      val again = text(c.call(find, ujson.Obj()))
      c.close()
      (ended.left.map(e => (e.message, e.logged)), again, fake.starts) ==> (
        Left(
          (
            "ended before it answered (exit 3); it starts again on the next call",
            Some("ended before it answered (exit 3); it starts again on the next call: boom")
          )
        ),
        Right("found"),
        2
      )
    }

    test(
      "a call not answered in time stops its program, failing it and each call queued behind it"
    ) {
      val fake = new FakeStdioServer
      fake.stalls("show")
      fake.answers("find", "found")
      val c = client(fake, Vector("show", "find"), timeout = 300.millis)
      val show = listed(c, "show")
      val find = listed(c, "find")
      val got = together(2)(i => c.call(Vector(show, find)(i), ujson.Obj()))
      val killed = fake.kills
      val after = text(c.call(find, ujson.Obj()))
      c.close()
      (got.map(_.left.map(_.message)), killed, after, fake.starts) ==> (
        Vector(
          Left(
            "could not be reached: no answer in time, so its program was stopped; it starts again on the next call"
          ),
          Left(
            "could not be reached: its program was stopped for not answering an earlier call before " +
              "this one was asked; it starts again on the next call"
          )
        ),
        1,
        Right("found"),
        2
      )
    }

    test(
      "each call's deadline runs from when it was asked, so one queued behind a slow call can run out before it is sent"
    ) {
      val fake = new FakeStdioServer
      fake.slow("show", 250)
      fake.slow("find", 350)
      fake.answers("show", "shown")
      fake.answers("find", "found")
      val c = client(fake, Vector("show", "find"), timeout = 400.millis)
      val show = listed(c, "show")
      val find = listed(c, "find")
      val got = together(2)(i => c.call(Vector(show, find)(i), ujson.Obj()))
      c.close()
      got.map(a => text(a).left.map(_ => "unreachable")) ==> Vector(
        Right("shown"),
        Left("unreachable")
      )
    }

    test(
      "a call asked as its program starts may take the cold-start allowance; past it, the timeout holds"
    ) {
      def answered(coldStart: FiniteDuration): Either[String, String] = {
        val fake = new FakeStdioServer
        fake.slow("show", 300)
        fake.answers("show", "shown")
        val c = client(fake, Vector("show"), timeout = 100.millis, coldStart = coldStart)
        val got = text(c.call(listed(c, "show"), ujson.Obj())).left.map(_ => "unreachable")
        c.close()
        got
      }
      (answered(2.seconds), answered(Duration.Zero)) ==> (Right("shown"), Left("unreachable"))
    }

    test(
      "lines answering no request of grit's are passed over, and a request the server sends is refused"
    ) {
      val fake = new FakeStdioServer
      fake.answers("show", "shown")
      fake.before(
        Vector(
          ujson
            .Obj("jsonrpc" -> "2.0", "method" -> "notifications/message", "params" -> ujson.Obj()),
          ujson.Obj("jsonrpc" -> "2.0", "id" -> 999, "result" -> ujson.Obj()),
          ujson.Obj("jsonrpc" -> "2.0", "id" -> "s1", "method" -> "roots/list")
        )
      )
      val c = client(fake, Vector("show"))
      val got = text(c.call(listed(c, "show"), ujson.Obj()))
      c.close()
      (got, fake.received.lastOption) ==> (Right("shown"), Some(Rpc.refusal(ujson.Str("s1"))))
    }

    test("a program that cannot start fails the call in the host's words") {
      val fake = new FakeStdioServer
      fake.refusesStart()
      val c = client(fake, Vector("show"))
      val got = c.tools().left.map(_.message)
      c.close()
      got ==> Left("could not be reached: its program could not start: /opt/fake/server: not found")
    }

    test("close kills the program, and fails a call in flight and every call after it") {
      val fake = new FakeStdioServer
      fake.stalls("show")
      val c = client(fake, Vector("show"))
      val show = listed(c, "show")
      val pool = Executors.newVirtualThreadPerTaskExecutor()
      val inFlight = pool.submit(new Callable[Either[McpError, grit.mcp.wire.Answer]] {
        def call() = c.call(show, ujson.Obj())
      })
      Thread.sleep(100)
      c.close()
      val closed = Left("could not be reached: its connection was closed")
      (
        inFlight.get().left.map(_.message),
        c.call(show, ujson.Obj()).left.map(_.message),
        fake.kills
      ) ==> (closed, closed, 1)
    }

    test("calls made together each get their own answer") {
      val fake = new FakeStdioServer
      val names = Vector("a", "b", "c", "d")
      names.foreach(n => fake.answers(n, s"answer $n"))
      val c = client(fake, names)
      val tools = names.map(listed(c, _))
      val got = together(tools.size)(i => text(c.call(tools(i), ujson.Obj())))
      c.close()
      got ==> names.map(n => Right(s"answer $n"))
    }
  }
}
