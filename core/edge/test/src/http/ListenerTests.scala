package grit.edge.http

import java.net.{ConnectException, Socket, SocketException}
import java.nio.charset.StandardCharsets.{ISO_8859_1, UTF_8}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, TimeUnit}

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import grit.core.edge.EdgeRefusal

import utest.*

/** [[Listener]] over a real socket on a loopback port: what it hands `answer`, the requests it
  * answers itself, and that no request outlives [[Listener.close]]. Bounds are shortened to
  * 300 ms so each test stays fast.
  */
object ListenerTests extends TestSuite {

  private val Short = 300.millis
  private val limits =
    Limits(bodyMost = 64, readWithin = Short, closeWithin = Short, interruptedWithin = Short)

  /** What a test's listener logged. */
  private final class Logged {
    private val lines = new ConcurrentLinkedQueue[String]()

    /** Counted down when the listener logs that it is closing. */
    val closing = new CountDownLatch(1)
    val log: String -> Unit = line => {
      lines.add(line)
      if (line.startsWith("closing")) closing.countDown()
    }
    def all: Vector[String] = lines.asScala.toVector
  }

  private def opened(
      at: String,
      answer: (Request, String => Unit) => Response,
      logged: Logged
  ): Listener^{answer} =
    Listen
      .parse(at)
      .flatMap(l => Listener.opened(l, answer, logged.log, limits).left.map(_.message))
      .fold(why => throw new java.lang.AssertionError(why), l => l)

  /** A response as a client read it: its status, headers by lower-case name, and body. */
  private final case class Got(status: Int, headers: Map[String, String], body: String)

  private def got(raw: String): Got = {
    val (head, body) = raw.indexOf("\r\n\r\n") match {
      case -1 => (raw, "")
      case at => (raw.substring(0, at), raw.substring(at + 4))
    }
    val lines = head.split("\r\n").toVector
    val status = lines.headOption.flatMap(_.split(" ").lift(1)).flatMap(_.toIntOption).getOrElse(-1)
    val headers = lines
      .drop(1)
      .flatMap(l =>
        l.indexOf(':') match {
          case -1 => None
          case at => Some(l.substring(0, at).trim.toLowerCase -> l.substring(at + 1).trim)
        }
      )
      .toMap
    Got(status, headers, body)
  }

  /** Everything the server sends on `socket` until it closes the connection. */
  private def readAll(socket: Socket): String =
    new String(socket.getInputStream.readAllBytes(), UTF_8)

  /** `bytes` as a request carries them: each byte one character, sent by [[send]] as that byte. */
  private def octets(bytes: String, charset: java.nio.charset.Charset): String =
    new String(bytes.getBytes(charset), ISO_8859_1)

  private def connect(host: String, port: Int): Socket = {
    val s = new Socket(host, port)
    s.setSoTimeout(3000)
    s
  }

  /** `raw` sent on a connection of its own, and everything sent back until the server closes it. */
  private def send(port: Int, raw: String, host: String = "127.0.0.1"): String = {
    val s = connect(host, port)
    try {
      s.getOutputStream.write(raw.getBytes(ISO_8859_1))
      s.getOutputStream.flush()
      readAll(s)
    } finally s.close()
  }

  private def get(target: String, headers: String = ""): String =
    s"GET $target HTTP/1.1\r\nHost: test\r\n${headers}Connection: close\r\n\r\n"

  /** A POST of `body`, its characters each one byte ([[octets]]). */
  private def post(target: String, body: String): String =
    s"POST $target HTTP/1.1\r\nHost: test\r\nContent-Length: ${body.length}\r\nConnection: close\r\n\r\n$body"

  private val echo: (Request, String => Unit) -> Response = (r, _) =>
    Response.text(
      Status.Ok,
      s"${r.verb.name} ${r.path.mkString("|")} ${r.query("q")} ${r.header("x-thing")} [${r.body}]"
    )

  val tests = Tests {
    test("a request is answered with what answer returns, its path, query and headers as sent") {
      val logged = new Logged
      val l = opened("127.0.0.1:0", echo, logged)
      try {
        val reply = got(send(l.at.port, get("/a/b%20c?q=1", "X-THING: v\r\n")))
        (
          l.at.host,
          l.at.port != 0,
          reply.status,
          reply.headers.get("content-type"),
          reply.body
        ) ==> (
          "127.0.0.1",
          true,
          200,
          Some("text/plain; charset=utf-8"),
          "GET a|b c Some(1) Some(v) []"
        )
      } finally l.close()
    }

    test("a body is handed on as text; HEAD is answered with no body") {
      val seen = new ConcurrentLinkedQueue[String]()
      val answer: (Request, String => Unit) => Response = (r, said) => {
        seen.add(s"${r.verb.name} [${r.body}]")
        echo(r, said)
      }
      val l = opened("127.0.0.1:0", answer, new Logged)
      try {
        val posted = got(send(l.at.port, post("/p", octets("héllo", UTF_8))))
        val head =
          got(send(l.at.port, "HEAD /h HTTP/1.1\r\nHost: test\r\nConnection: close\r\n\r\n"))
        (posted.status, posted.body, head.status, head.body, seen.asScala.toVector) ==> (
          200,
          "POST p None None [héllo]",
          200,
          "",
          Vector("POST [héllo]", "HEAD []")
        )
      } finally l.close()
    }

    test("a body over the cap, sent whole or chunked, is answered 413 without calling answer") {
      val calls = new AtomicInteger()
      val answer: (Request, String => Unit) => Response = (r, said) => {
        calls.incrementAndGet(); echo(r, said)
      }
      val l = opened("127.0.0.1:0", answer, new Logged)
      try {
        val whole = got(send(l.at.port, post("/p", "a" * 65)))
        val chunk = "a" * 40
        val chunked = got(
          send(
            l.at.port,
            "POST /p HTTP/1.1\r\nHost: test\r\nTransfer-Encoding: chunked\r\nConnection: close\r\n\r\n" +
              s"28\r\n$chunk\r\n28\r\n$chunk\r\n0\r\n\r\n"
          )
        )
        val atCap = got(send(l.at.port, post("/p", "a" * 64)))
        (whole.status, chunked.status, atCap.status, calls.get) ==> (413, 413, 200, 1)
      } finally l.close()
    }

    test(
      "a body not UTF-8 or a target not a path is answered 400, and * 404, without calling answer"
    ) {
      val calls = new AtomicInteger()
      val answer: (Request, String => Unit) => Response = (r, said) => {
        calls.incrementAndGet(); echo(r, said)
      }
      val l = opened("127.0.0.1:0", answer, new Logged)
      try {
        val notUtf8 = got(send(l.at.port, post("/p", "\u00c3(")))
        val absolute = got(send(l.at.port, get("http://example.org/x")))
        val star = got(send(l.at.port, get("*")))
        (notUtf8.status, absolute.status, star.status, calls.get) ==> (400, 400, 404, 0)
      } finally l.close()
    }

    test(
      "a client that stalls mid-body sees an end within the read bound, answer not called, others answered"
    ) {
      val calls = new AtomicInteger()
      val answer: (Request, String => Unit) => Response = (r, said) => {
        calls.incrementAndGet(); echo(r, said)
      }
      val l = opened("127.0.0.1:0", answer, new Logged)
      try {
        val stalled = connect("127.0.0.1", l.at.port)
        try {
          stalled.getOutputStream.write(
            "POST /p HTTP/1.1\r\nHost: test\r\nContent-Length: 10\r\n\r\nabc".getBytes(ISO_8859_1)
          )
          stalled.getOutputStream.flush()
          val other = got(send(l.at.port, get("/other")))
          val ended =
            try { readAll(stalled); true }
            catch { case _: SocketException => true }
          (other.status, ended, calls.get) ==> (200, true, 1)
        } finally stalled.close()
      } finally l.close()
    }

    test(
      "an answer that throws is answered 500, its class, message, verb and path logged, never the query"
    ) {
      val logged = new Logged
      val answer: (Request, String => Unit) => Response =
        (_, _) => throw new IllegalStateException("boom")
      val l = opened("127.0.0.1:0", answer, logged)
      try {
        val reply = got(send(l.at.port, get("/x?q=secret")))
        (
          reply.status,
          logged.all.filter(_.contains("java.lang.IllegalStateException: boom")).size
        ) ==> (500, 1)
        assert(logged.all.exists(line => line.contains("GET /x") && line.contains("boom")))
        logged.all.filter(_.contains("secret")) ==> Vector.empty
      } finally l.close()
    }

    test("an answer's 405 carries Allow, naming its verbs") {
      val answer: (Request, String => Unit) => Response =
        (_, _) => Response.text(Status.NotAllowed(Vector(Verb.Get, Verb.Post)), "")
      val l = opened("127.0.0.1:0", answer, new Logged)
      try {
        val reply = got(send(l.at.port, get("/x")))
        (reply.status, reply.headers.get("allow"), reply.body) ==> (405, Some("GET, POST"), "")
      } finally l.close()
    }

    test("two requests on one kept-alive connection are both answered") {
      val l = opened("127.0.0.1:0", echo, new Logged)
      try {
        val raw = send(l.at.port, "GET /one HTTP/1.1\r\nHost: test\r\n\r\n" + get("/two"))
        (raw.split("HTTP/1.1 200").length - 1, raw.contains("GET one"), raw.contains("GET two")) ==>
          (2, true, true)
      } finally l.close()
    }

    test("[::1] is listened on and answered") {
      val l = opened("[::1]:0", echo, new Logged)
      try {
        val reply = got(send(l.at.port, get("/six"), host = "::1"))
        (l.at.shown.startsWith("[::1]:"), l.at.loopback, reply.status, reply.body) ==>
          (true, true, 200, "GET six None None []")
      } finally l.close()
    }

    test("a port in use is refused") {
      val first = opened("127.0.0.1:0", echo, new Logged)
      try {
        val refused = Listen.parse(first.at.shown).exists { at =>
          Listener.opened(at, echo, _ => (), limits) match {
            case Left(EdgeRefusal.Refused(_)) => true
            case Left(_) => false
            case Right(second) => { second.close(); false }
          }
        }
        refused ==> true
      } finally first.close()
    }

    test(
      "close interrupts a request still running past its bound and returns once it has answered"
    ) {
      val logged = new Logged
      val started = new CountDownLatch(1)
      val answer: (Request, String => Unit) => Response = (_, _) => {
        started.countDown()
        try { Thread.sleep(10000); Response.text(Status.Ok, "slept") }
        catch { case _: InterruptedException => Response.text(Status.Unavailable, "interrupted") }
      }
      val l = opened("127.0.0.1:0", answer, logged)
      val client = connect("127.0.0.1", l.at.port)
      try {
        client.getOutputStream.write(get("/slow").getBytes(ISO_8859_1))
        client.getOutputStream.flush()
        assert(started.await(3, TimeUnit.SECONDS))
        val from = System.nanoTime()
        l.close()
        val took = (System.nanoTime() - from).nanos
        val reply = got(readAll(client))
        (reply.status, reply.body, took < 2.seconds) ==> (503, "interrupted", true)
        assert(
          logged.all.exists(line => line.contains("GET /slow") && line.contains("still running"))
        )
      } finally client.close()
    }

    test(
      "an answer that ignores its interrupt holds close until it returns; a request meanwhile is answered 503"
    ) {
      val logged = new Logged
      val started = new CountDownLatch(1)
      val release = new CountDownLatch(1)
      val finished = new AtomicBoolean(false)
      val calls = new AtomicInteger()
      val answer: (Request, String => Unit) => Response = (_, _) => {
        calls.incrementAndGet()
        started.countDown()
        var waiting = true
        while (waiting)
          try { release.await(); waiting = false }
          catch { case _: InterruptedException => () }
        finished.set(true)
        Response.text(Status.Ok, "done")
      }
      val l = Listener
        .opened(
          Listen.parse("127.0.0.1:0").getOrElse(throw new java.lang.AssertionError("address")),
          answer,
          logged.log,
          limits.copy(closeWithin = 1.second)
        )
        .fold(why => throw new java.lang.AssertionError(why.message), l => l)
      val client = connect("127.0.0.1", l.at.port)
      val closed = new CountDownLatch(1)
      val finishedAtClose = new AtomicBoolean(false)
      try {
        client.getOutputStream.write(get("/held").getBytes(ISO_8859_1))
        client.getOutputStream.flush()
        assert(started.await(3, TimeUnit.SECONDS))
        Thread
          .ofVirtual()
          .start(() => { l.close(); finishedAtClose.set(finished.get); closed.countDown() })
        assert(logged.closing.await(3, TimeUnit.SECONDS))
        val meanwhile = got(send(l.at.port, get("/late")))
        val heldPastBounds = !closed.await(2500, TimeUnit.MILLISECONDS)
        release.countDown()
        val returned = closed.await(3, TimeUnit.SECONDS)
        (meanwhile.status, calls.get, heldPastBounds, returned, finishedAtClose.get) ==>
          (503, 1, true, true, true)
        assert(
          logged.all.count(line =>
            line.contains("GET /held") && line.contains("still running")
          ) >= 2
        )
      } finally {
        release.countDown()
        client.close()
      }
    }

    test("after close, nothing is listening; closing again does nothing") {
      val l = opened("127.0.0.1:0", echo, new Logged)
      l.close()
      l.close()
      val refused =
        try { send(l.at.port, get("/x")); false }
        catch { case _: ConnectException => true }
      refused ==> true
    }
  }
}
