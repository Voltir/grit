package grit.outline.mcp

import grit.outline.locate.Root
import grit.outline.query.Roots

import utest.*

object ServerTests extends TestSuite {

  /** A checkout under a fresh temp dir: an empty `build.mill`, `src/p/S.scala` holding `source`, compiled into its `out`. */
  private def checkout(name: String, source: String): Root = {
    val dir = os.temp.dir(prefix = s"outline-$name-")
    os.write(dir / "build.mill", "")
    val src = dir / "src" / "p" / "S.scala"
    os.write(src, source, createFolders = true)
    val classes = dir / "out" / "m" / "compile.dest" / "classes"
    os.makeDir.all(classes)
    val reporter = dotty.tools.dotc.Main.process(
      Array(
        "-d",
        classes.toString,
        "-classpath",
        System.getProperty("java.class.path"),
        src.toString
      )
    )
    assert(!reporter.hasErrors)
    Root(dir)
  }

  /** `A.keep` sits on lines 9-10: its Scaladoc on 9, its definition on 10. */
  private val keepFixture: String = Vector(
    "package p",
    "",
    "/** A. */",
    "object A {",
    "  private val pad = 0",
    "",
    "",
    "",
    "  /** Keep. */",
    "  def keep(a: Int): Int = a",
    "}",
    ""
  ).mkString("\n")

  private val one = "package p\n/** One. */\nobject S { def f(a: Int): Int = a }\n"
  private val two = "package p\n/** Two. */\nobject S { def f(a: String, b: Int): String = a }\n"

  private def request(id: Int, method: String, params: ujson.Value): String =
    ujson.write(ujson.Obj("jsonrpc" -> "2.0", "id" -> id, "method" -> method, "params" -> params))

  private def call(id: Int, tool: String, arguments: ujson.Value): String =
    request(id, "tools/call", ujson.Obj("name" -> tool, "arguments" -> arguments))

  private def reply(line: String, state: State): (Option[ujson.Value], State) = {
    val (text, next) = Server.handle(line, state)
    (text.map(ujson.read(_)), next)
  }

  private def field(v: ujson.Value, key: String): Option[ujson.Value] =
    v.objOpt.flatMap(_.collectFirst { case (k, x) if k == key => x })

  private def resultOf(r: Option[ujson.Value]): Option[ujson.Value] =
    r.flatMap(field(_, "result"))

  /** The text of a `tools/call` result's first content item. */
  private def textOf(r: Option[ujson.Value]): Option[String] =
    resultOf(r)
      .flatMap(field(_, "content"))
      .flatMap(_.arrOpt)
      .flatMap(_.headOption)
      .flatMap(field(_, "text"))
      .flatMap(_.strOpt)

  private def isError(r: Option[ujson.Value]): Option[ujson.Value] =
    resultOf(r).flatMap(field(_, "isError"))

  private val empty: State = State(Roots.empty(6000), Root(os.pwd))

  def tests = Tests {

    test("initialize echoes a supported client version and answers the request's id") {
      val line = request(
        1,
        "initialize",
        ujson.Obj("protocolVersion" -> "2025-06-18", "capabilities" -> ujson.Obj())
      )
      val (r, _) = reply(line, empty)
      assert(r.flatMap(field(_, "id")) == Some(ujson.Num(1)))
      assert(
        resultOf(r).flatMap(field(_, "protocolVersion")).flatMap(_.strOpt) == Some("2025-06-18")
      )
    }

    test("initialize answers 2025-11-25 to a version the server does not offer") {
      val line = request(2, "initialize", ujson.Obj("protocolVersion" -> "1999-01-01"))
      val (r, _) = reply(line, empty)
      assert(
        resultOf(r).flatMap(field(_, "protocolVersion")).flatMap(_.strOpt) == Some("2025-11-25")
      )
    }

    test("notifications/initialized gets no reply") {
      val line = """{"jsonrpc":"2.0","method":"notifications/initialized"}"""
      val (text, _) = Server.handle(line, empty)
      assert(text == None)
    }

    test("tools/list names exactly show, family, uses and area, and no description mentions grit") {
      val (r, _) = reply(request(3, "tools/list", ujson.Obj()), empty)
      val tools =
        resultOf(r).flatMap(field(_, "tools")).flatMap(_.arrOpt).getOrElse(ujson.Arr().arr)
      val names = tools.flatMap(field(_, "name")).flatMap(_.strOpt).toSet
      assert(names == Set("show", "family", "uses", "area"))
      assert(!ujson.write(resultOf(r).getOrElse(ujson.Null)).toLowerCase.contains("grit"))
    }

    test("show's description asks for every symbol in one call") {
      val (r, _) = reply(request(3, "tools/list", ujson.Obj()), empty)
      val tools =
        resultOf(r).flatMap(field(_, "tools")).flatMap(_.arrOpt).getOrElse(ujson.Arr().arr)
      val show = tools.find(t => field(t, "name").flatMap(_.strOpt).contains("show"))
      assert(show.flatMap(field(_, "description")).flatMap(_.strOpt).exists(_.contains("one call")))
    }

    test("tools/call show returns the named signature with its line range, not an error") {
      val fixture = checkout("keep", keepFixture)
      val line = call(
        4,
        "show",
        ujson.Obj("symbols" -> ujson.Arr("A.keep"), "root" -> fixture.dir.toString)
      )
      val (r, _) = reply(line, empty)
      assert(textOf(r).exists(_.contains("9-10 def A.keep")))
      assert(isError(r) == Some(ujson.Bool(false)))
    }

    test("tools/call show with symbols as a string, not an array, is an error result") {
      val (r, _) = reply(call(5, "show", ujson.Obj("symbols" -> "A.keep")), empty)
      assert(isError(r) == Some(ujson.Bool(true)))
    }

    test("alternating calls for two roots each answer from their own root") {
      val r1 = checkout("one", one)
      val r2 = checkout("two", two)
      val (a, s1) = reply(
        call(6, "show", ujson.Obj("symbols" -> ujson.Arr("S.f"), "root" -> r1.dir.toString)),
        State(Roots.empty(6000), r1)
      )
      val (b, s2) = reply(
        call(7, "show", ujson.Obj("symbols" -> ujson.Arr("S.f"), "root" -> r2.dir.toString)),
        s1
      )
      val (c, _) = reply(
        call(8, "show", ujson.Obj("symbols" -> ujson.Arr("S.f"), "root" -> r1.dir.toString)),
        s2
      )
      assert(textOf(a).exists(_.contains("def f(a: Int): Int")))
      assert(textOf(b).exists(_.contains("def f(a: String, b: Int): String")))
      assert(textOf(c).exists(_.contains("def f(a: Int): Int")))
    }

    test("an unknown method gets -32601 and unparsable JSON gets -32700 with a null id") {
      val (unknown, _) = reply(request(9, "resources/frobnicate", ujson.Obj()), empty)
      assert(
        field(unknown.getOrElse(ujson.Null), "error")
          .flatMap(field(_, "code"))
          .flatMap(_.numOpt) == Some(-32601.0)
      )
      val (bad, _) = reply("{", empty)
      assert(
        bad.flatMap(field(_, "error")).flatMap(field(_, "code")).flatMap(_.numOpt) == Some(-32700.0)
      )
      assert(bad.flatMap(field(_, "id")) == Some(ujson.Null))
    }
  }
}
