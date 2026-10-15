package grit.outline.mcp

import grit.outline.locate.Root
import grit.outline.query.{Help, Roots}

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

  /** The worktree root: the nearest directory at or above the test's working directory that holds `build.mill`. */
  private def repoRoot: String = {
    def up(dir: os.Path): os.Path =
      if (os.exists(dir / "build.mill")) dir
      else {
        val parent = dir / os.up
        if (parent == dir) throw new Exception("no build.mill above the test's working directory")
        else up(parent)
      }
    up(os.pwd).toString
  }

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

    test("initialize's instructions state which text is the file's own") {
      val line = request(
        1,
        "initialize",
        ujson.Obj("protocolVersion" -> "2025-06-18", "capabilities" -> ujson.Obj())
      )
      val (r, _) = reply(line, empty)
      val instructions = resultOf(r).flatMap(field(_, "instructions")).flatMap(_.strOpt)
      assert(instructions.exists(_.contains(Help.fileText)))
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

    test(
      "tools/list names exactly show, family, uses, area, find, tests and file, and no description mentions grit"
    ) {
      val (r, _) = reply(request(3, "tools/list", ujson.Obj()), empty)
      val tools =
        resultOf(r).flatMap(field(_, "tools")).flatMap(_.arrOpt).getOrElse(ujson.Arr().arr)
      val names = tools.flatMap(field(_, "name")).flatMap(_.strOpt).toSet
      assert(names == Set("show", "family", "uses", "area", "find", "tests", "file"))
      assert(!ujson.write(resultOf(r).getOrElse(ujson.Null)).toLowerCase.contains("grit"))
    }

    test("show's description asks for every symbol in one call") {
      val (r, _) = reply(request(3, "tools/list", ujson.Obj()), empty)
      val tools =
        resultOf(r).flatMap(field(_, "tools")).flatMap(_.arrOpt).getOrElse(ujson.Arr().arr)
      val show = tools.find(t => field(t, "name").flatMap(_.strOpt).contains("show"))
      assert(show.flatMap(field(_, "description")).flatMap(_.strOpt).exists(_.contains("one call")))
    }

    test(
      "each tool's description is its Help purpose, then its examples as the CLI writes them, and ends with fileText only for the tools that print file text"
    ) {
      val (r, _) = reply(request(4, "tools/list", ujson.Obj()), empty)
      val tools =
        resultOf(r).flatMap(field(_, "tools")).flatMap(_.arrOpt).getOrElse(ujson.Arr().arr)
      Help.entries.foreach { e =>
        val tool = tools.find(t => field(t, "name").flatMap(_.strOpt).contains(e.query))
        val described = tool.flatMap(field(_, "description")).flatMap(_.strOpt)
        assert(described == Some(Help.description(e)))
        assert(
          described.exists(_.endsWith(Help.fileText)) ==
            Vector("show", "family", "tests", "file").contains(e.query)
        )
      }
    }

    test("a parameter's description is its flag's sentence in Help") {
      val (r, _) = reply(request(5, "tools/list", ujson.Obj()), empty)
      val tools =
        resultOf(r).flatMap(field(_, "tools")).flatMap(_.arrOpt).getOrElse(ujson.Arr().arr)
      val show = tools.find(t => field(t, "name").flatMap(_.strOpt).contains("show"))
      val body = show
        .flatMap(field(_, "inputSchema"))
        .flatMap(field(_, "properties"))
        .flatMap(field(_, "body"))
        .flatMap(field(_, "description"))
        .flatMap(_.strOpt)
      assert(body == Help.sentence(Help.show, "body"))
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

    test("every tool offers max_kb, default 12, and a max_kb below 1 is an error result") {
      val (r, _) = reply(request(9, "tools/list", ujson.Obj()), empty)
      val tools =
        resultOf(r).flatMap(field(_, "tools")).flatMap(_.arrOpt).getOrElse(ujson.Arr().arr)
      val defaults = tools.map(t =>
        field(t, "inputSchema")
          .flatMap(field(_, "properties"))
          .flatMap(field(_, "max_kb"))
          .flatMap(field(_, "default"))
          .flatMap(_.numOpt)
      )
      assert(defaults.size == Help.entries.size && defaults.forall(_ == Some(12.0)))
      val (bad, _) =
        reply(call(10, "find", ujson.Obj("pattern" -> "A", "max_kb" -> 0)), empty)
      assert(isError(bad) == Some(ujson.Bool(true)))
      assert(textOf(bad).exists(_.contains("max_kb must be a whole number from 1")))
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

    test(
      "a call without root, after one that named another, says it answers for the default checkout"
    ) {
      val r1 = checkout("default", one)
      val r2 = checkout("other", two)
      val (_, s1) = reply(
        call(6, "show", ujson.Obj("symbols" -> ujson.Arr("S.f"), "root" -> r2.dir.toString)),
        State(Roots.empty(6000), r1)
      )
      val (r, _) = reply(call(7, "show", ujson.Obj("symbols" -> ujson.Arr("S.f"))), s1)
      val lines = textOf(r).getOrElse("").linesIterator.toVector
      assert(lines.headOption.exists(_.startsWith(s"## root ${r1.dir} ")))
      assert(
        lines.lift(1) == Some(
          s"-- answered for the default checkout, ${r1.dir}; pass root to ask about another"
        )
      )
      assert(lines.exists(_.contains("def f(a: Int): Int")))
    }

    test("a session whose calls named no root but the default gets no default-checkout line") {
      val r1 = checkout("only", one)
      val (_, s1) = reply(
        call(6, "show", ujson.Obj("symbols" -> ujson.Arr("S.f"), "root" -> r1.dir.toString)),
        State(Roots.empty(6000), r1)
      )
      val (r, _) = reply(call(7, "show", ujson.Obj("symbols" -> ujson.Arr("S.f"))), s1)
      val text = textOf(r).getOrElse("")
      assert(text.contains("def f(a: Int): Int"))
      assert(!text.contains("default checkout"))
    }

    test("a request whose handling throws gets a -32603 error and the next request is answered") {
      val written = Vector.newBuilder[String]
      val ended = Server.serveLines(
        Iterator(request(1, "tools/call", ujson.Obj()), request(2, "ping", ujson.Obj())),
        0,
        written += _,
        _ => ()
      ) { (line, n) =>
        if (field(ujson.read(line), "method").flatMap(_.strOpt) == Some("tools/call"))
          throw new NoClassDefFoundError("p/Gone")
        else (Some(s"answered $line"), n + 1, None)
      }
      val replies = written.result()
      assert(ended == Server.Ended.Closed)
      assert(replies.size == 2)
      val failed = ujson.read(replies(0))
      assert(field(failed, "id").flatMap(_.numOpt) == Some(1.0))
      assert(field(failed, "error").flatMap(field(_, "code")).flatMap(_.numOpt) == Some(-32603.0))
      assert(replies(1) == s"answered ${request(2, "ping", ujson.Obj())}")
    }

    test("three linkage errors in a row end serving, naming the last, and no later line is read") {
      val logged = Vector.newBuilder[String]
      val lines = Iterator.tabulate(5)(i => request(i, "ping", ujson.Obj()))
      val ended = Server.serveLines(lines, 0, _ => (), logged += _) { (_, n) =>
        throw new NoClassDefFoundError(s"p/Gone$n")
      }
      assert(ended match {
        case Server.Ended.Unlinked(e) => e.getMessage == "p/Gone0"
        case Server.Ended.Closed => false
      })
      assert(lines.size == 2)
      assert(logged.result().count(_.contains("NoClassDefFoundError")) == 3)
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

    test(
      "show's tests argument counts the tests by default, lists them as list, and drops both as none"
    ) {
      def shown(id: Int, tests: Option[String], state: State): (String, State) = {
        val args = ujson.Obj(
          "symbols" -> ujson.Arr("grit.outline.render.Render.oneLine"),
          "root" -> repoRoot
        )
        tests.foreach(t => args("tests") = t)
        val (r, next) = reply(call(id, "show", args), state)
        (textOf(r).getOrElse(""), next)
      }
      val (summary, warm) = shown(6, None, empty)
      val (listed, warmer) = shown(7, Some("list"), warm)
      val (none, _) = shown(8, Some("none"), warmer)
      assert(summary.contains("\n-- tests of Render.oneLine: "))
      assert(!summary.contains("## tests exercising"))
      assert(listed.contains("## tests exercising grit.outline.render.Render.oneLine"))
      assert(!listed.contains("-- tests of"))
      assert(none.contains("def Render.oneLine"))
      assert(!none.contains("## tests exercising") && !none.contains("-- tests of"))
    }
  }
}
