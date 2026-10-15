package grit.outline.mcp

import scala.util.Try
import scala.util.control.NonFatal

import grit.outline.locate.{MemoLayout, MillLayout, Root}
import grit.outline.query.{Config, Entry, Help, Query, Roots}
import grit.outline.render.TestsShown

/** The server between calls: each root's cache, the root a call without one uses, and whether a call so far
  * named a root other than that one.
  */
final case class State(roots: Roots, defaultRoot: Root, namedOther: Boolean = false)

object Server {

  private val versions = Vector("2025-11-25", "2025-06-18", "2025-03-26", "2024-11-05")
  private val latest = "2025-11-25"
  private val cachedFiles = 6000

  /** A JSON object holding `items` in order. */
  private def obj(items: Seq[(String, ujson.Value)]): ujson.Obj = {
    val out = ujson.Obj()
    items.foreach { case (key, value) => out(key) = value }
    out
  }

  /** A parameter `name` of `e`, typed by `fields` and described by its sentence in Help. */
  private def prop(e: Entry, name: String, fields: (String, ujson.Value)*): (String, ujson.Value) =
    name -> obj(fields ++ Help.sentence(e, name).map(s => "description" -> ujson.Str(s)))

  /** The MCP tool for `e`: its description from Help, its parameters `properties`, and those it requires. */
  private def tool(
      e: Entry,
      properties: Vector[(String, ujson.Value)],
      required: Vector[String]
  ): ujson.Value =
    ujson.Obj(
      "name" -> e.query,
      "description" -> Help.description(e),
      "inputSchema" -> ujson.Obj(
        "type" -> "object",
        "properties" -> obj(
          properties :+ prop(
            e,
            "max_kb",
            "type" -> "integer",
            "minimum" -> 1,
            "default" -> Help.defaultMaxKb
          )
        ),
        "required" -> ujson.Arr(required.map(ujson.Str(_))*),
        "additionalProperties" -> false
      )
    )

  private val showTool: ujson.Value = tool(
    Help.show,
    Vector(
      prop(Help.show, "symbols", "type" -> "array", "items" -> ujson.Obj("type" -> "string")),
      prop(Help.show, "depth", "type" -> "integer", "minimum" -> 0, "maximum" -> 2, "default" -> 1),
      prop(Help.show, "body", "type" -> "array", "items" -> ujson.Obj("type" -> "string")),
      prop(Help.show, "private", "type" -> "boolean"),
      prop(Help.show, "docs", "type" -> "boolean"),
      prop(
        Help.show,
        "tests",
        "type" -> "string",
        "enum" -> ujson.Arr("summary", "list", "none"),
        "default" -> "summary"
      ),
      prop(Help.show, "root", "type" -> "string")
    ),
    Vector("symbols")
  )

  private val familyTool: ujson.Value = tool(
    Help.family,
    Vector(
      prop(Help.family, "trait", "type" -> "string"),
      prop(Help.family, "member", "type" -> "string"),
      prop(Help.family, "body", "type" -> "boolean"),
      prop(Help.family, "root", "type" -> "string")
    ),
    Vector("trait")
  )

  private val testsTool: ujson.Value = tool(
    Help.tests,
    Vector(
      prop(Help.tests, "suite", "type" -> "string"),
      prop(Help.tests, "test", "type" -> "string"),
      prop(Help.tests, "body", "type" -> "array", "items" -> ujson.Obj("type" -> "string")),
      prop(Help.tests, "fixtures", "type" -> "boolean"),
      prop(Help.tests, "root", "type" -> "string")
    ),
    Vector("suite")
  )

  private val fileTool: ujson.Value = tool(
    Help.file,
    Vector(
      prop(Help.file, "target", "type" -> "string"),
      prop(Help.file, "root", "type" -> "string")
    ),
    Vector("target")
  )

  private val areaTool: ujson.Value = tool(
    Help.area,
    Vector(
      prop(Help.area, "names", "type" -> "array", "items" -> ujson.Obj("type" -> "string")),
      prop(Help.area, "level", "type" -> "integer"),
      prop(Help.area, "root", "type" -> "string")
    ),
    Vector.empty
  )

  private val findTool: ujson.Value = tool(
    Help.find,
    Vector(
      prop(Help.find, "pattern", "type" -> "string"),
      prop(Help.find, "private", "type" -> "boolean"),
      prop(Help.find, "root", "type" -> "string")
    ),
    Vector("pattern")
  )

  private val usesTool: ujson.Value = tool(
    Help.uses,
    Vector(
      prop(Help.uses, "symbols", "type" -> "array", "items" -> ujson.Obj("type" -> "string")),
      prop(Help.uses, "in", "type" -> "string"),
      prop(Help.uses, "outside", "type" -> "string"),
      prop(Help.uses, "sites", "type" -> "boolean"),
      prop(Help.uses, "root", "type" -> "string")
    ),
    Vector("symbols")
  )

  /** The reply to one JSON-RPC line (none for a notification), and the state after it. */
  def handle(line: String, state: State): (Option[String], State) = {
    val (reply, next, _) = process(line, state)
    (reply, next)
  }

  /** Serves stdin to stdout, one request at a time, until `serveLines` ends; logs to stderr only, and says
    * there why it ends.
    */
  def serve(defaultRoot: Root): Unit = {
    val out = new java.io.PrintStream(
      new java.io.FileOutputStream(java.io.FileDescriptor.out),
      true,
      java.nio.charset.StandardCharsets.UTF_8
    )
    val in = new java.io.BufferedReader(
      new java.io.InputStreamReader(System.in, java.nio.charset.StandardCharsets.UTF_8)
    )
    val lines = Iterator.continually(Option(in.readLine())).takeWhile(_.isDefined).flatten
    val ended = serveLines(
      lines,
      State(Roots.empty(cachedFiles), defaultRoot),
      r => {
        out.print(r + "\n")
        out.flush()
      },
      System.err.println
    )(process)
    ended match {
      case Ended.Closed => System.err.println("outline mcp: ending, since stdin closed")
      case Ended.Unlinked(last) =>
        System.err.println(
          s"outline mcp: ending, since three requests in a row failed to link (the last on $last): " +
            "its classes are gone or were changed under it, and a restart starts it from a fresh copy"
        )
        sys.exit(3)
    }
  }

  /** Why `serveLines` ended. */
  enum Ended {

    /** Its lines ran out: the client closed stdin. */
    case Closed

    /** Three requests in a row failed to link, `last` the third: the server's classes are gone, or were
      * changed, under it.
      */
    case Unlinked(last: LinkageError)
  }

  /** Answers each of `lines` in turn from `start`, through `step`: each reply it gives goes to `write`, and a
    * line naming a tool call and its reply's size to `log`. A request whose `step` throws an exception, a
    * `LinkageError` or a `StackOverflowError` gets a -32603 error reply, the state is kept, and a line
    * naming the throwable and the start of the request goes to `log`; the third such `LinkageError` in a
    * row ends serving. Any other throwable is logged so and rethrown.
    */
  def serveLines[S](
      lines: Iterator[String],
      start: S,
      write: String => Unit,
      log: String => Unit
  )(step: (String, S) => (Option[String], S, Option[String])): Ended = {
    var state = start
    var unlinked = 0
    var ended: Option[Ended] = None
    while (ended.isEmpty && lines.hasNext) {
      val line = lines.next()
      def failed(e: Throwable): Unit = {
        log(s"outline mcp: a request failed on $e: ${line.take(300)}")
        Try(ujson.read(line)).toOption.flatMap(field(_, "id")).foreach { id =>
          write(ujson.write(errorReply(id, -32603, s"outline failed on this request: $e")))
        }
      }
      try {
        val (reply, next, tool) = step(line, state)
        tool.foreach(name =>
          log(s"$name ${reply.fold(0)(_.getBytes(java.nio.charset.StandardCharsets.UTF_8).length)} B")
        )
        reply.foreach(write)
        state = next
        unlinked = 0
      } catch {
        case e: LinkageError =>
          failed(e)
          unlinked += 1
          if (unlinked >= 3) ended = Some(Ended.Unlinked(e))
        case e: StackOverflowError =>
          failed(e)
          unlinked = 0
        case NonFatal(e) =>
          failed(e)
          unlinked = 0
        case e: Throwable =>
          log(s"outline mcp: ending on $e, in the request ${line.take(300)}")
          throw e
      }
    }
    ended.getOrElse(Ended.Closed)
  }

  /** As `handle`, and the name of the tool a `tools/call` line named. */
  private def process(line: String, state: State): (Option[String], State, Option[String]) =
    if (line.trim.isEmpty) (None, state, None)
    else
      Try(ujson.read(line)).toOption match {
        case None =>
          (Some(ujson.write(errorReply(ujson.Null, -32700, "parse error"))), state, None)
        case Some(msg) => message(msg, state)
      }

  private def message(msg: ujson.Value, state: State): (Option[String], State, Option[String]) =
    if (msg.objOpt.isEmpty)
      (
        Some(ujson.write(errorReply(ujson.Null, -32600, "invalid request: not an object"))),
        state,
        None
      )
    else
      field(msg, "id") match {
        case None => (None, state, None)
        case Some(id) =>
          val params = field(msg, "params").getOrElse(ujson.Obj())
          field(msg, "method").flatMap(_.strOpt) match {
            case None => answered(id, Left((-32600, "invalid request: no method")), state, None)
            case Some("initialize") => answered(id, Right(initialize(params)), state, None)
            case Some("ping") => answered(id, Right(ujson.Obj()), state, None)
            case Some("tools/list") =>
              answered(
                id,
                Right(
                  ujson.Obj(
                    "tools" -> ujson
                      .Arr(showTool, familyTool, usesTool, areaTool, findTool, testsTool, fileTool)
                  )
                ),
                state,
                None
              )
            case Some("tools/call") =>
              val (body, next, tool) = callTool(params, state)
              answered(id, body, next, tool)
            case Some(other) =>
              answered(id, Left((-32601, s"method not found: $other")), state, None)
          }
      }

  private def answered(
      id: ujson.Value,
      body: Either[(Int, String), ujson.Value],
      state: State,
      tool: Option[String]
  ): (Option[String], State, Option[String]) = {
    val envelope = body match {
      case Right(result) => ujson.Obj("jsonrpc" -> "2.0", "id" -> id, "result" -> result)
      case Left((code, message)) => errorReply(id, code, message)
    }
    (Some(ujson.write(envelope)), state, tool)
  }

  private def errorReply(id: ujson.Value, code: Int, message: String): ujson.Value =
    ujson.Obj(
      "jsonrpc" -> "2.0",
      "id" -> id,
      "error" -> ujson.Obj("code" -> code, "message" -> message)
    )

  private def initialize(params: ujson.Value): ujson.Value = {
    val requested =
      field(params, "protocolVersion").flatMap(_.strOpt).filter(versions.contains).getOrElse(latest)
    val instructions =
      s"Outline answers questions about a Scala project's structure from its compiled TASTy, through the tools ${Help.entries.map(_.query).mkString(", ")}.\n\n${Help.fileText}"
    ujson.Obj(
      "protocolVersion" -> requested,
      "capabilities" -> ujson.Obj("tools" -> ujson.Obj("listChanged" -> false)),
      "serverInfo" -> ujson.Obj("name" -> "outline", "version" -> "0.1.0"),
      "instructions" -> instructions
    )
  }

  private def callTool(
      params: ujson.Value,
      state: State
  ): (Either[(Int, String), ujson.Value], State, Option[String]) =
    field(params, "name").flatMap(_.strOpt) match {
      case None => (Left((-32602, "tools/call needs a tool name")), state, None)
      case Some(name) =>
        val args = field(params, "arguments").getOrElse(ujson.Obj())
        maxKbArg(args) match {
          case Left(problem) =>
            (
              Right(toolResult(s"bad arguments for $name: $problem", isError = true)),
              state,
              Some(name)
            )
          case Right(cap) =>
            val named = rootArg(args).toOption.flatten
            val (body, next, tool) = run(name, args, state, cap)
            val noted =
              if (named.isEmpty && state.namedOther) body.map(forDefault(_, state.defaultRoot))
              else body
            (noted, next.copy(namedOther = state.namedOther || named.exists(_ != state.defaultRoot)), tool)
        }
    }

  /** `result` with a line after its `## root` line saying it answers for `root`, the default checkout; any
    * other result, an error's or one with no such line, as it is.
    */
  private def forDefault(result: ujson.Value, root: Root): ujson.Value = {
    val text = field(result, "content")
      .flatMap(_.arrOpt)
      .flatMap(_.headOption)
      .flatMap(field(_, "text"))
      .flatMap(_.strOpt)
    val failed = field(result, "isError").flatMap(_.boolOpt).getOrElse(true)
    text match {
      case Some(t) if !failed && t.startsWith("## root") =>
        val (first, rest) = t.span(_ != '\n')
        val note = s"-- answered for the default checkout, ${root.dir}; pass root to ask about another"
        toolResult(s"$first\n$note$rest", isError = false)
      case _ => result
    }
  }

  /** The call of the tool `name` with `args`, its answer cut at `cap` bytes. */
  private def run(
      name: String,
      args: ujson.Value,
      state: State,
      cap: Int
  ): (Either[(Int, String), ujson.Value], State, Option[String]) =
    name match {
      case "show" =>
        val (result, next) = show(args, state, cap)
        (Right(result), next, Some(name))
      case "family" =>
        val (result, next) = family(args, state, cap)
        (Right(result), next, Some(name))
      case "uses" =>
        val (result, next) = uses(args, state, cap)
        (Right(result), next, Some(name))
      case "tests" =>
        val (result, next) = tests(args, state, cap)
        (Right(result), next, Some(name))
      case "area" =>
        val (result, next) = area(args, state, cap)
        (Right(result), next, Some(name))
      case "find" =>
        val (result, next) = find(args, state, cap)
        (Right(result), next, Some(name))
      case "file" =>
        val (result, next) = file(args, state, cap)
        (Right(result), next, Some(name))
      case other => (Left((-32602, s"unknown tool: $other")), state, Some(other))
    }

  private final case class ShowArgs(
      symbols: Vector[String],
      depth: Int,
      body: Set[String],
      withPrivate: Boolean,
      root: Option[Root],
      tests: TestsShown,
      fullDocs: Boolean
  )

  private final case class FamilyArgs(
      name: String,
      member: Option[String],
      withBody: Boolean,
      root: Option[Root]
  )

  /** `run` with `root`'s query settings, or the error result a query answers with when its `.outline.conf` does not read. */
  private def configured(root: Root, state: State)(
      run: Config => (ujson.Value, State)
  ): (ujson.Value, State) =
    Config.read(root) match {
      case Left(message) => (toolResult(message, isError = true), state)
      case Right(config) => run(config)
    }

  private def show(args: ujson.Value, state: State, cap: Int): (ujson.Value, State) =
    parseShow(args) match {
      case Left(problem) => (toolResult(s"bad arguments for show: $problem", isError = true), state)
      case Right(a) =>
        configured(a.root.getOrElse(state.defaultRoot), state) { config =>
          val (answer, roots) = Query.show(
            a.root.getOrElse(state.defaultRoot),
            new MemoLayout(MillLayout),
            config,
            state.roots,
            a.symbols,
            a.depth,
            a.body,
            a.withPrivate,
            cap,
            a.tests,
            a.fullDocs
          )
          (toolResult(answer.text, isError = false), state.copy(roots = roots))
        }
    }

  private def family(args: ujson.Value, state: State, cap: Int): (ujson.Value, State) =
    parseFamily(args) match {
      case Left(problem) =>
        (toolResult(s"bad arguments for family: $problem", isError = true), state)
      case Right(a) =>
        configured(a.root.getOrElse(state.defaultRoot), state) { config =>
          val (answer, roots) = Query.family(
            a.root.getOrElse(state.defaultRoot),
            new MemoLayout(MillLayout),
            config,
            state.roots,
            a.name,
            a.member,
            a.withBody,
            cap
          )
          (toolResult(answer.text, isError = false), state.copy(roots = roots))
        }
    }

  private final case class UsesArgs(
      symbols: Vector[String],
      in: Option[String],
      outside: Option[String],
      root: Option[Root],
      allSites: Boolean
  )

  private def uses(args: ujson.Value, state: State, cap: Int): (ujson.Value, State) =
    parseUses(args) match {
      case Left(problem) => (toolResult(s"bad arguments for uses: $problem", isError = true), state)
      case Right(a) =>
        configured(a.root.getOrElse(state.defaultRoot), state) { config =>
          val (answer, roots) = Query.uses(
            a.root.getOrElse(state.defaultRoot),
            new MemoLayout(MillLayout),
            config,
            state.roots,
            a.symbols,
            a.in,
            a.outside,
            cap,
            a.allSites
          )
          (toolResult(answer.text, isError = false), state.copy(roots = roots))
        }
    }

  private def parseUses(args: ujson.Value): Either[String, UsesArgs] =
    for {
      symbols <- stringList(args, "symbols", required = true)
      in <- optText(args, "in")
      outside <- optText(args, "outside")
      root <- rootArg(args)
      allSites <- flag(args, "sites")
    } yield UsesArgs(symbols, in, outside, root, allSites)

  private def parseShow(args: ujson.Value): Either[String, ShowArgs] =
    for {
      symbols <- stringList(args, "symbols", required = true)
      depth <- depthArg(args)
      body <- stringList(args, "body", required = false)
      withPrivate <- flag(args, "private")
      tests <- testsArg(args)
      fullDocs <- flag(args, "docs")
      root <- rootArg(args)
    } yield ShowArgs(symbols, depth, body.toSet, withPrivate, root, tests, fullDocs)

  /** The `tests` argument of show: "summary" (the default), "list" or "none". */
  private def testsArg(args: ujson.Value): Either[String, TestsShown] =
    field(args, "tests") match {
      case None => Right(TestsShown.Summary)
      case Some(v) =>
        v.strOpt match {
          case Some("summary") => Right(TestsShown.Summary)
          case Some("list") => Right(TestsShown.Listed)
          case Some("none") => Right(TestsShown.Dropped)
          case _ => Left("tests must be one of summary, list or none")
        }
    }

  private def parseFamily(args: ujson.Value): Either[String, FamilyArgs] =
    for {
      name <- reqText(args, "trait")
      member <- optText(args, "member")
      withBody <- flag(args, "body")
      root <- rootArg(args)
    } yield FamilyArgs(name, member, withBody, root)

  private final case class TestsArgs(
      suite: String,
      test: Option[String],
      body: Set[String],
      fixtures: Boolean,
      root: Option[Root]
  )

  private def tests(args: ujson.Value, state: State, cap: Int): (ujson.Value, State) =
    parseTests(args) match {
      case Left(problem) =>
        (toolResult(s"bad arguments for tests: $problem", isError = true), state)
      case Right(t) =>
        configured(t.root.getOrElse(state.defaultRoot), state) { config =>
          val (answer, roots) = Query.tests(
            t.root.getOrElse(state.defaultRoot),
            new MemoLayout(MillLayout),
            config,
            state.roots,
            t.suite,
            t.test,
            t.body,
            cap,
            t.fixtures
          )
          (toolResult(answer.text, isError = false), state.copy(roots = roots))
        }
    }

  private def parseTests(args: ujson.Value): Either[String, TestsArgs] =
    for {
      suite <- optText(args, "suite").flatMap(_.toRight("suite is required"))
      test <- optText(args, "test")
      body <- stringList(args, "body", required = false)
      fixtures <- flag(args, "fixtures")
      root <- rootArg(args)
    } yield TestsArgs(suite, test, body.toSet, fixtures, root)

  private final case class AreaArgs(
      names: Vector[String],
      level: Int,
      root: Option[Root]
  )

  private def area(args: ujson.Value, state: State, cap: Int): (ujson.Value, State) =
    parseArea(args) match {
      case Left(problem) => (toolResult(s"bad arguments for area: $problem", isError = true), state)
      case Right(a) =>
        configured(a.root.getOrElse(state.defaultRoot), state) { config =>
          val (answer, roots) = Query.area(
            a.root.getOrElse(state.defaultRoot),
            new MemoLayout(MillLayout),
            config,
            state.roots,
            a.names,
            a.level,
            cap
          )
          (toolResult(answer.text, isError = false), state.copy(roots = roots))
        }
    }

  private def parseArea(args: ujson.Value): Either[String, AreaArgs] =
    for {
      names <- stringList(args, "names", required = false)
      level <- levelArg(args)
      root <- rootArg(args)
    } yield AreaArgs(names, level, root)

  private def find(args: ujson.Value, state: State, cap: Int): (ujson.Value, State) =
    (for {
      pattern <- reqText(args, "pattern")
      withPrivate <- flag(args, "private")
      root <- rootArg(args)
    } yield (pattern, withPrivate, root.getOrElse(state.defaultRoot))) match {
      case Left(problem) => (toolResult(s"bad arguments for find: $problem", isError = true), state)
      case Right((pattern, withPrivate, root)) =>
        configured(root, state) { config =>
          val (answer, roots) =
            Query.find(
              root,
              new MemoLayout(MillLayout),
              config,
              state.roots,
              pattern,
              withPrivate,
              cap
            )
          (toolResult(answer.text, isError = false), state.copy(roots = roots))
        }
    }

  private def file(args: ujson.Value, state: State, cap: Int): (ujson.Value, State) =
    (for {
      target <- reqText(args, "target")
      root <- rootArg(args)
    } yield (target, root.getOrElse(state.defaultRoot))) match {
      case Left(problem) => (toolResult(s"bad arguments for file: $problem", isError = true), state)
      case Right((target, root)) =>
        configured(root, state) { config =>
          val (answer, roots) =
            Query.file(root, new MemoLayout(MillLayout), config, state.roots, target, cap)
          (toolResult(answer.text, isError = false), state.copy(roots = roots))
        }
    }

  private def levelArg(args: ujson.Value): Either[String, Int] =
    field(args, "level") match {
      case None => Right(1)
      case Some(v) =>
        v.numOpt
          .filter(n => n == n.floor)
          .map(_.toInt)
          .toRight("level must be an integer")
    }

  private def stringList(
      args: ujson.Value,
      key: String,
      required: Boolean
  ): Either[String, Vector[String]] =
    field(args, key) match {
      case None if required => Left(s"$key is required")
      case None => Right(Vector.empty)
      case Some(v) =>
        v.arrOpt match {
          case Some(items) =>
            val strings = items.flatMap(_.strOpt).toVector
            if (strings.size == items.size) Right(strings)
            else Left(s"$key must be an array of strings")
          case None => Left(s"$key must be an array of strings")
        }
    }

  /** The `max_kb` argument in bytes, `Help.defaultMaxKb` KB when absent. */
  private def maxKbArg(args: ujson.Value): Either[String, Int] =
    field(args, "max_kb") match {
      case None => Right(Help.defaultMaxKb * 1024)
      case Some(v) =>
        v.numOpt
          .filter(n => n == n.floor && n >= 1)
          .map(n => n.toInt * 1024)
          .toRight("max_kb must be a whole number from 1")
    }

  private def depthArg(args: ujson.Value): Either[String, Int] =
    field(args, "depth") match {
      case None => Right(1)
      case Some(v) =>
        v.numOpt
          .filter(n => n == n.floor && n >= 0 && n <= 2)
          .map(_.toInt)
          .toRight(
            "depth must be an integer from 0 to 2"
          )
    }

  private def flag(args: ujson.Value, key: String): Either[String, Boolean] =
    flagOr(args, key, default = false)

  /** The boolean `key` of the arguments, `default` when absent. */
  private def flagOr(args: ujson.Value, key: String, default: Boolean): Either[String, Boolean] =
    field(args, key) match {
      case None => Right(default)
      case Some(v) => v.boolOpt.toRight(s"$key must be a boolean")
    }

  private def reqText(args: ujson.Value, key: String): Either[String, String] =
    field(args, key) match {
      case None => Left(s"$key is required")
      case Some(v) => v.strOpt.toRight(s"$key must be a string")
    }

  private def optText(args: ujson.Value, key: String): Either[String, Option[String]] =
    field(args, key) match {
      case None => Right(None)
      case Some(v) => v.strOpt.toRight(s"$key must be a string").map(Some(_))
    }

  private def rootArg(args: ujson.Value): Either[String, Option[Root]] =
    optText(args, "root").map(_.map(p => Root(os.Path(p, os.pwd))))

  private def field(v: ujson.Value, key: String): Option[ujson.Value] =
    v.objOpt.flatMap(_.collectFirst { case (k, x) if k == key => x })

  private def toolResult(text: String, isError: Boolean): ujson.Value =
    ujson.Obj(
      "content" -> ujson.Arr(ujson.Obj("type" -> "text", "text" -> text)),
      "isError" -> isError
    )
}
