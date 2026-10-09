package grit.outline.mcp

import scala.util.Try

import grit.outline.locate.{MillLayout, Root}
import grit.outline.query.{Query, Roots}

/** The server between calls: each root's cache, and the root a call without one uses. */
final case class State(roots: Roots, defaultRoot: Root)

object Server {

  private val versions = Vector("2025-11-25", "2025-06-18", "2025-03-26", "2024-11-05")
  private val latest = "2025-11-25"
  private val cap = 80000
  private val cachedFiles = 6000

  private val showTool: ujson.Value = ujson.Obj(
    "name" -> "show",
    "description" -> "Signatures with their full Scaladoc for named symbols (Name, Name.member or fully qualified), grouped by source file with line ranges that start at the Scaladoc, plus one-line outlines of the project types their signatures name (depth 1, at most 2); bodies only for members named in `body`. Read from compiled TASTy under `root` (a checkout or worktree; default the server's).",
    "inputSchema" -> ujson.Obj(
      "type" -> "object",
      "properties" -> ujson.Obj(
        "symbols" -> ujson.Obj(
          "type" -> "array",
          "items" -> ujson.Obj("type" -> "string"),
          "description" -> "Symbols to show: Name, Name.member or fully qualified."
        ),
        "depth" -> ujson.Obj(
          "type" -> "integer",
          "minimum" -> 0,
          "maximum" -> 2,
          "default" -> 1,
          "description" -> "How many levels of project types to outline."
        ),
        "body" -> ujson.Obj(
          "type" -> "array",
          "items" -> ujson.Obj("type" -> "string"),
          "description" -> "Member names whose bodies to include."
        ),
        "private" -> ujson.Obj("type" -> "boolean", "description" -> "Include private members."),
        "root" -> ujson.Obj("type" -> "string", "description" -> "Checkout or worktree to read.")
      ),
      "required" -> ujson.Arr("symbols"),
      "additionalProperties" -> false
    )
  )

  private val familyTool: ujson.Value = ujson.Obj(
    "name" -> "family",
    "description" -> "A trait or abstract class, its implementations, the abstract contracts whose members name it, and the suites extending each contract, grouped by file with line ranges; with `member`, only that member in each.",
    "inputSchema" -> ujson.Obj(
      "type" -> "object",
      "properties" -> ujson.Obj(
        "trait" -> ujson
          .Obj("type" -> "string", "description" -> "The trait or abstract class, by name."),
        "member" -> ujson.Obj("type" -> "string", "description" -> "Only this member in each."),
        "body" -> ujson.Obj("type" -> "boolean", "description" -> "Include the member's body."),
        "root" -> ujson.Obj("type" -> "string", "description" -> "Checkout or worktree to read.")
      ),
      "required" -> ujson.Arr("trait"),
      "additionalProperties" -> false
    )
  )

  /** The reply to one JSON-RPC line (none for a notification), and the state after it. */
  def handle(line: String, state: State): (Option[String], State) = {
    val (reply, next, _) = process(line, state)
    (reply, next)
  }

  /** Serves stdin to stdout, one request at a time, until stdin closes; logs to stderr only. */
  def serve(defaultRoot: Root): Unit = {
    val out = new java.io.PrintStream(
      new java.io.FileOutputStream(java.io.FileDescriptor.out),
      true,
      java.nio.charset.StandardCharsets.UTF_8
    )
    val in = new java.io.BufferedReader(
      new java.io.InputStreamReader(System.in, java.nio.charset.StandardCharsets.UTF_8)
    )
    val lines = LazyList.continually(Option(in.readLine())).takeWhile(_.isDefined).flatten
    lines.foldLeft(State(Roots.empty(cachedFiles), defaultRoot)) { (state, line) =>
      val started = System.nanoTime()
      val (reply, next, tool) = process(line, state)
      tool.foreach(name =>
        System.err.println(s"$name ${(System.nanoTime() - started) / 1000000} ms")
      )
      reply.foreach { r =>
        out.print(r + "\n")
        out.flush()
      }
      next
    }
    ()
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
                Right(ujson.Obj("tools" -> ujson.Arr(showTool, familyTool))),
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
    ujson.Obj(
      "protocolVersion" -> requested,
      "capabilities" -> ujson.Obj("tools" -> ujson.Obj("listChanged" -> false)),
      "serverInfo" -> ujson.Obj("name" -> "outline", "version" -> "0.1.0")
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
        name match {
          case "show" =>
            val (result, next) = show(args, state)
            (Right(result), next, Some(name))
          case "family" =>
            val (result, next) = family(args, state)
            (Right(result), next, Some(name))
          case other => (Left((-32602, s"unknown tool: $other")), state, Some(other))
        }
    }

  private final case class ShowArgs(
      symbols: Vector[String],
      depth: Int,
      body: Set[String],
      withPrivate: Boolean,
      root: Option[Root]
  )

  private final case class FamilyArgs(
      name: String,
      member: Option[String],
      withBody: Boolean,
      root: Option[Root]
  )

  private def show(args: ujson.Value, state: State): (ujson.Value, State) =
    parseShow(args) match {
      case Left(problem) => (toolResult(s"bad arguments for show: $problem", isError = true), state)
      case Right(a) =>
        val (answer, roots) = Query.show(
          a.root.getOrElse(state.defaultRoot),
          MillLayout,
          state.roots,
          a.symbols,
          a.depth,
          a.body,
          a.withPrivate,
          cap
        )
        (toolResult(answer.text, isError = false), state.copy(roots = roots))
    }

  private def family(args: ujson.Value, state: State): (ujson.Value, State) =
    parseFamily(args) match {
      case Left(problem) =>
        (toolResult(s"bad arguments for family: $problem", isError = true), state)
      case Right(a) =>
        val (answer, roots) = Query.family(
          a.root.getOrElse(state.defaultRoot),
          MillLayout,
          state.roots,
          a.name,
          a.member,
          a.withBody,
          cap
        )
        (toolResult(answer.text, isError = false), state.copy(roots = roots))
    }

  private def parseShow(args: ujson.Value): Either[String, ShowArgs] =
    for {
      symbols <- stringList(args, "symbols", required = true)
      depth <- depthArg(args)
      body <- stringList(args, "body", required = false)
      withPrivate <- flag(args, "private")
      root <- rootArg(args)
    } yield ShowArgs(symbols, depth, body.toSet, withPrivate, root)

  private def parseFamily(args: ujson.Value): Either[String, FamilyArgs] =
    for {
      name <- reqText(args, "trait")
      member <- optText(args, "member")
      withBody <- flag(args, "body")
      root <- rootArg(args)
    } yield FamilyArgs(name, member, withBody, root)

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
    field(args, key) match {
      case None => Right(false)
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
