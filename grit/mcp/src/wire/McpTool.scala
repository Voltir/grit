package grit.mcp.wire

import grit.core.tool.ToolName

/** A tool a server listed that grit may offer: marked read-only (`readOnlyHint: true`), with
  * no argument mirrored into a header (`x-mcp-header`), and offered as `offered`, its name
  * under the server's prefix. `name` is the server's own; `does`, its title (the tool's, else
  * its annotations') and description as "Title: description", or whichever of them it has;
  * `inputSchema`, its schema as listed.
  */
final case class McpTool private (
    name: String,
    offered: ToolName,
    does: String,
    inputSchema: ujson.Obj
)

object McpTool {

  /** One page of a `tools/list` result: the tools grit may offer, those skipped and why, in
    * the order listed; `next`, the cursor of the next page, `None` on the last (an empty
    * string is a cursor); `ttlMs`, how long the list stays fresh, never negative.
    */
  final case class Page private[wire] (
      tools: Vector[McpTool],
      skipped: Vector[Skipped],
      next: Option[String],
      ttlMs: Long
  )

  /** `result`, a `tools/list` page from a server whose tools are offered as `{prefix}_{name}`,
    * an absent or negative `ttlMs` read as 0; [[McpError.Unreadable]] when it has no `tools`
    * array, or a `nextCursor` that is not a string or a `ttlMs` that is not a whole number.
    */
  def page(result: ujson.Obj, prefix: String): Either[McpError, Page] = {
    val o = result.obj
    for {
      listed <- o
        .get("tools")
        .flatMap(_.arrOpt)
        .toRight(McpError.Unreadable("a tools/list result has no tools array"))
      next <- o.get("nextCursor") match {
        case None | Some(ujson.Null) => Right(None)
        case Some(ujson.Str(c)) => Right(Some(c))
        case Some(_) =>
          Left(McpError.Unreadable("a tools/list result's nextCursor is not a string"))
      }
      ttl <- o.get("ttlMs") match {
        case None | Some(ujson.Null) => Right(0L)
        case Some(ujson.Num(n)) if n.isWhole => Right(n.toLong.max(0L))
        case Some(_) =>
          Left(McpError.Unreadable("a tools/list result's ttlMs is not a whole number"))
      }
    } yield {
      val read = listed.toVector.map(tool(_, prefix))
      Page(read.collect { case Right(t) => t }, read.collect { case Left(s) => s }, next, ttl)
    }
  }

  private def tool(entry: ujson.Value, prefix: String): Either[Skipped, McpTool] =
    for {
      o <- entry.objOpt.toRight(Skipped.Malformed("a listed tool is not an object"))
      name <- o
        .get("name")
        .flatMap(_.strOpt)
        .toRight(Skipped.Malformed("a listed tool has no name"))
      schema <- o
        .get("inputSchema")
        .flatMap(_.objOpt)
        .toRight(Skipped.Malformed(s"$name's inputSchema is not an object"))
      description <- text(o.get("description"), s"$name's description")
      title <- text(o.get("title"), s"$name's title")
      annotations = o.get("annotations").flatMap(_.objOpt)
      hinted <- text(annotations.flatMap(_.get("title")), s"$name's annotations' title")
      _ <- Either.cond(
        annotations.flatMap(_.get("readOnlyHint")).contains(ujson.True),
        (),
        Skipped.NotReadOnly(name)
      )
      _ <- Either.cond(!mirrors(ujson.Obj.from(schema)), (), Skipped.HasHeaderParams(name))
      offered <- ToolName.of(s"${prefix}_$name").left.map(why => Skipped.BadName(name, why))
    } yield new McpTool(
      name,
      offered,
      Vector(title.orElse(hinted), description).flatten.mkString(": "),
      ujson.Obj.from(schema)
    )

  /** Whether `v` has an `x-mcp-header` key anywhere within it. */
  private def mirrors(v: ujson.Value): Boolean = v match {
    case ujson.Obj(fields) => fields.exists((k, inner) => k == "x-mcp-header" || mirrors(inner))
    case ujson.Arr(items) => items.exists(mirrors)
    case _ => false
  }

  /** `v` as text: `None` when absent or null, `Malformed` naming `what` when not a string. */
  private def text(v: Option[ujson.Value], what: String): Either[Skipped, Option[String]] =
    v match {
      case None | Some(ujson.Null) => Right(None)
      case Some(ujson.Str(s)) => Right(Some(s))
      case Some(_) => Left(Skipped.Malformed(s"$what is not a string"))
    }
}

/** Why a listed tool is not offered. */
enum Skipped {

  /** `tool` is not marked read-only: its `readOnlyHint` is absent or not `true`. */
  case NotReadOnly(tool: String)

  /** `tool`, under the server's prefix, is not a grit tool name: `why`. */
  case BadName(tool: String, why: String)

  /** `tool` mirrors an argument into a header (`x-mcp-header`), which grit does not send. */
  case HasHeaderParams(tool: String)

  /** An entry of the list that is not a tool: `why`. */
  case Malformed(why: String)

  /** `tool` is not one the server's allowlist names. */
  case NotAllowed(tool: String)

  /** A line for the log. */
  def message: String = this match {
    case NotReadOnly(tool) => s"$tool is not offered: it is not marked read-only"
    case BadName(tool, why) => s"$tool is not offered: $why"
    case HasHeaderParams(tool) =>
      s"$tool is not offered: it mirrors an argument into a header (x-mcp-header)"
    case Malformed(why) => s"a listed tool is not offered: $why"
    case NotAllowed(tool) => s"$tool is not offered: the server's allowlist does not name it"
  }
}
