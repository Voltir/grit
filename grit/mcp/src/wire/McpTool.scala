package grit.mcp.wire

import grit.core.tool.ToolName

/** A tool a server listed that grit may offer: marked read-only (`readOnlyHint: true`), its
  * `x-mcp-header` annotations within the spec's constraints, and offered as `offered`, its
  * name under the server's prefix. `name` is the server's own; `does`, its title (the tool's,
  * else its annotations') and description as "Title: description", or whichever of them it
  * has; `inputSchema`, its schema as listed; `params`, the arguments a call mirrors into
  * headers, in the order the schema lists them.
  */
final case class McpTool private (
    name: String,
    offered: ToolName,
    does: String,
    inputSchema: ujson.Obj,
    params: Vector[McpTool.Param]
)

object McpTool {

  /** An argument a call mirrors into a header: the one at `path`, a chain of `properties` keys
    * from the schema's root, sent as `Mcp-Param-{header}`.
    */
  final case class Param private[wire] (path: Vector[String], header: String)

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
      params <- mirrored(ujson.Obj.from(schema)).left.map(why => Skipped.BadHeader(name, why))
      offered <- ToolName.of(s"${prefix}_$name").left.map(why => Skipped.BadName(name, why))
    } yield new McpTool(
      name,
      offered,
      Vector(title.orElse(hinted), description).flatten.mkString(": "),
      ujson.Obj.from(schema),
      params
    )

  /** The arguments `schema` annotates with `x-mcp-header`, or why an annotation breaks the
    * spec's constraints (`streamable-http.mdx` §Schema Extension), naming the first that does.
    */
  private def mirrored(schema: ujson.Obj): Either[String, Vector[Param]] = {
    val found = annotations(schema, Vector.empty, Some(Vector.empty))
    found.collectFirst { case Left(why) => why } match {
      case Some(why) => Left(why)
      case None =>
        val params = found.collect { case Right(p) => p }
        params.zipWithIndex
          .collectFirst(Function.unlift { (p, i) =>
            params
              .take(i)
              .find(_.header.equalsIgnoreCase(p.header))
              .map(first =>
                s"the x-mcp-header at ${pointer(p.path)}, ${p.header}, repeats ${first.header}, case aside"
              )
          })
          .toLeft(params)
    }
  }

  /** Every `x-mcp-header` in `v`, a schema at JSON pointer `at`: the argument each annotates,
    * or why it may not. `property` is the chain of `properties` keys that reached `v`, `None`
    * once anything else has.
    */
  private def annotations(
      v: ujson.Value,
      at: Vector[String],
      property: Option[Vector[String]]
  ): Vector[Either[String, Param]] = v match {
    case ujson.Obj(fields) =>
      val own = fields.get("x-mcp-header").toVector.map { header =>
        val where = s"the x-mcp-header at ${render(at)}"
        for {
          path <- property
            .filter(_.nonEmpty)
            .toRight(s"$where is not on a property reached through properties alone")
          name <- header.strOpt
            .filter(h => h.nonEmpty && h.forall(token))
            .toRight(s"$where is not a header name: ${ujson.write(header)}")
          _ <- fields.get("type") match {
            case Some(ujson.Str("string" | "integer" | "boolean")) => Right(())
            case kind =>
              Left(
                s"$where is on a property of ${kind.fold("no type")(k => s"type ${ujson.write(k)}")}, " +
                  "not string, integer or boolean"
              )
          }
        } yield Param(path, name)
      }
      own ++ fields.toVector.flatMap {
        case ("x-mcp-header" | "enum" | "const" | "default" | "examples", _) => Vector.empty
        case ("properties", ujson.Obj(named)) =>
          named.toVector.flatMap((name, inner) =>
            annotations(inner, at :+ "properties" :+ name, property.map(_ :+ name))
          )
        case (key, inner) => annotations(inner, at :+ key, None)
      }
    case ujson.Arr(items) =>
      items.toVector.zipWithIndex.flatMap((inner, i) => annotations(inner, at :+ i.toString, None))
    case _ => Vector.empty
  }

  /** A property's chain of `properties` keys as a JSON pointer into the schema. */
  private def pointer(path: Vector[String]): String = render(path.flatMap(Vector("properties", _)))

  /** `at` as a JSON pointer: `/` for the root. */
  private def render(at: Vector[String]): String =
    if (at.isEmpty) "/" else at.map(_.replace("~", "~0").replace("/", "~1")).mkString("/", "/", "")

  /** Whether `c` is an HTTP token character (RFC 9110 §5.6.2, `tchar`). */
  private def token(c: Char): Boolean =
    (c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') ||
      "!#$%&'*+-.^_`|~".contains(c)

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

  /** `tool`'s `x-mcp-header` annotations break the spec's constraints (`streamable-http.mdx`
    * §Schema Extension), so a client may not offer it: `why`, naming the first that does.
    */
  case BadHeader(tool: String, why: String)

  /** An entry of the list that is not a tool: `why`. */
  case Malformed(why: String)

  /** `tool` is not one the server's allowlist names. */
  case NotAllowed(tool: String)

  /** `tool` is not one the server's scope can hold to its bounds (`grit.mcp.scope.McpScope`). */
  case OutOfScope(tool: String)

  /** A line for the log. */
  def message: String = this match {
    case NotReadOnly(tool) => s"$tool is not offered: it is not marked read-only"
    case BadName(tool, why) => s"$tool is not offered: $why"
    case BadHeader(tool, why) =>
      s"$tool is not offered: its x-mcp-header annotations break the spec: $why"
    case Malformed(why) => s"a listed tool is not offered: $why"
    case NotAllowed(tool) => s"$tool is not offered: the server's allowlist does not name it"
    case OutOfScope(tool) =>
      s"$tool is not offered: its server's scope cannot hold it to its bounds"
  }
}
