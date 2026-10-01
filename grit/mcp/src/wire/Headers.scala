package grit.mcp.wire

import java.nio.charset.StandardCharsets.UTF_8
import java.util.Base64

/** The headers a request carries besides `Authorization`. */
object Headers {

  /** `raw` as a header value: as it is when every character is visible ASCII, a space or a
    * tab, it neither starts nor ends with a space or a tab, and it is not `=?base64?…?=`;
    * else that shape around the Base64 of its UTF-8.
    */
  def encoded(raw: String): String = {
    def visible(c: Char) = c >= 0x21 && c <= 0x7e
    def blank(c: Char) = c == ' ' || c == '\t'
    val plain = raw.forall(c => visible(c) || blank(c)) &&
      !raw.headOption.exists(blank) && !raw.lastOption.exists(blank) &&
      !(raw.startsWith(Open) && raw.endsWith(Close) && raw.length >= Open.length + Close.length)
    if (plain) raw
    else s"$Open${Base64.getEncoder.encodeToString(raw.getBytes(UTF_8))}$Close"
  }

  private val Open = "=?base64?"
  private val Close = "?="

  /** The headers of a request making `call`: the content types grit sends and reads, the
    * protocol version ([[Rpc.Version]]), its method, and for a `tools/call` its tool's name as
    * the server lists it and an `Mcp-Param-{header}` for each argument it mirrors
    * ([[McpTool.params]]), each value [[encoded]]. An argument is mirrored as text: a string as
    * it is, a boolean as `true` or `false`, an integer within ±(2^53^−1) in decimal. One that
    * is absent or null is not mirrored, as the spec says; nor is one of any other kind, which
    * breaks the tool's schema and which the server refuses.
    */
  def of(call: Rpc.Call): Vector[(String, String)] = {
    val named = call match {
      case Rpc.Call.ListTools(_) => Vector.empty
      case Rpc.Call.CallTool(tool, arguments) =>
        ("Mcp-Name" -> encoded(tool.name)) +: tool.params.flatMap(p =>
          at(arguments, p.path).flatMap(text).map(v => s"Mcp-Param-${p.header}" -> encoded(v))
        )
    }
    Vector(
      "Accept" -> "application/json, text/event-stream",
      "Content-Type" -> "application/json",
      "MCP-Protocol-Version" -> Rpc.Version,
      "Mcp-Method" -> call.method
    ) ++ named
  }

  /** The value at `path` in `arguments`, each step an object's key. */
  private def at(arguments: ujson.Value, path: Vector[String]): Option[ujson.Value] =
    path.foldLeft(Option(arguments))((v, key) => v.flatMap(_.objOpt).flatMap(_.get(key)))

  /** The largest integer a double holds exactly, 2^53^−1: the spec's safe range. */
  private val Safe = 9007199254740991L.toDouble

  /** An argument as a header's text, when it is a string, a boolean or a safe integer. */
  private def text(v: ujson.Value): Option[String] = v match {
    case ujson.Str(s) => Some(s)
    case ujson.Bool(b) => Some(b.toString)
    case ujson.Num(n) if n.isWhole && n.abs <= Safe => Some(n.toLong.toString)
    case _ => None
  }
}
