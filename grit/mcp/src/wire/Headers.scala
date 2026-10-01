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
    * protocol version ([[Rpc.Version]]), its method, and for a `tools/call` its tool's name
    * as the server lists it, [[encoded]].
    */
  def of(call: Rpc.Call): Vector[(String, String)] = {
    val name = call match {
      case Rpc.Call.ListTools(_) => None
      case Rpc.Call.CallTool(tool, _) => Some("Mcp-Name" -> encoded(tool.name))
    }
    Vector(
      "Accept" -> "application/json, text/event-stream",
      "Content-Type" -> "application/json",
      "MCP-Protocol-Version" -> Rpc.Version,
      "Mcp-Method" -> call.method
    ) ++ name
  }
}
