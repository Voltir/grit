package grit.mcp.wire

import grit.edge.http.Response

/** What a server says of itself at `server/discover` and `initialize`: its `name` and
  * `version`, and `instructions` for the model, if any.
  */
final case class ServerInfo(name: String, version: String, instructions: Option[String])

/** What a POST to an MCP endpoint, read by [[Rpc.received]], leaves its server to do. */
sealed trait Posted

object Posted {

  /** Nothing more: send `response`. */
  final case class Answered(response: Response) extends Posted

  /** `tools/list` from `cursor`, `None` for the first page. */
  final case class List private[wire] (
      cursor: Option[String],
      private[wire] val id: ujson.Value,
      private[wire] val revision: Revision
  ) extends Posted {

    /** The answer to this `tools/list`: `Tools` a page, in 2026-07-28 with `ttlMs: 0` and a
      * private `cacheScope`; `NoSuchCursor` error -32602.
      */
    def answered(listing: Listing): Response = listing match {
      case Listing.Tools(tools, next) =>
        val result = ujson.Obj("tools" -> ujson.Arr.from(tools))
        next.foreach(c => result("nextCursor") = c)
        if (revision == Revision.Jul2026) {
          result("ttlMs") = 0
          result("cacheScope") = "private"
        }
        Rpc.answer(id, revision, result)
      case Listing.NoSuchCursor =>
        Rpc.refused(
          Rpc.Refusal(id, Code.InvalidParams, "no page of tools has that cursor"),
          Rpc.invalidStatus(revision)
        )
    }
  }

  /** `tools/call` of `tool`, as the client named it, with `arguments`, `{}` when it sent none. */
  final case class Call private[wire] (
      tool: String,
      arguments: ujson.Obj,
      private[wire] val id: ujson.Value,
      private[wire] val revision: Revision
  ) extends Posted {

    /** The answer to this `tools/call`: `Done` a text result, `Failed` one with `isError: true`;
      * `BadArguments` that error result, saying `why`, from 2025-11-25 and error -32602 in
      * 2025-06-18; `NoSuchTool` error -32602.
      */
    def answered(result: CallResult): Response = {
      def text(said: String, isError: Boolean) =
        Rpc.answer(
          id,
          revision,
          ujson.Obj(
            "content" -> ujson.Arr(ujson.Obj("type" -> "text", "text" -> said)),
            "isError" -> isError
          )
        )
      def invalid(why: String) =
        Rpc.refused(Rpc.Refusal(id, Code.InvalidParams, why), Rpc.invalidStatus(revision))
      result match {
        case CallResult.Done(said) => text(said, false)
        case CallResult.Failed(said) => text(said, true)
        case CallResult.BadArguments(why) if revision == Revision.Jun2025 => invalid(why)
        case CallResult.BadArguments(why) => text(why, true)
        case CallResult.NoSuchTool => invalid(s"no tool is named $tool")
      }
    }
  }
}

/** A `tools/list` answer: a page of `tools`, each [[McpTool.described]] or as another server
  * lists one, and `next`, the next page's cursor, `None` on the last; or a cursor this server
  * never gave.
  */
enum Listing {
  case Tools(tools: Vector[ujson.Obj], next: Option[String])
  case NoSuchCursor
}

/** What a `tools/call` came to: the tool's text, `Done`, or `Failed` for its own failure;
  * arguments the tool could not read, `why`; or a tool this server does not offer.
  */
enum CallResult {
  case Done(text: String)
  case Failed(text: String)
  case BadArguments(why: String)
  case NoSuchTool
}
