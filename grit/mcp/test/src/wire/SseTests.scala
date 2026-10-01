package grit.mcp.wire

import utest.*

/** [[Sse.response]]: the JSON-RPC response to one request, read from its event stream. */
object SseTests extends TestSuite {

  private val progress =
    """{"jsonrpc":"2.0","method":"notifications/progress","params":{"progressToken":1,"progress":50}}"""
  private val answer = """{"jsonrpc":"2.0","id":7,"result":{"resultType":"complete","tools":[]}}"""

  val tests = Tests {
    test(
      "notifications and comments before the response are passed over, and nothing after it is read"
    ) {
      // streamable-http.mdx, Receiving Messages: notifications may precede the response, which
      // SHOULD end the stream; a line beginning with a colon is a comment.
      val lines = Iterator(
        ": keep-alive",
        "event: message",
        s"data: $progress",
        "",
        "id: 2",
        s"data: $answer",
        "",
        "data: after",
        ""
      )
      Sse.response(lines, 7) ==> Right(ujson.read(answer))
      lines.toVector ==> Vector("data: after", "")
    }

    test("an event's data lines are joined by newlines, one space after the colon dropped") {
      val lines = Iterator(
        """data:{"jsonrpc":"2.0",""",
        """data:  "id":7,""",
        """data: "result":{"text":"a"}}""",
        ""
      )
      Sse.response(lines, 7) ==> Right(
        ujson.read("{\"jsonrpc\":\"2.0\",\n \"id\":7,\n\"result\":{\"text\":\"a\"}}")
      )
    }

    test("lines ending in a carriage return read as the same lines") {
      Sse.response(Iterator(s"data: $answer\r", "\r"), 7) ==> Right(ujson.read(answer))
    }

    test("a response to another request is passed over; an error with no id is this request's") {
      val other = """{"jsonrpc":"2.0","id":3,"result":{}}"""
      val error = """{"jsonrpc":"2.0","error":{"code":-32700,"message":"Parse error"}}"""
      Sse.response(Iterator(s"data: $other", "", s"data: $answer", ""), 7) ==> Right(
        ujson.read(answer)
      )
      Sse.response(Iterator(s"data: $error", ""), 7) ==> Right(ujson.read(error))
    }

    test("a stream that ends before the response, or mid-event, came to nothing") {
      val ended = Left(McpError.Unreachable("the event stream ended before the response"))
      Sse.response(Iterator(s"data: $progress", ""), 7) ==> ended
      Sse.response(Iterator(s"data: $answer"), 7) ==> ended
      Sse.response(Iterator.empty, 7) ==> ended
    }

    test("an event whose data is not JSON is unreadable") {
      Sse.response(Iterator("data: {\"jsonrpc\":", ""), 7) ==>
        Left(McpError.Unreadable("an event's data is not JSON: {\"jsonrpc\":"))
    }
  }
}
