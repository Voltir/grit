package grit.mcp.wire

import grit.core.host.Clipped

import utest.*

/** [[Answer.of]]: a call's result as the text the model reads. */
object AnswerTests extends TestSuite {

  private def result(content: ujson.Value*): ujson.Obj =
    ujson.Obj("resultType" -> "complete", "content" -> ujson.Arr.from(content))

  private def text(t: String): ujson.Obj = ujson.Obj("type" -> "text", "text" -> t)

  val tests = Tests {
    test("an isError result is the tool's error, its text the model reads") {
      // server/tools.mdx, Error Handling: a tool execution error.
      val failed =
        result(text("Invalid departure date: must be in the future. Current date is 08/08/2025."))
      failed("isError") = true
      Answer.of(failed).map(a => (a.text, a.isError)) ==>
        Right(("Invalid departure date: must be in the future. Current date is 08/08/2025.", true))
      Answer.of(result(text("ok"))).map(a => (a.text, a.isError)) ==> Right(("ok", false))
      val fine = result(text("ok"))
      fine("isError") = false
      Answer.of(fine).map(a => (a.text, a.isError)) ==> Right(("ok", false))
    }

    test(
      "each content block is a line: text and embedded text as they are, links named, the rest omitted"
    ) {
      // server/tools.mdx, Tool Result: each content type's example.
      val answer = Answer.of(
        result(
          text("Tool result text"),
          ujson.Obj("type" -> "image", "data" -> "base64-encoded-data", "mimeType" -> "image/png"),
          ujson.Obj(
            "type" -> "audio",
            "data" -> "base64-encoded-audio-data",
            "mimeType" -> "audio/wav"
          ),
          ujson.Obj(
            "type" -> "resource_link",
            "uri" -> "file:///project/src/main.rs",
            "name" -> "main.rs",
            "description" -> "Primary application entry point",
            "mimeType" -> "text/x-rust"
          ),
          ujson.Obj(
            "type" -> "resource",
            "resource" -> ujson.Obj(
              "uri" -> "file:///project/src/main.rs",
              "mimeType" -> "text/x-rust",
              "text" -> "fn main() {\n    println!(\"Hello world!\");\n}"
            )
          ),
          ujson.Obj(
            "type" -> "resource",
            "resource" -> ujson
              .Obj("uri" -> "file:///logo.png", "mimeType" -> "image/png", "blob" -> "AAAA")
          ),
          ujson.Obj("type" -> "image", "data" -> "AAAA"),
          ujson.Obj("type" -> "hologram")
        )
      )
      answer.map(_.text) ==> Right(
        Vector(
          "Tool result text",
          "[image/png omitted]",
          "[audio/wav omitted]",
          "[resource main.rs: file:///project/src/main.rs]",
          "fn main() {\n    println!(\"Hello world!\");\n}",
          "[image/png file:///logo.png omitted]",
          "[image omitted]",
          "[hologram omitted]"
        ).mkString("\n")
      )
    }

    test("structured content is shown as JSON only when no block is text") {
      // server/tools.mdx, Structured Content: a tool SHOULD also send it as text.
      val structured = ujson.Obj("temperature" -> 22.5, "conditions" -> "Partly cloudy")
      val alone = result(ujson.Obj("type" -> "image", "data" -> "AAAA", "mimeType" -> "image/png"))
      alone("structuredContent") = structured
      val beside = result(text("""{"temperature": 22.5, "conditions": "Partly cloudy"}"""))
      beside("structuredContent") = structured
      Vector(alone, beside).map(Answer.of(_).map(_.text)) ==> Vector(
        Right("[image/png omitted]\n{\"temperature\":22.5,\"conditions\":\"Partly cloudy\"}"),
        Right("""{"temperature": 22.5, "conditions": "Partly cloudy"}""")
      )
    }

    test("a long answer is cut to its head, saying how much was kept") {
      val lines = (1 to Clipped.MaxLines + 5).map(n => s"line $n")
      Answer.of(result(text(lines.mkString("\n")))).map(_.text) ==> Right(
        lines.take(Clipped.MaxLines).mkString("\n") +
          s"\n\n[The answer was cut to its first ${Clipped.MaxLines} of ${Clipped.MaxLines + 5} lines.]"
      )
    }

    test("a result with no content array, or an isError that is not a boolean, is unreadable") {
      val odd = result(text("x"))
      odd("isError") = "yes"
      Vector(ujson.Obj("resultType" -> "complete"), odd).map(Answer.of) ==> Vector(
        Left(McpError.Unreadable("a tools/call result has no content array")),
        Left(McpError.Unreadable("a tools/call result's isError is not a boolean"))
      )
    }
  }
}
