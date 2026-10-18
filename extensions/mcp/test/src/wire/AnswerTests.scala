package grit.mcp.wire

import grit.core.host.Clipped
import grit.core.visibility.{Facts, Itemised}

import utest.*

/** [[Answer.of]]: a call's result as the text the model reads. */
object AnswerTests extends TestSuite {

  private def result(content: ujson.Value*): ujson.Obj =
    ujson.Obj("resultType" -> "complete", "content" -> ujson.Arr.from(content))

  private def text(t: String): ujson.Obj = ujson.Obj("type" -> "text", "text" -> t)

  /** Thirty objects of about 2 KB each, as a server lists them: together over
    * [[Clipped.MaxBytes]], each well under it.
    */
  private val thirty: Vector[ujson.Value] =
    (1 to 30).toVector.map(n => ujson.Obj("n" -> n, "body" -> ("x" * 2000)))

  private def bytes(s: String): Int = s.getBytes(java.nio.charset.StandardCharsets.UTF_8).length

  /** The most of `items`, from the first, whose compact JSON, each followed by a comma but
    * the last, fits in `room` bytes; computed from the items' own sizes.
    */
  private def most(items: Vector[ujson.Value], room: Int): Int =
    items
      .map(i => bytes(ujson.write(i)) + 1)
      .scanLeft(-1)(_ + _)
      .lastIndexWhere(_ <= room)

  private def showing(k: Int, n: Int): String =
    s"[Showing the first $k of the $n items in this answer; call again asking for fewer, or " +
      "for the next page, to see the rest.]"

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

    test(
      "an answer over the limit that is a JSON array, on one line or pretty-printed, shows its leading whole items, compact, and how many of how many"
    ) {
      // `[` and `]` around the items.
      val k = most(thirty, Clipped.MaxBytes - 2)
      val shown = ujson.write(ujson.Arr.from(thirty.take(k))) + "\n\n" + showing(k, 30)
      Vector(ujson.write(ujson.Arr.from(thirty)), ujson.write(ujson.Arr.from(thirty), indent = 2))
        .map(t => Answer.of(result(text(t))).map(_.text)) ==> Vector(Right(shown), Right(shown))
    }

    test(
      "an object holding one array, over the limit, keeps its other keys and its leading items; the blocks after it still fit"
    ) {
      // A search's shape, and the withheld note a server's scope adds after it.
      def page(items: Vector[ujson.Value]): ujson.Obj =
        ujson.Obj(
          "total_count" -> 30,
          "incomplete_results" -> false,
          "items" -> ujson.Arr.from(items)
        )
      val withheld = "Withheld as outside this server's scope (repo a/b): 2 of the 30 results."
      val k = most(
        thirty,
        Clipped.MaxBytes - bytes(ujson.write(page(Vector.empty))) - 1 - bytes(withheld)
      )
      Answer.of(result(text(ujson.write(page(thirty))), text(withheld))).map(_.text) ==> Right(
        ujson.write(page(thirty.take(k))) + "\n" + withheld + "\n\n" + showing(k, 30)
      )
    }

    test(
      "a result's facts are its structured content: whole when its text is, its list cut to " +
        "the items the text keeps when that is cut by items, and none when it is cut by lines"
    ) {
      def page(items: Vector[ujson.Value]): ujson.Obj =
        ujson.Obj("total_count" -> 30, "items" -> ujson.Arr.from(items))
      def beside(shown: ujson.Value, payload: ujson.Value): ujson.Obj = {
        val r = result(text(ujson.write(shown)))
        r("structuredContent") = payload
        r
      }
      def structured(payload: ujson.Value): ujson.Obj = beside(payload, payload)
      val k = most(thirty, Clipped.MaxBytes - bytes(ujson.write(page(Vector.empty))))
      val two = ujson.Obj("items" -> ujson.Arr.from(thirty), "more" -> ujson.Arr(1, 2))
      // Items small enough that all thirty fit as facts: only the text's cut holds them to k.
      val small = (1 to 30).toVector.map(n => ujson.Obj("n" -> n): ujson.Value)
      Vector(
        structured(page(thirty.take(2))),
        structured(page(thirty)),
        structured(two),
        result(text(ujson.write(page(thirty.take(2))))),
        beside(page(thirty), page(small)),
        beside(page(thirty), page(small.take(29)))
      ).map(Answer.of(_).map(_.facts)) ==> Vector(
        Right(Some(Facts.of(page(thirty.take(2))))),
        Right(Some(Facts.of(page(thirty.take(k))))),
        Right(None),
        Right(None),
        Right(Some(Facts.of(page(small.take(k))))),
        Right(None)
      )
    }

    test(
      "a whole text's facts are held to the text's limits: a list over them cut to its " +
        "leading items that fit, anything else over them none"
    ) {
      def page(items: Vector[ujson.Value]): ujson.Obj =
        ujson.Obj("total_count" -> 30, "items" -> ujson.Arr.from(items))
      def beside(payload: ujson.Value): ujson.Obj = {
        val r = result(text("Found 30 results."))
        r("structuredContent") = payload
        r
      }
      val k = most(thirty, Clipped.MaxBytes - bytes(ujson.write(page(Vector.empty))))
      val two = ujson.Obj("items" -> ujson.Arr.from(thirty), "more" -> ujson.Arr(1, 2))
      Vector(beside(page(thirty)), beside(two)).map(Answer.of(_).map(a => (a.text, a.facts))) ==>
        Vector(
          Right(("Found 30 results.", Some(Facts.of(page(thirty.take(k)))))),
          Right(("Found 30 results.", None))
        )
    }

    test("a cut answer's facts are those its server's extension gave beside the head") {
      val said = Facts.of(ujson.Obj("head" -> "rendered from this"))
      Answer
        .cut(result(text("x")), Itemised.Parts("head", Vector.empty, Some(said)))
        .map(_.facts) ==>
        Right(Some(said))
    }

    test("an object holding two arrays, over the limit, is cut by lines as any text is") {
      val two = ujson.Obj("items" -> ujson.Arr.from(thirty), "more" -> ujson.Arr(1, 2))
      Answer.of(result(text(ujson.write(two)))).map(_.text) ==> Right(
        s"[The answer's first line alone is over ${Clipped.MaxBytes} bytes, so none of it is shown.]"
      )
    }

    test("a result with no content array, or an isError that is not a boolean, is unreadable") {
      val odd = result(text("x"))
      odd("isError") = "yes"
      Vector(ujson.Obj("resultType" -> "complete"), odd).map(Answer.of(_)) ==> Vector(
        Left(McpError.Unreadable("a tools/call result has no content array")),
        Left(McpError.Unreadable("a tools/call result's isError is not a boolean"))
      )
    }
  }
}
