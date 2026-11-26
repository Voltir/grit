package grit.models

import grit.core.message.StopReason

import utest.*

/** A tool call's arguments as a provider sent them, read in stages before they are refused. */
object ToolArgumentsTests extends TestSuite {

  private def read(raw: String, stop: StopReason): ujson.Value =
    ToolArguments.read(Some(ujson.Str(raw)), stop)

  val tests = Tests {
    test("a trailing comma before a close is dropped, however the reply stopped") {
      val sent = """{"path":"a.txt","lines":[1,2,],}"""
      (read(sent, StopReason.ToolUse), read(sent, StopReason.MaxTokens)) ==> (
        ujson.Obj("path" -> "a.txt", "lines" -> ujson.Arr(1, 2)),
        ujson.Obj("path" -> "a.txt", "lines" -> ujson.Arr(1, 2))
      )
    }

    test("brackets left open are closed only when the reply stopped to call its tools") {
      val sent = """{"edits":[{"old":"a","new":"b"},"""
      (
        read(sent, StopReason.ToolUse),
        read(sent, StopReason.MaxTokens),
        read(sent, StopReason.Other(""))
      ) ==> (
        ujson.Obj("edits" -> ujson.Arr(ujson.Obj("old" -> "a", "new" -> "b"))),
        ujson.Str(sent),
        ujson.Str(sent)
      )
    }

    test("what no repair makes an object is kept whole as sent") {
      val unrepairable = Vector(
        """{"a":""",
        """{"a":1,"b"""",
        """{"text":"cut in the middle of a str""",
        """[1,2""",
        """{"a":1]"""
      )
      unrepairable.map(read(_, StopReason.ToolUse)) ==> unrepairable.map(ujson.Str(_))
    }

    test("a comma inside a string is the string's") {
      read("""{"text":"a,}","n":1""", StopReason.ToolUse) ==>
        ujson.Obj("text" -> "a,}", "n" -> 1)
    }
  }
}
