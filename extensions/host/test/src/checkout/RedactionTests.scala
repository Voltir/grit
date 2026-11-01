package grit.host.checkout

import grit.core.secret.Variable

import utest.*

/** [[Redaction]]: what a tool is shown in place of a secret's value. */
object RedactionTests extends TestSuite {

  private val redaction = Redaction.of(
    Map(
      "SLACK_BOT_TOKEN" -> "  xoxb-1234567890 ",
      "GITHUB_TOKEN" -> "xoxb-1234567890-and-more",
      "DEPLOY_KEY" -> "-----BEGIN KEY-----\nabcdefghijkl\nshort\n-----END KEY-----",
      "GRIT_PASSWORD" -> "hunter2",
      "EMPTY_TOKEN" -> "  ",
      "GRIT_WINDOW_SIZE" -> "1234567890"
    )
  )

  val tests = Tests {
    test("a value anywhere in the text is replaced by the mark naming its variable") {
      redaction("token=xoxb-1234567890 end\nagain xoxb-1234567890") ==>
        "token=[redacted secret: SLACK_BOT_TOKEN] end\nagain [redacted secret: SLACK_BOT_TOKEN]"
    }

    test("of two values where one holds the other, the longer is redacted first") {
      redaction("a xoxb-1234567890-and-more b") ==> "a [redacted secret: GITHUB_TOKEN] b"
    }

    test("a multi-line value is redacted line by line, keeping the text's line count") {
      val text = "key:\n-----BEGIN KEY-----\nabcdefghijkl\nshort\n-----END KEY-----\ndone"
      redaction(text) ==>
        "key:\n[redacted secret: DEPLOY_KEY]\n[redacted secret: DEPLOY_KEY]\nshort\n" +
        "[redacted secret: DEPLOY_KEY]\ndone"
    }

    test("a value's suffix the text starts with, and its prefix it ends with, are redacted") {
      redaction("34567890 then xoxb-1234567890-and") ==>
        "[redacted secret: SLACK_BOT_TOKEN] then [redacted secret: GITHUB_TOKEN]"
      redaction("4567890 then xoxb-12") ==> "4567890 then xoxb-12"
    }

    test("a value under 8 characters is left alone, and listed as unguarded") {
      redaction("hunter2 and hunter2") ==> "hunter2 and hunter2"
      redaction.unguarded.map(Variable.value) ==> Vector("GRIT_PASSWORD")
    }

    test("a value not named like a secret is left alone; the variables redacted are listed") {
      redaction("size 1234567890") ==> "size 1234567890"
      redaction.variables.map(Variable.value) ==>
        Vector("DEPLOY_KEY", "GITHUB_TOKEN", "SLACK_BOT_TOKEN")
    }

    test("the start-up line names the variables redacted and those too short to redact") {
      redaction.described ==>
        "Redacting the values of DEPLOY_KEY, GITHUB_TOKEN, SLACK_BOT_TOKEN from tool output; " +
        "too short to redact: GRIT_PASSWORD."
      Redaction.of(Map.empty).described ==> "Redacting no values from tool output."
    }

    test("a long value's cut ends are redacted at both edges, in time linear in the value") {
      val value = new scala.util.Random(42).alphanumeric.take(20000).mkString
      val long = Redaction.of(Map("LONG_TOKEN" -> value))
      val other = new scala.util.Random(7).alphanumeric.take(20000).mkString
      long(value.takeRight(15000) + " middle " + value.take(12000)) ==>
        "[redacted secret: LONG_TOKEN] middle [redacted secret: LONG_TOKEN]"
      long(other) ==> other
      // Every cut end of this value but the shortest agrees with this text's edge in all but its
      // last character, so a scan that compares each length's part in turn is quadratic here.
      val worst = Redaction.of(Map("LONG_TOKEN" -> ("a" * 400000 + "b")))
      val near = "a" * 399999 + "c"
      val started = System.nanoTime()
      worst(near) ==> near
      val took = (System.nanoTime() - started) / 1000000
      assert(took < 1000)
    }
  }
}
