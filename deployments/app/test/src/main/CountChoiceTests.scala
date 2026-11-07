package grit.app.main

import grit.core.message.Tokens

import utest.*

/** The counts a run reads from its environment: tokens (`GRIT_WINDOW_TOKENS`) and milliseconds
  * (`GRIT_STUB_DELAY_MS`).
  */
object CountChoiceTests extends TestSuite {

  val tests = Tests {
    test("a token count: unset is the default; a non-negative whole number, or an error") {
      val read = (raw: Option[String]) =>
        RunChoices.tokens(
          raw.fold(Map.empty)(r => Map("GRIT_WINDOW_TOKENS" -> r)),
          "GRIT_WINDOW_TOKENS",
          Tokens(7)
        )
      (read(None), read(Some(" 0 ")), read(Some("1200")), read(Some("-1")), read(Some("1k"))) ==> (
        Right(Tokens(7)),
        Right(Tokens(0)),
        Right(Tokens(1200)),
        Left("GRIT_WINDOW_TOKENS is not a non-negative whole number"),
        Left("GRIT_WINDOW_TOKENS is not a non-negative whole number")
      )
    }

    test("milliseconds: unset is 0; a non-negative whole number, or an error") {
      (
        RunChoices.millis(Map.empty, "GRIT_STUB_DELAY_MS"),
        RunChoices.millis(Map("GRIT_STUB_DELAY_MS" -> "250"), "GRIT_STUB_DELAY_MS"),
        RunChoices.millis(Map("GRIT_STUB_DELAY_MS" -> "soon"), "GRIT_STUB_DELAY_MS")
      ) ==> (Right(0L), Right(250L), Left("GRIT_STUB_DELAY_MS is not a non-negative whole number"))
    }
  }
}
