package grit.core.host

import grit.core.secret.Variable

import utest.*

/** [[RedactionMark]]: the text a tool shows in a secret's place. */
object RedactionMarkTests extends TestSuite {

  val tests = Tests {
    test(
      "a mark names its variable in brackets, and any mark is found whichever variable it names"
    ) {
      RedactionMark.of(Variable("SLACK_BOT_TOKEN")) ==> "[redacted secret: SLACK_BOT_TOKEN]"
      RedactionMark.Example ==> "[redacted secret: NAME]"
      (
        RedactionMark.in("token: [redacted secret: OTHER_KEY] here"),
        RedactionMark.in("token: [redacted: OTHER_KEY]"),
        RedactionMark.in("no mark at all")
      ) ==> (true, false, false)
    }
  }
}
