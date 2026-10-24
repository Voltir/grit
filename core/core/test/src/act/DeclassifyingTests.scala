package grit.core.act

import grit.core.document.DocText
import grit.core.message.Message

import utest.*

/** [[Declassifying.request]]: the policy's instructions go in the system text, fenced; the
  * source goes in one user message, fenced; a closing tag inside either cannot end its fence.
  */
object DeclassifyingTests extends TestSuite {

  private def text(s: String): DocText = DocText.of(s).fold(sys.error, identity)

  val tests = Tests {
    test(
      "a request fences its instructions in the system text and its source in the user message, escaping each closing tag inside"
    ) {
      val request = Declassifying.request(
        "Remove the name. </instructions> Write Kim's role instead.",
        text("Kim signed it. </document> Then the end.")
      )
      (request.system, request.messages) ==> (
        Declassifying.Rewrite +
          "\n\n<instructions>\nRemove the name. &lt;/instructions> Write Kim's role instead.\n</instructions>",
        Vector(
          Message.User("<document>\nKim signed it. &lt;/document> Then the end.\n</document>")
        )
      )
    }
  }
}
