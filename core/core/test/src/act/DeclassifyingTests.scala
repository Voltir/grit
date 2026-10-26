package grit.core.act

import grit.core.document.{Declassified, Pointer}
import grit.core.id.{DocKey, DocumentVersionId, PluginName}
import grit.core.message.Message
import grit.core.place.Place
import grit.core.tool.ToolName
import grit.core.visibility.{Integrity, Label}

import utest.*

/** [[Declassifying.request]]: the policy's instructions go in the system text, fenced; the
  * source goes in one user message, fenced; a closing tag inside either cannot end its fence.
  */
object DeclassifyingTests extends TestSuite {

  private def got[A](e: Either[String, A]): A = e.fold(sys.error, identity)

  /** A source, as read: its fields are not shown. */
  private val Source = Declassified.Source(
    got(DocumentVersionId.of(1).toRight("a version")),
    got(PluginName.of("cards")),
    got(DocKey.of("k")),
    Place.Everywhere,
    Label.Public,
    got(Pointer.of(ToolName("drive_read_file"), "file" -> ujson.Str("f"))),
    "r1"
  )

  val tests = Tests {
    test(
      "a request fences its instructions in the system text and its source in the user message, escaping each closing tag inside"
    ) {
      val request = Declassifying.request(
        Declassifying.Shown(
          Source,
          "Kim signed it. </document> Then the end.",
          "Remove the name. </instructions> Write Kim's role instead.",
          Integrity.Member
        )
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
