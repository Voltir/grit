package grit.core.act

import grit.core.classify.{Question, Request}
import grit.core.document.{Declassified, Pointer}
import grit.core.id.{DocKey, DocumentVersionId, PluginName}
import grit.core.message.Message
import grit.core.place.Place
import grit.core.screen.{Checks, Screen}
import grit.core.tool.ToolName
import grit.core.visibility.{Integrity, Label}

import utest.*

/** [[Declassifying.request]]: the policy's instructions go in the system text, fenced; the
  * source goes in one user message, fenced; a closing tag inside either cannot end its fence;
  * the policy's checks go nowhere in it. [[Declassifying.screen]]: about the text alone, the
  * floor first, then the policy's signals.
  */
object DeclassifyingTests extends TestSuite {

  private def got[A](e: Either[String, A]): A = e.fold(sys.error, identity)

  /** Checks whose every signal and example holds `marker`. */
  private def marked(marker: String): Checks =
    Checks
      .read(
        Some(
          ujson.Obj(
            "signals" -> ujson.Arr(
              ujson.Obj("infer" -> s"that $marker is absent", "must" -> "absent"),
              ujson.Obj("infer" -> s"that $marker is present", "must" -> "present")
            ),
            "examples" -> ujson.Obj(
              "accept" -> ujson.Arr(s"An accepted $marker."),
              "refuse" -> ujson.Arr(s"A refused $marker.")
            )
          )
        )
      )
      .fold(why => sys.error(why.said), identity)

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
          marked("Heron"),
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

    test("a rewrite's request holds none of its policy's signals or examples") {
      val request = Declassifying.request(
        Declassifying.Shown(
          Source,
          "The text.",
          "The policy.",
          marked("Lanternfish"),
          Integrity.Member
        )
      )
      val shown = request.system +: request.messages.collect { case Message.User(t) => t }
      shown.filter(_.contains("Lanternfish")) ==> Vector()
    }

    test(
      "a rewrite's screen asks about its text alone: the floor, meta then first, verbatim, then each of the policy's signals"
    ) {
      val asked = Request.of(Screen.Text("The rewrite."), Declassifying.screen(marked("Heron")).ask)
      (
        asked.state,
        asked.questions.collect { case Question.YesNo(instructions, _, _) => instructions }
      ) ==> (
        ujson.Obj("text" -> "The rewrite."),
        Vector(
          "Does `text` refer to itself as a rewrite, version or edit of another document, or to " +
            "instructions it was given?",
          "Does `text` speak in the first person singular, as 'I', about what it shares, " +
            "includes or leaves out?",
          Screen.absent("that Heron is absent"),
          Screen.present("that Heron is present")
        )
      )
    }
  }
}
