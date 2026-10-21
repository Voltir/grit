package grit.eval.harness.reply

import java.time.Instant

import grit.core.context.{AssemblyNote, Querying, Results, Retrieved, SectionTag}
import grit.core.document.{DocText, Document, Placement, Pointer}
import grit.core.id.{DocKey, DocumentVersionId, PluginName, TurnSeq}
import grit.core.message.{Message, Tokens, Usage}
import grit.core.period.Probability
import grit.core.place.Place
import grit.core.provider.{ModelRequest, ToolSchema}
import grit.core.tool.ToolName
import grit.core.visibility.{Integrity, Label}
import grit.turn.{TurnRecord, TurnRelevance}

import TurnRelevance.Verdict
import utest.*

/** A turn's request again with a line for each document its judge rejected. */
object PointerLinesTests extends TestSuite {

  private def got[A](e: Either[String, A]): A = e.fold(sys.error(_), identity)
  private def version(n: Long): DocumentVersionId = got(DocumentVersionId.of(n).toRight("version"))
  private def p(x: Double): Probability = Probability.clamped(x)
  private val at = Instant.parse("2026-10-16T12:00:00Z")

  private def document(n: Long, text: String, tool: Option[String]): Document =
    Document(
      version(n),
      got(PluginName.of("cards")),
      got(DocKey.of(s"card-$n")),
      Label.Public,
      Integrity.Member,
      got(Place.read("slack:T1/C1")),
      got(DocText.of(text)),
      ujson.Obj(),
      tool.map(t => got(Pointer.of(got(ToolName.of(t)), "id" -> ujson.Str(s"f$n")))),
      at,
      Placement(0, at),
      None
    )

  private def schema(name: String) = ToolSchema(name, "reads a file", ujson.Obj())

  // The window's section and document, then the turn's own message.
  private val request = ModelRequest(
    "system",
    Vector(
      Message.User("[afar] another conversation, shown by grit"),
      Message.User("[doc] Notes, kept by grit"),
      Message.User("what is the Falcon budget?")
    ),
    Vector(schema("drive_read"))
  )

  val tests = Tests {
    test("the documents rejected are those the verdicts rejected, in the results' order") {
      val retrieval = TurnRecord.Retrieval(
        Querying.Written(
          "Falcon budget",
          AssemblyNote.Queried("Falcon budget", "writer", Usage.Zero, Tokens.Zero)
        ),
        Results(
          Vector(
            Retrieved.Doc(version(3)) -> 3.0,
            Retrieved.Own(TurnSeq(0)) -> 2.5,
            Retrieved.Doc(version(1)) -> 2.0,
            Retrieved.Doc(version(2)) -> 1.0
          )
        ),
        Vector(
          Retrieved.Doc(version(2)) -> Verdict.Rejected(p(0.2)),
          Retrieved.Own(TurnSeq(0)) -> Verdict.Rejected(p(0.1)),
          Retrieved.Doc(version(1)) -> Verdict.Passed(p(0.8)),
          Retrieved.Doc(version(3)) -> Verdict.Rejected(p(0.3))
        )
      )
      PointerLines.rejected(retrieval) ==> Vector(version(3), version(2))
    }

    test("each rejected document a tool offered reads is one line after the window's own") {
      val budget = document(1, "Falcon budget card\nthe budget is 4200", Some("drive_read"))
      val elsewhere = document(2, "Deploy notes", Some("github_read"))
      val bare = document(3, "Kept with no pointer", None)
      PointerLines.variant(request, Vector(budget, elsewhere, bare)).messages ==> Vector(
        Message.User("[afar] another conversation, shown by grit"),
        Message.User("[doc] Notes, kept by grit"),
        Message.User(
          PointerLines.Lead +
            "\n- Falcon budget card: Read it with drive_read {\"id\":\"f1\"}"
        ),
        Message.User("what is the Falcon budget?")
      )
    }

    test("a first line is cut to the title's characters") {
      val long = document(1, "x" * 200, Some("drive_read"))
      PointerLines.variant(request, Vector(long)).messages.lift(2) ==> Some(
        Message.User(
          PointerLines.Lead +
            s"\n- ${"x" * PointerLines.Title}: Read it with drive_read {\"id\":\"f1\"}"
        )
      )
    }

    test("a request is unchanged when no rejected document has a pointer it offers") {
      PointerLines.variant(request, Vector(document(3, "Kept with no pointer", None))) ==>
        request
    }

    test("the lead line names documents, never the program") {
      (
        PointerLines.Lead.startsWith(SectionTag.Document.tag),
        PointerLines.Lead.toLowerCase.contains("grit")
      ) ==> (true, false)
    }
  }
}
