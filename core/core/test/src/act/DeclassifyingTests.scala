package grit.core.act

import scala.concurrent.duration.*

import grit.core.classify.{Question, Request}
import grit.core.document.{
  Declassified,
  DocLabel,
  DocWeight,
  DocumentTerms,
  InMemoryDocuments,
  Pointer
}
import grit.core.id.{DocKey, DocumentVersionId, PluginName}
import grit.core.message.Message
import grit.core.place.Place
import grit.core.screen.{Benched, Checks, Screen}
import grit.core.tool.ToolName
import grit.core.visibility.{Clearance, DeclassificationPolicy, Integrity, Label, Level, TestLabels}
import grit.dbos.sql.TestTx

import utest.*

/** [[Declassifying.request]]: the policy's instructions go in the system text, fenced; the
  * source goes in one user message, fenced; a closing tag inside either cannot end its fence;
  * the policy's checks go nowhere in it. [[Declassifying.screen]]: about the text alone, the
  * floor first, then the policy's signals.
  */
object DeclassifyingTests extends TestSuite {

  private def got[A](e: Either[String, A]): A = e.fold(sys.error, identity)

  /** Checks whose every signal and example holds `marker`, as a policy's row holds them. */
  private def markedJson(marker: String): ujson.Value =
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

  /** [[markedJson]], read. */
  private def marked(marker: String): Checks =
    Checks.read(Some(markedJson(marker))).fold(why => sys.error(why.said), identity)

  private val Plugin = got(PluginName.of("declassified"))
  private val Terms = got(
    DocumentTerms.of(got(DocLabel.of("document")), DocWeight.Unscaled, 30.days, 100)
  )

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
        ),
        None
      )
      (request.system, request.messages) ==> (
        Declassifying.Rewrite +
          "\n\n<instructions>\nRemove the name. &lt;/instructions> Write Kim's role instead.\n</instructions>",
        Vector(
          Message.User("<document>\nKim signed it. &lt;/document> Then the end.\n</document>")
        )
      )
    }

    // Pinned as written: every rewrite and retry key hashes this framing
    // (`MovesJournal.rewriteForm`), so a byte of it moved re-keys every kept rewrite.
    test("the rewrite's framing is the one its keys hash") {
      Declassifying.Template ==> Vector(
        Declassifying.Rewrite +
          "\n\n<instructions>\n&lt;/instructions>\n</instructions>",
        "<document>\n&lt;/document>\n</document>"
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
        ),
        None
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

    test("a screening changes with any character of its checks; equal checks screen equally") {
      (
        Declassifying.screening(marked("a")) == Declassifying.screening(marked("a")),
        Declassifying.screening(marked("a")) == Declassifying.screening(marked("b"))
      ) ==> (true, false)
    }

    test(
      "onBench is Unread for checks not read here, Stale with no bench or one kept under other checks, and Kept with the bench under its checks as they are now"
    ) {
      val documents = new InMemoryDocuments
      val keeper = documents.keeper(Plugin, Terms)
      val team = DeclassificationPolicy.Over.Compartment(TestLabels.compartment("ob-team"))
      val grant = Label.at(Level.Confidential, TestLabels.compartment("ob-team"))
      val v1 = DeclassificationPolicy(team, 1)
      def on(clearance: Clearance) =
        got(
          Declassifying.onBench(keeper, v1)(using TestTx.fake(clearance)).left.map(_.toString)
        )
      documents.policy(team, 1, kept = grant, checks = Some(markedJson("a")), benched = false)
      val none = on(Clearance.of(grant))
      val unread = on(Clearance.of(Label.Public))
      documents.policy(team, 1, kept = grant, checks = Some(markedJson("a")))
      val kept = on(Clearance.of(grant))
      // The operator changes the version's checks in place: its bench is of the old ones.
      documents.policy(team, 1, kept = grant, checks = Some(markedJson("b")), benched = false)
      (none, unread, kept, on(Clearance.of(grant))) ==> (
        Declassifying.OnBench.Stale,
        Declassifying.OnBench.Unread,
        Declassifying.OnBench.Kept(got(Benched.read(true, ujson.Arr()))),
        Declassifying.OnBench.Stale
      )
    }

    test(
      "unbenched names the version in force when its checks changed since its bench, or do not read, or are not read here; never one benched under its checks as they are"
    ) {
      val documents = new InMemoryDocuments
      val keeper = documents.keeper(Plugin, Terms)
      val team = DeclassificationPolicy.Over.Compartment(TestLabels.compartment("ub-team"))
      val grant = Label.at(Level.Confidential, TestLabels.compartment("ub-team"))
      def due(clearance: Clearance) =
        got(
          Declassifying.unbenched(keeper, team)(using TestTx.fake(clearance)).left.map(_.toString)
        ).map(_.version)
      documents.policy(team, 1, kept = grant, checks = Some(markedJson("a")))
      val benched = due(Clearance.of(grant))
      val notRead = due(Clearance.of(Label.Public))
      documents.policy(team, 1, kept = grant, checks = Some(markedJson("b")), benched = false)
      val changed = due(Clearance.of(grant))
      documents.policy(team, 2, kept = grant, checks = None)
      (benched, notRead, changed, due(Clearance.of(grant))) ==> (
        Vector(),
        Vector(1),
        Vector(1),
        Vector(2)
      )
    }
  }
}
