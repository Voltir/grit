package grit.eval.harness.corpus

import grit.core.id.CorpusName
import grit.core.place.{Place, Service}
import grit.core.triage.Corpus

import utest.*

/** A deployment's corpora read from the file written beside a corpus. */
object KnowledgeJsonTests extends TestSuite {

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  val tests = Tests {
    test("read keeps each source in the order declared, with the service it supplies or none") {
      val read = right(
        KnowledgeJson.read(
          """{"sources": [
            |  {"name": "github", "line": "the repository", "within": "slack:", "supplies": "github"},
            |  {"name": "conversations", "line": "past talk", "within": "slack:acme", "supplies": null},
            |  {"name": "notes", "line": "the notes", "within": "fs:/x"}
            |]}""".stripMargin
        )
      )
      def source(n: String, line: String, within: String, supplies: Option[String]) =
        Corpus(
          right(CorpusName.of(n)),
          line,
          right(Place.read(within)),
          supplies.map(s => right(Service.of(s)))
        )
      read.all ==> Vector(
        source("github", "the repository", "slack:", Some("github")),
        source("conversations", "past talk", "slack:acme", None),
        source("notes", "the notes", "fs:/x", None)
      )
    }

    test("read refuses a source declared twice, naming it") {
      KnowledgeJson.read(
        """{"sources": [{"name": "a", "line": "x", "within": "slack:"},
          |{"name": "a", "line": "y", "within": "slack:"}]}""".stripMargin
      ) ==> Left("knowledge: a is declared twice")
    }
  }
}
