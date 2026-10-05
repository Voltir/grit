package grit.core.document

import scala.concurrent.duration.*

import utest.*

object DocumentTermsTests extends TestSuite {

  private val label = DocLabel.of("notes").fold(sys.error, identity)

  val tests = Tests {
    test("a document's text is anything but blank") {
      DocText.of(" two\nlines ").map(DocText.value) ==> Right(" two\nlines ")
      DocText.of(" \n\t") ==> Left("a document's text is not blank")
      DocText.of("") ==> Left("a document's text is not blank")
    }

    test("a label is one line, not blank") {
      DocLabel.of("digest: recent").map(DocLabel.value) ==> Right("digest: recent")
      DocLabel.of("  ") ==> Left("a document label is not blank")
      DocLabel.of("two\nlines") ==> Left("a document label is one line, with no control character")
      DocLabel.of("tab\there") ==> Left("a document label is one line, with no control character")
    }

    test("a weight is positive and finite") {
      DocWeight.of(0.25).map(DocWeight.value) ==> Right(0.25)
      for (w <- Vector(0.0, -1.0, Double.NaN, Double.PositiveInfinity))
        DocWeight.of(w) ==> Left(s"a document weight is positive and finite, not $w")
    }

    test("terms keep their documents a minute or more, and at least one current") {
      DocumentTerms.of(label, DocWeight.Unscaled, 1.minute, 1).map(t => (t.retention, t.bound)) ==>
        Right((1.minute, 1))
      DocumentTerms.of(label, DocWeight.Unscaled, 59.seconds, 1) ==>
        Left("documents are kept a minute or more after they stop being current, not 59 seconds")
      DocumentTerms.of(label, DocWeight.Unscaled, 1.day, 0) ==>
        Left("a plugin's documents are bounded at 1 or more, not 0")
    }
  }
}
