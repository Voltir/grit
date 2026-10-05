package grit.core.id

import utest.*

object DocumentIdsTests extends TestSuite {

  val tests = Tests {
    test("a key is one line of 1 to 200 characters") {
      DocKey.of("slack:acme/#grit").map(DocKey.value) ==> Right("slack:acme/#grit")
      DocKey.of("x" * 200).map(DocKey.value) ==> Right("x" * 200)
      DocKey.of("") ==> Left("a document's key is 1 to 200 characters")
      DocKey.of("x" * 201) ==> Left("a document's key is 1 to 200 characters")
      DocKey.of("two\nlines") ==> Left("a document's key holds no control character")
      DocKey.of("tab\there") ==> Left("a document's key holds no control character")
    }

    test("a version is at least 1") {
      DocumentVersion.of(1).map(DocumentVersion.value) ==> Some(1L)
      DocumentVersion.of(0) ==> None
      DocumentVersion.of(-4) ==> None
    }
  }
}
