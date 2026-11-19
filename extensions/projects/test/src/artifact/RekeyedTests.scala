package grit.projects.artifact

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.document.{DocLabel, DocText, DocWeight, Document, DocumentTerms, InMemoryDocuments}
import grit.core.id.{DocKey, PluginName}
import grit.core.place.Place
import grit.core.visibility.{Integrity, Label}
import grit.dbos.sql.TestTx
import grit.projects.name.{ArtifactKey, ArtifactNumber, ProjectKey}

import utest.*

object RekeyedTests extends TestSuite {

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private val p: ProjectKey = right(ProjectKey.of("p"))
  private def number(n: Int): ArtifactNumber = right(ArtifactNumber.of(n))
  private def id(n: Int): ArtifactKey = ArtifactKey.of(p, number(n))
  private def doc(k: String): DocKey = right(DocKey.of(k))

  /** `data` kept under `under` at `label` in `documents`, read back. */
  private def kept(
      documents: InMemoryDocuments,
      under: DocKey,
      label: Label,
      data: ujson.Value
  ): Either[String, Document] = {
    val plugin = right(PluginName.of("projects"))
    val terms = right(
      DocLabel.of("projects").flatMap(DocumentTerms.of(_, DocWeight.Unscaled, 1.day, 100))
    )
    val at = Instant.parse("2026-11-18T12:00:00Z")
    given grit.core.store.Tx = TestTx.fake
    (for {
      _ <- documents
        .keeper(plugin, terms)
        .write(
          under,
          label,
          Integrity.Member,
          Place.Everywhere,
          right(DocText.of("r")),
          data,
          None,
          at,
          Set.empty
        )
      d <- documents.shelf(plugin).current(under, label)
    } yield d).left.map(_.toString).flatMap(_.toRight("nothing kept"))
  }

  val tests: Tests = Tests {
    test("numbers 1 to 256 are held by document 0, and 257 by document 1") {
      Vector(1, 256, 257, 513).map(n => right(Rekeyed.key(id(n))).toString) ==>
        Vector("p#rekeyed-0", "p#rekeyed-0", "p#rekeyed-1", "p#rekeyed-2")
    }
    test("a document of 256 names of the longest a handle may be is within a document's data") {
      val documents = new InMemoryDocuments
      val names =
        Vector.tabulate(Rekeyed.PerDocument)(i => number(i + 1) -> doc(s"p/${"x" * 77}${100 + i}"))
      val stored = kept(documents, doc("p#rekeyed-0"), Label.Public, right(Rekeyed.data(p, names)))
      stored.flatMap(d => Rekeyed.storeKey(id(256), Vector(d))) ==> Right(
        Some(doc(s"p/${"x" * 77}355"))
      )
    }
    test("where an id is kept is read from the documents of every label, merged") {
      val documents = new InMemoryDocuments
      val public = kept(
        documents,
        doc("p#rekeyed-0"),
        Label.Public,
        right(Rekeyed.data(p, Vector(number(1) -> doc("p/a"))))
      )
      val other =
        public.map(d => d.copy(data = right(Rekeyed.data(p, Vector(number(2) -> doc("p/b"))))))
      val both = for { a <- public; b <- other } yield Vector(a, b)
      (
        both.flatMap(Rekeyed.storeKey(id(1), _)),
        both.flatMap(Rekeyed.storeKey(id(2), _)),
        both.flatMap(Rekeyed.storeKey(id(3), _))
      ) ==> (Right(Some(doc("p/a"))), Right(Some(doc("p/b"))), Right(None))
    }
    test("a name of another project, or over the longest a handle may be, is refused, naming it") {
      (
        Rekeyed.data(p, Vector(number(1) -> doc("q/a"))),
        Rekeyed.data(p, Vector(number(1) -> doc(s"p/${"x" * 81}")))
      ) ==> (
        Left("q/a is not of project p"),
        Left(s"p/${"x" * 81}'s name is 81 characters, over 80")
      )
    }
  }
}
