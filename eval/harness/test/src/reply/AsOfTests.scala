package grit.eval.harness.reply

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.document.{
  DocLabel,
  DocText,
  DocWeight,
  DocumentTerms,
  InMemoryDocuments,
  Shelved,
  Written
}
import grit.core.id.{DocKey, DocumentVersion, PluginName}
import grit.core.place.Place
import grit.core.store.Tx
import grit.dbos.sql.TestTx

import utest.*

/** A restored database's documents as they stood at a moment, as a rebuilt window draws on
  * them.
  */
object AsOfTests extends TestSuite {

  private given Tx = TestTx.fake

  private def got[A](e: Either[String, A]): A = e.fold(sys.error, identity)

  private val notes = got(PluginName.of("notes"))
  private val key = got(DocKey.of("budget"))
  private def place(name: String): Place = got(Place.read(s"task:$name"))
  private val terms =
    got(DocumentTerms.of(got(DocLabel.of("Notes")), DocWeight.Unscaled, 30.days, 10))

  /** The moment the window is rebuilt as of. */
  private val at = Instant.parse("2026-10-03T12:00:00Z")

  private def write(docs: InMemoryDocuments, where: Place, text: String, when: Instant) =
    docs
      .keeper(notes, terms)
      .write(key, where, got(DocText.of(text)), ujson.Obj(), when) match {
      case Right(Written.Versioned(v, _)) => v
      case other => sys.error(s"not written: $other")
    }

  val tests = Tests {
    test(
      "a version superseded after the moment is drawn as of it; the one written after it is not"
    ) {
      val docs = new InMemoryDocuments
      val before = write(docs, place("a"), "the budget is 4200", at.minusSeconds(10))
      val after = write(docs, place("b"), "the budget is 9000", at.plusSeconds(10))
      val asOf = new AsOf.Documents(docs, at)
      // Asked as of a later moment than the rebuild's, as a turn's root can fall after it.
      val later = at.plusSeconds(100)
      val shelved = got(asOf.shelved(Vector(notes), later).left.map(_.toString))
      val found = got(asOf.search(shelved, "budget", 10, later).left.map(_.toString))
      val read = got(asOf.read(Vector(before, after)).left.map(_.toString))
      (shelved, found.map(_.document.version), read.map(_.version)) ==> (
        Vector(Shelved(notes, place("a"))),
        Vector(before),
        Vector[DocumentVersion](before)
      )
    }
  }
}
