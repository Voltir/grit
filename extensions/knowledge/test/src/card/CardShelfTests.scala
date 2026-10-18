package grit.knowledge.card

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.document.{
  DocLabel,
  DocText,
  DocWeight,
  DocumentTerms,
  InMemoryDocuments,
  Pointer,
  Written
}
import grit.core.id.{CorpusName, DocKey, DocumentVersion, PluginName}
import grit.core.place.Place
import grit.core.tool.ToolName
import grit.core.visibility.{Integrity, Label}
import grit.dbos.sql.TestTx
import grit.knowledge.ref.Ref

import utest.*

object CardShelfTests extends TestSuite {

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private val name = right(PluginName.of("knowledge"))
  private val corpus = right(CorpusName.of("shared"))
  private val terms =
    right(DocumentTerms.of(right(DocLabel.of("knowledge card")), DocWeight.Unscaled, 30.days, 100))
  private val drive = right(ToolName.of("drive_read"))

  private given grit.core.store.Tx = TestTx.fake

  private def at(s: Int): Instant = Instant.EPOCH.plusSeconds(s.toLong)

  /** A card of the Drive file `id`, titled `title` when given. */
  private def card(id: String, title: Option[String]): Card =
    Card.file(
      corpus,
      id,
      right(UnitVersion.of("2026-01-01T00:00:00Z")),
      right(CardSummary.of("What the file holds.", Vector.empty)),
      right(Pointer.of(drive, "file" -> ujson.Str(id))),
      title
    )

  /** A store holding `cards`, each written at its instant, and the versions it gave them. */
  private def kept(cards: Vector[(Card, Instant)]): (InMemoryDocuments, Vector[DocumentVersion]) = {
    val docs = new InMemoryDocuments
    val keeper = docs.keeper(name, terms)
    val versions = cards.map((c, t) =>
      keeper
        .write(
          c.docKey,
          Label.Public,
          Integrity.Member,
          Place.Everywhere,
          c.text,
          c.data,
          Some(c.pointer),
          t
        ) match {
        case Right(Written.Versioned(v, _, _)) => v
        case other => throw new java.lang.AssertionError(other.toString)
      }
    )
    (docs, versions)
  }

  val tests = Tests {
    test("the cards changed since an instant, most recent first, at their versions") {
      val a = card("f1", Some("Plan"))
      val b = card("f2", None)
      val c = card("f3", Some("Notes"))
      val (docs, versions) = kept(Vector(a -> at(1), b -> at(2), c -> at(3)))
      CardShelf.over(docs.shelf(name)).changed(at(2), 10) ==> Right(
        Vector(
          CardShelf.Seen(versions(2), c.key, "Notes", Some(Ref.Drive("f3"))),
          CardShelf.Seen(versions(1), b.key, "f2", Some(Ref.Drive("f2")))
        )
      )
    }

    test("at most n, the most recent") {
      val (docs, versions) = kept(Vector(card("f1", None) -> at(1), card("f2", None) -> at(2)))
      CardShelf.over(docs.shelf(name)).changed(at(0), 1).map(_.map(_.version)) ==>
        Right(Vector(versions(1)))
    }

    test("a document of the plugin that is no card is not seen") {
      val docs = new InMemoryDocuments
      right(
        docs
          .keeper(name, terms)
          .write(
            right(DocKey.of("not-a-card")),
            Label.Public,
            Integrity.Member,
            Place.Everywhere,
            right(DocText.of("x")),
            ujson.Obj(),
            None,
            at(1)
          )
          .left
          .map(_.toString)
      )
      CardShelf.over(docs.shelf(name)).changed(at(0), 10) ==> Right(Vector.empty)
    }
  }
}
