package grit.dbos.engine

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.document.{DocLabel, DocText, DocWeight, DocumentSearch, DocumentTerms, Shelved}
import grit.core.id.{DocKey, PluginName}
import grit.core.period.Probability
import grit.core.place.{Namespace, Place}
import grit.core.store.StoreError
import grit.core.triage.{Cues, Fact}
import grit.core.visibility.{Clearance, Integrity, Label}
import grit.dbos.sql.{LiveDb, SqlDocuments, SqlTombstones, TestPostgres}

import utest.*

/** The document index's score as [[Fact.RecordScore]] reads it: the scale
  * [[Fact.PlainMatch]] assumes, BM25 summed over a query's words, never normalised.
  */
object DocumentScaleTests extends TestSuite {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("document_scale")
    LiveEngine.open(c, "test").close()
    c
  }

  private val documents = new SqlDocuments(new SqlTombstones)

  private def got[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private def ok[A](e: Either[StoreError, A]): A =
    e.fold(err => throw new java.lang.AssertionError(s"$err"), identity)

  private val At = Instant.parse("2026-03-01T00:00:00Z")
  private val notes = got(PluginName.of("notes"))
  private val here = Place.under(Namespace.Task, Vector("documents", "here"))
  private val terms = got(
    DocumentTerms.of(got(DocLabel.of("notes")), DocWeight.Unscaled, 30.days, 500)
  )

  /** 1,500 made-up words of three syllables, none a stop word. */
  private val words: Vector[String] = {
    val syllables = for { c <- "bdgklmnprstvz"; v <- "aiou" } yield s"$c$v"
    (for { a <- syllables; b <- syllables; c <- syllables } yield a + b + c).toVector
      .grouped(90)
      .map(_.head)
      .take(1500)
      .toVector
  }

  /** 150 documents of 60 words each, drawn from [[words]] by a fixed seed. */
  private val corpus: Vector[Vector[String]] = {
    val random = new scala.util.Random(42)
    Vector.fill(150)(Vector.fill(60)(words(random.nextInt(words.size))))
  }

  private val texts: Vector[String] = corpus.map(_.mkString(" "))

  /** The best match for `query` among the corpus, and how [[Fact.RecordScore]] reads it. */
  private def best(query: String): (Option[String], Double) =
    LiveDb
      .transaction(config, Clearance.of(Label.Public)) {
        ok(documents.search(Vector(Shelved(notes, here)), query, 1, At.plusSeconds(60)))
      }
      .headOption match {
      case Some(DocumentSearch.Hit(d, score)) =>
        (
          Some(DocKey.value(d.key)),
          Probability.value(
            Fact.RecordScore.read(Cues.Empty.copy(records = Some(Cues.Match(notes, score, 1))))
          )
        )
      case None => (None, 0.0)
    }

  val tests = Tests {
    test(
      "a passage quoted from a document plainly matches it; one word it shares with the corpus does not"
    ) {
      val keeper = documents.keeper(notes, terms)
      LiveDb.transaction(config, Clearance.of(Label.Public)) {
        corpus.indices.foreach { i =>
          ok(
            keeper.write(
              got(DocKey.of(s"d$i")),
              Label.Public,
              Integrity.Member,
              here,
              got(DocText.of(texts(i))),
              ujson.Obj(),
              None,
              At
            )
          )
        }
      }
      val (quoted, quotedRead) = best(corpus(0).slice(20, 40).mkString(" "))
      val (_, oneRead) = best(corpus(0)(0))
      assert(quoted == Some("d0"), quotedRead >= 0.5, oneRead > 0, oneRead < 0.5)
    }
  }
}
