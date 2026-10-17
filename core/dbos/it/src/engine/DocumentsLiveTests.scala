package grit.dbos.engine

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.document.{DocLabel, DocText, DocWeight, Document, DocumentTerms}
import grit.core.durable.Durable
import grit.core.id.{DocKey, PluginName, WorkflowId}
import grit.core.place.{Namespace, Place}
import grit.core.plugin.{DocumentPosting, Documents, Plugin}
import grit.core.visibility.{Clearance, Integrity, Label, Level}
import grit.dbos.internal.Reader
import grit.dbos.sql.{LiveDb, SqlDocuments, SqlTombstones, TestPostgres}

import utest.*

/** What an engine declares of its plugins' documents when it launches, as the database and a
  * reader beside it read it.
  */
object DocumentsLiveTests extends TestSuite {

  private val nothing = (id: WorkflowId) => (_: Durable^) ?=> WorkflowId.value(id)

  private def got[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private def name(s: String): PluginName = got(PluginName.of(s))

  private val Terms: DocumentTerms =
    got(DocumentTerms.of(got(DocLabel.of("notes")), DocWeight.Unscaled, 2.minutes, 10))

  /** A plugin with documents under `terms` that writes none, or with no documents; it no longer
    * declares any document whose data holds `gone`.
    */
  private final class Keeping(val name: PluginName, terms: Option[DocumentTerms]) extends Plugin {
    val version: Int = 1
    override val documents: Option[Documents] = terms.map(t =>
      new Documents {
        val terms: DocumentTerms = t
        val posting: Option[DocumentPosting] = None
        override def undeclared(document: Document): Boolean =
          document.data.objOpt.exists(_.contains("gone"))
      }
    )
  }

  private def launch(engine: Engine^, plugins: Vector[Plugin]): Unit =
    engine.launch(nothing, nothing, nothing, nothing, nothing, LiveEngine.Unplaced, plugins)

  val tests = Tests {
    test("an engine declares its plugins' terms at launch; one launched without is not, but kept") {
      val config = TestPostgres.freshDatabase("documents_declared")
      val documents = new SqlDocuments(new SqlTombstones)
      val (p, q) = (name("with-documents"), name("without-documents"))
      val first = LiveEngine.open(config, "test")
      try launch(first, Vector(new Keeping(p, Some(Terms)), new Keeping(q, None)))
      finally first.close()
      LiveDb.transaction(config)(documents.declared()) ==> Right(Vector(p -> Terms))
      val reader = Reader.open(config)
      try reader.all.read(reader.documents.declared()) ==> Right(Vector(p -> Terms))
      finally reader.close()
      val second = LiveEngine.open(config, "test")
      try launch(second, Vector(new Keeping(q, None)))
      finally second.close()
      LiveDb.transaction(config)(documents.declared()) ==> Right(Vector())
      LiveDb.transaction(config)(documents.kept()) ==> Right(Vector(p))
    }

    test(
      "an engine withdraws at launch, at every label, each document its plugin no longer declares"
    ) {
      val config = TestPostgres.freshDatabase("documents_swept")
      val documents = new SqlDocuments(new SqlTombstones)
      val p = name("sweeping")
      val plugin = Vector(new Keeping(p, Some(Terms)))
      val first = LiveEngine.open(config, "test")
      try launch(first, plugin)
      finally first.close()
      val keeper = documents.keeper(p, Terms)
      val written = Instant.parse("2026-01-01T00:00:00Z")
      // Each written under a clearance of its own label, so its floor keeps it there.
      def put(k: String, label: Label, data: ujson.Value) =
        LiveDb
          .transaction(config, Clearance.of(label))(
            keeper.write(
              got(DocKey.of(k)),
              label,
              Integrity.Member,
              Place.under(Namespace.Task, Vector.empty),
              got(DocText.of(k)),
              data,
              None,
              written
            )
          )
          .isRight ==> true
      put("stays", Label.Public, ujson.Obj())
      put("public", Label.Public, ujson.Obj("gone" -> true))
      put("confidential", Label.at(Level.Confidential), ujson.Obj("gone" -> true))
      val second = LiveEngine.open(config, "test")
      try launch(second, plugin)
      finally second.close()
      LiveDb
        .transaction(config)(documents.shelf(p).newest(10))
        .map(_.map(d => DocKey.value(d.key))) ==>
        Right(Vector("stays"))
    }
  }
}
