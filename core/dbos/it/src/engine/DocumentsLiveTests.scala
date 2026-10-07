package grit.dbos.engine

import scala.concurrent.duration.*

import grit.core.document.{DocLabel, DocWeight, DocumentKeeper, DocumentTerms}
import grit.core.durable.Durable
import grit.core.id.{PluginName, WorkflowId}
import grit.core.plugin.{Documents, Plugin}
import grit.core.store.{ClosedPeriod, StoreError, Tx}
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

  /** A plugin with documents under `terms` that writes none, or with no documents. */
  private final class Keeping(val name: PluginName, terms: Option[DocumentTerms]) extends Plugin {
    val version: Int = 1
    override val documents: Option[Documents] = terms.map(t =>
      new Documents {
        val terms: DocumentTerms = t
        def post(closed: ClosedPeriod, keeper: DocumentKeeper)(using
            Tx^
        ): Either[StoreError, Unit] = Right(())
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
  }
}
