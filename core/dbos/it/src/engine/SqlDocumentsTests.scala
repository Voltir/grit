package grit.dbos.engine

import grit.core.document.{
  DocumentContract,
  DocumentKeeper,
  DocumentSearch,
  DocumentShelf,
  DocumentStore,
  DocumentTerms,
  InMemoryDocuments,
  Pointer
}
import grit.core.id.{DocumentVersionId, PluginName}
import grit.core.review.DocumentReviews
import grit.core.store.{Origin, Savepoints, Tombstones, Tx}
import grit.core.tool.ToolName
import grit.core.visibility.{Clearance, DeclassificationPolicy, Label, Visibility}
import grit.dbos.sql.{
  LiveDb,
  SqlDocumentReviews,
  SqlDocumentSearch,
  SqlDocuments,
  SqlLabels,
  SqlSavepoints,
  SqlStatements,
  SqlTombstones,
  TestPostgres
}

/** The documents contract, kept by the SQL store against a real Postgres. */
object SqlDocumentsTests extends DocumentContract {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_documents")
    LiveEngine.open(c, "test").close()
    c
  }

  protected val tombstones: Tombstones = new SqlTombstones

  private val documents = new SqlDocuments(tombstones)

  protected val savepoints: Savepoints = SqlSavepoints

  protected val store: DocumentStore = documents

  // The read half over the read-only class an attached link is handed.
  override protected val search: DocumentSearch = new SqlDocumentSearch

  protected def keeper(plugin: PluginName, terms: DocumentTerms): DocumentKeeper =
    documents.keeper(plugin, terms)

  protected def shelf(plugin: PluginName): DocumentShelf = documents.shelf(plugin)

  protected def opened[A](clearance: Clearance)(body: (Tx^) ?=> A): A =
    LiveDb.transaction(config, clearance)(body)

  protected val reviews: DocumentReviews = new SqlDocumentReviews

  protected def openedUnder[A](clearance: Clearance, visibility: Visibility)(body: (Tx^) ?=> A): A =
    LiveDb.underAt(config, visibility, clearance)(body)

  protected def room(origin: Origin): Unit = {
    val _ = LiveDb.conversation(config, origin)
  }

  /** `sql`, its parameters set by `bind`, run and committed. */
  private def run(sql: String)(bind: java.sql.PreparedStatement => Unit): Unit =
    LiveDb
      .transaction(config)(SqlStatements.update(sql)(bind))
      .fold(
        e => throw new java.lang.AssertionError(s"$e"),
        _ => ()
      )

  protected def policyAt(
      over: DeclassificationPolicy.Over,
      version: Int,
      text: Option[String],
      kept: Label,
      checks: Option[ujson.Value],
      status: InMemoryDocuments.Status
  ): Unit =
    run(
      s"""INSERT INTO grit.declassification_policies
         |       (over, version, classification_id, text, checks, status, on_publish, set_at, set_by)
         |VALUES (?, ?, ${SqlLabels.Interned}, ?, ?::jsonb, ?, ?, now(), 'local')""".stripMargin
    ) { ps =>
      ps.setString(1, over.written)
      ps.setInt(2, version)
      SqlLabels.bind(ps, 3, kept)
      ps.setString(5, text.orNull)
      ps.setString(6, checks.map(ujson.write(_)).orNull)
      val onPublish = status match {
        case InMemoryDocuments.Status.Draft => None
        case InMemoryDocuments.Status.Withdraw => Some("withdraw")
        case InMemoryDocuments.Status.Keep => Some("keep")
      }
      ps.setString(7, if (onPublish.isEmpty) "draft" else "published")
      ps.setString(8, onPublish.orNull)
    }

  protected def application(
      pointer: Pointer,
      version: Int,
      over: Option[DeclassificationPolicy.Over]
  ): Unit =
    run(
      """INSERT INTO grit.declassification_applications (pointer, version, over, set_at, set_by)
        |VALUES (?::jsonb, ?, ?, now(), 'local')""".stripMargin
    ) { ps =>
      ps.setString(
        1,
        ujson.Obj("arguments" -> pointer.arguments, "tool" -> ToolName.value(pointer.tool)).render()
      )
      ps.setInt(2, version)
      ps.setString(3, over.map(_.written).orNull)
    }

  protected def audited(v: DocumentVersionId): Option[InMemoryDocuments.Audit] =
    LiveDb
      .transaction(config)(
        SqlStatements.many(
          """SELECT source_version, policy_version, application_version, attempt
          |  FROM grit.declassifications WHERE document_version = ?""".stripMargin
        )(_.setLong(1, DocumentVersionId.value(v))) { rs =>
          (
            source = DocumentVersionId
              .of(rs.getLong("source_version"))
              .getOrElse(throw new java.lang.AssertionError("a source version")),
            policy = rs.getInt("policy_version"),
            application = rs.getInt("application_version"),
            attempt = rs.getInt("attempt")
          )
        }
      )
      .fold(e => throw new java.lang.AssertionError(s"$e"), _.headOption)
}
