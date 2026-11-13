package grit.dbos.engine

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.document.{DocLabel, DocRef, DocText, DocWeight, DocumentTerms, Written}
import grit.core.edge.{EdgeContracts, EdgeWatch, EdgeWatchContract}
import grit.core.id.{ClaimHolder, DocKey, DocumentVersionId, SessionId}
import grit.core.identity.Principal
import grit.core.place.Place
import grit.core.session.{Event, Session}
import grit.core.visibility.{Clearance, Integrity, Label, Visibility}
import grit.dbos.sql.TestPostgres
import grit.dbos.sql.{LiveDb, Opener, Pool, SqlDocuments, SqlIdentities, SqlSessions, SqlTombstones}

/** The watch contract, kept by the SQL listener against a real Postgres, a watch opened for each
  * loan on a database whose reader is enrolled once.
  */
object SqlEdgeWatchTests extends EdgeWatchContract {

  import EdgeContracts.*
  import EdgeWatchContract.got

  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_edge_watch")
    LiveEngine.open(c, "test").close()
    c
  }

  private lazy val reader: Principal.Person =
    LiveDb
      .transaction(config)(
        SqlIdentities.enroll(Set(Reader)).flatMap(_ => SqlIdentities.person(Reader))
      )
      .fold(e => throw new java.lang.AssertionError(e.toString), identity) match {
      case Some(p: Principal.Person) => p
      case other => throw new java.lang.AssertionError(s"the reader is no person: $other")
    }

  private val Terms: DocumentTerms =
    got(DocumentTerms.of(got(DocLabel.of("notes")), DocWeight.Unscaled, 2.minutes, 10))

  protected def watching[A](body: EdgeWatch^ => A): A = {
    val _ = reader
    val dataSource = Pool.open(config, readOnly = false)
    try {
      val w = new SqlWatch(config, dataSource, new Opener(Visibility.Shipped))
      try body(w)
      finally w.close()
    } finally dataSource.close()
  }

  protected def hold(key: DocKey, ref: DocRef, written: Instant): DocumentVersionId =
    LiveDb.transaction(config, Clearance.of(Label.Public)) {
      new SqlDocuments(new SqlTombstones)
        .keeper(EdgeWatchContract.Plugin, Terms)
        .write(
          key,
          Label.Public,
          Integrity.Member,
          Room,
          got(DocText.of("held")),
          ujson.Obj(),
          None,
          written,
          Set(ref)
        ) match {
        case Right(Written.Versioned(version, _, _)) => version
        case other => throw new java.lang.AssertionError(s"not versioned: $other")
      }
    }

  protected def commit(
      session: SessionId,
      holder: ClaimHolder,
      worktree: Place,
      at: Instant
  ): Event.Shape = {
    val shape = got(Event.Shape.committed(worktree, "c0ffee", None))
    val header = Session.Header(session, None, reader, Label.Public, worktree, Some(holder))
    val event = got(Event.of(1, at, Event.Kind.Commit, shape))
    LiveDb
      .transaction(config, Clearance.of(Label.Public))(
        new SqlSessions().record(header, Vector(event))
      )
      .fold(e => throw new java.lang.AssertionError(e.toString), _ => shape)
  }
}
