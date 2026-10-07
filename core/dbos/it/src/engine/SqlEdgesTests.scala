package grit.dbos.engine

import java.sql.DriverManager

import scala.util.Using

import grit.core.edge.{Desk, EdgeDirectory, EdgesContract, ToolRequests}
import grit.core.id.{ConversationId, PrincipalId}
import grit.core.place.Place
import grit.core.store.{Origin, Tx}
import grit.core.visibility.{Clearance, Visibility}
import grit.dbos.sql.{LiveDb, Opener, SqlEdgeDirectory, SqlToolRequests, TestPostgres}

import dev.dbos.transact.DBOSClient
import org.postgresql.ds.PGSimpleDataSource

/** The edges contract, kept by the SQL requests, directory and desks against a real
  * Postgres. Each desk holds a connection until it is killed or the suite's JVM ends.
  */
object SqlEdgesTests extends EdgesContract {

  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_edges")
    LiveEngine.open(c, "test").close()
    c
  }

  private lazy val dataSource = {
    val ds = new PGSimpleDataSource()
    ds.setURL(config.jdbcUrl)
    ds.setUser(config.user)
    ds.setPassword(config.password)
    ds
  }

  private lazy val client = new DBOSClient(dataSource)

  protected val requests: ToolRequests = new SqlToolRequests()
  protected val directory: EdgeDirectory = new SqlEdgeDirectory()

  protected def transaction[A](body: (Tx^) ?=> A): A = LiveDb.transaction(config)(body)

  protected def transaction[A](clearance: Clearance, visibility: Visibility)(
      body: (Tx^) ?=> A
  ): A =
    Using.resource(DriverManager.getConnection(config.jdbcUrl, config.user, config.password)) {
      conn =>
        conn.setAutoCommit(false)
        try {
          val a = body(using new Opener(visibility).at(clearance, conn))
          conn.commit()
          a
        } catch { case e: Throwable => conn.rollback(); throw e }
    }

  protected def conversation(name: String): ConversationId =
    LiveDb.conversation(config, Origin.Task("edges", name)).id

  protected def desk(places: Set[Place]): Desk^ =
    SqlDesk.open(
      config,
      dataSource,
      client,
      PrincipalId.Local,
      places,
      LiveEngine.Identity,
      LiveDb.Trialled
    ) match {
      case Right(d) => d
      case Left(e) => sys.error(s"no desk: $e")
    }

  protected def kill(desk: Desk^): Unit = desk match {
    case d: SqlDesk => d.close()
    case _ => sys.error("not a SqlDesk")
  }
}
