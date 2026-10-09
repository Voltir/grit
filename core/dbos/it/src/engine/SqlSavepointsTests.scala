package grit.dbos.engine

import java.time.Instant
import java.util.concurrent.atomic.AtomicReference

import scala.concurrent.duration.*
import scala.util.{Try, Using}

import grit.core.document.{DocLabel, DocText, DocWeight, DocumentTerms}
import grit.core.id.{DocKey, PluginName}
import grit.core.place.{Namespace, Place}
import grit.core.store.StoreError
import grit.core.visibility.{Clearance, Label}
import grit.dbos.sql.{LiveDb, SqlDocuments, SqlSavepoints, SqlTombstones, TestPostgres}

import utest.*

/** What a savepoint keeps of a transaction in Postgres when a statement under it fails (two
  * transactions writing one document's key at once, the second under a savepoint) or its body
  * throws.
  */
object SqlSavepointsTests extends TestSuite {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_savepoints")
    LiveEngine.open(c, "test").close()
    c
  }

  private def got[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private val plugin = got(PluginName.of("raced"))
  private val terms =
    got(DocumentTerms.of(got(DocLabel.of("raced")), DocWeight.Unscaled, 30.days, 100))
  private val keeper = new SqlDocuments(new SqlTombstones).keeper(plugin, terms)
  private val here = Place.under(Namespace.Task, Vector("savepoints"))
  private val At = Instant.parse("2026-03-01T00:00:00Z")

  /** Every transaction's clearance: what it writes is kept, and read back, at public. */
  private val Public = Clearance.of(Label.Public)

  /** Writes `body` under `k`, publicly, in the transaction given. */
  private def write(k: String, body: String)(using grit.core.store.Tx^) =
    keeper.write(got(DocKey.of(k)), Label.Public, here, got(DocText.of(body)), ujson.Obj(), At)

  /** The current text under `k`, read in a transaction of its own. */
  private def current(k: String): Option[String] =
    LiveDb
      .transaction(config, Public)(keeper.current(got(DocKey.of(k)), Label.Public))
      .fold(e => throw new java.lang.AssertionError(s"$e"), _.map(d => DocText.value(d.text)))

  /** Whether another backend of this database waits on a lock. */
  private def waiting(): Boolean =
    LiveDb.connected(config) { c =>
      Using.resource(
        c.prepareStatement(
          """SELECT count(*) FROM pg_stat_activity
            | WHERE datname = current_database() AND wait_event_type = 'Lock'""".stripMargin
        )
      ) { ps =>
        Using.resource(ps.executeQuery())(rs => rs.next() && rs.getInt(1) > 0)
      }
    }

  val tests = Tests {
    test("a write raced under a savepoint is a DatabaseError, and its transaction goes on") {
      LiveDb.transaction(config, Public)(write("k", "before")).fold(e => sys.error(s"$e"), _ => ())
      val second = new AtomicReference[Option[Try[(String, Unit)]]](None)
      LiveDb
        .transaction(config, Public) {
          write("k", "first").fold(e => sys.error(s"$e"), _ => ())
          val racer = new Thread(() =>
            second.set(Some(Try(LiveDb.transaction(config, Public) {
              val raced = SqlSavepoints.atomic(write("k", "second").map(_ => ()))
              val after = write("after", "after")
              (
                raced.fold(
                  {
                    case StoreError.DatabaseError(_) => "DatabaseError"
                    case other => other.toString
                  },
                  _ => "written"
                ),
                after.fold(e => sys.error(s"after: $e"), _ => ())
              )
            })))
          )
          racer.start()
          val until = System.currentTimeMillis() + 10000
          while (!waiting() && System.currentTimeMillis() < until) Thread.sleep(20)
          // Committed here, with the second writer waiting on the key's row.
          racer
        }
        .join(10000)
      (second.get.map(_.map(_._1)), current("k"), current("after")) ==>
        (Some(scala.util.Success("DatabaseError")), Some("first"), Some("after"))
    }

    test(
      "a body that throws under a savepoint is rolled back to it, its exception rethrown, and its transaction goes on"
    ) {
      val thrown = LiveDb.transaction(config, Public) {
        val caught = Try(
          SqlSavepoints.atomic[Unit] {
            write("thrown", "thrown").fold(e => sys.error(s"$e"), _ => ())
            throw new IllegalStateException("the body broke")
          }
        )
        write("went-on", "went on").fold(e => sys.error(s"$e"), _ => ())
        caught.failed.map(e => (e.getClass.getSimpleName, e.getMessage)).toOption
      }
      (thrown, current("thrown"), current("went-on")) ==> (
        Some(("IllegalStateException", "the body broke")),
        None,
        Some("went on")
      )
    }
  }
}
