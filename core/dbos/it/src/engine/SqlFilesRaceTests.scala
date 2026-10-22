package grit.dbos.engine

import java.io.{ByteArrayInputStream, InputStream}
import java.time.Instant
import java.util.concurrent.{BrokenBarrierException, CyclicBarrier, TimeUnit, TimeoutException}

import scala.concurrent.duration.*

import grit.core.clock.SetClock
import grit.core.files.{FileArea, FileError, FileItem, FileOrigin, Incoming, PutKey}
import grit.core.host.TempBlobs
import grit.core.id.EdgeName
import grit.core.place.Place
import grit.core.store.{Jot, StoreError, Tx}
import grit.core.visibility.{Integrity, Provenance, Readers, Subject}
import grit.dbos.sql.{LiveDb, SqlFileCatalog, TestPostgres}

import utest.*

/** Puts racing for the area's room: the lock lets one at a time check it and keep. */
object SqlFilesRaceTests extends TestSuite {

  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_files_race")
    LiveEngine.open(c, "test").close()
    c
  }

  /** `inner`, each write waiting, once its body has returned and before it commits, for another
    * to reach the same point, for at most a second.
    */
  private final class Paired(inner: Jot, barrier: CyclicBarrier) extends Jot {
    def write[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      inner.write(subject) {
        val done = body
        try { val _ = barrier.await(1, TimeUnit.SECONDS) }
        catch { case _: TimeoutException | _: BrokenBarrierException => () }
        done
      }
  }

  private def source(text: String): Incoming = new Incoming {
    def declared: Option[Long] = None
    def read[A](body: InputStream^ => A): Either[String, A] =
      Right(body(new ByteArrayInputStream(text.getBytes("UTF-8"))))
  }

  val tests = Tests {
    test("two puts at once never keep more than the most the files may have together") {
      val start = Instant.parse("2026-10-22T09:00:00Z")
      val place =
        Place.read("service:drive").fold(e => throw new java.lang.AssertionError(e), identity)
      val item = FileItem(Provenance(place, Integrity.Member), Readers.AtPlace)
      val origin = FileOrigin(EdgeName("drive"), "d1", None, start)
      TempBlobs.around { blobs =>
        val area = new FileArea(
          new SqlFileCatalog,
          blobs,
          new Paired(SqlFilesTests.jot(config), new CyclicBarrier(2)),
          new SetClock(start),
          FileArea.Bounds(10.minutes, maxFile = 10, maxTotal = 12)
        )
        def put(n: Int, text: String): Either[FileError, Long] =
          area
            .put(PutKey.read(s"race:$n"), "f.txt", None, origin, item, source(text))(_.bytes)
            .map(_._1.bytes)
        @volatile var other: Option[Either[FileError, Long]] = None
        val t = Thread.ofVirtual().start(() => other = Some(put(1, "eight 1!")))
        val mine = put(2, "eight 2!")
        t.join()
        val kept = LiveDb.transaction(config)(new SqlFileCatalog().blobs()).map(_.values.sum)
        (Vector(Some(mine), other).flatten.count(_ == Left(FileError.Full)), kept) ==> (
          1,
          Right(8L)
        )
      }
    }
  }
}
