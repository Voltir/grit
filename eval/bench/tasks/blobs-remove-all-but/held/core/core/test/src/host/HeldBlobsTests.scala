package grit.core.host

import java.io.{ByteArrayInputStream, InputStream}
import java.time.Instant
import java.util.concurrent.atomic.AtomicInteger

import grit.core.id.Sha256

import utest.*

/** The bench's held tests of `Blobs.removeAllBut` (blobs-remove-all-but), over [[TempBlobs]]. */
object HeldBlobsTests extends TestSuite {

  private def bytes(s: String): InputStream = new ByteArrayInputStream(s.getBytes("UTF-8"))

  private def sha(s: String): Sha256 = Sha256.digest(s.getBytes("UTF-8").toVector)

  val tests = Tests {
    test("removeAllBut removes every blob not named, keeps the named, and counts the removed") {
      TempBlobs.around { blobs =>
        for (s <- Vector("one", "two", "three"))
          blobs.receive(bytes(s), 10)(_.keep()) ==> Right(Right(()))
        (blobs.removeAllBut(Set(sha("two"), sha("never kept"))), blobs.kept()) ==>
          (Right(2), Right(Vector(sha("two"))))
      }
    }

    test("a removal failing partway answers its failure, the blob removed before it gone and the rest kept") {
      TempBlobs.around { blobs =>
        for (s <- Vector("one", "two", "three"))
          blobs.receive(bytes(s), 10)(_.keep()) ==> Right(Right(()))
        val removes = new AtomicInteger(0)
        // `blobs`, but its second removal fails.
        val failing = new Blobs {
          def receive[A](in: InputStream^, max: Long)(body: Pending^ => A) =
            blobs.receive(in, max)(body)
          def at(s: Sha256) = blobs.at(s)
          def kept() = blobs.kept()
          def remove(s: Sha256) =
            if (removes.incrementAndGet() == 2) Left(BlobError.Failed("disk")) else blobs.remove(s)
          def clearPending(before: Instant) = blobs.clearPending(before)
        }
        (failing.removeAllBut(Set.empty), blobs.kept().map(_.size)) ==>
          (Left(BlobError.Failed("disk")), Right(2))
      }
    }
  }
}
