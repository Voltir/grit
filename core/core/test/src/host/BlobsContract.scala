package grit.core.host

import java.io.{ByteArrayInputStream, IOException, InputStream}
import java.nio.file.Files
import java.time.Instant
import java.util.concurrent.{CyclicBarrier, TimeUnit}

import grit.core.id.Sha256

import utest.*

/** The contract every [[Blobs]] keeps, run against [[TempBlobs]] in core and `LocalBlobs` in
  * grit.host.
  */
abstract class BlobsContract extends TestSuite {

  /** `body` over empty blobs, removed after. */
  protected def fresh[A](body: Blobs^ => A): A

  private def bytes(s: String): InputStream = new ByteArrayInputStream(s.getBytes("UTF-8"))

  private def sha(s: String): Sha256 = Sha256.digest(s.getBytes("UTF-8").toVector)

  val tests = Tests {
    test("receiving more than max keeps nothing and runs no body") {
      fresh { blobs =>
        var ran = false
        val got = blobs.receive(bytes("eleven byte"), 10) { _ => ran = true }
        (got, ran, blobs.kept()) ==> (Left(BlobError.TooLarge(10)), false, Right(Vector.empty))
      }
    }
    test("a kept blob is whole under its digest, and keeping the same bytes again leaves it") {
      fresh { blobs =>
        val first = blobs.receive(bytes("hello"), 10)(p => (p.sha256, p.bytes, p.keep()))
        val second = blobs.receive(bytes("hello"), 10)(_.keep())
        val text = blobs.at(sha("hello")).map(Files.readString(_))
        (first, second, text, blobs.kept()) ==>
          (
            Right((sha("hello"), 5L, Right(()))),
            Right(Right(())),
            Some("hello"),
            Right(Vector(sha("hello")))
          )
      }
    }
    test("a pending blob not kept is gone when its body returns") {
      fresh { blobs =>
        val got = blobs.receive(bytes("draft"), 10)(_.sha256)
        (got, blobs.at(sha("draft")), blobs.kept()) ==>
          (Right(sha("draft")), None, Right(Vector.empty))
      }
    }
    test("a source failing partway keeps nothing, saying so") {
      fresh { blobs =>
        val failing = new InputStream {
          def read(): Int = throw new IOException("reset")
        }
        (blobs.receive(failing, 10)(_.keep()), blobs.kept()) ==>
          (Left(BlobError.Source("IOException: reset")), Right(Vector.empty))
      }
    }
    test("remove deletes a kept blob, and is nothing for one not kept") {
      fresh { blobs =>
        val _ = blobs.receive(bytes("gone"), 10)(_.keep())
        val before = blobs.kept()
        (before, blobs.remove(sha("gone")), blobs.remove(sha("gone")), blobs.at(sha("gone"))) ==>
          (Right(Vector(sha("gone"))), Right(()), Right(()), None)
      }
    }
    test("removeAllBut removes every blob not named and keeps the named, counting them") {
      fresh { blobs =>
        for (s <- Vector("one", "two", "three"))
          blobs.receive(bytes(s), 10)(_.keep()) ==> Right(Right(()))
        (blobs.removeAllBut(Set(sha("two"), sha("never kept"))), blobs.kept()) ==>
          (Right(2), Right(Vector(sha("two"))))
      }
    }
    test("a removal failing partway leaves the blobs removed before it removed and the rest kept") {
      fresh { blobs =>
        for (s <- Vector("one", "two", "three"))
          blobs.receive(bytes(s), 10)(_.keep()) ==> Right(Right(()))
        val removes = new java.util.concurrent.atomic.AtomicInteger(0)
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
    test("clearing removes the pending blobs written before the time given, counting them") {
      fresh { blobs =>
        val later = Instant.now().plusSeconds(3600)
        val got = blobs.receive(bytes("draft"), 10) { p =>
          val none = blobs.clearPending(Instant.EPOCH)
          val cleared = blobs.clearPending(later)
          (none, cleared, p.keep().isLeft)
        }
        (got, blobs.clearPending(later), blobs.kept()) ==>
          (Right((Right(0), Right(1), true)), Right(0), Right(Vector.empty))
      }
    }

    test("receives on many threads at once each keep their own blob") {
      fresh { blobs =>
        val n = 32
        val start = new CyclicBarrier(n)
        val inside = new CyclicBarrier(n)
        val kept = new Array[Option[Either[BlobError, Either[BlobError, Unit]]]](n)
        val threads = (0 until n).map { i =>
          Thread.ofPlatform().start { () =>
            val _ = start.await(5, TimeUnit.SECONDS)
            kept(i) = Some(blobs.receive(bytes(s"blob $i"), 100) { p =>
              val _ = inside.await(5, TimeUnit.SECONDS)
              p.keep()
            })
          }
        }
        threads.foreach(_.join())
        (kept.count(_ == Some(Right(Right(())))), blobs.kept().map(_.size)) ==> (n, Right(n))
      }
    }
  }
}
