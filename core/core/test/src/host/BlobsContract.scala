package grit.core.host

import java.io.{ByteArrayInputStream, IOException, InputStream}
import java.nio.file.Files
import java.time.Instant

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
  }
}
