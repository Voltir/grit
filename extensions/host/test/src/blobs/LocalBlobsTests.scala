package grit.host.blobs

import java.io.{ByteArrayInputStream, IOException, InputStream}
import java.nio.channels.FileChannel
import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Path, StandardOpenOption}
import java.time.Instant

import scala.jdk.CollectionConverters.*

import grit.core.host.BlobError
import grit.core.id.Sha256

import utest.*

/** [[LocalBlobs]] in a temporary directory. */
object LocalBlobsTests extends TestSuite {

  /** A fresh directory, given to `body`, deleted after. */
  private def inDir[A](body: Path => A): A = {
    val dir = Files.createTempDirectory("grit-blobs")
    try body(dir)
    finally Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete)
  }

  private def opened(root: Path): LocalBlobs = LocalBlobs.open(root) match {
    case Right(b) => b
    case Left(e) => throw new java.lang.AssertionError(e.toString)
  }

  private def bytes(s: String): InputStream = new ByteArrayInputStream(s.getBytes("UTF-8"))

  private def names(dir: Path): Vector[String] =
    if (!Files.isDirectory(dir)) Vector.empty
    else {
      val s = Files.list(dir)
      try s.toList.asScala.map(_.getFileName.toString).toVector.sorted
      finally s.close()
    }

  val tests = Tests {
    test("receiving more than max keeps nothing and runs no body") {
      inDir { root =>
        val blobs = opened(root)
        var ran = false
        val got = blobs.receive(bytes("eleven byte"), 10) { _ => ran = true }
        (got, ran, blobs.kept(), names(root.resolve("incoming"))) ==>
          (Left(BlobError.TooLarge(10)), false, Right(Vector.empty), Vector.empty)
      }
    }
    test("a kept blob is whole under its digest, and keeping the same bytes again leaves it") {
      inDir { root =>
        val blobs = opened(root)
        val sha = Sha256.digest("hello".getBytes("UTF-8").toVector)
        val first = blobs.receive(bytes("hello"), 10)(p => (p.sha256, p.bytes, p.keep()))
        val second = blobs.receive(bytes("hello"), 10)(_.keep())
        val text = blobs.at(sha).map(Files.readString(_))
        (first, second, text, blobs.kept(), names(root.resolve("incoming"))) ==>
          (
            Right((sha, 5L, Right(()))),
            Right(Right(())),
            Some("hello"),
            Right(Vector(sha)),
            Vector.empty
          )
      }
    }
    test("a pending blob not kept is gone when its body returns") {
      inDir { root =>
        val blobs = opened(root)
        val sha = blobs.receive(bytes("draft"), 10)(_.sha256)
        (sha.map(blobs.at), blobs.kept(), names(root.resolve("incoming"))) ==>
          (Right(None), Right(Vector.empty), Vector.empty)
      }
    }
    test("a source failing partway keeps nothing, saying so") {
      inDir { root =>
        val blobs = opened(root)
        val failing = new InputStream {
          def read(): Int = throw new IOException("reset")
        }
        (blobs.receive(failing, 10)(_.keep()), names(root.resolve("incoming"))) ==>
          (Left(BlobError.Source("IOException: reset")), Vector.empty)
      }
    }
    test("remove deletes a kept blob, and is nothing for one not kept") {
      inDir { root =>
        val blobs = opened(root)
        val sha = Sha256.digest("gone".getBytes("UTF-8").toVector)
        val _ = blobs.receive(bytes("gone"), 10)(_.keep())
        val before = blobs.kept()
        (before, blobs.remove(sha), blobs.remove(sha), blobs.at(sha), blobs.kept()) ==>
          (Right(Vector(sha)), Right(()), Right(()), None, Right(Vector.empty))
      }
    }
    test("clearPending removes the pending blobs last written before the cutoff, and no newer") {
      inDir { root =>
        val blobs = opened(root)
        val incoming = root.resolve("incoming")
        val cut = Instant.parse("2026-10-22T12:00:00Z")
        val old = Files.writeString(incoming.resolve("old.part"), "cut short")
        Files.setLastModifiedTime(old, FileTime.from(cut.minusSeconds(60)))
        val fresh = Files.writeString(incoming.resolve("fresh.part"), "arriving")
        Files.setLastModifiedTime(fresh, FileTime.from(cut.plusSeconds(60)))
        (blobs.clearPending(cut), names(incoming)) ==> (Right(1), Vector("fresh.part"))
      }
    }
    test("leftovers removes each grit-drive directory whose lock no process holds") {
      inDir { temp =>
        def scratch(name: String): Path = {
          val dir = Files.createDirectory(temp.resolve(name))
          Files.writeString(dir.resolve("read-1.part"), "copy")
          Files.createFile(temp.resolve(s"$name.lock"))
        }
        val _ = scratch("grit-drive-dead")
        val live = scratch("grit-drive-live")
        val _ = Files.createDirectory(temp.resolve("other"))
        // Held by another JVM in life; a lock this one holds is held to its own tryLock too.
        val channel = FileChannel.open(live, StandardOpenOption.WRITE)
        try {
          val lock = channel.lock()
          val removed = LocalBlobs.leftovers(temp)
          lock.release()
          (removed, names(temp)) ==>
            (Right(1), Vector("grit-drive-live", "grit-drive-live.lock", "other"))
        } finally channel.close()
      }
    }
  }
}
