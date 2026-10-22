package grit.host.blobs

import java.io.ByteArrayInputStream
import java.nio.channels.FileChannel
import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Path, StandardOpenOption}
import java.time.Instant

import scala.jdk.CollectionConverters.*

import grit.core.host.{Blobs, BlobsContract}

import utest.*

/** The blobs contract, kept by [[LocalBlobs]] in a temporary directory. */
object LocalBlobsTests extends BlobsContract {
  protected def fresh[A](body: Blobs^ => A): A = LocalBlobsDirectoryTests.inDir { root =>
    body(LocalBlobsDirectoryTests.opened(root))
  }
}

/** [[LocalBlobs]]' directories: what it leaves pending, and what it clears. */
object LocalBlobsDirectoryTests extends TestSuite {

  /** A fresh directory, given to `body`, deleted after. */
  def inDir[A](body: Path => A): A = {
    val dir = Files.createTempDirectory("grit-blobs")
    try body(dir)
    finally Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete)
  }

  def opened(root: Path): LocalBlobs = LocalBlobs.open(root) match {
    case Right(b) => b
    case Left(e) => throw new java.lang.AssertionError(e.toString)
  }

  private def names(dir: Path): Vector[String] =
    if (!Files.isDirectory(dir)) Vector.empty
    else {
      val s = Files.list(dir)
      try s.toList.asScala.map(_.getFileName.toString).toVector.sorted
      finally s.close()
    }

  val tests = Tests {
    test("nothing is left pending after a receive, kept, not kept or refused") {
      inDir { root =>
        val blobs = opened(root)
        def in(s: String) = new ByteArrayInputStream(s.getBytes("UTF-8"))
        val _ = blobs.receive(in("kept"), 10)(_.keep())
        val _ = blobs.receive(in("dropped"), 10)(_.sha256)
        val _ = blobs.receive(in("eleven byte"), 10)(_.keep())
        (names(root.resolve("incoming")), names(root.resolve("blobs")).size) ==> (Vector.empty, 1)
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
