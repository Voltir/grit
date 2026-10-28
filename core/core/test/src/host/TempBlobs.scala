package grit.core.host

import java.io.{IOException, InputStream}
import java.nio.file.{Files, Path}
import java.time.Instant

import scala.jdk.CollectionConverters.*

import grit.core.id.Sha256

/** [[Blobs]] for tests, keeping [[BlobsContract]]: each blob a file in `dir`, a pending one held
  * in memory, written when its receive read it; one cleared by [[clearPending]] cannot be kept.
  */
final class TempBlobs(dir: Path) extends Blobs {

  /** Each pending blob's number and when it was written. */
  // Only ever replaced by a new immutable map, as a directory's listing would be, and only
  // under this object's lock, so receives on several threads lose none (as files would not).
  @caps.unsafe.untrackedCaptures
  private var pending = Map.empty[Long, Instant]

  // A number, as `pending`.
  @caps.unsafe.untrackedCaptures
  private var last = 0L

  /** A new pending blob's number, written now. */
  private def written(): Long = synchronized {
    last += 1
    pending = pending.updated(last, Instant.now())
    last
  }

  private def isPending(id: Long): Boolean = synchronized(pending.contains(id))

  private def done(id: Long): Unit = synchronized { pending = pending.removed(id) }

  def receive[A](in: InputStream^, max: Long)(body: Pending^ => A): Either[BlobError, A] =
    (try Right(in.readNBytes((max + 1).min(Int.MaxValue.toLong).toInt).toVector)
    catch {
      case e: IOException => Left(BlobError.Source(s"${e.getClass.getSimpleName}: ${e.getMessage}"))
    }).flatMap { read =>
      if (read.size > max) Left(BlobError.TooLarge(max))
      else {
        val id = written()
        try
          Right(body(new Pending {
            def sha256: Sha256 = Sha256.digest(read)
            def bytes: Long = read.size.toLong
            def keep(): Either[BlobError, Unit] =
              if (!isPending(id)) Left(BlobError.Failed("its pending blob was cleared"))
              else {
                val out = Files.newOutputStream(dir.resolve(sha256.hex))
                try read.foreach(b => out.write(b.toInt))
                finally out.close()
                Right(())
              }
          }))
        finally done(id)
      }
    }

  def at(sha256: Sha256): Option[Path] =
    Some(dir.resolve(sha256.hex)).filter(Files.isRegularFile(_))

  def kept(): Either[BlobError, Vector[Sha256]] = {
    val s = Files.list(dir)
    try Right(s.toList.asScala.toVector.flatMap(p => Sha256.read(p.getFileName.toString)))
    finally s.close()
  }

  def remove(sha256: Sha256): Either[BlobError, Unit] = {
    val _ = Files.deleteIfExists(dir.resolve(sha256.hex))
    Right(())
  }

  def clearPending(before: Instant): Either[BlobError, Int] = synchronized {
    val gone = pending.filter((_, written) => written.isBefore(before)).keySet
    pending = pending.removedAll(gone)
    Right(gone.size)
  }
}

object TempBlobs {

  /** `body` over blobs in a new temporary directory, deleted after. */
  def around[A](body: TempBlobs => A): A = {
    val dir = Files.createTempDirectory("grit-temp-blobs")
    try body(new TempBlobs(dir))
    finally Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete)
  }
}
