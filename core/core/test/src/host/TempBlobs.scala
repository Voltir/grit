package grit.core.host

import java.io.{IOException, InputStream}
import java.nio.file.{Files, Path}
import java.time.Instant

import scala.jdk.CollectionConverters.*

import grit.core.id.Sha256

/** [[Blobs]] for tests, keeping [[BlobsContract]]: each blob a file in `dir`, a pending one held
  * in memory. Nothing is ever pending past its block, so [[clearPending]] has none to clear.
  */
final class TempBlobs(dir: Path) extends Blobs {

  def receive[A](in: InputStream^, max: Long)(body: Pending^ => A): Either[BlobError, A] =
    (try Right(in.readNBytes((max + 1).min(Int.MaxValue.toLong).toInt).toVector)
    catch {
      case e: IOException => Left(BlobError.Source(s"${e.getClass.getSimpleName}: ${e.getMessage}"))
    }).flatMap { read =>
      if (read.size > max) Left(BlobError.TooLarge(max))
      else
        Right(body(new Pending {
          def sha256: Sha256 = Sha256.digest(read)
          def bytes: Long = read.size.toLong
          def keep(): Either[BlobError, Unit] = {
            val out = Files.newOutputStream(dir.resolve(sha256.hex))
            try read.foreach(b => out.write(b.toInt))
            finally out.close()
            Right(())
          }
        }))
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

  def clearPending(before: Instant): Either[BlobError, Int] = Right(0)
}

object TempBlobs {

  /** `body` over blobs in a new temporary directory, deleted after. */
  def around[A](body: TempBlobs => A): A = {
    val dir = Files.createTempDirectory("grit-temp-blobs")
    try body(new TempBlobs(dir))
    finally Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete)
  }
}
