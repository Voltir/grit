package grit.host.blobs

import java.io.{IOException, InputStream}
import java.nio.channels.{FileChannel, OverlappingFileLockException}
import java.nio.file.{
  FileAlreadyExistsException,
  Files,
  Path,
  StandardCopyOption,
  StandardOpenOption
}
import java.security.MessageDigest
import java.time.Instant

import scala.jdk.CollectionConverters.*

import grit.core.host.{BlobError, Blobs, Pending}
import grit.core.id.Sha256

/** [[Blobs]] in `root`, a directory of this machine: each blob the file `blobs/<sha256>`,
  * received into `incoming/` and moved in whole.
  */
final class LocalBlobs private (root: Path) extends Blobs {
  import LocalBlobs.{failed, listed, words}

  private val stored = root.resolve(LocalBlobs.Stored)
  private val incoming = root.resolve(LocalBlobs.Incoming)

  def receive[A](in: InputStream^, max: Long)(body: Pending^ => A): Either[BlobError, A] =
    (try Right(Files.createTempFile(incoming, "receive-", ".part"))
    catch { case e: IOException => Left(failed(e)) }).flatMap { temp =>
      try
        written(in, max, temp).map { (sha, n) =>
          body(new Pending {
            def sha256: Sha256 = sha
            def bytes: Long = n
            def keep(): Either[BlobError, Unit] = moved(temp, sha)
          })
        }
      finally {
        // Gone already when it was kept.
        try { val _ = Files.deleteIfExists(temp) }
        catch { case _: IOException => () }
      }
    }

  def at(sha256: Sha256): Option[Path] =
    Some(stored.resolve(sha256.hex)).filter(Files.isRegularFile(_))

  def kept(): Either[BlobError, Vector[Sha256]] =
    listed(stored).map(_.flatMap(p => Sha256.read(p.getFileName.toString)))

  def remove(sha256: Sha256): Either[BlobError, Unit] =
    try { val _ = Files.deleteIfExists(stored.resolve(sha256.hex)); Right(()) }
    catch { case e: IOException => Left(failed(e)) }

  def clearPending(before: Instant): Either[BlobError, Int] =
    listed(incoming).flatMap { all =>
      try
        Right(all.count { p =>
          Files.getLastModifiedTime(p).toInstant.isBefore(before) && Files.deleteIfExists(p)
        })
      catch { case e: IOException => Left(failed(e)) }
    }

  /** `in`'s bytes written to `to`, their digest and count, reading no more than one past `max`. */
  private def written(in: InputStream^, max: Long, to: Path): Either[BlobError, (Sha256, Long)] =
    (try Right(Files.newOutputStream(to))
    catch { case e: IOException => Left(failed(e)) }).flatMap { out =>
      val digest = MessageDigest.getInstance("SHA-256")
      var total = 0L
      var stopped: Option[BlobError] = None
      var more = true
      try {
        while (more && stopped.isEmpty) {
          // A short read is the end: readNBytes blocks until it has them all or `in` ends.
          (try Right(in.readNBytes((max + 1 - total).min(64L * 1024).toInt))
          catch { case e: IOException => Left(BlobError.Source(words(e))) }) match {
            case Left(e) => stopped = Some(e)
            case Right(chunk) =>
              total += chunk.length
              if (total > max) stopped = Some(BlobError.TooLarge(max))
              else if (chunk.isEmpty) more = false
              else {
                digest.update(chunk)
                try out.write(chunk)
                catch { case e: IOException => stopped = Some(failed(e)) }
              }
          }
        }
      } finally {
        try out.close()
        catch { case e: IOException => stopped = stopped.orElse(Some(failed(e))) }
      }
      stopped.toLeft(()).flatMap { _ =>
        Sha256
          .of(digest.digest().toVector)
          .map(_ -> total)
          .toRight(BlobError.Failed("the digest is not 32 bytes"))
      }
    }

  /** `temp` moved in as `sha`'s blob, in one rename; one kept already stays. */
  private def moved(temp: Path, sha: Sha256): Either[BlobError, Unit] = {
    val target = stored.resolve(sha.hex)
    if (Files.isRegularFile(target)) Right(())
    else
      try { val _ = Files.move(temp, target, StandardCopyOption.ATOMIC_MOVE); Right(()) }
      catch {
        // Kept meanwhile, by another receive of the same bytes.
        case _: FileAlreadyExistsException => Right(())
        case e: IOException => Left(failed(e))
      }
  }
}

object LocalBlobs {

  /** Blobs in `root`, its `blobs` and `incoming` directories made when missing; or why not. */
  def open(root: Path): Either[BlobError, LocalBlobs] =
    try {
      val _ = Files.createDirectories(root.resolve(Stored))
      val _ = Files.createDirectories(root.resolve(Incoming))
      Right(new LocalBlobs(root))
    } catch { case e: IOException => Left(failed(e)) }

  /** Each `grit-drive-*` directory in `temp` whose `.lock` file beside it no process holds a lock
    * on, removed with its lock file, how many: the copies an earlier grit's Drive edge read in a
    * directory of its own and left when its process was killed. One whose lock is held, one with
    * no lock file, and anything else, are left as they are.
    */
  def leftovers(temp: Path): Either[BlobError, Int] =
    listed(temp).map(_.filter { p =>
      val name = p.getFileName.toString
      name.startsWith("grit-drive-") && name.endsWith(".lock")
    }.count(unheld))

  private val Stored = "blobs"
  private val Incoming = "incoming"

  /** `lock`'s directory and `lock` removed, when no process holds a lock on it; whether they were.
    * Opening a lock file this process held would let go its lock (POSIX locks), but nothing here
    * takes one any more.
    */
  private def unheld(lock: Path): Boolean =
    try {
      val channel = FileChannel.open(lock, StandardOpenOption.WRITE)
      try {
        val taken =
          try Option(channel.tryLock())
          catch { case _: OverlappingFileLockException => None }
        taken.exists { _ =>
          cleared(lock.resolveSibling(lock.getFileName.toString.stripSuffix(".lock")))
          Files.deleteIfExists(lock)
        }
      } finally channel.close()
    } catch { case _: IOException => false }

  /** `dir` removed with what is in it, as far as this machine allows. */
  private def cleared(dir: Path): Unit =
    if (Files.isDirectory(dir)) {
      val walk = Files.walk(dir)
      try
        walk.toList.asScala.reverse.foreach { p =>
          try { val _ = Files.deleteIfExists(p) }
          catch { case _: IOException => () }
        }
      finally walk.close()
    }

  /** The entries of `dir`. */
  private def listed(dir: Path): Either[BlobError, Vector[Path]] =
    try {
      val s = Files.list(dir)
      try Right(s.toList.asScala.toVector)
      finally s.close()
    } catch { case e: IOException => Left(failed(e)) }

  private def failed(e: IOException): BlobError = BlobError.Failed(words(e))

  /** An exception as the operator reads it: its class, and its message when it has one. */
  private def words(e: IOException): String =
    Option(e.getMessage).fold(e.getClass.getSimpleName)(m => s"${e.getClass.getSimpleName}: $m")
}
