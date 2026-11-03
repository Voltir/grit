package grit.host.checkout

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*
import scala.util.Try

import grit.core.host.{HostError, Memory, RelPath}

/** [[Memory]] under `root`, read from Linux's `/proc`: each process's working directory and
  * resident set (`VmRSS`), and the machine's `MemAvailable`. A process whose working directory
  * this user cannot read, as another user's, or that ends while it is read, counts for nothing.
  */
final class LocalMemory(root: Path) extends Memory {

  private val checkout = new Checkout(root)

  def held(dir: RelPath): Either[HostError, Memory.Held] =
    for {
      at <- checkout.find(dir)
      _ <- Either.cond(Files.isDirectory(at), (), HostError.NotFound(dir))
      available <- kilobytes(LocalMemory.Proc.resolve("meminfo"), "MemAvailable:")
        .toRight(HostError.Failed("this machine does not say how much memory it has available"))
    } yield Memory.Held(resident(at), available * 1024)

  /** The resident bytes of every process working in `at` or below it. */
  private def resident(at: Path): Long =
    Try {
      val listing = Files.list(LocalMemory.Proc)
      try listing.iterator.asScala.toVector
      finally listing.close()
    }.getOrElse(Vector.empty)
      .filter(_.getFileName.toString.forall(_.isDigit))
      .flatMap { p =>
        Try(Files.readSymbolicLink(p.resolve("cwd"))).toOption
          .filter(_.startsWith(at))
          .flatMap(_ => kilobytes(p.resolve("status"), "VmRSS:"))
      }
      .sum * 1024

  /** The number of kilobytes the line of `file` starting `field` gives, if it gives one. */
  private def kilobytes(file: Path, field: String): Option[Long] =
    Try(Files.readAllLines(file).asScala.toVector).toOption
      .flatMap(_.find(_.startsWith(field)))
      .flatMap(_.stripPrefix(field).trim.takeWhile(_.isDigit).toLongOption)
}

private object LocalMemory {

  /** Where Linux shows its processes and its memory. */
  val Proc: Path = Path.of("/proc")
}
