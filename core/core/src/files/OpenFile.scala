package grit.core.files

import java.io.IOException
import java.nio.file.{Files as JFiles, Path}
import java.util.zip.{ZipException, ZipFile}

import scala.jdk.CollectionConverters.*

import grit.core.clock.Clock

/** One file opened for reading: valid only within the block that opened it
  * ([[OpenFile.reading]]), which capture checking keeps it from outliving. Read-only.
  */
trait OpenFile extends caps.SharedCapability {

  /** The name its sender gave it. */
  def name: String

  /** Its size in bytes. */
  def bytes: Long

  /** What its bytes are ([[Kind.sniff]]). */
  def kind: Kind

  /** A path to its bytes on this machine, read-only, valid only within the block. */
  def path: Path

  /** Whether more than `limits.time` has passed since it was opened. */
  def late(limits: ReadLimits): Boolean

  /** `range` of it as the model reads it: the tree [[tree]] gives, then [[ModelText.of]]. */
  final def read(formats: Formats, range: FileRange): Either[Unread, FileRead] =
    tree(formats).map(ModelText.of(_, range))

  /** Its document as the first of `formats.of(kind)` that does not answer [[Unread.NotMine]]
    * reads it, or that format's refusal; [[Unread.TooLarge]] past `formats.limits.bytes`, read
    * by none; [[Unread.Unsupported]] when every format answers `NotMine`, or none is listed.
    */
  final def tree(formats: Formats): Either[Unread, Understood] =
    if (bytes > formats.limits.bytes) Left(Unread.TooLarge(bytes, formats.limits.bytes))
    else {
      val unsupported: Either[Unread, Understood] = Left(Unread.Unsupported(kind))
      formats
        .of(kind)
        .iterator
        .map(_.read(this, formats.limits))
        .find(_ != Left(Unread.NotMine))
        .getOrElse(unsupported)
    }
}

object OpenFile {

  /** `body` over the file at `at`, named `named` by its sender, who claimed it `claimed` (a
    * media type), timed from now on `clock`. `Left` with the cause, in words for the operator,
    * when it cannot be read.
    */
  def reading[A](at: Path, named: String, claimed: Option[String], clock: Clock^)(
      body: OpenFile^ => A
  ): Either[String, A] = {
    // Only the opening is caught here: an exception `body` throws is its own.
    val sized =
      try {
        val size = JFiles.size(at)
        val in = JFiles.newInputStream(at)
        val head =
          try in.readNBytes(Kind.Head).toVector
          finally in.close()
        Right((size, Kind.sniff(head, entries(at), claimed)))
      } catch {
        case e: IOException => Left(s"${e.getClass.getSimpleName} reading the file")
      }
    sized.map { (size, sniffed) =>
      val opened = clock.millis()
      val file = new OpenFile {
        def name: String = named
        def bytes: Long = size
        def kind: Kind = sniffed
        def path: Path = at
        def late(limits: ReadLimits): Boolean = clock.millis() - opened > limits.time.toMillis
      }
      body(file)
    }
  }

  /** A zip's entries' names, the first 10,000; none when it is not a zip that opens. */
  private def entries(path: Path): Vector[String] =
    try {
      val zip = new ZipFile(path.toFile)
      try zip.entries().asScala.take(10000).map(_.getName).toVector
      finally zip.close()
    } catch { case _: ZipException | _: IOException => Vector.empty }
}
