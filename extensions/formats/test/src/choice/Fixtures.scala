package grit.formats.choice

import java.io.OutputStream
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.time.Instant
import java.util.zip.{ZipEntry, ZipOutputStream}

import grit.core.clock.SetClock
import grit.core.files.{FileRange, FileRead, Format, Formats, ReadLimits, Understood, Unread}
import grit.prose.form.{Block, Doc}
import grit.prose.markdown.Markdown

/** Files a test writes, each to a temporary directory of its own, and read back. */
object Fixtures {

  /** A file named `name` that `write` fills. */
  def written(name: String)(write: OutputStream^ => Unit): Path = {
    val dir = Files.createTempDirectory("grit-formats")
    dir.toFile.deleteOnExit()
    val path = dir.resolve(name)
    val out = Files.newOutputStream(path)
    try write(out)
    finally out.close()
    path.toFile.deleteOnExit()
    path
  }

  /** A file holding `bytes`, named `name`. */
  def file(name: String, bytes: Seq[Byte]): Path =
    written(name)(out => bytes.foreach(b => out.write(b.toInt)))

  /** A zip of `entries`, each a name and its text, in order. */
  def zipOf(name: String, entries: Seq[(String, String)]): Path = written(name) { out =>
    val zip = new ZipOutputStream(out)
    entries.foreach { (name, text) =>
      zip.putNextEntry(new ZipEntry(name))
      zip.write(text.getBytes(UTF_8))
      zip.closeEntry()
    }
    zip.finish()
  }

  def zip(name: String, entries: (String, String)*): Path = zipOf(name, entries)

  /** `format`'s reading of the file at `path`, under `limits`, on a clock that never moves. */
  def read(
      format: Format,
      path: Path,
      limits: ReadLimits = ReadLimits.Default
  ): Either[Unread, Understood] =
    open(path)(f => format.read(f, limits))

  /** `formats`' reading of `range` of the file at `path`, its sender claiming it `claimed`. */
  def readAll(
      formats: Formats,
      path: Path,
      range: FileRange = FileRange.Start,
      claimed: Option[String] = None
  ): Either[Unread, FileRead] =
    open(path, claimed)(f => f.read(formats, range))

  private def open[A](path: Path, claimed: Option[String] = None)(
      body: grit.core.files.OpenFile^ => A
  ): A =
    grit.core.files.OpenFile
      .reading(path, path.getFileName.toString, claimed, new SetClock(Instant.EPOCH))(body)
      .fold(e => throw new java.lang.AssertionError(e), identity)

  /** `understood`'s blocks, their notes dropped. */
  def blocks(understood: Either[Unread, Understood]): Either[Unread, Vector[Block[Nothing]]] =
    understood.map(_.doc.unnoted.blocks)

  /** `block` as the model reads it. */
  def plainText(block: Block[Nothing]): String = Markdown.write(Doc(Vector(block)))
}
