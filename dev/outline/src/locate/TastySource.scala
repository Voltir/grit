package grit.outline.locate

import scala.util.Try

import dotty.tools.dotc.core.tasty.{AttributeUnpickler, TastyUnpickler}
import dotty.tools.tasty.{TastyFormat, TastyReader}

/** What a `.tasty` file records of the source it was compiled from: the source's path as the compiler was given it, and
  * the length of each of its lines.
  */
final case class TastySource(path: String, lineSizes: Vector[Int]) {

  /** Whether `path` names `file`, a path relative to a root: the compiler records an absolute path, so the last segments
    * decide.
    */
  def isOf(file: os.RelPath): Boolean = {
    val recorded = path.split('/').toVector.filter(_.nonEmpty)
    file.segments.nonEmpty && recorded.endsWith(file.segments)
  }
}

object TastySource {

  /** What `tasty` records of its source; none when the file cannot be read as TASTy, or it records no source path or no
    * positions.
    */
  def of(tasty: os.Path): Option[TastySource] =
    Try {
      val unpickler = TastyUnpickler(os.read.bytes(tasty))
      for {
        path <- unpickler
          .unpickle(
            new TastyUnpickler.SectionUnpickler[Option[String]](TastyFormat.AttributesSection) {
              def unpickle(reader: TastyReader, names: TastyUnpickler.NameTable): Option[String] =
                AttributeUnpickler(reader, names).attributes.sourceFile
            }
          )
          .flatten
        sizes <- unpickler.unpickle(
          new TastyUnpickler.SectionUnpickler[Vector[Int]](TastyFormat.PositionsSection) {
            // The section's head as `PositionUnpickler.ensureDefined` reads it: the line count, then each line's size.
            // `PositionUnpickler.lineSizes` is package-private to the compiler, so the head is read here.
            def unpickle(reader: TastyReader, names: TastyUnpickler.NameTable): Vector[Int] = {
              val lines = reader.readNat()
              // Each size takes a byte at least: a count past the section's end is not TASTy, not an array to allocate.
              require(lines <= reader.endAddr.index - reader.currentAddr.index)
              Vector
                .fill(lines)(reader.readLongNat())
                .map(n => if (n == 0xffffffffL) -1 else n.toInt)
            }
          }
        )
      } yield TastySource(path, sizes)
    }.toOption.flatten

  /** The line sizes the compiler records for a source of `text`: one per line, lines split at `\n` alone, each size in
    * UTF-16 chars without its `\n`.
    */
  def lineSizes(text: String): Vector[Int] =
    text.split("\n", -1).toVector.map(_.length)
}
