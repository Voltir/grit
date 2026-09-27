package grit.host

import java.io.{BufferedReader, InputStreamReader}
import java.nio.charset.{CharacterCodingException, CodingErrorAction, StandardCharsets}
import java.nio.file.{Files, Path}

import scala.util.Using
import scala.util.control.NonFatal

import grit.core.host.{InstructionFile, Instructions}
import grit.core.place.Directory

/** [[Instructions]] over this machine's files. */
final class LocalInstructions extends Instructions {

  def around(dir: Directory): Vector[InstructionFile] = {
    val start = Path.of(Directory.value(dir))
    if (!Files.isDirectory(start)) Vector.empty
    else
      Iterator
        .iterate(Option(start))(_.flatMap(p => Option(p.getParent)))
        .takeWhile(_.nonEmpty)
        .flatten
        .toVector
        .reverse
        .flatMap(LocalInstructions.fileIn)
  }
}

private object LocalInstructions {

  /** `dir`'s instruction file: the first of [[Instructions.Names]] that is a regular file,
    * read; `None` when there is none, or it cannot be read as UTF-8.
    */
  def fileIn(dir: Path): Option[InstructionFile] =
    Instructions.Names.map(dir.resolve).find(Files.isRegularFile(_)).flatMap(read)

  /** `file`'s text up to [[Instructions.MaxBytes]] of UTF-8, cut before a character the cap
    * would split; `None` when it cannot be read or is not UTF-8 (as far as it is read).
    */
  private def read(file: Path): Option[InstructionFile] =
    try {
      val decoder = StandardCharsets.UTF_8
        .newDecoder()
        .onMalformedInput(CodingErrorAction.REPORT)
        .onUnmappableCharacter(CodingErrorAction.REPORT)
      Using.resource(
        new BufferedReader(new InputStreamReader(Files.newInputStream(file), decoder))
      ) { in =>
        val text = new StringBuilder
        var bytes = 0
        var cut = false
        var next = in.read()
        while (next >= 0 && !cut) {
          val high = next.toChar
          // A character outside the BMP is two chars and four bytes.
          val (chars, size) =
            if (Character.isHighSurrogate(high)) {
              val low = in.read()
              (if (low >= 0) s"$high${low.toChar}" else high.toString, 4)
            } else (high.toString, utf8Bytes(high))
          if (bytes + size > Instructions.MaxBytes) cut = true
          else {
            text.append(chars)
            bytes += size
            next = in.read()
          }
        }
        Some(InstructionFile(file.toString, text.toString, cut))
      }
    } catch {
      case _: CharacterCodingException => None
      case NonFatal(_) => None
    }

  /** How many bytes UTF-8 writes `c`, a character inside the BMP, in. */
  private def utf8Bytes(c: Char): Int =
    if (c < 0x80) 1 else if (c < 0x800) 2 else 3
}
