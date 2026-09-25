package grit.core.host

/** One edit to a file: its passage `oldText`, replaced by `newText`. */
final case class Replace(oldText: String, newText: String)

object Replace {

  /** `text`, the file `path`, with every one of `edits` applied, and where each landed.
    * Every `oldText` is matched against `text` as it was before any edit, exactly: no
    * whitespace or quote is loosened. Each must occur exactly once (occurrences that overlap
    * each other count separately), and no two may overlap. All apply or none does.
    *
    * Line endings are compared as `\n`: `\r\n` and a lone `\r` in `text` or an edit match
    * `\n`. The result keeps `text`'s line ending (`\r\n` when its first line break is
    * one) and its byte order mark.
    *
    * Refused, naming the first failing edit by its index from 0 (all emptiness first, then
    * each edit in order, then overlaps in file order): an empty `oldText`; one not found
    * ([[EditError.Numbered]] when it would be found with each line's [[LineNumbers]] prefix
    * taken off); one found more than once; two that overlap; and edits that leave the text
    * as it was, which includes none at all.
    */
  def onto(
      path: RelPath,
      text: String,
      edits: Seq[Replace]
  ): Either[EditError, (String, Edited)] = {
    val (bom, body) =
      if (text.startsWith("﻿")) ("﻿", text.drop(1)) else ("", text)
    val crlf = {
      val lf = body.indexOf('\n')
      lf > 0 && body.charAt(lf - 1) == '\r'
    }
    val base = lf(body)
    val wanted = edits.toVector.map(e => Replace(lf(e.oldText), lf(e.newText)))
    wanted.indexWhere(_.oldText.isEmpty) match {
      case i if i >= 0 => Left(EditError.EmptyOld(path, i))
      case _ =>
        wanted.zipWithIndex
          .foldLeft[Either[EditError, Vector[Found]]](Right(Vector.empty)) { case (acc, (e, i)) =>
            acc.flatMap { found =>
              positions(base, e.oldText) match {
                case Vector() =>
                  val numbered = LineNumbers.strip(e.oldText).exists(positions(base, _).nonEmpty)
                  Left(if (numbered) EditError.Numbered(path, i) else EditError.NotFound(path, i))
                case Vector(at) => Right(found :+ Found(i, at, e))
                case many => Left(EditError.Repeated(path, i, many.size))
              }
            }
          }
          .flatMap { found =>
            val sorted = found.sortBy(_.at)
            sorted
              .zip(sorted.drop(1))
              .find((a, b) => a.at + a.edit.oldText.length > b.at) match {
              case Some((a, b)) => Left(EditError.Overlap(path, a.index, b.index))
              case None =>
                val out = sorted.foldRight(base) { (f, acc) =>
                  acc.substring(0, f.at) + f.edit.newText +
                    acc.substring(f.at + f.edit.oldText.length)
                }
                if (out == base) Left(EditError.NoChange(path))
                else {
                  val starts = sorted
                    .foldLeft((Vector.empty[Int], 0)) { case ((so, shift), f) =>
                      (so :+ (f.at + shift), shift + f.edit.newText.length - f.edit.oldText.length)
                    }
                    ._1
                  val at = starts.map(s => 1 + out.substring(0, s).count(_ == '\n'))
                  val ended = if (crlf) out.replace("\n", "\r\n") else out
                  Right((bom + ended, Edited(at)))
                }
            }
          }
    }
  }

  /** An edit found once, at `at` in the base text; `index` is its place in the request. */
  private final case class Found(index: Int, at: Int, edit: Replace)

  private def lf(s: String): String = s.replace("\r\n", "\n").replace('\r', '\n')

  /** Every index where `needle` starts in `hay`, overlapping ones included. */
  private def positions(hay: String, needle: String): Vector[Int] =
    Iterator
      .iterate(hay.indexOf(needle))(i => hay.indexOf(needle, i + 1))
      .takeWhile(_ >= 0)
      .toVector
}

/** Where a file's edits landed: the line, counting from 1, on which each replacement starts
  * in the new file, in file order.
  */
final case class Edited(at: Vector[Int])

/** Why a file could not be edited; nothing was changed. `edit` is the index, from 0, of the
  * edit that failed. `message` is what the model reads.
  */
enum EditError {
  case Host(error: HostError)
  case EmptyOld(path: RelPath, edit: Int)
  case NotFound(path: RelPath, edit: Int)

  /** `oldText` was not found, but is, without the number and tab [[LineNumbers]] shows
    * before each line: it was copied from `read`'s output with them.
    */
  case Numbered(path: RelPath, edit: Int)
  case Repeated(path: RelPath, edit: Int, times: Int)
  case Overlap(path: RelPath, edit: Int, other: Int)
  case NoChange(path: RelPath)

  /** What failed, and what to send instead. */
  def message: String = this match {
    case Host(error) => error.message
    case EmptyOld(p, i) =>
      s"edits[$i].oldText is empty; give the exact text in ${RelPath.value(p)} to replace."
    case NotFound(p, i) =>
      s"edits[$i].oldText was not found in ${RelPath.value(p)}. It must match the file " +
        "exactly, including whitespace and line breaks; read the file again to copy it."
    case Numbered(p, i) =>
      s"edits[$i].oldText was not found in ${RelPath.value(p)}: each of its lines starts " +
        "with the line number and tab `read` shows, which are not part of the file. Send " +
        "oldText, and newText, without them."
    case Repeated(p, i, n) =>
      s"edits[$i].oldText occurs $n times in ${RelPath.value(p)}; it must occur exactly " +
        "once. Include more of the surrounding lines to make it unique."
    case Overlap(p, i, j) =>
      s"edits[$i] and edits[$j] overlap in ${RelPath.value(p)}. Merge them into one edit, " +
        "or target separate passages."
    case NoChange(p) => s"No change made to ${RelPath.value(p)}: the edits leave it as it was."
  }
}
