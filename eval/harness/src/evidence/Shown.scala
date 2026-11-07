package grit.eval.harness.evidence

import scala.util.matching.Regex

import grit.core.host.RelPath
import grit.eval.harness.bench.{Checked, PerPass, Tried}

/** A shape of the bench's own grading text, as its writer puts it in a line. Every shape the bench
  * writes is a case here, and a new one is added here alone: [[Shown.of]] scans for each case.
  */
enum GradeShape extends caps.Pure {

  /** A trial's pass-and-distance line, as [[Tried.verdict]] writes it, anywhere in a line. */
  case Verdict

  /** A check's outcome or the checks' stop, as [[Checked.lines]] writes them, a line each. */
  case CheckOutcome

  /** The bench summary's header line or one of its rows, as [[PerPass.lines]] writes them. */
  case Summary

  /** Whether `line` holds this shape. */
  def in(line: String): Boolean = this match {
    case Verdict => GradeShape.Verdicts.findFirstIn(line).isDefined
    case CheckOutcome =>
      GradeShape.Outcomes.matches(line.trim) || line.trim.startsWith("checks stopped: ")
    case Summary =>
      val columns = line.trim.split(" {2,}").toVector
      columns == PerPass.Header || (
        columns.size == PerPass.Header.size &&
          GradeShape.Counted.forall(i => columns.lift(i).exists(_.forall(_.isDigit)))
      )
  }
}

object GradeShape {
  private val Verdicts: Regex = "passed: (true|false); distance: ".r
  private val Outcomes: Regex = "check: .+ (passed|failed)".r

  /** The summary's columns that count trials, each a whole number in every row. */
  private val Counted: Vector[Int] = Vector("trials", "passed").map(PerPass.Header.indexOf)
}

/** A file an evidence checkout was to hold, and each grade shape found in it, in [[GradeShape]]'s
  * order, at least one.
  */
final case class GradeFound private[evidence] (path: RelPath, shapes: Vector[GradeShape])
    extends caps.Pure

/** An evidence checkout's files, each holding none of the bench's grade shapes. */
opaque type Shown = Vector[(RelPath, String)]

object Shown {

  /** `files` as an evidence checkout may hold them, or each that holds a grade shape, in order. */
  def of(files: Vector[(RelPath, String)]): Either[Vector[GradeFound], Shown] = {
    val found = files.flatMap { (path, text) =>
      val lines = text.linesIterator.toVector
      val shapes = GradeShape.values.toVector.filter(shape => lines.exists(shape.in))
      Option.when(shapes.nonEmpty)(GradeFound(path, shapes))
    }
    Either.cond(found.isEmpty, files, found)
  }

  extension (shown: Shown) {

    /** Each file, its path and its text, in the order [[Shown.of]] was given them. */
    def files: Vector[(RelPath, String)] = shown
  }
}
