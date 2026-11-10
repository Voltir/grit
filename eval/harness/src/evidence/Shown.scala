package grit.eval.harness.evidence

import grit.core.host.RelPath
import grit.eval.harness.bench.{Checked, PerPass, Standing, Tried}

/** A shape of the bench's own grading text, as its writer puts it in a line. Every shape the bench
  * writes is a case here, and a new one is added here alone: [[Shown.of]] scans for each case.
  */
enum GradeShape extends caps.Pure {

  /** A trial's pass-and-distance line, as [[grit.eval.harness.bench.Tried.verdict]] writes it,
    * anywhere in a line.
    */
  case Verdict

  /** A check's outcome or the checks' stop, as [[grit.eval.harness.bench.Checked.lines]] writes
    * them, a line each.
    */
  case CheckOutcome

  /** The bench summary's header line or one of its rows, as [[PerPass.lines]] writes them. */
  case Summary

  /** Whether `line` holds this shape. */
  def in(line: String): Boolean = this match {
    case Verdict => Tried.Verdicts.findFirstIn(line).isDefined
    case CheckOutcome =>
      Checked.Outcome.matches(line.trim) || line.trim.startsWith(Checked.Stopped)
    case Summary =>
      val columns = line.trim.split(" {2,}").toVector
      columns == PerPass.Header || (
        columns.size == PerPass.Header.size &&
          GradeShape.Counted.forall(i => columns.lift(i).exists(_.forall(_.isDigit)))
      )
  }
}

object GradeShape {

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
      val shapes = shapesIn(text)
      Option.when(shapes.nonEmpty)(GradeFound(path, shapes))
    }
    Either.cond(found.isEmpty, files, found)
  }

  /** Each grade shape some line of `text` holds, in [[GradeShape]]'s order. */
  private def shapesIn(text: String): Vector[GradeShape] = {
    val lines = text.linesIterator.toVector
    GradeShape.values.toVector.filter(shape => lines.exists(shape.in))
  }

  /** `standing`, when it holds none of the bench's grade shapes, so a coder may run under it; or
    * the shapes found, under [[StandingPath]].
    */
  def scanned(standing: Standing): Either[GradeFound, Standing] = {
    val shapes = shapesIn(standing.text)
    Either.cond(shapes.isEmpty, standing, GradeFound(StandingPath, shapes))
  }

  /** The path a standing text's grade shapes are found under: `standing.md`. */
  val StandingPath: RelPath = Proposal.path("standing.md")

  extension (shown: Shown) {

    /** Each file, its path and its text, in the order [[Shown.of]] was given them. */
    def files: Vector[(RelPath, String)] = shown
  }
}
