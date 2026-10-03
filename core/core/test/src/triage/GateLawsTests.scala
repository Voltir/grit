package grit.core.triage

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.id.QuestionName
import grit.core.period.Probability

import utest.*

/** The gate algebra's laws, checked exhaustively over a small domain: three readings (two
  * yes/no's and a choice's key), each unanswered or answered at 0.2, 0.5 or 0.8, and every
  * gate up to two levels deep over a bound at least or below one half on each.
  */
object GateLawsTests extends TestSuite {

  private def n(text: String): QuestionName =
    QuestionName.of(text).getOrElse(throw new java.lang.AssertionError(text))

  private val half = Probability.clamped(0.5)

  private val (qa, qb, qc) = (n("a"), n("b"), n("c"))
  private val readings: Vector[Reading] =
    Vector(Reading.Yes(qa), Reading.Yes(qb), Reading.Key(qc, "k"))

  /** Each reading unanswered (`None`) or answered at a value, in its question's kind. */
  private val values: Vector[Option[Double]] = Vector(None, Some(0.2), Some(0.5), Some(0.8))

  private def answered(a: Option[Double], b: Option[Double], c: Option[Double]) =
    VectorMap.from(
      a.map(qa -> Answer.YesNo(_)).toVector ++ b.map(qb -> Answer.YesNo(_)) ++
        c.map(v => qc -> Answer.Choice("k", Vector(Answer.Weight("k", v)), 0.0))
    )

  private val answerSets: Vector[VectorMap[QuestionName, Answer]] =
    for { a <- values; b <- values; c <- values } yield answered(a, b, c)

  /** Every answer set that answers at least what `answers` does, with the same values. */
  private def extensions(
      answers: VectorMap[QuestionName, Answer]
  ): Vector[VectorMap[QuestionName, Answer]] =
    answerSets.filter(more => answers.forall((k, v) => more.get(k).contains(v)))

  private val leaves: Vector[Gate] =
    readings.flatMap(r =>
      Vector(Gate.Holds(Bound.AtLeast(r, half)), Gate.Holds(Bound.Below(r, half)))
    )

  private val depthOne: Vector[Gate] =
    leaves ++ Vector(Gate.Open) ++
      (for { x <- leaves; y <- leaves } yield Vector(Gate.all(x, y), Gate.either(x, y))).flatten

  private val depthTwo: Vector[Gate] =
    depthOne ++
      (for { x <- depthOne; y <- depthOne } yield Vector(Gate.all(x, y), Gate.either(x, y))).flatten

  private def failures(c: Gate.Checked): Vector[Gate.Failed] = c match {
    case Gate.Checked.Fails(first, rest) => first +: rest
    case Gate.Checked.Passes | Gate.Checked.Unread(_) => Vector.empty
  }

  private def unread(c: Gate.Checked): Option[Reading] = c match {
    case Gate.Checked.Unread(reading) => Some(reading)
    case Gate.Checked.Passes | Gate.Checked.Fails(_, _) => None
  }

  /** Each `(case, gate, answers)` in `cases` where `law` does not hold; empty when it holds. */
  private def broken[A](cases: Vector[A])(law: A => Boolean): Vector[A] =
    cases.filterNot(law).take(3)

  private val pairs: Vector[(Gate, Gate)] = for { x <- depthOne; y <- depthOne } yield (x, y)

  val tests = Tests {
    test("reads of all and of either is the union of their parts' reads, in order") {
      broken(pairs)((x, y) =>
        Gate.all(x, y).reads.toVector == (x.reads ++ y.reads).toVector &&
          Gate.either(x, y).reads.toVector == (x.reads ++ y.reads).toVector
      ) ==> Vector.empty
    }

    test(
      "all fails when any part fails, with every part's failures in order, whatever is unread; otherwise is Unread at its first unread part; otherwise passes"
    ) {
      broken(for { (x, y) <- pairs; a <- answerSets } yield (x, y, a)) { (x, y, a) =>
        val parts = Vector(x.check(a), y.check(a))
        val expected =
          parts.flatMap(failures) match {
            case first +: rest => Gate.Checked.Fails(first, rest)
            case _ =>
              parts.flatMap(unread).headOption.fold(Gate.Checked.Passes)(Gate.Checked.Unread(_))
          }
        Gate.all(x, y).check(a) == expected
      } ==> Vector.empty
    }

    test(
      "either passes when any part passes; otherwise is Unread at its first unread part; otherwise fails with every part's failures in order"
    ) {
      broken(for { (x, y) <- pairs; a <- answerSets } yield (x, y, a)) { (x, y, a) =>
        val parts = Vector(x.check(a), y.check(a))
        val expected =
          if (parts.contains(Gate.Checked.Passes)) Gate.Checked.Passes
          else
            parts.flatMap(unread).headOption match {
              case Some(r) => Gate.Checked.Unread(r)
              case None =>
                parts.flatMap(failures) match {
                  case first +: rest => Gate.Checked.Fails(first, rest)
                  case _ => Gate.Checked.Passes
                }
            }
        Gate.either(x, y).check(a) == expected
      } ==> Vector.empty
    }

    test("Open and all() pass everything; all(g) checks as g, and either(g, g) decides as g") {
      broken(for { g <- depthTwo; a <- answerSets } yield (g, a)) { (g, a) =>
        Gate.Open.check(a) == Gate.Checked.Passes && Gate.all().check(a) == Gate.Checked.Passes &&
        Gate.all(g).check(a) == g.check(a) && Gate.either(g, g).drafts(a) == g.drafts(a)
      } ==> Vector.empty
    }

    test("grouping changes no result: all and either are associative") {
      broken(for { x <- leaves; y <- depthOne; z <- leaves; a <- answerSets } yield (x, y, z, a)) {
        (x, y, z, a) =>
          Gate.all(Gate.all(x, y), z).check(a) == Gate.all(x, Gate.all(y, z)).check(a) &&
          Gate.either(Gate.either(x, y), z).check(a) == Gate.either(x, Gate.either(y, z)).check(a)
      } ==> Vector.empty
    }

    test(
      "Unread names a reading the gate reads and the answers do not answer, and every failed bound reads one the gate reads"
    ) {
      broken(for { g <- depthTwo; a <- answerSets } yield (g, a)) { (g, a) =>
        val checked = g.check(a)
        unread(checked).forall(r => g.reads.contains(r) && r.in(a).isEmpty) &&
        failures(checked).forall(f => g.reads.contains(f.bound.reading))
      } ==> Vector.empty
    }

    test("answering more never turns Passes or Fails into anything else") {
      broken(for { g <- depthTwo; a <- answerSets } yield (g, a)) { (g, a) =>
        g.drafts(a)
          .forall(decided => extensions(a).forall(more => g.drafts(more).contains(decided)))
      } ==> Vector.empty
    }
  }
}
