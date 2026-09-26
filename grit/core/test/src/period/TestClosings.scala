package grit.core.period

import grit.core.id.PeriodSeq

/** Closings for tests in any module: one with prose alone, or with balance lines. */
object TestClosings {

  /** A closing of `prose` and `outcome`, no changes, and an empty balance. */
  def prose(prose: String, outcome: Option[String] = None): Closing =
    Closing(
      Flows.of(prose, outcome, Vector.empty).getOrElse(throw new java.lang.AssertionError(prose)),
      Balance.empty
    )

  /** A line of `section` reading `text`, added at `since` and touched at `touched`. */
  def line(section: Section, text: String, since: Long, touched: Long): Line =
    (for {
      s <- PeriodSeq.of(since).toRight(s"period $since")
      t <- PeriodSeq.of(touched).toRight(s"period $touched")
      l <- Line.of(section, text, s, t)
    } yield l).fold(e => throw new java.lang.AssertionError(e), identity)

  /** The balance of `lines`, in order. */
  def balance(lines: Line*): Balance =
    Balance.of(lines.toVector).fold(e => throw new java.lang.AssertionError(e), identity)
}
