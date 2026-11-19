package grit.projects.artifact

/** Where an artifact sits among its siblings, the artifacts of its parent (or, with none, those
  * with none): they are listed in rank order. The letters a–z, compared as text, never ending in
  * `a`, so a rank always fits before any other.
  */
opaque type Rank = String

object Rank {

  /** 32: the most characters a rank holds. */
  val Longest: Int = 32

  /** `s` as a rank, or why not: empty, holding a character outside a–z, ending in `a`, or over
    * [[Longest]] characters.
    */
  def of(s: String): Either[String, Rank] =
    if (s.isEmpty) Left("a rank is empty")
    else if (s.exists(c => c < 'a' || c > 'z')) Left(s"a rank holds the letters a–z alone: $s")
    else if (s.endsWith("a")) Left(s"a rank does not end in a: $s")
    else if (s.length > Longest) Left(s"a rank holds at most $Longest characters: $s")
    else Right(s)

  def value(r: Rank): String = r

  /** `n`: the rank a head of the old form, which holds none, reads as. */
  val Middle: Rank = "n"

  /** The letters an append steps on in: once they are all `z`, it halves the room left. */
  private val Steps: String = "zzzz"

  given Ordering[Rank] = Ordering.String

  /** A rank after `before` and before `after`, either absent meaning that end (both absent:
    * `n`), or why not: `before` not below `after`, or no rank between them within [[Longest]]
    * characters.
    */
  def between(before: Option[Rank], after: Option[Rank]): Either[String, Rank] =
    (before, after) match {
      case (Some(b), Some(a)) if b >= a => Left(s"rank $b is not below rank $a")
      case (Some(b), None) if !b.startsWith(Steps) =>
        // An append steps on in the first four letters rather than halving the room left, so
        // the appends a parent's children take stay at four letters for about 228,000 of them.
        val base = b.take(Steps.length).padTo(Steps.length, 'a')
        val next = base.foldLeft(0)((n, c) => n * 26 + (c - 'a')) + 1
        val text = (0 until Steps.length).reverse
          .map(i => ('a' + next / math.pow(26, i).toInt % 26).toChar)
          .mkString
        Right(text.take(text.lastIndexWhere(_ != 'a') + 1))
      case _ =>
        val r = mid(before.getOrElse(""), after)
        if (r.length > Longest)
          Left(
            s"no rank of at most $Longest characters fits between " +
              s"${before.getOrElse("the start")} and ${after.getOrElse("the end")}"
          )
        else Right(r)
    }

  /** `siblings`' ranks, in their order, with a newcomer's inserted at `at` (taken as 0 below 0,
    * and as their count above it): theirs kept and the newcomer's [[between]] its neighbours'; or,
    * when none fits there, one rank more than they hold, ascending and evenly spread over the
    * ranks of the shortest length that holds them all.
    */
  def among(siblings: Vector[Rank], at: Int): Vector[Rank] = {
    val (before, after) = siblings.splitAt(at.max(0))
    between(before.lastOption, after.headOption)
      .fold(_ => spread(siblings.size + 1), r => (before :+ r) ++ after)
  }

  /** `k` ranks, ascending, evenly spread over the ranks of the shortest length that holds `k`;
    * none when `k` is not above 0.
    */
  private[artifact] def spread(k: Int): Vector[Rank] =
    if (k < 1) Vector.empty
    else {
      val length = Iterator.from(1).find(l => BigInt(26).pow(l) > k).getOrElse(1)
      val room = BigInt(26).pow(length)
      Vector.tabulate(k) { i =>
        val at = (room * (i + 1) + (k + 1) / 2) / (k + 1)
        val text = Vector
          .iterate(at, length)(_ / 26)
          .map(d => ('a' + (d % 26).toInt).toChar)
          .reverse
          .mkString
        // Its trailing `a`s dropped keeps the order: nothing ranks between `b` and `ba`.
        text.take(text.lastIndexWhere(_ != 'a') + 1)
      }
    }

  /** The rank `between` gives, of any length, `lo` (possibly empty) below `hi`, none ending in
    * `a`: each is at most one character longer than the longer bound.
    */
  private def mid(lo: String, hi: Option[String]): String = {
    // Past its end, `lo` reads as `a`s.
    val shared =
      hi.fold(0)(h => h.indices.takeWhile(i => lo.lift(i).getOrElse('a') == h(i)).size)
    if (shared > 0) hi.fold("")(_.take(shared)) + mid(lo.drop(shared), hi.map(_.drop(shared)))
    else {
      val low = lo.headOption.fold(0)(_ - 'a')
      val high = hi.flatMap(_.headOption).fold(26)(_ - 'a')
      if (high - low > 1) ('a' + (low + high) / 2).toChar.toString
      else
        hi match {
          case Some(h) if h.length > 1 => h.take(1)
          case _ => ('a' + low).toChar.toString + mid(lo.drop(1), None)
        }
    }
  }
}
