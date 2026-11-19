package grit.projects.artifact

import scala.util.Random

import utest.*

object RankTests extends TestSuite {
  private def rank(s: String): Rank =
    Rank.of(s).fold(why => throw new java.lang.AssertionError(why), identity)

  /** A rank from `r`: up to [[Rank.Longest]] letters, often of a few letters alone, so that
    * bounds share prefixes, tie and leave no room.
    */
  private def drawn(r: Random^): Rank = {
    val letters = if (r.nextBoolean()) "abz" else "abcdefghijklmnopqrstuvwxyz"
    val length = 1 + r.nextInt(Rank.Longest)
    val text = Vector.fill(length)(letters(r.nextInt(letters.length))).mkString
    val kept = text.take(text.lastIndexWhere(_ != 'a') + 1)
    rank(if (kept.isEmpty) "b" else kept)
  }

  val tests = Tests {
    test(
      "between gives a rank strictly between its bounds, or refuses only bounds out of order or a bound of the longest length"
    ) {
      val r = new Random(7)
      val wrong = (1 to 3000).flatMap { _ =>
        val before = Option.when(r.nextInt(4) > 0)(drawn(r))
        val after = Option.when(r.nextInt(4) > 0)(drawn(r))
        val longest = (before ++ after).map(Rank.value(_).length).maxOption.getOrElse(0)
        val ordered = (before, after) match {
          case (Some(b), Some(a)) => Ordering[Rank].lt(b, a)
          case _ => true
        }
        Rank.between(before, after) match {
          case Right(got) =>
            val valid = Rank.of(Rank.value(got)) == Right(got)
            val above = before.forall(b => Ordering[Rank].lt(b, got))
            val below = after.forall(a => Ordering[Rank].lt(got, a))
            Option.when(!(ordered && valid && above && below))((before, after, Right(got)))
          case Left(why) =>
            Option.when(ordered && longest < Rank.Longest)((before, after, Left(why)))
        }
      }
      (wrong.take(3), Rank.between(None, None)) ==> (Vector.empty, Right(rank("n")))
    }
    test("between refuses bounds out of order, and no room past the longest rank, naming them") {
      val full = rank("z" * Rank.Longest)
      (
        Rank.between(Some(rank("p")), Some(rank("c"))),
        Rank.between(Some(rank("n")), Some(rank("n"))),
        Rank.between(Some(full), None)
      ) ==> (
        Left("rank p is not below rank c"),
        Left("rank n is not below rank n"),
        Left(s"no rank of at most 32 characters fits between ${"z" * 32} and the end")
      )
    }
    test("a rank after the last steps on in its first four letters, so appends stay short") {
      val appended = Iterator.iterate(rank("n"))(r => Rank.between(Some(r), None).getOrElse(r))
      val fiveThousand = appended.drop(5000).next()
      (
        Vector("n", "nazz", "nabcq", "zzzz").map(s => Rank.between(Some(rank(s)), None)),
        Rank.value(fiveThousand).length <= 5
      ) ==> (
        Vector(Right(rank("naab")), Right(rank("nb")), Right(rank("nabd")), Right(rank("zzzzn"))),
        true
      )
    }
    test("among keeps the siblings' ranks and puts the newcomer between its neighbours") {
      val siblings = Vector("h", "n", "u").map(rank)
      (
        Rank.among(siblings, 1).map(Rank.value),
        Rank.among(siblings, 3).map(Rank.value),
        Rank.among(siblings, 0).map(Rank.value),
        Rank.among(Vector.empty, 0).map(Rank.value)
      ) ==> (
        Vector("h", "k", "n", "u"),
        Vector("h", "n", "u", "uaab"),
        Vector("d", "h", "n", "u"),
        Vector("n")
      )
    }
    test("among re-spreads every rank evenly when none fits at the place asked, ties included") {
      val tied = Vector("n", "n", "n").map(rank)
      val full = Vector(rank("n"), rank("n" + "a" * (Rank.Longest - 2) + "b"))
      (
        Rank.among(tied, 1).map(Rank.value),
        Rank.among(full, 1).map(Rank.value)
      ) ==> (Vector("f", "k", "q", "v"), Vector("h", "n", "u"))
    }
    test("among re-spreads into ascending ranks of the shortest length that holds them") {
      val respread = (0 to 700).map(k => Rank.among(Vector.fill(k)(rank("n")), k / 2))
      val wrong = respread.zipWithIndex.filterNot { (ranks, k) =>
        val ascending = ranks.zip(ranks.drop(1)).forall((a, b) => Ordering[Rank].lt(a, b))
        val valid = ranks.forall(r => Rank.of(Rank.value(r)) == Right(r))
        ranks.size == k + 1 && ascending && valid
      }
      (
        wrong.map(_._2).take(3),
        respread(24).map(Rank.value(_).length).distinct,
        respread(25).map(Rank.value(_).length).max
      ) ==> (Vector.empty, Vector(1), 2)
    }
    test(
      "of refuses an empty rank, a letter outside a–z, a trailing a and one over 32 characters"
    ) {
      (
        Rank.of(""),
        Rank.of("nB"),
        Rank.of("na"),
        Rank.of("n" * 33).isLeft,
        Rank.of("nb").map(Rank.value)
      ) ==> (
        Left("a rank is empty"),
        Left("a rank holds the letters a–z alone: nB"),
        Left("a rank does not end in a: na"),
        true,
        Right("nb")
      )
    }
  }
}
