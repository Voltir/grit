package grit.std.near

/** Which strings of a set are nearest one asked for, by text alone. */
object Near {

  /** At most `most` of `among`, nearest `asked` first, never `asked` itself: first those that
    * extend `asked` or that it extends, the closer in length first; then those within an edit
    * distance of a third of `asked`'s length, rounded down, the smaller first; ties in string
    * order. Empty when none is that near.
    */
  def nearest(asked: String, among: Seq[String], most: Int): Vector[String] = {
    val others = among.toVector.distinct.filterNot(_ == asked)
    val extending = others
      .filter(k => k.startsWith(asked) || asked.startsWith(k))
      .sortBy(k => ((k.length - asked.length).abs, k))
    val within = asked.length / 3
    val close = others
      .filterNot(extending.contains)
      .map(k => k -> distance(asked, k))
      .filter(_._2 <= within)
      .sortBy((k, d) => (d, k))
      .map(_._1)
    (extending ++ close).take(most)
  }

  /** The Levenshtein distance between `a` and `b`. */
  private def distance(a: String, b: String): Int = {
    // The table's rows, one per character of `a`: each cell from the one left of it (the second of
    // `done`), the one above (`up`) and the one above-left (`diagonal`).
    val first: Vector[Int] = Vector.range(0, b.length + 1)
    val last = a.toVector.zipWithIndex.foldLeft(first) { (row: Vector[Int], at: (Char, Int)) =>
      val pairs: Vector[(Char, (Int, Int))] = b.toVector.zip(row.zip(row.drop(1)))
      pairs
        .foldLeft((Vector(at._2 + 1), at._2 + 1)) {
          (done: (Vector[Int], Int), cell: (Char, (Int, Int))) =>
            val (diagonal, up) = cell._2
            val swap = if (cell._1 == at._1) 0 else 1
            val here = math.min(math.min(done._2 + 1, up + 1), diagonal + swap)
            (done._1 :+ here, here)
        }
        ._1
    }
    last.lastOption.getOrElse(a.length)
  }
}
