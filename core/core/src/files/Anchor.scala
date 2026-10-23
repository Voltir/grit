package grit.core.files

/** Where a passage was in a file's text: its first [[Anchor.Words]] words (`quote`, at most
  * [[Anchor.QuoteMost]] characters), up to [[Anchor.Context]] characters either side of them,
  * and where it began as a fraction of the text's length.
  */
final case class Anchor(quote: String, before: String, after: String, at: Double) {

  /** As a reader's `near` argument gives it: its position to four places, `|`, then its
    * quote between `⟪` and `⟫` within its context: `0.4213|the policy states ⟪All accounts are
    * reviewed every ninety days by⟫ the system owner`.
    */
  def written: String = {
    val position = BigDecimal(at.max(0.0).min(1.0)).setScale(4, BigDecimal.RoundingMode.HALF_UP)
    s"${position.toString}|$before⟪$quote⟫$after"
  }
}

object Anchor {

  /** The anchor `near` writes ([[Anchor.written]]), or why not, in words the model reads. */
  def read(near: String): Either[String, Anchor] = {
    val refused = Left("Give near as a piece's pointer gives it, copied whole.")
    val bar = near.indexOf('|')
    val open = near.indexOf('⟪', bar + 1)
    val close = near.lastIndexOf('⟫')
    if (bar < 0 || open < 0 || close < open) refused
    else
      near.substring(0, bar).trim.toDoubleOption.filter(a => a >= 0 && a <= 1) match {
        case None => refused
        case Some(at) =>
          Right(
            Anchor(
              near.substring(open + 1, close),
              near.substring(bar + 1, open),
              near.substring(close + 1),
              at
            )
          )
      }
  }

  /** How many words a quote holds. */
  val Words: Int = 8

  /** The most characters of a quote; past it, cut. */
  val QuoteMost: Int = 96

  /** How many characters either side of a quote an anchor keeps. */
  val Context: Int = 32

  /** The anchor of the passage of `text` from character `start` to `end`, each held within
    * the text. Its quote and context stop short of a `⟪` or `⟫`, which [[written]] marks its
    * quote with.
    */
  def of(text: String, start: Int, end: Int): Anchor = {
    val from = start.max(0).min(text.length)
    val to = end.max(from).min(text.length)
    val ends = Word.findAllMatchIn(text.substring(from, to)).map(from + _.end).take(Words).toVector
    val quoteEnd = ends.lastOption.getOrElse(from).min(from + QuoteMost)
    val quote = text.substring(from, quoteEnd).takeWhile(!Marks.contains(_))
    val before = text.substring((from - Context).max(0), from).reverse.takeWhile(!Marks.contains(_))
    val after = text
      .substring(from + quote.length, (from + quote.length + Context).min(text.length))
      .takeWhile(!Marks.contains(_))
    Anchor(quote, before.reverse, after, if (text.isEmpty) 0.0 else from.toDouble / text.length)
  }

  /** What [[written]] marks a quote with. */
  private val Marks: Set[Char] = Set('⟪', '⟫')

  /** Where `anchor`'s quote lies in `text` now, its first character and the one past its
    * last: exactly by its context, else nearest its position, else the best match at least
    * [[Alike]] alike; `None` when it is gone, or it quotes nothing.
    */
  def find(anchor: Anchor, text: String): Option[(Int, Int)] = {
    val quote = anchor.quote
    val want = math.round(anchor.at * text.length)
    def nearest(starts: Vector[Int]): Option[Int] = starts.minByOption(s => (s - want).abs)
    if (quote.isEmpty) None
    else {
      val hits = Iterator
        .iterate(text.indexOf(quote))(s => text.indexOf(quote, s + 1))
        .takeWhile(_ >= 0)
        .toVector
      val inContext = hits.filter { s =>
        text.startsWith(anchor.before, s - anchor.before.length) &&
        text.startsWith(anchor.after, s + quote.length)
      }
      nearest(inContext)
        .orElse(nearest(hits))
        .map(s => (s, s + quote.length))
        .orElse(fuzzy(quote, text, want))
    }
  }

  /** How alike a passage must be to a quote, as one less the share of its characters an
    * edit changes, to be found as it.
    */
  val Alike: Double = 0.8

  private val Word = """\S+""".r

  /** The run of `text`'s words, one fewer to one more than `quote` holds, most alike it and at
    * least [[Alike]] alike, nearest `want` among the equally alike.
    */
  private def fuzzy(quote: String, text: String, want: Long): Option[(Int, Int)] = {
    val q = quote.toLowerCase
    val n = Word.findAllMatchIn(quote).size
    val words = Word.findAllMatchIn(text).map(m => (m.start, m.end)).toVector
    val lower = text.toLowerCase
    // (how alike, distance from `want`, start, end) of the best so far.
    var best = Option.empty[(Double, Long, Int, Int)]
    for {
      size <- (n - 1).max(1) to n + 1
      i <- 0 to words.size - size
    } {
      val (start, end) = (words(i)._1, words(i + size - 1)._2)
      val most = q.length.max(end - start)
      val limit = (most * (1 - Alike)).toInt
      distance(q, lower, start, end, limit).foreach { d =>
        val alike = 1 - d.toDouble / most
        val off = (start - want).abs
        if (best.forall((a, o, _, _) => alike > a || (alike == a && off < o)))
          best = Some((alike, off, start, end))
      }
    }
    best.map((_, _, s, e) => (s, e))
  }

  /** The edit distance of `q` from `text`'s characters `start` until `end`, when at most
    * `limit`.
    */
  private def distance(q: String, text: String, start: Int, end: Int, limit: Int): Option[Int] = {
    val m = end - start
    if ((m - q.length).abs > limit) None
    else {
      // Two rows of the edit table, each column past `limit` of its diagonal held at `far`;
      // `row` is computed from `above`, then copied into it.
      val far = limit + 1
      val above = Array.tabulate(m + 1)(j => if (j <= limit) j else far)
      val row = Array.fill(m + 1)(far)
      var i = 1
      var within = true
      while (within && i <= q.length) {
        val (lo, hi) = ((i - limit).max(1), (i + limit).min(m))
        row(0) = if (i <= limit) i else far
        if (lo > 1) row(lo - 1) = far
        var least = row(0)
        var j = lo
        while (j <= hi) {
          val cost = if (q.charAt(i - 1) == text.charAt(start + j - 1)) 0 else 1
          val d = (above(j - 1) + cost).min(above(j) + 1).min(row(j - 1) + 1).min(far)
          row(j) = d
          least = least.min(d)
          j += 1
        }
        if (hi < m) row(hi + 1) = far
        Array.copy(row, 0, above, 0, m + 1)
        within = least <= limit
        i += 1
      }
      Option.when(within && above(m) <= limit)(above(m))
    }
  }
}
