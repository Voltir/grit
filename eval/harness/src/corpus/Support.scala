package grit.eval.harness.corpus

/** How much of a reply a shown part of its window carries: the share, in [0, 1], of the
  * reply's distinctive words the part contains. A lexical heuristic, until people's labels
  * measure it.
  */
opaque type Support = Double

object Support {

  /** `reply`'s support in `shown`: of the reply's distinctive words (numbers, capitalised
    * words, and words of five letters or more, none of them in [[Stop]] or among `asked`'s
    * words), the share `shown` contains; 0 when the reply has none. Words are runs of letters
    * and digits, compared case-folded.
    */
  def of(reply: String, asked: String, shown: String): Support = {
    val said = words(asked).map(_.toLowerCase).toSet
    val distinct = words(reply)
      .filter(w => w.forall(_.isDigit) || w.head.isUpper || w.length >= 5)
      .map(_.toLowerCase)
      .filterNot(w => Stop.contains(w) || said.contains(w))
      .distinct
    if (distinct.isEmpty) 0.0
    else {
      val there = words(shown).map(_.toLowerCase).toSet
      distinct.count(there.contains).toDouble / distinct.size
    }
  }

  /** The support at or over which a part counts as used: 0.3. A lexical heuristic's line,
    * until people's labels measure it.
    */
  val Used: Support = 0.3

  /** `parts`' `k` most supported, most first, a tie in the order given; a part with no
    * support is never among them. None when `k` is under 1.
    */
  def top(parts: Vector[Part], k: Int): Vector[(Part, Support)] =
    parts.flatMap(p => p.support.map(p -> _)).sortBy(-_._2).take(k)

  /** The support written as `value`; `None` outside [0, 1]. */
  def read(value: Double): Option[Support] = Option.when(value >= 0 && value <= 1)(value)

  extension (s: Support) {
    def value: Double = s

    /** At or over [[Used]]. */
    def used: Boolean = s >= Used
  }

  /** Words never distinctive, however long or capitalised: English function words and the
    * commonest verbs, lower-case.
    */
  val Stop: Set[String] = Set(
    "a",
    "about",
    "above",
    "after",
    "again",
    "against",
    "all",
    "also",
    "an",
    "and",
    "any",
    "are",
    "as",
    "at",
    "be",
    "because",
    "been",
    "before",
    "being",
    "below",
    "between",
    "both",
    "but",
    "by",
    "can",
    "could",
    "did",
    "do",
    "does",
    "doing",
    "down",
    "during",
    "each",
    "every",
    "few",
    "for",
    "from",
    "further",
    "had",
    "has",
    "have",
    "having",
    "he",
    "her",
    "here",
    "hers",
    "him",
    "his",
    "how",
    "i",
    "if",
    "in",
    "into",
    "is",
    "it",
    "its",
    "just",
    "may",
    "me",
    "might",
    "more",
    "most",
    "must",
    "my",
    "no",
    "nor",
    "not",
    "now",
    "of",
    "off",
    "on",
    "once",
    "only",
    "or",
    "other",
    "others",
    "our",
    "ours",
    "out",
    "over",
    "own",
    "same",
    "shall",
    "she",
    "should",
    "since",
    "so",
    "some",
    "still",
    "such",
    "than",
    "that",
    "the",
    "their",
    "theirs",
    "them",
    "then",
    "there",
    "these",
    "they",
    "thing",
    "things",
    "think",
    "this",
    "those",
    "through",
    "to",
    "too",
    "under",
    "until",
    "up",
    "upon",
    "us",
    "very",
    "want",
    "was",
    "we",
    "well",
    "were",
    "what",
    "when",
    "where",
    "whether",
    "which",
    "while",
    "who",
    "whom",
    "whose",
    "why",
    "will",
    "with",
    "within",
    "without",
    "would",
    "yes",
    "yet",
    "you",
    "your",
    "yours",
    "really",
    "maybe",
    "perhaps",
    "already",
    "always",
    "never",
    "anyone",
    "everyone",
    "someone",
    "something",
    "anything",
    "everything",
    "nothing",
    "going",
    "looks",
    "looking",
    "seems",
    "sounds",
    "thanks",
    "thank",
    "please",
    "sorry",
    "right",
    "around",
    "though",
    "although",
    "however",
    "probably",
    "actually",
    "basically"
  )

  private def words(text: String): Vector[String] =
    text.split("[^\\p{L}\\p{N}]+").toVector.filter(_.nonEmpty)
}
