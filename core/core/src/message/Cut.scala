package grit.core.message

/** Text cut to a budget for a reader, at a word's edge, with `…` where text was left out. */
object Cut {

  /** `text` whole when it has at most `chars` characters; else its start, ending at the end of
    * a word, then `…`, all within `chars`; a word alone longer than that is cut inside it,
    * never inside a character. Empty when `chars` is under 1 and `text` is not empty.
    */
  def head(text: String, chars: Int): String =
    if (text.length <= chars) text
    else if (chars < 1) ""
    else {
      val room = text.take(chars - 1) match {
        case r if r.lastOption.exists(_.isHighSurrogate) => r.dropRight(1)
        case r => r
      }
      val words =
        if (text.lift(room.length).exists(_.isWhitespace)) room
        else room.take(math.max(0, room.lastIndexWhere(_.isWhitespace)))
      (if (words.isBlank) room else words.stripTrailing) + "…"
    }

  /** [[head]] from the other end: `text`'s end, starting at the start of a word, after `…`. */
  def tail(text: String, chars: Int): String =
    // StringBuilder's reverse keeps each surrogate pair in order, so `head` reads a valid text.
    head(text.reverse, chars).reverse
}
