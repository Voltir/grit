package grit.core.id

/** One line of text with a cap on its length, as a name or key made by an edge or a plugin is. */
object Bounded {

  /** `s`, or why it is not `what`: empty, over `longest` characters, or holding a control
    * character (a line break among them).
    */
  def text(what: String, s: String, longest: Int): Either[String, String] =
    if (s.isEmpty || s.length > longest) Left(s"$what is 1 to $longest characters")
    else if (s.exists(_.isControl)) Left(s"$what holds no control character")
    else Right(s)
}
