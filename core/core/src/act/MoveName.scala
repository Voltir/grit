package grit.core.act

/** A move's name within its run, and its steps': `move:{name}`, then `move:{name}:{phase}`. 1 to
  * 200 characters, none a control character or `:`.
  */
opaque type MoveName = String

object MoveName {

  /** `text`, or why not: empty, over 200 characters, or holding a control character or `:`. */
  def of(text: String): Either[String, MoveName] =
    if (text.isEmpty) Left("a move's name is not empty")
    else if (text.length > 200) Left("a move's name is at most 200 characters")
    else if (text.exists(c => c == ':' || c.isControl))
      Left("a move's name holds no ':' and no control character")
    else Right(text)

  def value(name: MoveName): String = name
}
