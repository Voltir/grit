package grit.core.act

import scala.compiletime.ops.string.Matches
import scala.compiletime.{constValue, constValueOpt, error}

/** A move's name within its run, and its steps': `ask:{name}`, `call:{name}` or `keep:{name}`,
  * then each with `:{phase}` after it. 1 to 200 characters, none a control character or `:`.
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

  /** The literal `name`, which [[of]] would read: a literal it would refuse, or a name that is
    * not a literal, does not compile.
    */
  inline def apply[N <: String & Singleton](inline name: N): MoveName =
    inline constValueOpt[N] match {
      case None => error("a move's name is a literal; MoveName.of reads one made at run time")
      case Some(_) =>
        inline if (constValue[Matches[N, "[^:\\p{Cc}]{1,200}"]]) name
        else error("a move's name is 1 to 200 characters, none of them ':' or a control character")
    }

  def value(name: MoveName): String = name
}
