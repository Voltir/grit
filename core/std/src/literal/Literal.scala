package grit.std.literal

import scala.quoted.{Expr, Quotes}

/** A string constant read, as the compiler runs, by the reader a type's `literal` names. */
object Literal {

  /** The value `read` makes of the constant `text`, as a constant. A compile error at `text`
    * with `read`'s reason when `read` refuses it, and "a literal is a string constant" when
    * `text` is not one. For a macro's implementation alone: `read` runs in the compiler, so a
    * `literal` called from a file `read` itself needs (its type's own, or a helper's it calls)
    * is a cyclic macro dependency and does not compile (docs/capture-checking.md, "Macros").
    */
  def checked(text: Expr[String], read: String -> Either[String, String])(using
      q: Quotes
  ): Expr[String] =
    text.value match {
      case None => q.reflect.report.errorAndAbort("a literal is a string constant", text)
      case Some(constant) =>
        read(constant) match {
          case Left(why) => q.reflect.report.errorAndAbort(why, text)
          case Right(value) => Expr(value)
        }
    }
}
