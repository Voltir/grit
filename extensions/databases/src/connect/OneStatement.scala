package grit.databases.connect

/** A statement's text with no `;` but trailing ones: at most one statement, whatever the driver
  * splits at, since only `;` separates statements in Postgres. A `;` inside a string or a
  * comment is refused too.
  */
opaque type OneStatement = String

object OneStatement {

  /** `text` trimmed, its trailing `;`s dropped, or `None` when that is blank or holds a `;`. */
  def of(text: String): Option[OneStatement] = {
    val body = text.trim.reverse.dropWhile(c => c == ';' || c.isWhitespace).reverse
    Option.when(body.nonEmpty && !body.contains(';'))(body)
  }

  def text(statement: OneStatement): String = statement
}
