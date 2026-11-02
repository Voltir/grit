package grit.core.host

import grit.core.secret.Variable

/** The text a tool shows in a secret's place, `[redacted secret: VARIABLE]`: it names the
  * variable, never the value.
  */
object RedactionMark {

  private val Opening = "[redacted secret: "

  /** The mark standing for `variable`'s value. */
  def of(variable: Variable): String = s"$Opening${Variable.value(variable)}]"

  /** Whether `text` holds a mark, whichever variable it names. */
  def in(text: String): Boolean = text.contains(Opening)

  /** The mark as grit's words to a model show one: `[redacted secret: NAME]`. */
  val Example: String = of(Variable("NAME"))
}
