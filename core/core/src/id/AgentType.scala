package grit.core.id

/** An agent's type as a claim's holder keeps it whole and a method names it: 1 to [[Longest]]
  * characters that slugging leaves as they are ([[Slug.slugged]]), and not a hashed name's form,
  * eight hex digits alone or after a `-`.
  */
opaque type AgentType = String

object AgentType {

  /** 40: the most characters a type kept whole holds. */
  val Longest: Int = 40

  /** `text` as an agent type, or why not. */
  def of(text: String): Either[String, AgentType] =
    Either.cond(
      text.nonEmpty && Slug.slugged(text, Longest) == text && !Hashed.matches(text),
      text,
      s"an agent's type is kept whole only as a slug of at most $Longest characters that is " +
        s"not eight hex digits, alone or after a '-': $text"
    )

  def value(t: AgentType): String = t

  private val Hashed = "(?:.*-)?[0-9a-f]{8}".r
}
