package grit.core.tool

import scala.compiletime.ops.string.Matches

/** A tool's name: a lowercase ASCII letter, then lowercase letters, digits or `_`, at most 64
  * characters in all. Every provider grit speaks to accepts it.
  */
opaque type ToolName = String

object ToolName {

  /** The rule, as a regular expression over the whole name. */
  type Rule = "[a-z][a-z0-9_]{0,63}"

  /** `name` as a tool name; `Left` says which rule it breaks. */
  def of(name: String): Either[String, ToolName] =
    if (name.isEmpty) Left("a tool name is empty")
    else if (name.length > 64) Left(s"a tool name is at most 64 characters: $name")
    else if (!name.matches(scala.compiletime.constValue[Rule]))
      Left(s"a tool name is a lowercase letter, then lowercase letters, digits or _: $name")
    else Right(name)

  /** A literal name; one that breaks the rule does not compile. */
  def apply[S <: String & Singleton](name: S)(using Matches[S, Rule] =:= true): ToolName = name

  def value(name: ToolName): String = name
}
