package grit.commands.machine

import grit.core.id.Slug

/** The name of a machine, a computer or sandbox that holds files and runs commands, as the
  * deployment's configuration gives it to a credential; a slug ([[Slug.check]], a leading digit
  * allowed) of at most [[MachineName.Longest]] characters. Never read from a host's name.
  */
opaque type MachineName = String

object MachineName {

  /** 32: the most characters a machine's name holds. */
  val Longest: Int = 32

  /** `s` as a machine's name, or why not. */
  def of(s: String): Either[String, MachineName] =
    Slug.check("a machine's name", s, Some(Longest), leadingDigit = true)

  def value(w: MachineName): String = w
}
