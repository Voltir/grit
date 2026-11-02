package grit.core.secret

/** A value grit holds that grants authority outside it, such as an API key. No label admits it:
  * it has no JSON or `Journaled` form, shows as `Secret(VARIABLE)`, is equal only to itself, and
  * its value leaves only through [[Reveal]].
  */
final class Secret private[secret] (val variable: Variable, private[secret] val value: String)
    extends caps.Pure {
  override def toString: String = s"Secret(${Variable.value(variable)})"
}

object Secret {

  /** `variable`'s value in `env`, blanks around it trimmed, or why not. */
  def read(env: Map[String, String], variable: Variable): Either[Refusal, Secret] = {
    val name = Variable.value(variable)
    if (!Named.variable(name)) Left(Refusal.NotNamedLikeOne(variable))
    else
      env.get(name) match {
        case None => Left(Refusal.Unset(variable))
        case Some(v) if v.trim.isEmpty => Left(Refusal.Blank(variable))
        case Some(v) => Right(new Secret(variable, v.trim))
      }
  }

  /** Why a secret was not read. */
  enum Refusal {

    /** [[Named.variable]] does not hold of its name. */
    case NotNamedLikeOne(variable: Variable)

    case Unset(variable: Variable)

    /** Set, but empty or blank. */
    case Blank(variable: Variable)

    /** One line a person reads, naming the variable. */
    def message: String = this match {
      case NotNamedLikeOne(v) =>
        s"${Variable.value(v)} is not named like a secret: its name holds none of KEY, TOKEN, SECRET or PASSWORD"
      case Unset(v) => s"${Variable.value(v)} is not set"
      case Blank(v) => s"${Variable.value(v)} is set but blank"
    }
  }
}

/** A secret's value, for the library call or the file that needs it. Named only where the law's
  * `only-quarantines-reveal` allows.
  */
object Reveal {
  def text(s: Secret): String = s.value
}
