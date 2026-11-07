package grit.core.secret

/** The name of an environment variable, such as `SLACK_BOT_TOKEN`; never its value. */
opaque type Variable = String

object Variable {
  def apply(name: String): Variable = name
  def value(v: Variable): String = v

  /** `variable`'s value in `env`, blanks around it trimmed; `Unset` when it is not there,
    * `Blank` when it is empty or blank.
    */
  def read(
      env: Map[String, String],
      variable: Variable
  ): Either[Secret.Refusal.Unset | Secret.Refusal.Blank, String] =
    env.get(variable) match {
      case None => Left(new Secret.Refusal.Unset(variable))
      case Some(v) if v.trim.isEmpty => Left(new Secret.Refusal.Blank(variable))
      case Some(v) => Right(v.trim)
    }
}
