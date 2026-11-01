package grit.core.secret

/** The name of an environment variable, such as `SLACK_BOT_TOKEN`; never its value. */
opaque type Variable = String

object Variable {
  def apply(name: String): Variable = name
  def value(v: Variable): String = v
}
