package grit.core.secret

/** A [[Secret]] for a test, read as [[Secret.read]] reads one; a fixture that names a variable
  * not named like a secret, or a blank value, fails the test that builds it.
  */
object TestSecrets {

  def of(variable: Variable, value: String): Secret =
    Secret.read(Map(Variable.value(variable) -> value), variable) match {
      case Right(s) => s
      case Left(why) => throw new java.lang.AssertionError(why.message)
    }
}
