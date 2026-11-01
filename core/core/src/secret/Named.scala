package grit.core.secret

/** What is named like a secret: an environment variable that holds one, a file that holds them. */
object Named {

  /** Whether the environment variable `name` holds a secret: it contains KEY, TOKEN, SECRET or
    * PASSWORD, in any case.
    */
  def variable(name: String): Boolean = {
    val upper = name.toUpperCase
    Vector("KEY", "TOKEN", "SECRET", "PASSWORD").exists(upper.contains)
  }

  /** Whether the path segment `name` is a secrets file: `.env`, or `.env.` and anything. */
  def file(name: String): Boolean = name == ".env" || name.startsWith(".env.")
}
