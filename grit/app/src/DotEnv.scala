package grit.app

/** A `.env` file: local settings, secrets included, for a developer's machine only. It is
  * gitignored; `.env.example` lists what grit reads. A variable set in the real
  * environment wins over the file.
  *
  * Lines are `NAME=value`, optionally prefixed `export `. Blank lines and lines starting
  * with `#` are skipped. A value may be wrapped in one pair of matching quotes, which are
  * removed; nothing else is interpreted (no escapes, no `${...}`).
  */
object DotEnv {

  /** The variables `contents` sets, or the first line it cannot read. An error names the
    * line and, when it can, the variable, never a value.
    */
  def parse(contents: String): Either[String, Map[String, String]] =
    contents.linesIterator.zipWithIndex
      .map { case (raw, i) => (raw.trim, i + 1) }
      .filterNot { case (line, _) => line.isEmpty || line.startsWith("#") }
      .foldLeft[Either[String, Map[String, String]]](Right(Map.empty)) {
        case (acc, (line, number)) =>
          acc.flatMap { vars =>
            val body = line.stripPrefix("export ").trim
            body.indexOf('=') match {
              case i if i <= 0 => Left(s".env line $number: expected NAME=value")
              case i =>
                val name = body.take(i).trim
                if (!name.matches("[A-Za-z_][A-Za-z0-9_]*"))
                  Left(s".env line $number: not a variable name")
                else Right(vars.updated(name, unquote(body.drop(i + 1).trim)))
            }
          }
      }

  /** `env` over the variables of the file at `path`, if it exists: the environment wins. */
  def load(
      path: java.nio.file.Path,
      env: Map[String, String]
  ): Either[String, Map[String, String]] =
    if (!java.nio.file.Files.isRegularFile(path)) Right(env)
    else parse(java.nio.file.Files.readString(path)).map(_ ++ env)

  private def unquote(value: String): String =
    if (value.length >= 2 && (value.head == '"' || value.head == '\'') && value.last == value.head)
      value.substring(1, value.length - 1)
    else value
}
