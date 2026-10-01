package grit.core.tool

/** Why a tool call's arguments could not be read. `field` is a path: a top-level name, or into
  * a list's item as `edits[2].oldText`, counting from 0. `got` is the JSON sent, cut to
  * [[ArgsError.Shown]] characters.
  */
enum ArgsError {

  /** The arguments were not a JSON object. */
  case NotAnObject(got: String)

  /** No value, or `null`, for the required `field`, which takes `accepts`. */
  case Missing(field: String, accepts: String)

  /** A property the object holding it does not have; `known` are the ones it does, in order. */
  case Unexpected(field: String, known: Vector[String])

  /** `field` held `got`, which is not what it `accepts`. */
  case Invalid(field: String, accepts: String, got: String)

  /** What the model reads when its call is refused: one sentence naming the field and what it
    * accepts, or saying the arguments must be an object.
    */
  def message: String = this match {
    case NotAnObject(got) => s"The arguments must be a JSON object, not $got."
    case Missing(field, accepts) => s"`$field` is missing: it takes $accepts."
    case Unexpected(field, known) =>
      val names = known.map(k => s"`$k`").mkString(", ")
      s"There is no argument `$field`; the arguments there are $names."
    case Invalid(field, accepts, got) => s"`$field` takes $accepts, not $got."
  }

  /** This error, from reading the object at `path`, as the enclosing read reports it. */
  private[tool] def within(path: String): ArgsError = this match {
    case NotAnObject(got) => Invalid(path, "an object", got)
    case Missing(field, accepts) => Missing(s"$path.$field", accepts)
    case Unexpected(field, known) => Unexpected(s"$path.$field", known)
    case Invalid(field, accepts, got) => Invalid(s"$path.$field", accepts, got)
  }
}

object ArgsError {

  /** The most characters of the sent JSON an error quotes. */
  val Shown = 100

  private[tool] def shown(v: ujson.Value): String = v.render().take(Shown)
}
