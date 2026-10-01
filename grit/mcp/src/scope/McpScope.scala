package grit.mcp.scope

import grit.mcp.wire.{McpTool, Skipped}

/** Argument values a call must carry to be within a scope: each named argument a JSON string
  * equal to its value, compared exactly, case included.
  */
final case class Bound private (values: Vector[(String, String)]) {

  /** The arguments it names. */
  private[scope] def names: Vector[String] = values.map(_._1)

  /** Whether `arguments` carries each of its values. */
  private[scope] def admits(arguments: ujson.Obj): Boolean =
    values.forall((name, value) => arguments.value.get(name).contains(ujson.Str(value)))

  /** "owner the-actual-best with repo actualbest". */
  private[scope] def written: String =
    values.map((name, value) => s"$name $value").mkString(" with ")
}

object Bound {

  /** The bound, or why not: an argument named twice, or a blank name or value. */
  def of(first: (String, String), rest: (String, String)*): Either[String, Bound] = {
    val values = first +: rest.toVector
    val names = values.map(_._1)
    values
      .collectFirst {
        case (name, _) if name.isBlank => "a bound's argument has a blank name"
        case (name, value) if value.isBlank => s"a bound's $name is blank"
      }
      .orElse(names.diff(names.distinct).headOption.map(n => s"a bound names $n twice"))
      .toLeft(new Bound(values))
  }
}

/** Which of a server's tools are offered, and which of their calls are sent. */
sealed trait McpScope {

  /** Why `tool` is not offered, or `None` when it is: every tool under [[McpScope.Open]]; under
    * [[McpScope.Within]], only a tool whose schema requires every argument some bound names.
    */
  def excludes(tool: McpTool): Option[Skipped]

  /** A call of `tool` with `arguments` as it is sent, or why it is not: sent as given under
    * [[McpScope.Open]]; under [[McpScope.Within]], sent as given when some bound whose every
    * argument `tool`'s schema requires has each of them in `arguments` as a string equal to its
    * value, and otherwise not sent, saying that `tool` is not offered ([[excludes]]), or naming
    * those bounds and the call's values of their arguments.
    */
  def request(tool: McpTool, arguments: ujson.Obj): Either[String, ujson.Obj]
}

object McpScope {

  /** Every tool offered, every call sent. */
  case object Open extends McpScope {
    def excludes(tool: McpTool): Option[Skipped] = None
    def request(tool: McpTool, arguments: ujson.Obj): Either[String, ujson.Obj] = Right(arguments)
  }

  /** Calls held to `bounds`. */
  final case class Within private[McpScope] (bounds: Vector[Bound]) extends McpScope {

    def excludes(tool: McpTool): Option[Skipped] =
      Option.when(holding(tool).isEmpty)(Skipped.OutOfScope(tool.name))

    def request(tool: McpTool, arguments: ujson.Obj): Either[String, ujson.Obj] = {
      val held = holding(tool)
      if (held.isEmpty) Left(s"its scope does not offer ${tool.name}")
      else if (held.exists(_.admits(arguments))) Right(arguments)
      else {
        val has = held.flatMap(_.names).distinct.map { name =>
          arguments.value.get(name) match {
            case Some(ujson.Str(value)) => s"$name $value"
            case Some(other) => s"$name ${ujson.write(other)}"
            case None => s"no $name"
          }
        }
        Left(
          s"its calls are limited to ${held.map(_.written).mkString(", or ")}, " +
            s"and this one has ${has.mkString(" with ")}"
        )
      }
    }

    /** The bounds whose every argument `tool`'s schema requires. */
    private def holding(tool: McpTool): Vector[Bound] = {
      val required = tool.inputSchema.value
        .get("required")
        .flatMap(_.arrOpt)
        .fold(Set.empty[String])(_.flatMap(_.strOpt).toSet)
      bounds.filter(_.names.forall(required.contains))
    }
  }

  /** The scope held to `first` and `rest`: a call is within it when it is within any of them. */
  def within(first: Bound, rest: Bound*): McpScope = Within(first +: rest.toVector)
}
