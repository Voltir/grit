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

  /** This bound less its arguments `declared` does not name. */
  private[scope] def within(declared: Set[String]): Bound =
    new Bound(values.filter((name, _) => declared.contains(name)))
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

/** Which of a server's tools are offered, which of their calls are sent, and which of their
  * answers' results are shown.
  */
sealed trait McpScope {

  /** Why `tool` is not offered, or `None` when it is: every tool under [[McpScope.Open]]; under
    * [[McpScope.Within]], a tool whose answers it attributes, or whose schema requires every
    * argument some bound names.
    */
  def excludes(tool: McpTool): Option[Skipped]

  /** A call of `tool` with `arguments` as it is sent, or why it is not: sent as given under
    * [[McpScope.Open]]. Under [[McpScope.Within]], a tool whose answers it attributes is sent
    * less its attribution's `unsent` arguments, when some bound has each of its arguments that
    * `tool`'s schema declares in `arguments` as a string equal to its value (a bound naming none
    * of them admits every call); any other tool is sent as given when some bound whose every
    * argument `tool`'s schema requires has each of them in `arguments` as a string equal to its
    * value. Otherwise it is not sent, saying that `tool` is not offered ([[excludes]]), or naming
    * those bounds and the call's values of their arguments.
    */
  def request(tool: McpTool, arguments: ujson.Obj): Either[String, ujson.Obj]

  /** `result`, a `tools/call` result of `tool`, as it may be shown: as it is under
    * [[McpScope.Open]], when `tool`'s answers are not attributed, or when its `isError` is
    * true. Otherwise its one text block's results outside every bound are removed, the
    * attribution's count, when the answer has one, is set to the number kept, and when any were
    * removed a second text block says how many, of how many, and the bounds. A result is within
    * a bound when its place has each of the bound's values. Each answer is filtered on its own,
    * so a paged search's count is of that page's kept results. `Left` saying why, with nothing
    * shown, when the result has a key other than `content`, `isError`, `_meta` and
    * `resultType`, its content is not one text block holding a JSON object with an array at the
    * attribution's `items`, or any result's place cannot be read or has other than one value per
    * key.
    */
  def shown(tool: McpTool, result: ujson.Obj): Either[String, ujson.Obj]
}

object McpScope {

  /** Every tool offered, every call sent, every answer shown. */
  case object Open extends McpScope {
    def excludes(tool: McpTool): Option[Skipped] = None
    def request(tool: McpTool, arguments: ujson.Obj): Either[String, ujson.Obj] = Right(arguments)
    def shown(tool: McpTool, result: ujson.Obj): Either[String, ujson.Obj] = Right(result)
  }

  /** Calls held to `bounds`; the tools in `answered`, by the server's own names, held by their
    * answers' places.
    */
  final case class Within private[McpScope] (
      bounds: Vector[Bound],
      answered: Map[String, Attribution]
  ) extends McpScope {

    def excludes(tool: McpTool): Option[Skipped] =
      Option.when(!answered.contains(tool.name) && holding(tool).isEmpty)(
        Skipped.OutOfScope(tool.name)
      )

    def request(tool: McpTool, arguments: ujson.Obj): Either[String, ujson.Obj] =
      answered.get(tool.name) match {
        case Some(attribution) =>
          val sent = ujson.Obj.from(arguments.value.filterNot((k, _) => attribution.unsent(k)))
          admitted(bounds.map(_.within(declared(tool))), sent)
        case None =>
          val held = holding(tool)
          if (held.isEmpty) Left(s"its scope does not offer ${tool.name}")
          else admitted(held, arguments)
      }

    def shown(tool: McpTool, result: ujson.Obj): Either[String, ujson.Obj] =
      answered.get(tool.name) match {
        case None => Right(result)
        case Some(_) if result.value.get("isError").contains(ujson.Bool(true)) => Right(result)
        case Some(attribution) =>
          for {
            _ <- result.value.keys
              .find(k => !Shown(k))
              .map(k => s"its answer has $k, which grit cannot hold to its scope")
              .toLeft(())
            text <- result.value
              .get("content")
              .flatMap(_.arrOpt)
              .map(_.toVector)
              .collect { case Vector(block) => block }
              .flatMap(_.objOpt)
              .filter(_.get("type").contains(ujson.Str("text")))
              .flatMap(_.get("text"))
              .flatMap(_.strOpt)
              .toRight("its answer is not one text block")
            payload <- scala.util
              .Try(ujson.read(text))
              .toOption
              .flatMap(_.objOpt)
              .map(ujson.Obj.from(_))
              .toRight("its answer's text is not a JSON object")
            items <- payload.value
              .get(attribution.items)
              .flatMap(_.arrOpt)
              .map(_.toVector)
              .toRight(s"its answer's text has no ${attribution.items} array")
            placed <- items.foldLeft[Either[String, Vector[(ujson.Value, ujson.Obj)]]](
              Right(Vector.empty)
            ) { (done, item) =>
              done.flatMap(d => placeOf(attribution, item).map(at => d :+ (item -> at)))
            }
          } yield {
            val kept = placed.collect { case (item, at) if bounds.exists(_.admits(at)) => item }
            payload(attribution.items) = ujson.Arr.from(kept)
            attribution.count.filter(payload.value.contains).foreach(payload(_) = kept.size)
            val withheld = items.size - kept.size
            val said = Option.when(withheld > 0)(
              ujson.Obj(
                "type" -> "text",
                "text" -> (s"Withheld as outside this server's scope " +
                  s"(${bounds.map(_.written).mkString(", or ")}): " +
                  s"$withheld of the ${items.size} results in this answer.")
              )
            )
            val shown = ujson.Obj.from(result.value)
            shown("content") = ujson.Arr.from(
              ujson.Obj("type" -> "text", "text" -> ujson.write(payload)) +: said.toVector
            )
            shown
          }
      }

    /** `arguments` when some of `held` admits them; otherwise why not, naming `held` and the
      * call's values of their arguments.
      */
    private def admitted(held: Vector[Bound], arguments: ujson.Obj): Either[String, ujson.Obj] =
      if (held.exists(_.admits(arguments))) Right(arguments)
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

    /** `item`'s place as arguments, by `attribution`, or why it cannot be read. */
    private def placeOf(attribution: Attribution, item: ujson.Value): Either[String, ujson.Obj] =
      attribution.place(item).left.map(why => s"a result's place cannot be read: $why").flatMap {
        values =>
          if (values.size == attribution.keys.size)
            Right(ujson.Obj.from(attribution.keys.zip(values.map(ujson.Str(_)))))
          else
            Left(
              s"a result's place has ${values.size} value${if (values.size == 1) "" else "s"}, " +
                s"not one for each of ${attribution.keys.mkString(", ")}"
            )
      }

    /** The bounds whose every argument `tool`'s schema requires. */
    private def holding(tool: McpTool): Vector[Bound] = {
      val required = names(tool, "required")
      bounds.filter(_.names.forall(required.contains))
    }

    /** The arguments `tool`'s schema declares. */
    private def declared(tool: McpTool): Set[String] =
      tool.inputSchema.value
        .get("properties")
        .flatMap(_.objOpt)
        .fold(Set.empty[String])(_.keySet.toSet)

    private def names(tool: McpTool, key: String): Set[String] =
      tool.inputSchema.value
        .get(key)
        .flatMap(_.arrOpt)
        .fold(Set.empty[String])(_.flatMap(_.strOpt).toSet)
  }

  /** The result keys a filtered answer may carry beside the content it filters. */
  private val Shown: Set[String] = Set("content", "isError", "_meta", "resultType")

  /** The scope held to `first` and `rest`, attributing no tool's answers: a call is within it
    * when it is within any of them.
    */
  def within(first: Bound, rest: Bound*): McpScope = Within(first +: rest.toVector, Map.empty)

  /** The scope held to `first` and `rest`, with the tools in `answered`, by the server's own
    * names, held by their answers' places; or why not: an attribution whose keys leave out an
    * argument some bound names, so that no result of its tool could be within that bound.
    */
  def attributed(
      answered: Map[String, Attribution],
      first: Bound,
      rest: Bound*
  ): Either[String, McpScope] = {
    val bounds = first +: rest.toVector
    answered.toVector
      .sortBy(_._1)
      .collectFirst(Function.unlift { (tool, attribution) =>
        bounds
          .flatMap(_.names)
          .find(!attribution.keys.contains(_))
          .map(name =>
            s"$tool's results are placed by ${attribution.keys.mkString(", ")}, " +
              s"never by a bound's $name"
          )
      })
      .toLeft(Within(bounds, answered))
  }
}
