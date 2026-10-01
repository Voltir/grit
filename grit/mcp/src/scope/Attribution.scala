package grit.mcp.scope

/** How a tool's answers name each result's place, so a scope can show only the results within
  * its bounds. An answer is one text block holding a JSON object whose array at `items` holds
  * the results, and whose number at `count`, when the attribution names one, counts them;
  * `place` reads a result's place as the values of `keys`, the arguments a call for that place
  * alone would carry, in their order, or says why it cannot. `unsent` are arguments a call of
  * the tool never carries, since they could leave the place out of its results.
  */
final case class Attribution private (
    items: String,
    count: Option[String],
    keys: Vector[String],
    unsent: Set[String],
    place: ujson.Value -> Either[String, Vector[String]]
)

object Attribution {

  /** The attribution, or why not: `items` or a key blank, no key, or a key repeated. */
  def of(
      items: String,
      count: Option[String],
      keys: Vector[String],
      unsent: Set[String],
      place: ujson.Value -> Either[String, Vector[String]]
  ): Either[String, Attribution] =
    if (items.isBlank) Left("an attribution's items is blank")
    else if (keys.isEmpty) Left("an attribution names no key")
    else if (keys.exists(_.isBlank)) Left("an attribution's key is blank")
    else
      keys
        .diff(keys.distinct)
        .headOption
        .map(k => s"an attribution names $k twice")
        .toLeft(new Attribution(items, count, keys, unsent, place))
}
