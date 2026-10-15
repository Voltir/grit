package grit.core.flow

import java.time.LocalDate

import scala.util.Try

/** A shape an extraction is answered in, chosen at run time, of at most [[Bounded.K]] bits. */
enum Shape {
  case YesNo

  /** A whole number from `min` to `max`. */
  case Number(min: Long, max: Long)

  case Day

  /** One of `options`, each at most 80 characters. */
  case OneOf(options: Vector[String])

  /** At most `max` characters of `[A-Za-z0-9_-]`. */
  case Token(max: Int)
}

object Shape {

  /** The most options a [[OneOf]] lists. */
  val MaxOptions: Int = 64

  /** The most characters in one option of a [[OneOf]]. */
  val MaxOption: Int = 80

  /** The most characters a [[Token]] allows. */
  val MaxToken: Int = 16

  /** The shape `json` writes, or why not, in words the model reads as its call's refusal
    * ([[grit.core.tool.ArgsError.Refused]]). `json` is an object whose `kind` is `yes_no`, `day`,
    * `number` (with whole `min` and `max`), `one_of` (with an `options` array of strings) or
    * `token` (with a whole `max`). Refused: an unknown kind or a missing field, an empty or
    * reversed range, no options, more than [[MaxOptions]] or one over [[MaxOption]]
    * characters, a token of fewer than 1 or more than [[MaxToken]] characters.
    */
  def of(json: ujson.Value): Either[String, Shape] =
    for {
      fields <- json.objOpt.toRight("a shape is a JSON object")
      kind <- fields.get("kind").flatMap(_.strOpt).toRight("a shape names its kind")
      shape <- kind match {
        case "yes_no" => Right(YesNo)
        case "day" => Right(Day)
        case "number" =>
          for {
            min <- whole(fields, "min")
            max <- whole(fields, "max")
            _ <- Either.cond(min <= max, (), s"a number's least, $min, is above its most, $max")
          } yield Number(min, max)
        case "one_of" =>
          fields
            .get("options")
            .flatMap(_.arrOpt)
            .flatMap(items => Some(items.toVector.flatMap(_.strOpt)).filter(_.size == items.size))
            .toRight("one_of lists its options as strings")
            .flatMap(options =>
              if (options.isEmpty || options.size > MaxOptions)
                Left(s"one_of lists 1 to $MaxOptions options, not ${options.size}")
              else if (options.exists(_.length > MaxOption))
                Left(s"each of one_of's options is at most $MaxOption characters")
              else Right(OneOf(options))
            )
        case "token" =>
          whole(fields, "max").flatMap(n =>
            if (n < 1) Left(s"a short code is at least 1 character, not $n")
            else if (n > MaxToken) Left(s"a short code is at most $MaxToken characters, not $n")
            else Right(Token(n.toInt))
          )
        case other => Left(s"no shape is called $other")
      }
    } yield shape

  private def whole(
      fields: collection.Map[String, ujson.Value],
      name: String
  ): Either[String, Long] =
    fields
      .get(name)
      .flatMap(_.numOpt)
      .filter(_.isWhole)
      .map(_.toLong)
      .toRight(s"a shape's $name is a whole number")

  /** `value` read as `shape` answers it: a JSON boolean a [[YesNo]], a whole number in range a
    * [[Number]], an ISO date (`2026-10-14`) a [[Day]], a string equal to one of the options a
    * [[OneOf]], a string a [[Token]] allows. `None` when it does not.
    */
  def answer(shape: Shape, value: ujson.Value): Option[Extracted] = shape match {
    case YesNo => value.boolOpt.map(Extracted.Yes(_))
    case Number(min, max) =>
      value.numOpt
        .filter(_.isWhole)
        .map(_.toLong)
        .filter(n => n >= min && n <= max)
        .map(
          Extracted.Number(_)
        )
    case Day => value.strOpt.flatMap(s => Try(LocalDate.parse(s)).toOption).map(Extracted.Day(_))
    case OneOf(options) => value.strOpt.filter(options.contains).map(Extracted.Chosen(_))
    case Token(max) =>
      value.strOpt
        .filter(s => s.nonEmpty && s.length <= max && s.forall(grit.core.flow.Token.allowed))
        .map(Extracted.Token(_))
  }
}

/** An answer in a [[Shape]]: bounded by its shape, so endorsable. */
enum Extracted {
  case Yes(b: Boolean)
  case Number(n: Long)
  case Day(d: LocalDate)
  case Chosen(text: String)
  case Token(text: String)
}
