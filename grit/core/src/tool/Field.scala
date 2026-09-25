package grit.core.tool

/** One argument of a tool: its JSON type, what the model is told it means, and how the value
  * the model sends is read. A field is required unless made [[optional]]; `accepts` is what
  * it takes, in the words [[ArgsError.message]] uses ("one of `current`, `new`").
  */
final class Field[A] private (
    shape: Boolean -> ujson.Obj,
    val accepts: String,
    decode: (String, ujson.Value) -> Either[ArgsError, A],
    absent: Option[A]
) {

  /** The same field, not required: absent or `null` reads as `None`. Under a strict schema
    * it is listed as required and its type also admits `null`, as strict mode demands.
    */
  def optional: Field[Option[A]] =
    new Field[Option[A]](
      strict => if (strict) Field.nullable(shape(strict)) else shape(strict),
      accepts,
      (path, v) => decode(path, v).map(Some(_)),
      Some(None)
    )

  private[tool] def required: Boolean = absent.isEmpty

  /** Its JSON Schema, built afresh on each call. */
  private[tool] def schema(strict: Boolean): ujson.Obj = shape(strict)

  /** The value sent at `path`, if any; `null` counts as absent. */
  private[tool] def read(path: String, sent: Option[ujson.Value]): Either[ArgsError, A] =
    sent.filterNot(_.isNull) match {
      case None => absent.toRight(ArgsError.Missing(path, accepts))
      case Some(v) => decode(path, v)
    }
}

object Field {

  /** Any string. */
  def text(meaning: String): Field[String] =
    plain(() => ujson.Obj("type" -> "string", "description" -> meaning), "text", _.strOpt)

  /** Exactly one of `first +: rest`, compared exactly (case and spaces count); the schema lists
    * them as an enum, so a strict provider cannot send another. A repeated option is listed
    * once.
    */
  def oneOf(meaning: String, first: String, rest: String*): Field[String] = {
    val options = (first +: rest.toVector).distinct
    plain(
      () =>
        ujson.Obj(
          "type" -> "string",
          "enum" -> ujson.Arr.from(options.map(ujson.Str(_))),
          "description" -> meaning
        ),
      if (options.size == 1) s"`$first`" else s"one of ${options.map(o => s"`$o`").mkString(", ")}",
      _.strOpt.filter(options.contains)
    )
  }

  /** A whole number from `min` to `max` inclusive; a JSON number with a fractional part is
    * refused. With `min > max` no value is accepted.
    */
  def count(meaning: String, min: Int, max: Int): Field[Int] =
    plain(
      () =>
        ujson.Obj(
          "type" -> "integer",
          "minimum" -> min,
          "maximum" -> max,
          "description" -> meaning
        ),
      s"a whole number from $min to $max",
      _.numOpt.filter(n => n.isWhole && n >= min && n <= max).map(_.toInt)
    )

  /** `true` or `false`. */
  def flag(meaning: String): Field[Boolean] =
    plain(
      () => ujson.Obj("type" -> "boolean", "description" -> meaning),
      "true or false",
      _.boolOpt
    )

  /** A list of at least `min` objects (0 when `min` is negative), each read by `item`, in
    * order: `Field.each("The edits.", Args.of((oldText = Field.text(…), newText = …)))`. A
    * failure inside an item is refused at its path, counting from 0: `edits[2].oldText`.
    */
  def each[T](meaning: String, item: Args[T], min: Int = 1): Field[List[T]] = {
    val least = min.max(0)
    val accepts =
      if (least == 0) "a list of objects" else s"a list of at least $least objects"
    new Field(
      strict =>
        ujson.Obj(
          "type" -> "array",
          "items" -> item.schema(strict),
          "minItems" -> least,
          "description" -> meaning
        ),
      accepts,
      (path, v) =>
        v.arrOpt.map(_.toVector).filter(_.size >= least) match {
          case None => Left(ArgsError.Invalid(path, accepts, ArgsError.shown(v)))
          case Some(items) =>
            items.zipWithIndex.foldRight[Either[ArgsError, List[T]]](Right(Nil)) {
              case ((sent, i), acc) =>
                item.read(sent).left.map(_.within(s"$path[$i]")).flatMap(t => acc.map(t :: _))
            }
        },
      None
    )
  }

  /** A field whose value `decode` reads alone, refused as not what it `accepts`. */
  private def plain[A](
      schema: () -> ujson.Obj,
      accepts: String,
      decode: ujson.Value -> Option[A]
  ): Field[A] =
    new Field(
      _ => schema(),
      accepts,
      (path, v) => decode(v).toRight(ArgsError.Invalid(path, accepts, ArgsError.shown(v))),
      None
    )

  /** `schema` with `null` added to its type and, when it has one, its enum. */
  private def nullable(schema: ujson.Obj): ujson.Obj = {
    val types = schema.value.get("type").toVector.flatMap {
      case ujson.Arr(ts) => ts.toVector
      case t => Vector(t)
    }
    val out = ujson.Obj.from(schema.value)
    out("type") = ujson.Arr.from((types :+ ujson.Str("null")).distinct)
    schema.value.get("enum").foreach { e =>
      out("enum") = ujson.Arr.from((e.arrOpt.toVector.flatten :+ ujson.Null).distinct)
    }
    out
  }
}
