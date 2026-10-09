package grit.core.schema

/** A reply's schema, and how what conforms to it reads into a `T`: `read`'s `Left` says why
  * not, in words a model can act on. [[Typed.json]] reads it as is.
  */
final case class Typed[T](schema: JsonSchema, read: Conforming -> Either[String, T])
    extends caps.Pure

object Typed {

  /** `schema`, its conforming JSON read as itself. */
  def json(schema: JsonSchema): Typed[Conforming] = Typed(schema, Right(_))
}
