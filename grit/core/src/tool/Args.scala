package grit.core.tool

import scala.NamedTuple.NamedTuple

import grit.core.model.ArgRepair

/** A tool's arguments: a JSON object whose properties are its fields, read into a `T`. Built
  * with [[Args.of]]; `map` and `refine` change what is read, never the schema.
  */
final class Args[T] private (
    shape: Boolean -> ujson.Obj,
    reader: (ujson.Value, Set[ArgRepair]) -> Either[ArgsError, T]
) {

  /** The JSON Schema of the arguments object, built afresh on each call: each field a
    * property, the required ones listed, no other property allowed. Under `strict` every
    * property is listed as required, an optional one admitting `null` instead (the rule of
    * OpenAI's strict mode).
    */
  def schema(strict: Boolean): ujson.Obj = shape(strict)

  /** The `T` that `arguments` hold, each field read with `repairs`. Refused, with the first
    * failure in this order: not an object; a property that is no field (the first sent);
    * then each field in order, missing when required, or invalid; then whatever `refine`
    * refused.
    */
  def read(arguments: ujson.Value, repairs: Set[ArgRepair]): Either[ArgsError, T] =
    reader(arguments, repairs)

  /** As [[read]] with every [[ArgRepair]]. */
  def read(arguments: ujson.Value): Either[ArgsError, T] =
    reader(arguments, ArgRepair.values.toSet)

  def map[U](f: T -> U): Args[U] =
    new Args(shape, (arguments, repairs) => reader(arguments, repairs).map(f))

  /** Reads as before, then `f`: a check across fields the schema cannot state, such as a field
    * required only when another holds some value. Its `Left` is the read's.
    */
  def refine[U](f: T -> Either[ArgsError, U]): Args[U] =
    new Args(shape, (arguments, repairs) => reader(arguments, repairs).flatMap(f))
}

object Args {

  /** Arguments with the recorded `schema`, shown as it is whether strict or not, read as the
    * JSON object sent, unchecked: for a tool known only by its record ([[Tool.gone]]).
    * Refused only when what is sent is not an object.
    */
  def raw(schema: ujson.Value): Args[ujson.Value] = {
    val shown: ujson.Obj = schema.objOpt.fold(ujson.Obj())(o => ujson.Obj.from(o))
    new Args(
      _ => ujson.Obj.from(shown.value),
      (arguments, _) =>
        arguments.objOpt
          .map(_ => arguments)
          .toRight(ArgsError.NotAnObject(ArgsError.shown(arguments)))
    )
  }

  /** The value each field of `V` reads into. */
  type Values[V <: Tuple] <: Tuple = V match {
    case EmptyTuple => EmptyTuple
    case Field[a] *: rest => a *: Values[rest]
  }

  /** The arguments named and typed by the named tuple `fields`:
    * `Args.of((about = Field.oneOf(…), name = Field.text(…).optional))` reads an
    * `(about: String, name: Option[String])`. A name used twice does not compile.
    */
  inline def of[N <: Tuple, V <: Tuple](fields: NamedTuple[N, V]): Args[NamedTuple[N, Values[V]]] =
    build[NamedTuple[N, Values[V]]](
      scala.compiletime.constValueTuple[N].productIterator.toVector.map(_.toString),
      fields.toTuple.productIterator.toVector.collect { case f: Field[?] => f }
    )

  /** `names(i)` names `fields(i)`; `T` is the tuple of the fields' values in that order. */
  private def build[T](names: Vector[String], fields: Vector[Field[?]]): Args[T] = {
    val named = names.zip(fields)
    new Args[T](
      strict =>
        ujson.Obj(
          "type" -> "object",
          "properties" -> ujson.Obj.from(named.map((n, f) => n -> f.schema(strict))),
          "required" -> ujson.Arr.from(
            named.collect { case (n, f) if strict || f.required => ujson.Str(n) }
          ),
          "additionalProperties" -> false
        ),
      (arguments, repairs) =>
        arguments.objOpt match {
          case None => Left(ArgsError.NotAnObject(ArgsError.shown(arguments)))
          case Some(sent) =>
            sent.keys.find(k => !names.contains(k)) match {
              case Some(k) => Left(ArgsError.Unexpected(k, names))
              case None =>
                named
                  .foldLeft[Either[ArgsError, Vector[Any]]](Right(Vector.empty)) {
                    case (acc, (n, f)) =>
                      acc.flatMap(vs => f.read(n, sent.get(n), repairs).map(vs :+ _))
                  }
                  // The values are in field order and each is its field's `A`, so the tuple
                  // they make is `T`, the tuple `Values` computes (and a named tuple is its
                  // values at runtime).
                  .map(vs => vs.foldRight[Tuple](EmptyTuple)(_ *: _).asInstanceOf[T])
            }
        }
    )
  }
}
