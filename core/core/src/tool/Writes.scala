package grit.core.tool

import scala.collection.immutable.VectorMap

import grit.core.place.Place

/** Where a hosted tool's calls write outside grit (ADR 0031): a call names one of `to`'s names
  * in its [[Writes.Field]] argument, and writes to that name's destination, labelled as its
  * `place` is. Several names may be one destination's. `describe` is what the model is told of
  * the argument.
  */
final case class Writes[D <: caps.Pure] private (
    to: VectorMap[String, D],
    place: D -> Place,
    describe: String
) {

  /** These, less the names whose place `keep` refuses, in order; `None` when none is left. */
  def narrowed(keep: Place => Boolean): Option[Writes[D]] = {
    val kept = to.filter((_, d) => keep(place(d)))
    Option.when(kept.nonEmpty)(new Writes(kept, place, describe))
  }

  /** These, each destination as its place: what an advertised entry carries. */
  def placed: Writes[Place] =
    new Writes(to.map((name, d) => name -> place(d)), Writes.itself, describe)

  /** The destination `name` names; `None` when it is none of these names. */
  def named(name: String): Option[D] = to.get(name)

  /** The destination placed at `where`; `None` when none is. */
  def at(where: Place): Option[D] = to.values.find(d => place(d) == where)

  /** `parameters`, a tool's schema, with [[Writes.Field]] a required property whose choices are
    * these names, described by `describe`: what the model is shown.
    */
  private[tool] def shown(parameters: ujson.Value): ujson.Obj = {
    val schema = ujson.Obj.from(parameters.objOpt.fold(Iterator.empty)(_.iterator))
    val choice = ujson.Obj(
      "type" -> "string",
      "enum" -> ujson.Arr.from(to.keys.map(ujson.Str(_))),
      "description" -> describe
    )
    val properties =
      schema.value.get("properties").flatMap(_.objOpt).fold(Iterator.empty)(_.iterator)
    schema("properties") =
      ujson.Obj.from(Iterator(Writes.Field -> choice) ++ properties.filter(_._1 != Writes.Field))
    val required = schema.value.get("required").flatMap(_.arrOpt).fold(Vector.empty)(_.toVector)
    schema("required") =
      ujson.Arr.from(ujson.Str(Writes.Field) +: required.filter(_ != ujson.Str(Writes.Field)))
    schema
  }

  /** The destination `sent`, a call's arguments, names in [[Writes.Field]], and the arguments
    * without it; [[CallError.Unwritable]] when they name none of these, or nothing.
    */
  private[tool] def chosen(
      tool: ToolName,
      sent: collection.Map[String, ujson.Value]
  ): Either[CallError, (D, ujson.Obj)] = {
    val named = sent.get(Writes.Field).filter(_ != ujson.Null)
    named.flatMap(_.strOpt).flatMap(this.named) match {
      case Some(d) => Right((d, Writes.without(sent)))
      case None =>
        Left(
          CallError.Unwritable(
            tool,
            named.map(v => v.strOpt.getOrElse(v.render()).take(CallError.Echoed)),
            to.keys.toVector
          )
        )
    }
  }
}

object Writes {

  /** `to`: the argument in which every writing tool's call names where it writes. The engine
    * offers its choices; an edge's tool is told the checked destination, never what was sent.
    */
  val Field: String = "to"

  /** Whether `parameters`, a tool's schema, declare [[Field]] among its properties: such a
    * tool is refused by [[Hosted.writing]] and [[ToolSet.read]], since only a writing tool's
    * destinations name it.
    */
  def declared(parameters: ujson.Value): Boolean =
    parameters.objOpt
      .flatMap(_.get("properties"))
      .flatMap(_.objOpt)
      .exists(_.contains(Field))

  /** `sent`, a call's arguments, without [[Field]]. */
  private[tool] def without(sent: collection.Map[String, ujson.Value]): ujson.Obj =
    ujson.Obj.from(sent.iterator.filter(_._1 != Field))

  /* One function value, so two placed sets of the same names are equal. */
  private val itself: Place -> Place = p => p

  /** Places as their own destinations, as [[Writes.placed]] makes them: how a stored set's
    * writes are read back equal to those it was written from; why not as [[of]].
    */
  private[tool] def placedAt(
      to: VectorMap[String, Place],
      describe: String
  ): Either[String, Writes[Place]] =
    of(to, itself, describe)

  /** These, or why not: `to` is empty, or a name is blank. */
  def of[D <: caps.Pure](
      to: VectorMap[String, D],
      place: D -> Place,
      describe: String
  ): Either[String, Writes[D]] =
    if (to.isEmpty) Left("a writing tool names no destination")
    else if (to.keys.exists(_.trim.isEmpty)) Left("a destination's name is blank")
    else Right(new Writes(to, place, describe))
}
