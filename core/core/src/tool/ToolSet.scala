package grit.core.tool

import scala.collection.immutable.VectorMap

import grit.core.id.ShortHash
import grit.core.place.Place
import grit.core.store.Tx
import grit.core.visibility.{Classification, Integrity}

/** A tool set as offered to one turn: each tool's name, description, schema, whether it asks
  * a person first, and its retry, in the order offered. Equal sets have equal ids
  * ([[ToolSet.id]]), so a set is kept once however many turns are offered it.
  */
final case class ToolSet private (tools: Vector[ToolSet.Entry]) {

  /** A content hash of the set's stored form ([[ToolSet.write]]). */
  def id: ToolSetId = ToolSetId(ShortHash.of(ujson.write(ToolSet.write(this))))

  /** Whether a turn's gate could ever hide an item or withhold a tool of this set: some tool
    * requires above the least integrity ([[Integrity.Untrusted]]).
    */
  def mayGate: Boolean = tools.exists(t => !Integrity.Untrusted.dominates(t.requires))

  /** The entry for the tool named `name`; `None` when the set has no such tool. */
  def named(name: ToolName): Option[ToolSet.Entry] = tools.find(_.name == name)

  /** What a person can no longer have done when the calls of the tool named `name` are
    * withheld: its entry's ([[ToolSet.Entry.plainly]]), else "use" and the name, as for an entry
    * declaring none.
    */
  def plainly(name: ToolName): String = named(name).fold(ToolSet.used(name))(_.plainly)
}

object ToolSet {

  /** One tool of a set: `parameters` is its schema as the model is shown it, not strict, less
    * [[Writes.Field]]; `writes`, where its calls write outside grit, `None` for a tool that
    * declares no such destination (its arguments still go to the service it runs at);
    * `requires`, the integrity a call must be stamped at to be bound ([[Toolbox.bind]]), by
    * default its shape's ([[Entry.byShape]]); `action`, its spec's ([[ToolSpec.action]]).
    */
  final case class Entry(
      name: ToolName,
      does: String,
      parameters: ujson.Value,
      asks: Boolean,
      retry: Retry,
      writes: Option[Writes[Place]],
      requires: Integrity,
      action: Option[String]
  ) {

    /** What a person can no longer have done when its calls are withheld, in words that follow
      * "you can't": its `action`, else "use" and its name ("use note").
      */
    def plainly: String = action.getOrElse(used(name))

    /** This entry as a call may reach it from a transaction that `sends` its arguments to the
      * service it runs at, or not: one that declares no destination only when it does; one
      * that writes, less the places what was read at `at` is not written to
      * ([[grit.core.store.Tx.writesTo]]); `None` when neither is left.
      */
    def offerable(sends: Boolean, at: Classification)(using Tx^): Option[Entry] =
      writes match {
        case None => Option.when(sends)(this)
        case Some(w) => w.narrowed(Tx.writesTo(_, at)).map(n => copy(writes = Some(n)))
      }
  }

  object Entry {

    /** An entry requiring what its shape does ([[byShape]]). */
    def apply(
        name: ToolName,
        does: String,
        parameters: ujson.Value,
        asks: Boolean,
        retry: Retry,
        writes: Option[Writes[Place]] = None,
        action: Option[String] = None
    ): Entry = Entry(name, does, parameters, asks, retry, writes, byShape(writes), action)

    /** What a tool requires unless it declares otherwise: [[Integrity.WritesOut]] for one that
      * writes outside grit (`writes` is some), nothing ([[Integrity.Untrusted]]) otherwise.
      */
    def byShape(writes: Option[Writes[Place]]): Integrity =
      writes.fold(Integrity.Untrusted)(_ => Integrity.WritesOut)
  }

  val Empty: ToolSet = ToolSet(Vector.empty)

  /* What a tool declaring no action is said as. */
  private def used(name: ToolName): String = s"use ${ToolName.value(name)}"

  /** `entries` as a set, in this order; `Left` naming the first name repeated, else the first
    * entry whose parameters declare [[Writes.Field]], so every set built reads back ([[read]]).
    */
  def of(entries: Vector[Entry]): Either[SetRefusal, ToolSet] = {
    val names = entries.map(_.name)
    names.diff(names.distinct).headOption match {
      case Some(repeated) => Left(DuplicateName(repeated))
      case None =>
        entries.find(e => Writes.declared(e.parameters)) match {
          case Some(declaring) => Left(DeclaresField(declaring.name))
          case None => Right(ToolSet(entries))
        }
    }
  }

  /** The stored form: `{"tools":[{"name","does","parameters","asks","retry"}, …]}`, in the
    * set's order, an entry with writes also holding `"writes": {"describe", "to": [{"name",
    * "place"}, …]}`, each place written, and one whose `requires` is not its shape's
    * ([[Entry.byShape]]) holding `"requires"`, written ([[Integrity.written]]), and one with an
    * action holding `"action"`. Each only on an entry that has it, so a set without any keeps
    * the form, and the id, it had before.
    * The id hashes it, so a change here changes every set's id.
    */
  def write(set: ToolSet): ujson.Value =
    ujson.Obj(
      "tools" -> ujson.Arr.from(set.tools.map { e =>
        val stored = ujson.Obj(
          "name" -> ToolName.value(e.name),
          "does" -> e.does,
          "parameters" -> e.parameters,
          "asks" -> e.asks,
          "retry" -> e.retry.key
        )
        e.writes.foreach { w =>
          stored("writes") = ujson.Obj(
            "describe" -> w.describe,
            "to" -> ujson.Arr.from(
              w.to.map((name, at) => ujson.Obj("name" -> name, "place" -> at.written))
            )
          )
        }
        if (e.requires != Entry.byShape(e.writes))
          stored("requires") = Integrity.written(e.requires)
        e.action.foreach(a => stored("action") = a)
        stored
      })
    )

  /** The set stored as `v` ([[write]]'s form), or why it is none; why not, also, when an
    * entry's parameters declare [[Writes.Field]]. An entry holding no `"requires"` requires
    * what its shape does ([[Entry.byShape]]); one holding no `"action"` has none.
    */
  def read(v: ujson.Value): Either[String, ToolSet] = {
    def field(o: collection.Map[String, ujson.Value], key: String): Either[String, ujson.Value] =
      o.get(key).toRight(s"a tool has no $key")
    def entry(t: ujson.Value): Either[String, Entry] =
      for {
        o <- t.objOpt.toRight("a tool is not an object")
        name <- field(o, "name").flatMap(_.strOpt.toRight("a tool's name is not a string"))
        does <- field(o, "does").flatMap(_.strOpt.toRight("a tool's does is not a string"))
        parameters <- field(o, "parameters")
        asks <- field(o, "asks").flatMap(_.boolOpt.toRight("a tool's asks is not a boolean"))
        retryKey <- field(o, "retry").flatMap(_.strOpt.toRight("a tool's retry is not a string"))
        retry <- Retry.of(retryKey).toRight(s"no retry $retryKey")
        named <- ToolName.of(name)
        _ <- Either.cond(!Writes.declared(parameters), (), DeclaresField(named).message)
        writes <- o.get("writes").fold(Right(None))(w => stored(w).map(Some(_)))
        requires <- o
          .get("requires")
          .fold[Either[String, Integrity]](Right(Entry.byShape(writes)))(r =>
            r.strOpt.toRight("a tool's requires is not a string").flatMap(Integrity.read)
          )
        action <- o
          .get("action")
          .fold[Either[String, Option[String]]](Right(None))(a =>
            a.strOpt.toRight("a tool's action is not a string").map(Some(_))
          )
      } yield Entry(named, does, parameters, asks, retry, writes, requires, action)
    def stored(w: ujson.Value): Either[String, Writes[Place]] =
      for {
        o <- w.objOpt.toRight("a tool's writes is not an object")
        describe <- field(o, "describe").flatMap(
          _.strOpt.toRight("a tool's writes' describe is not a string")
        )
        to <- field(o, "to").flatMap(_.arrOpt.toRight("a tool's writes' to is not an array"))
        named <- to.toVector
          .foldLeft[Either[String, Vector[(String, Place)]]](Right(Vector.empty)) { (acc, t) =>
            acc.flatMap { done =>
              for {
                n <- t.objOpt.toRight("a destination is not an object")
                name <- field(n, "name").flatMap(
                  _.strOpt.toRight("a destination's name is not a string")
                )
                written <- field(n, "place").flatMap(
                  _.strOpt.toRight("a destination's place is not a string")
                )
                at <- Place.read(written)
              } yield done :+ (name -> at)
            }
          }
        writes <- Writes.placedAt(VectorMap.from(named), describe)
      } yield writes
    for {
      o <- v.objOpt.toRight("a tool set is not an object")
      tools <- o.get("tools").flatMap(_.arrOpt).toRight("a tool set has no tools")
      entries <- tools.toVector.foldLeft[Either[String, Vector[Entry]]](Right(Vector.empty)) {
        (acc, t) => acc.flatMap(done => entry(t).map(done :+ _))
      }
      set <- of(entries).left.map(_.message)
    } yield set
  }
}

/** A tool set's content hash ([[ToolSet.id]]). */
opaque type ToolSetId = String

object ToolSetId {

  private[tool] def apply(value: String): ToolSetId = value

  def value(id: ToolSetId): String = id

  /** `text` as an id, or why not: not 16 lowercase hex digits. */
  def of(text: String): Either[String, ToolSetId] =
    if (text.length == 16 && text.forall(c => c.isDigit || ('a' to 'f').contains(c))) Right(text)
    else Left(s"not a tool set id: $text")
}
