package grit.core.tool

import grit.core.id.ShortHash

/** A tool set as offered to one turn: each tool's name, description, schema, whether it asks
  * a person first, and its retry, in the order offered. Equal sets have equal ids
  * ([[ToolSet.id]]), so a set is kept once however many turns are offered it.
  */
final case class ToolSet private (tools: Vector[ToolSet.Entry]) {

  /** A content hash of the set's stored form ([[ToolSet.write]]). */
  def id: ToolSetId = ToolSetId(ShortHash.of(ujson.write(ToolSet.write(this))))

  /** The entry for the tool named `name`; `None` when the set has no such tool. */
  def named(name: ToolName): Option[ToolSet.Entry] = tools.find(_.name == name)
}

object ToolSet {

  /** One tool of a set: `parameters` is its schema as the model is shown it, not strict. */
  final case class Entry(
      name: ToolName,
      does: String,
      parameters: ujson.Value,
      asks: Boolean,
      retry: Retry
  )

  val Empty: ToolSet = ToolSet(Vector.empty)

  /** `entries` as a set, in this order; `Left` naming the first name repeated. */
  def of(entries: Vector[Entry]): Either[DuplicateName, ToolSet] = {
    val names = entries.map(_.name)
    names.diff(names.distinct).headOption match {
      case Some(repeated) => Left(DuplicateName(repeated))
      case None => Right(ToolSet(entries))
    }
  }

  /** The stored form: `{"tools":[{"name","does","parameters","asks","retry"}, …]}`, in the
    * set's order. The id hashes it, so a change here changes every set's id.
    */
  def write(set: ToolSet): ujson.Value =
    ujson.Obj(
      "tools" -> ujson.Arr.from(set.tools.map { e =>
        ujson.Obj(
          "name" -> ToolName.value(e.name),
          "does" -> e.does,
          "parameters" -> e.parameters,
          "asks" -> e.asks,
          "retry" -> e.retry.key
        )
      })
    )

  /** The set stored as `v` ([[write]]'s form), or why it is none. */
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
      } yield Entry(named, does, parameters, asks, retry)
    for {
      o <- v.objOpt.toRight("a tool set is not an object")
      tools <- o.get("tools").flatMap(_.arrOpt).toRight("a tool set has no tools")
      entries <- tools.toVector.foldLeft[Either[String, Vector[Entry]]](Right(Vector.empty)) {
        (acc, t) => acc.flatMap(done => entry(t).map(done :+ _))
      }
      set <- of(entries).left.map(d => s"a tool set names ${ToolName.value(d.name)} twice")
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
