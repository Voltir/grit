package grit.core.document

import grit.core.id.DocumentVersionId
import grit.core.tool.{ToolName, Writes}

/** The hosted call that reads what a document stands for: `tool`, named as the model is
  * offered it (an MCP server's tool under its server's prefix), with [[arguments]]. A call
  * follows it when it names the same tool and every one of its arguments equals the call's
  * argument of that name; the call may hold more, so a read at another ref or page still follows.
  */
final case class Pointer private (tool: ToolName, private val pointed: ujson.Obj) {

  /** Its arguments, every object's keys sorted at every depth: a copy, so a change to it changes
    * no pointer.
    */
  def arguments: ujson.Obj = ujson.Obj.from(pointed.value.map((k, v) => k -> ujson.copy(v)))

  /** `Read it with {tool} {arguments as compact JSON}`: the line a window shows under the
    * document.
    */
  def shown: String = s"Read it with ${ToolName.value(tool)} ${ujson.write(pointed)}"

  /** This pointer with its argument `name` set to `value`, in place of any it held, or why not,
    * as [[Pointer.of]] refuses.
    */
  def setting(name: String, value: ujson.Value): Either[String, Pointer] = {
    val others = pointed.value.toVector.filter(_._1 != name).map((k, v) => k -> ujson.copy(v))
    Pointer.of(tool, name -> value, others*)
  }

  /** Whether a call of `tool` with `arguments` follows it; one whose `arguments` is not an
    * object never does.
    */
  def followedBy(tool: ToolName, arguments: ujson.Value): Boolean =
    arguments match {
      case call: ujson.Obj =>
        tool == this.tool &&
        pointed.value.forall { case (key, value) => call.value.get(key).contains(value) }
      case _ => false
    }
}

object Pointer {

  /** One call a turn made: `tool` as offered, its `arguments`, and whether its result was not an
    * error.
    */
  final case class Call(tool: ToolName, arguments: ujson.Value, succeeded: Boolean)

  /** The versions among `documents` whose pointer one of `calls` followed and that succeeded;
    * once each, in `documents`' order.
    */
  def followedIn(documents: Vector[Document], calls: Vector[Call]): Vector[DocumentVersionId] =
    documents.collect {
      case d
          if d.pointer
            .exists(p => calls.exists(c => c.succeeded && p.followedBy(c.tool, c.arguments))) =>
        d.version
    }.distinct

  /** This pointer, its arguments kept with every object's keys sorted at every depth, or why
    * not: two arguments of one name; one named [[grit.core.tool.Writes.Field]] (a call naming
    * its destination writes, ADR 0031; a pointer names a read); or their compact JSON over 1000
    * characters.
    */
  def of(
      tool: ToolName,
      first: (String, ujson.Value),
      rest: (String, ujson.Value)*
  ): Either[String, Pointer] = {
    val names = (first +: rest).map(_._1)
    val arguments = sorted(ujson.Obj.from(first +: rest))
    names.find(name => names.count(_ == name) > 1) match {
      case Some(name) => Left(s"a pointer names argument $name twice")
      case None if names.contains(Writes.Field) =>
        Left(s"a pointer names no destination (\"${Writes.Field}\"): it reads")
      case None if ujson.write(arguments).length > 1000 =>
        Left("a pointer's arguments are over 1000 characters of JSON")
      case None => Right(new Pointer(tool, arguments))
    }
  }

  /** A copy of `obj`, every object's keys sorted at every depth. */
  private def sorted(obj: ujson.Obj): ujson.Obj =
    ujson.Obj.from(obj.value.toVector.sortBy(_._1).map((k, v) => k -> canonical(v)))

  private def canonical(value: ujson.Value): ujson.Value =
    value match {
      case obj: ujson.Obj => sorted(obj)
      case arr: ujson.Arr => ujson.Arr.from(arr.value.map(canonical))
      case other => other
    }
}
