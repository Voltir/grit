package grit.core.document

import grit.core.tool.{ToolName, Writes}

/** The hosted call that reads what a document stands for: `tool`, named as the model is
  * offered it (an MCP server's tool under its server's prefix), with `arguments`. A call follows
  * it when it names the same tool and every one of `arguments` equals the call's argument of that
  * name; the call may hold more, so a read at another ref or page still follows.
  */
final case class Pointer private (tool: ToolName, arguments: ujson.Obj) {

  /** `Read it with {tool} {arguments as compact JSON, keys sorted}`: the line a window shows
    * under the document.
    */
  def shown: String = s"Read it with ${ToolName.value(tool)} ${ujson.write(arguments)}"

  /** Whether a call of `tool` with `arguments` follows it; one whose `arguments` is not an
    * object never does.
    */
  def followedBy(tool: ToolName, arguments: ujson.Value): Boolean =
    arguments match {
      case call: ujson.Obj =>
        tool == this.tool &&
        this.arguments.value.forall { case (key, value) => call.value.get(key).contains(value) }
      case _ => false
    }
}

object Pointer {

  /** This pointer, its arguments kept with their keys sorted, or why not: two arguments of one
    * name; one named [[grit.core.tool.Writes.Field]] (a call naming its destination writes,
    * ADR 0031; a pointer names a read); or their compact JSON over 1000 characters.
    */
  def of(
      tool: ToolName,
      first: (String, ujson.Value),
      rest: (String, ujson.Value)*
  ): Either[String, Pointer] = {
    val sorted = (first +: rest).sortBy(_._1)
    val names = sorted.map(_._1)
    val arguments = sorted.foldLeft(ujson.Obj()) { (obj, pair) =>
      obj.value.update(pair._1, pair._2)
      obj
    }
    names.find(name => names.count(_ == name) > 1) match {
      case Some(name) => Left(s"a pointer names argument $name twice")
      case None if names.contains(Writes.Field) =>
        Left(s"a pointer names no destination (\"${Writes.Field}\"): it reads")
      case None if ujson.write(arguments).length > 1000 =>
        Left("a pointer's arguments are over 1000 characters of JSON")
      case None => Right(new Pointer(tool, arguments))
    }
  }
}
