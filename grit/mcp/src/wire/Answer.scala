package grit.mcp.wire

import grit.core.host.{Clipped, Kept}

/** What a call answered: `text`, what the model reads, and whether the tool reported an
  * error (`isError`).
  */
final case class Answer private (text: String, isError: Boolean)

object Answer {

  /** The answer a complete `tools/call` result is, `isError` absent read as false. Its text is
    * the result's content blocks, one after another on lines of their own: text, and an
    * embedded resource's text, as they are; a resource link as `[resource {name}: {uri}]`;
    * an image, audio, binary resource or a block of an unknown type as `[{mimeType}
    * omitted]`; then `structuredContent` as compact JSON, only when no block is text. Cut to
    * [[Clipped]]'s limits from the head, with a hint saying how much was kept.
    * [[McpError.Unreadable]] when the result has no `content` array, or an `isError` that is
    * not a boolean.
    */
  def of(result: ujson.Obj): Either[McpError, Answer] = {
    val o = result.obj
    for {
      content <- o
        .get("content")
        .flatMap(_.arrOpt)
        .toRight(McpError.Unreadable("a tools/call result has no content array"))
      isError <- o.get("isError") match {
        case None | Some(ujson.Null) => Right(false)
        case Some(ujson.Bool(b)) => Right(b)
        case Some(_) => Left(McpError.Unreadable("a tools/call result's isError is not a boolean"))
      }
    } yield {
      val blocks = content.toVector.map(block)
      val structured =
        o.get("structuredContent")
          .filter(_ => !blocks.exists(_._2))
          .map(ujson.write(_))
      val all = blocks.map(_._1) ++ structured
      new Answer(Clipped.head(all.mkString("\n"), hint).show, isError)
    }
  }

  /** `block` as the model reads it, and whether it is text. */
  private def block(block: ujson.Value): (String, Boolean) = {
    def str(v: ujson.Value, key: String): Option[String] =
      v.objOpt.flatMap(_.get(key)).flatMap(_.strOpt)
    str(block, "type") match {
      case Some("text") => (str(block, "text").getOrElse(""), true)
      case Some("resource_link") =>
        (
          s"[resource ${str(block, "name").getOrElse("")}: ${str(block, "uri").getOrElse("")}]",
          false
        )
      case Some("resource") =>
        val r = block.objOpt.flatMap(_.get("resource")).getOrElse(ujson.Null)
        str(r, "text") match {
          case Some(text) => (text, true)
          case None =>
            val what = (str(r, "mimeType").toVector ++ str(r, "uri")).mkString(" ")
            (s"[${if (what.isEmpty) "resource" else what} omitted]", false)
        }
      case kind => (s"[${str(block, "mimeType").orElse(kind).getOrElse("content")} omitted]", false)
    }
  }

  private def hint(kept: Option[Kept]): Option[String] =
    kept.map { k =>
      if (k.lines == 0)
        s"The answer's first line alone is over ${Clipped.MaxBytes} bytes, so none of it is shown."
      else s"The answer was cut to its first ${k.lines} of ${k.total} lines."
    }
}
