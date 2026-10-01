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
    * omitted]`; then `structuredContent` as compact JSON, only when no block is text. When
    * that is over [[Clipped]]'s limits and its first text block that parses as a JSON array,
    * or as an object with exactly one array-valued key, can be cut to at least one leading
    * item with which it fits, that block is cut to the most such items, as compact JSON,
    * with a hint saying how many of how many items are shown when some are left out.
    * Otherwise it is cut to [[Clipped]]'s limits from the head, with a hint saying how much
    * was kept.
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
      val clipped = byItems(all, blocks.map(_._2)).getOrElse(Clipped.head(all.mkString("\n"), hint))
      new Answer(clipped.show, isError)
    }
  }

  /** Whether `text` is whole within [[Clipped]]'s limits: [[Clipped.head]] keeps it as it is
    * only then.
    */
  private def fits(text: String): Boolean = Clipped.head(text, _ => None).text == text

  /** `all`, the answer's blocks as the model reads them, with its first text block
    * (`isText`) that is a JSON list (`listed`) cut to the most leading items with which `all`
    * fits, as compact JSON, and a hint saying how many of how many are shown when some are
    * left out; `None` when `all` fits whole, no text block is a list, or not even one item
    * fits.
    */
  private def byItems(all: Vector[String], isText: Vector[Boolean]): Option[Clipped] =
    if (fits(all.mkString("\n"))) None
    else
      all.indices.iterator
        .filter(i => isText.lift(i).contains(true))
        .flatMap(i => all.lift(i).flatMap(listed).map(i -> _))
        .nextOption()
        .flatMap { (i, list) =>
          val items = list.items
          def joined(k: Int): String =
            all.updated(i, ujson.write(list.holding(items.take(k)))).mkString("\n")
          // The most items that fit: joined(k) grows with k, so search for the last k that fits.
          var lo = 0
          var hi = items.size
          while (lo < hi) {
            val mid = (lo + hi + 1) / 2
            if (fits(joined(mid))) lo = mid else hi = mid - 1
          }
          val k = lo
          val n = items.size
          Option.when(k > 0) {
            if (k == n) Clipped.head(joined(k), hint)
            else
              Clipped.head(
                joined(k),
                {
                  case None =>
                    Some(
                      s"Showing the first $k of the $n items in this answer; call again " +
                        "asking for fewer, or for the next page, to see the rest."
                    )
                  case kept => hint(kept)
                }
              )
          }
        }

  /** A JSON list as an answer's text block holds it: a bare array, or the one array-valued
    * field `key` of an object whose fields, in order, are `fields`.
    */
  private enum Listed {
    case Bare(items: Vector[ujson.Value])
    case Field(fields: Vector[(String, ujson.Value)], key: String, items: Vector[ujson.Value])

    def items: Vector[ujson.Value]

    /** The list with `kept` in place of its items. */
    def holding(kept: Vector[ujson.Value]): ujson.Value = this match {
      case Bare(_) => ujson.Arr.from(kept)
      case Field(fields, key, _) =>
        ujson.Obj.from(fields.map((f, v) => if (f == key) f -> ujson.Arr.from(kept) else f -> v))
    }
  }

  /** `text` as a list, when it parses as a JSON array, or as an object with exactly one
    * array-valued field.
    */
  private def listed(text: String): Option[Listed] =
    scala.util.Try(ujson.read(text)).toOption.flatMap {
      case ujson.Arr(items) => Some(Listed.Bare(items.toVector))
      case o: ujson.Obj =>
        val fields = o.value.toVector
        fields.collect { case (key, ujson.Arr(items)) => (key, items.toVector) } match {
          case Vector((key, items)) => Some(Listed.Field(fields, key, items))
          case _ => None
        }
      case _ => None
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
