package grit.mcp.wire

import grit.core.host.Clipped
import grit.core.tool.Trimmed
import grit.core.visibility.{Accounted, Facts, Itemised, ResultPart}

/** What a call answered: `text`, what the model reads, whether the tool reported an error
  * (`isError`), what its server's extension read of its items (`items`), and the parts it cut
  * the answer into (`parts`, `None` when it did not), each read only by those its readers admit
  * ([[grit.core.tool.Outcome.readable]]), and the data `text` is rendered from (`facts`).
  */
final case class Answer private (
    text: String,
    isError: Boolean,
    items: Accounted,
    parts: Option[Vector[ResultPart]],
    facts: Option[Facts]
)

object Answer {

  /** The answer a complete `tools/call` result is, its items `items`, `isError` absent read as
    * false. Its text is
    * the result's content blocks, one after another on lines of their own: text, and an
    * embedded resource's text, as they are; a resource link as `[resource {name}: {uri}]`;
    * an image, audio, binary resource or a block of an unknown type as `[{mimeType}
    * omitted]`; then `structuredContent` as compact JSON, only when no block is text. When
    * that is over [[Clipped]]'s limits and its first text block that parses as a JSON array,
    * or as an object with exactly one array-valued key, can be cut to at least one leading
    * item with which it fits, that block is cut to the most such items, as compact JSON,
    * with a hint saying how many of how many items are shown when some are left out.
    * Otherwise it is cut to [[Clipped]]'s limits from the head, with a hint saying how much
    * was kept. Its facts are the result's `structuredContent`, as the result holds it (under a
    * scope that is open, as the server sent it, unfiltered): whole when the text is not cut;
    * when the text is cut by items and `structuredContent` is such a list of as many items, that
    * list cut to the same leading items; otherwise none, as when there is no
    * `structuredContent`.
    * [[McpError.Unreadable]] when the result has no `content` array, or an `isError` that is
    * not a boolean.
    */
  def of(result: ujson.Obj, items: Accounted = Accounted.Unread): Either[McpError, Answer] =
    read(result).map { (content: Vector[ujson.Value], isError: Boolean) =>
      val blocks = content.map(block)
      val structure = result.obj.get("structuredContent")
      val all = blocks.map(_._1) ++ structure.filter(_ => !blocks.exists(_._2)).map(ujson.write(_))
      val whole = all.mkString("\n")
      if (Clipped.fits(whole)) new Answer(whole, isError, items, None, structure.map(Facts.of))
      else
        byItems(all, blocks.map(_._2)) match {
          case Some((clipped, kept, of)) =>
            // The structure's list, when it is one of as many items, cut as the text's was.
            val facts = structure
              .flatMap(listedIn)
              .filter(_.items.size == of)
              .map(list => Facts.of(list.holding(list.items.take(kept))))
            new Answer(clipped.show, isError, items, None, facts)
          case None =>
            new Answer(Clipped.head(whole, Clipped.cutHint).show, isError, items, None, None)
        }
    }

  /** The answer a complete `tools/call` result is as its server's extension cut it
    * (`itemised`): its text the cut's head, its items unread, its parts the cut's, the two
    * fitted to [[Clipped]]'s limits as [[Trimmed.of]] trims them, its facts the cut's as they
    * are, `isError` read as [[of]] reads it. [[McpError.Unreadable]] as for [[of]].
    */
  def cut(result: ujson.Obj, itemised: Itemised.Parts): Either[McpError, Answer] =
    read(result).map { (_: Vector[ujson.Value], isError: Boolean) =>
      val trimmed = Trimmed.of(itemised.head, itemised.parts)
      new Answer(trimmed.head, isError, Accounted.Unread, trimmed.parts, itemised.facts)
    }

  /** `result`'s content blocks and whether it is an error; [[McpError.Unreadable]] when it has
    * no `content` array, or an `isError` that is not a boolean.
    */
  private def read(result: ujson.Obj): Either[McpError, (Vector[ujson.Value], Boolean)] = {
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
    } yield (content.toVector, isError)
  }

  /** `all`, the answer's blocks as the model reads them, with its first text block
    * (`isText`) that is a JSON list (`listed`) cut to the most leading items with which `all`
    * fits, as compact JSON, and a hint saying how many of how many are shown when some are
    * left out, with how many it kept of how many; `None` when `all` fits whole, no text block
    * is a list, or not even one item fits.
    */
  private def byItems(all: Vector[String], isText: Vector[Boolean]): Option[(Clipped, Int, Int)] =
    if (Clipped.fits(all.mkString("\n"))) None
    else
      all.indices.iterator
        .filter(i => isText.lift(i).contains(true))
        .flatMap(i => all.lift(i).flatMap(listed).map(i -> _))
        .nextOption()
        .flatMap { (i, list) =>
          val items = list.items
          def joined(k: Int): String =
            all.updated(i, ujson.write(list.holding(items.take(k)))).mkString("\n")
          // joined(k) grows with k, so the most items that fit are the last k that does.
          val k = Clipped.most(items.size)(joined)
          val n = items.size
          Option.when(k > 0) {
            val clipped =
              if (k == n) Clipped.head(joined(k), Clipped.cutHint)
              else
                Clipped.head(
                  joined(k),
                  {
                    case None =>
                      Some(
                        s"Showing the first $k of the $n items in this answer; call again " +
                          "asking for fewer, or for the next page, to see the rest."
                      )
                    case kept => Clipped.cutHint(kept)
                  }
                )
            (clipped, k, n)
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
    scala.util.Try(ujson.read(text)).toOption.flatMap(listedIn)

  /** `v` as a list, when it is a JSON array, or an object with exactly one array-valued field. */
  private def listedIn(v: ujson.Value): Option[Listed] =
    v match {
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

}
