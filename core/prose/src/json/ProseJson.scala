package grit.prose.json

import java.time.{LocalDate, LocalDateTime}

import scala.util.Try

import grit.prose.form.{Block, Cell, Column, Doc, Item, Level, Mark, Span, Text}

/** Prose as grit stores it: a flat list of blocks, a list item's level and kind as fields, under
  * a schema version (`"v"`), read back to the doc it was written from. Notes are written by
  * `note`, once each, and referred to by index from the spans carrying them. Its records are
  * described in `core/prose/README.md`.
  */
object ProseJson {

  /** 1: the version [[write]] writes and the one [[read]] reads. */
  val Version: Int = 1

  /** `doc` as stored, `{"v": [[Version]], "notes": [...], "blocks": [...]}`: each distinct note
    * of `doc` written by `note` once, in the order it first appears, and named by its index
    * from each span carrying it; the blocks as a flat list of records, a nested list's items by
    * their level and a nested quote's blocks by their depth. [[read]] of it, given a `note`
    * reader that inverts `note`, is `doc`. Total: every doc is written.
    */
  def write[A <: caps.Pure](doc: Doc[A], note: A -> ujson.Value): ujson.Value = {
    val notes: Vector[A] = doc.notes.distinct
    val index: Map[A, Int] = notes.zipWithIndex.toMap
    def text(t: Text[A]): ujson.Value = ujson.Arr.from(t.spans.map(span(_, index)))
    ujson.Obj(
      "v" -> Version,
      "notes" -> ujson.Arr.from(notes.map(note)),
      "blocks" -> ujson.Arr.from(stream(doc.blocks, text))
    )
  }

  /** `json` as the doc it was written from, or why not: another version (named), a note `note`
    * refuses (its reason), or not this form.
    */
  def read[A <: caps.Pure](
      json: ujson.Value,
      note: ujson.Value -> Either[String, A]
  ): Either[String, Doc[A]] =
    for {
      o <- obj(json, "prose")
      _ <- o.get("v").flatMap(_.numOpt) match {
        case Some(n) if n == Version => Right(())
        case Some(n) => Left(s"prose of version ${fmt(n)}, which this reader does not know")
        case None => Left("prose with no version")
      }
      notes <- array(o.get("notes"), "prose's notes").flatMap(traverse(_)(note))
      records <- array(o.get("blocks"), "prose's blocks").flatMap(traverse(_)(obj(_, "a block")))
      blocks <- new Reader[A](notes).stream(records)
    } yield Doc(blocks)

  // ---------------------------------------------------------------------------------------
  // Writing. A doc is a stream of records. A list is its items, each `"item"` at level 1 with
  // its first paragraph as its `"text"`, the rest of its blocks after it a level deeper: a
  // nested list's items as items, any other block as `"item-more"` holding it. A quote is its
  // blocks, each as `"quote"` holding it, a quote inside one a depth deeper. A list with no
  // items is one `"list"` record. A list or quote directly after another of its kind opens
  // with `"new"`, which would otherwise continue it (core/prose/README.md says how).
  // ---------------------------------------------------------------------------------------

  private def stream[A <: caps.Pure](
      blocks: Vector[Block[A]],
      text: Text[A] -> ujson.Value
  ): Vector[ujson.Obj] =
    blocks.indices.toVector.flatMap { k =>
      val fresh = (blocks.lift(k - 1), blocks.lift(k)) match {
        case (Some(Block.Bullets(_)), Some(Block.Bullets(_))) => true
        case (Some(Block.Numbered(_, _)), Some(Block.Numbered(_, _))) => true
        case (Some(Block.Quote(_)), Some(Block.Quote(_))) => true
        case _ => false
      }
      blocks.lift(k).toVector.flatMap(records(_, fresh, text))
    }

  private def records[A <: caps.Pure](
      b: Block[A],
      fresh: Boolean,
      text: Text[A] -> ujson.Value
  ): Vector[ujson.Obj] = b match {
    case Block.Bullets(items) => list("bullet", None, items, fresh, text)
    case Block.Numbered(start, items) => list("number", Some(start), items, fresh, text)
    case Block.Quote(bs) =>
      val inner = stream(bs, text)
      val lifted =
        if (inner.isEmpty) Vector(ujson.Obj("type" -> "quote", "depth" -> 1))
        else inner.map(quoted)
      lifted.zipWithIndex.map { (r, k) =>
        if (fresh && k == 0) r("new") = 1
        r
      }
    case Block.Paragraph(t) => Vector(ujson.Obj("type" -> "para", "text" -> text(t)))
    case Block.Heading(l, t) =>
      Vector(ujson.Obj("type" -> "heading", "level" -> l.value, "text" -> text(t)))
    case Block.Code(language, lines) =>
      val o = ujson.Obj("type" -> "code", "lines" -> ujson.Arr.from(lines.map(ujson.Str(_))))
      language.foreach(l => o("language") = l)
      Vector(o)
    case Block.Table(columns, rows) =>
      Vector(
        ujson.Obj(
          "type" -> "table",
          "columns" -> ujson.Arr.from(
            columns.map(c => ujson.Obj("name" -> text(c.name)))
          ),
          "rows" -> ujson.Arr.from(rows.map(r => ujson.Arr.from(r.map(cell(_, text)))))
        )
      )
    case Block.Rule => Vector(ujson.Obj("type" -> "rule"))
  }

  private def list[A <: caps.Pure](
      kind: String,
      start: Option[Int],
      items: Vector[Item[A]],
      fresh: Boolean,
      text: Text[A] -> ujson.Value
  ): Vector[ujson.Obj] =
    if (items.isEmpty) {
      val o = ujson.Obj("type" -> "list", "list" -> kind)
      start.foreach(s => o("start") = s)
      Vector(o)
    } else
      items.zipWithIndex.flatMap { (item, k) =>
        val head = ujson.Obj("type" -> "item", "list" -> kind, "level" -> 1)
        if (k == 0) start.foreach(s => head("start") = s)
        if (k == 0 && fresh) head("new") = true
        val rest = item.blocks.headOption match {
          case Some(Block.Paragraph(t)) =>
            head("text") = text(t)
            item.blocks.drop(1)
          case _ => item.blocks
        }
        head +: stream(rest, text).map(deeper)
      }

  /** `r`, a record of an item's blocks, as a record of the stream its list is in. */
  private def deeper(r: ujson.Obj): ujson.Obj = r.value.get("type").flatMap(_.strOpt) match {
    case Some("item") | Some("item-more") =>
      r("level") = r.value.get("level").flatMap(_.numOpt).fold(1.0)(_ + 1)
      r
    case _ => ujson.Obj("type" -> "item-more", "level" -> 1, "block" -> r)
  }

  /** `r`, a record of a quote's blocks, as a record of the stream the quote is in. */
  private def quoted(r: ujson.Obj): ujson.Obj = r.value.get("type").flatMap(_.strOpt) match {
    case Some("quote") =>
      r("depth") = r.value.get("depth").flatMap(_.numOpt).fold(1.0)(_ + 1)
      r.value.get("new").flatMap(_.numOpt).foreach(n => r("new") = n + 1)
      r
    case _ => ujson.Obj("type" -> "quote", "depth" -> 1, "block" -> r)
  }

  private def cell[A <: caps.Pure](c: Cell[A], text: Text[A] -> ujson.Value): ujson.Obj = {
    val o = c match {
      case Cell.Words(t, _) => ujson.Obj("w" -> text(t))
      case Cell.Number(n, shown, _) =>
        ujson.Obj("n" -> n.bigDecimal.toPlainString, "shown" -> text(shown))
      case Cell.Date(d, shown, _) => ujson.Obj("d" -> d.toString, "shown" -> text(shown))
      case Cell.Truth(b, _) => ujson.Obj("b" -> b)
      case Cell.Empty(_) => ujson.Obj("empty" -> true)
    }
    c.formula.foreach(f => o("formula") = f)
    o
  }

  private def span[A <: caps.Pure](s: Span[A], index: Map[A, Int]): ujson.Value = {
    val o = ujson.Obj("t" -> s.text)
    if (s.marks.nonEmpty) o("m") = ujson.Arr.from(s.marks.toVector.map(mark).sortBy(ujson.write(_)))
    if (s.notes.nonEmpty)
      o("n") = ujson.Arr.from(s.notes.flatMap(n => index.get(n).map(ujson.Num(_))))
    o
  }

  private def mark(m: Mark): ujson.Value = m match {
    case Mark.Strong => "strong"
    case Mark.Emphasis => "emphasis"
    case Mark.Code => "code"
    case Mark.Link(href) => ujson.Obj("link" -> href)
  }

  // ---------------------------------------------------------------------------------------
  // Reading: the stream back to blocks, a run of items to its lists, a run of quotes to its
  // quotes, each record of an item or a quote read in the stream of that item's or quote's
  // blocks.
  // ---------------------------------------------------------------------------------------

  private type Read[B] = Either[String, B]

  /** A record as read: its fields. */
  private type Obj = Map[String, ujson.Value]

  private final class Reader[A <: caps.Pure](notes: Vector[A]) {

    def stream(records: Vector[Obj]): Read[Vector[Block[A]]] = {
      // Each record's run: items and their more with items, quotes with quotes, else alone.
      def run(r: Obj): String = typeOf(r) match {
        case "item" | "item-more" => "item"
        case other => other
      }
      val grouped =
        runs(records)((before, r) =>
          run(before) == run(r) && (run(r) == "item" || run(r) == "quote")
        )
      traverse(grouped)(rs =>
        rs.headOption.map(run) match {
          case Some("item") => lists(rs)
          case Some("quote") => quotes(rs)
          case _ => traverse(rs)(leaf).map(_.flatten)
        }
      ).map(_.flatten)
    }

    private def lists(records: Vector[Obj]): Read[Vector[Block[A]]] = {
      // Each item: its record, then the records of its later blocks, a level deeper.
      val items = runs(records)((_, r) => !(typeOf(r) == "item" && level(r).contains(1)))
      for {
        read <- traverse(items)(rs => item(rs).map(i => Listed(rs.headOption, i)))
        grouped = runs(read)((before, next) =>
          before.head.flatMap(kindOf) == next.head.flatMap(kindOf) &&
            !next.head.flatMap(_.get("new")).flatMap(_.boolOpt).contains(true)
        )
        blocks <- traverse(grouped)(g =>
          g.headOption.flatMap(_.head) match {
            case Some(h) if kindOf(h).contains("bullet") => Right(Block.Bullets(g.map(_.item)))
            case Some(h) if kindOf(h).contains("number") =>
              int(h.get("start"), "a numbered list's start").map(Block.Numbered(_, g.map(_.item)))
            case other => Left(s"a list of no kind: ${other.fold("none")(shownRecord)}")
          }
        )
      } yield blocks
    }

    /** The item `records` opens with, its later blocks read from the rest. */
    private def item(records: Vector[Obj]): Read[Item[A]] = {
      val head = records.headOption.getOrElse(Map.empty)
      val more = records.drop(1)
      for {
        _ <- typeOf(head) match {
          case "item" if level(head).contains(1) => Right(())
          case "item" =>
            Left(s"an item at level ${level(head).getOrElse(0)} with no item before it at level 1")
          case _ => Left("an item-more with no item before it at its level")
        }
        first <- head
          .get("text")
          .fold(Right(Vector.empty): Read[Vector[Block[A]]])(t =>
            text(Some(t), "an item's text").map(t => Vector(Block.Paragraph(t)))
          )
        inner <- traverse(more)(shallower)
        rest <- stream(inner.flatten)
      } yield Item(first ++ rest)
    }

    /** `r`, a record of the stream an item's list is in, as a record of that item's blocks. */
    private def shallower(r: Obj): Read[Vector[Obj]] =
      (typeOf(r), level(r)) match {
        case ("item-more", Some(1)) => r.get("block").toVector.traverseObj("an item-more's block")
        case (_, Some(n)) if n > 1 => Right(Vector(r.updated("level", ujson.Num(n - 1))))
        case _ => Left(s"a list's record at no level: ${shownRecord(r)}")
      }

    private def quotes(records: Vector[Obj]): Read[Vector[Block[A]]] = {
      val grouped = runs(records)((_, r) => !r.get("new").flatMap(_.numOpt).contains(1))
      traverse(grouped)(g =>
        traverse(g)(unquoted).flatMap(inner => stream(inner.flatten)).map(Block.Quote(_))
      )
    }

    /** `r`, a record of the stream a quote is in, as a record of that quote's blocks. */
    private def unquoted(r: Obj): Read[Vector[Obj]] =
      r.get("depth").flatMap(_.numOpt) match {
        case Some(1) => r.get("block").toVector.traverseObj("a quote's block")
        case Some(d) if d > 1 && d.isWhole =>
          val deeper = r.updated("depth", ujson.Num(d - 1))
          Right(Vector(r.get("new").flatMap(_.numOpt) match {
            case Some(n) if n > 1 => deeper.updated("new", ujson.Num(n - 1))
            case _ => deeper - "new"
          }))
        case _ => Left(s"a quote at no depth: ${shownRecord(r)}")
      }

    private def leaf(r: Obj): Read[Vector[Block[A]]] = typeOf(r) match {
      case "para" => text(r.get("text"), "a paragraph's text").map(t => Vector(Block.Paragraph(t)))
      case "heading" =>
        for {
          l <- int(r.get("level"), "a heading's level")
          t <- text(r.get("text"), "a heading's text")
        } yield Vector(Block.Heading(Level.of(l), t))
      case "code" =>
        array(r.get("lines"), "a code block's lines")
          .flatMap(traverse(_)(l => string(Some(l), "a code block's line")))
          .map(lines => Vector(Block.Code(r.get("language").flatMap(_.strOpt), lines)))
      case "table" =>
        for {
          columns <- array(r.get("columns"), "a table's columns").flatMap(traverse(_)(column))
          rows <- array(r.get("rows"), "a table's rows").flatMap(
            traverse(_)(row => array(Some(row), "a table's row").flatMap(traverse(_)(cell)))
          )
        } yield Vector(Block.table(columns, rows))
      case "list" =>
        kindOf(r) match {
          case Some("bullet") => Right(Vector(Block.Bullets(Vector.empty)))
          case Some("number") =>
            int(r.get("start"), "a numbered list's start").map(s =>
              Vector(Block.Numbered(s, Vector.empty))
            )
          case _ => Left(s"a list of no kind: ${shownRecord(r)}")
        }
      case "rule" => Right(Vector(Block.Rule))
      case other => Left(s"a block of no kind this reader knows: $other")
    }

    private def column(v: ujson.Value): Read[Column[A]] =
      for {
        o <- obj(v, "a column")
        name <- text(o.get("name"), "a column's name")
      } yield Column(name)

    private def cell(v: ujson.Value): Read[Cell[A]] =
      obj(v, "a cell").flatMap { o =>
        val formula = o.get("formula").flatMap(_.strOpt)
        def shown = plainText(o.get("shown"), "a cell's shown text")
        (o.get("w"), o.get("n"), o.get("d"), o.get("b"), o.get("empty")) match {
          case (Some(w), _, _, _, _) => text(Some(w), "a cell's words").map(Cell.Words(_, formula))
          case (_, Some(n), _, _, _) =>
            for {
              s <- string(Some(n), "a number cell's value")
              value <- Try(BigDecimal(s)).toOption.toRight(s"no number: $s")
              t <- shown
            } yield Cell.Number(value, t, formula)
          case (_, _, Some(d), _, _) =>
            for {
              s <- string(Some(d), "a date cell's value")
              value <- Try(LocalDate.parse(s): LocalDate | LocalDateTime)
                .orElse(Try(LocalDateTime.parse(s)))
                .toOption
                .toRight(s"no date: $s")
              t <- shown
            } yield Cell.Date(value, t, formula)
          case (_, _, _, Some(b), _) =>
            b.boolOpt.toRight(s"no truth: ${ujson.write(b)}").map(Cell.Truth(_, formula))
          case (_, _, _, _, Some(_)) => Right(Cell.Empty(formula))
          case _ => Left(s"a cell of no kind: ${ujson.write(v)}")
        }
      }

    private def text(v: Option[ujson.Value], what: String): Read[Text[A]] =
      array(v, what).flatMap(traverse(_)(span)).map(Text(_))

    /** A text whose spans carry no notes: a number's or a date's shown text. */
    private def plainText(v: Option[ujson.Value], what: String): Read[Text[Nothing]] =
      text(v, what).flatMap(t =>
        if (t.spans.exists(_.notes.nonEmpty)) Left(s"$what carries notes")
        else Right(Text(t.spans.map(s => Span(s.text, s.marks))))
      )

    private def span(v: ujson.Value): Read[Span[A]] =
      for {
        o <- obj(v, "a span")
        t <- string(o.get("t"), "a span's text")
        marks <- o
          .get("m")
          .fold(Right(Vector.empty): Read[Vector[Mark]])(m =>
            array(Some(m), "a span's marks").flatMap(traverse(_)(readMark))
          )
        noted <- o
          .get("n")
          .fold(Right(Vector.empty): Read[Vector[A]])(n =>
            array(Some(n), "a span's notes").flatMap(
              traverse(_)(i =>
                i.numOpt
                  .filter(_.isWhole)
                  .flatMap(k => notes.lift(k.toInt))
                  .toRight(s"a span's note is no note's index: ${ujson.write(i)}")
              )
            )
          )
      } yield Span(t, marks.toSet, noted)
  }

  private def readMark(v: ujson.Value): Read[Mark] = v.strOpt match {
    case Some("strong") => Right(Mark.Strong)
    case Some("emphasis") => Right(Mark.Emphasis)
    case Some("code") => Right(Mark.Code)
    case _ =>
      v.objOpt
        .flatMap(_.get("link"))
        .flatMap(_.strOpt)
        .map(Mark.Link(_))
        .toRight(s"no mark: ${ujson.write(v)}")
  }

  private def typeOf(r: Obj): String = r.get("type").flatMap(_.strOpt).getOrElse("")

  private def level(r: Obj): Option[Int] =
    r.get("level").flatMap(_.numOpt).filter(_.isWhole).map(_.toInt)

  private def kindOf(r: Obj): Option[String] = r.get("list").flatMap(_.strOpt)

  extension (vs: Vector[ujson.Value]) {
    private def traverseObj(what: String): Read[Vector[Obj]] = traverse(vs)(obj(_, what))
  }

  /** An item read, and the record it opened with. */
  private final case class Listed[A <: caps.Pure](head: Option[Obj], item: Item[A])

  /** `xs` cut into runs, each element joining the run before it when `joins` it and the run's
    * last element.
    */
  private def runs[X](xs: Vector[X])(joins: (X, X) => Boolean): Vector[Vector[X]] = {
    val out = Vector.newBuilder[Vector[X]]
    var current: Vector[X] = Vector.empty
    xs.foreach { x =>
      if (current.lastOption.exists(joins(_, x))) current = current :+ x
      else {
        if (current.nonEmpty) out += current
        current = Vector(x)
      }
    }
    if (current.nonEmpty) out += current
    out.result()
  }

  private def traverse[X, B](xs: Vector[X])(f: X => Read[B]): Read[Vector[B]] =
    xs.foldLeft(Right(Vector.empty): Read[Vector[B]])((acc, x) =>
      acc.flatMap(bs => f(x).map(bs :+ _))
    )

  private def obj(v: ujson.Value, what: String): Read[Obj] =
    v.objOpt.map(_.toMap).toRight(s"$what is no object: ${ujson.write(v)}")

  private def shownRecord(r: Obj): String = ujson.write(ujson.Obj.from(r))

  private def array(v: Option[ujson.Value], what: String): Read[Vector[ujson.Value]] =
    v.flatMap(_.arrOpt).map(_.toVector).toRight(s"$what: no array")

  private def string(v: Option[ujson.Value], what: String): Read[String] =
    v.flatMap(_.strOpt).toRight(s"$what: no string")

  private def int(v: Option[ujson.Value], what: String): Read[Int] =
    v.flatMap(_.numOpt)
      .filter(n => n.isWhole && n.abs <= Int.MaxValue)
      .map(_.toInt)
      .toRight(s"$what: no whole number")

  private def fmt(n: Double): String = if (n.isWhole) n.toLong.toString else n.toString
}
