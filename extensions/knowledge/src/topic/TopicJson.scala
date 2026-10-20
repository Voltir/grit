package grit.knowledge.topic

import java.time.{Instant, LocalDate}

import scala.util.Try

import grit.core.document.DocText
import grit.core.either.Traversal.traverse
import grit.core.id.{ConversationId, DocKey, DocumentVersion}
import grit.core.place.Place
import grit.core.stitch.StrandDigest
import grit.knowledge.ref.TopicKey
import grit.prose.form.{Block, Doc, Item, Level, Mark, Span, Text}
import grit.prose.markdown.Markdown

/** [[Topic]]'s data and text. A doc is written block by block, each an object keyed by its
  * kind; a span `{"text", "marks"?, "notes"?}`; a note `{"link": key}` or `{"claim": …}`, a
  * claim written whole at each span it notes.
  */
private object TopicJson {

  private type Read[A] = Either[String, A]

  def write(t: Topic): ujson.Value =
    ujson.Obj(
      "v" -> 1,
      "lead" -> doc(t.lead),
      "passages" -> ujson.Arr.from(t.passages.map(passage))
    )

  def read(key: TopicKey, v: ujson.Value): Read[Topic] =
    for {
      o <- obj(v, "a topic")
      _ <- o.get("v").flatMap(_.numOpt) match {
        case Some(1) => Right(())
        case Some(n) => Left(s"a topic of version ${n.toLong}, which this reader does not know")
        case None => Left("a topic with no version")
      }
      lead <- o.get("lead").toRight("a topic with no lead").flatMap(readDoc)
      passages <- array(o.get("passages"), "a topic's passages").flatMap(traverse(_)(readPassage))
      t = Topic.of(lead, passages)
      _ <- checked(key, t)
    } yield t

  def text(t: Topic): Read[DocText] = {
    val passages = t.passages.reverse.flatMap(p =>
      Block.Heading(Level.of(2), Text.plain(p.day.toString)) +: lowered(p.body).blocks
    )
    DocText.of(Markdown.write(Doc(lowered(t.lead).blocks ++ passages)))
  }

  /** `d` with a link note as a link to its topic's key where its span is no link already. */
  private def lowered(d: Doc[TopicNote]): Doc[Nothing] =
    d.mapSpans { s =>
      val link = s.notes.collectFirst { case TopicNote.Link(to) => Mark.Link(TopicKey.value(to)) }
      val linked = s.marks.exists {
        case Mark.Link(_) => true
        case _ => false
      }
      Span(s.text, if (linked) s.marks else s.marks ++ link)
    }

  /** `t`, or why not: two different claims of one id, or a relation naming a claim of `key`'s
    * topic that it does not hold.
    */
  private def checked(key: TopicKey, t: Topic): Read[Unit] = {
    val claims = (t.lead.notes ++ t.passages.flatMap(_.body.notes)).collect {
      case TopicNote.Claimed(c) => c
    }.distinct
    val ids = claims.map(_.id)
    val twice = ids.diff(ids.distinct).headOption
    val dangling = claims.iterator
      .flatMap(c => c.relations.map(r => (c.id, r.to)))
      .find((_, to) => to.topic == key && !ids.contains(to.claim))
    (twice, dangling) match {
      case (Some(id), _) => Left(s"two different claims of id ${ClaimId.value(id)}")
      case (_, Some((id, to))) =>
        Left(
          s"claim ${ClaimId.value(id)} bears on ${ClaimId.value(to.claim)}, which this topic does not hold"
        )
      case _ => Right(())
    }
  }

  private def passage(p: Passage): ujson.Value =
    ujson.Obj("day" -> p.day.toString, "from" -> lineage(p.from), "body" -> doc(p.body))

  private def readPassage(v: ujson.Value): Read[Passage] =
    for {
      o <- obj(v, "a passage")
      day <- string(o.get("day"), "a passage's day").flatMap(d =>
        Try(LocalDate.parse(d)).toOption.toRight(s"a passage's day is no date: $d")
      )
      from <- readLineage(o.get("from"))
      body <- o.get("body").toRight("a passage with no body").flatMap(readDoc)
    } yield Passage(day, from, body)

  /** An aside's data ([[Aside]]): `{"v": 2, "aside": [tag]}`, each tag as a lineage writes it
    * with `"at"`, when it was set aside, an ISO-8601 instant.
    */
  def aside(entries: Vector[Aside.Entry]): ujson.Value =
    ujson.Obj(
      "v" -> 2,
      "aside" -> ujson.Arr.from(entries.map(e => {
        val o = tag(e.tag)
        o("at") = e.at.toString
        o
      }))
    )

  /** The entries [[aside]] wrote; of version 1, which dated none, each dated `written`. */
  def readAside(v: ujson.Value, written: Instant): Read[Vector[Aside.Entry]] =
    obj(v, "an aside").flatMap(o =>
      o.get("v").flatMap(_.numOpt) match {
        case Some(1) =>
          readLineage(o.get("aside")).map(_.tags.map(Aside.Entry(_, written)))
        case Some(2) =>
          array(o.get("aside"), "an aside's entries").flatMap(
            traverse(_)(e =>
              for {
                t <- readTag(e)
                at <- string(obj(e, "an aside's entry").toOption.flatMap(_.get("at")), "its at")
                  .flatMap(s => Try(Instant.parse(s)).toOption.toRight(s"no instant: $s"))
              } yield Aside.Entry(t, at)
            )
          )
        case other => Left(s"an aside of version ${other.fold("none")(_.toString)}")
      }
    )

  private def lineage(l: Lineage): ujson.Value = ujson.Arr.from(l.tags.map(tag))

  private def tag(t: Lineage.Tag): ujson.Obj = t match {
    case Lineage.Tag.Strand(root, room, digest) =>
      ujson.Obj(
        "strand" -> ConversationId.value(root),
        "room" -> room.written,
        "digest" -> StrandDigest.value(digest)
      )
    case Lineage.Tag.Card(version) => ujson.Obj("card" -> DocumentVersion.value(version).toDouble)
    case Lineage.Tag.Topic(version) =>
      ujson.Obj("topic" -> DocumentVersion.value(version).toDouble)
  }

  private def readLineage(v: Option[ujson.Value]): Read[Lineage] =
    array(v, "a lineage").flatMap(traverse(_)(readTag)).map(Lineage.of)

  private def readTag(t: ujson.Value): Read[Lineage.Tag] =
    obj(t, "a lineage's tag").flatMap(o =>
      (o.get("strand"), o.get("card"), o.get("topic")) match {
        case (Some(root), _, _) =>
          for {
            r <- string(Some(root), "a strand tag's root")
            room <- string(o.get("room"), "a strand tag's room").flatMap(Place.read)
            digest <- string(o.get("digest"), "a strand tag's digest")
              .flatMap(StrandDigest.read)
          } yield Lineage.Tag.Strand(ConversationId(r), room, digest)
        case (_, Some(n), _) => version(n).map(Lineage.Tag.Card(_))
        case (_, _, Some(n)) => version(n).map(Lineage.Tag.Topic(_))
        case _ => Left(s"a lineage's tag of no kind: ${ujson.write(t)}")
      }
    )

  private def version(v: ujson.Value): Read[DocumentVersion] =
    v.numOpt
      .filter(_.isWhole)
      .flatMap(n => DocumentVersion.of(n.toLong))
      .toRight(s"no document's version: ${ujson.write(v)}")

  private def doc(d: Doc[TopicNote]): ujson.Value = blocks(d.blocks)

  private def readDoc(v: ujson.Value): Read[Doc[TopicNote]] = readBlocks(v).map(Doc(_))

  private def blocks(bs: Vector[Block[TopicNote]]): ujson.Value = ujson.Arr.from(bs.map(block))

  private def readBlocks(v: ujson.Value): Read[Vector[Block[TopicNote]]] =
    array(Some(v), "a doc's blocks").flatMap(traverse(_)(readBlock))

  private def items(is: Vector[Item[TopicNote]]): ujson.Value =
    ujson.Arr.from(is.map(i => blocks(i.blocks)))

  private def readItems(v: Option[ujson.Value]): Read[Vector[Item[TopicNote]]] =
    array(v, "a list's items").flatMap(traverse(_)(i => readBlocks(i).map(Item(_))))

  private def block(b: Block[TopicNote]): ujson.Value = b match {
    case Block.Paragraph(t) => ujson.Obj("paragraph" -> text(t))
    case Block.Heading(l, t) => ujson.Obj("heading" -> l.value, "text" -> text(t))
    case Block.Bullets(is) => ujson.Obj("bullets" -> items(is))
    case Block.Numbered(start, is) => ujson.Obj("numbered" -> items(is), "start" -> start)
    case Block.Quote(bs) => ujson.Obj("quote" -> blocks(bs))
    case Block.Code(language, lines) =>
      ujson.Obj(
        "code" -> ujson.Arr.from(lines.map(ujson.Str(_))),
        "language" -> language.fold(ujson.Null: ujson.Value)(ujson.Str(_))
      )
    case Block.Table(header, rows) =>
      ujson.Obj(
        "table" -> ujson.Arr.from(header.map(text)),
        "rows" -> ujson.Arr.from(rows.map(r => ujson.Arr.from(r.map(text))))
      )
    case Block.Rule => ujson.Obj("rule" -> true)
  }

  private def readBlock(v: ujson.Value): Read[Block[TopicNote]] =
    obj(v, "a block").flatMap { o =>
      def has(k: String) = o.contains(k)
      if (has("paragraph")) readText(o.get("paragraph")).map(Block.Paragraph(_))
      else if (has("heading"))
        for {
          l <- o.get("heading").flatMap(_.numOpt).toRight("a heading with no level")
          t <- readText(o.get("text"))
        } yield Block.Heading(Level.of(l.toInt), t)
      else if (has("bullets")) readItems(o.get("bullets")).map(Block.Bullets(_))
      else if (has("numbered"))
        for {
          start <- o.get("start").flatMap(_.numOpt).toRight("a numbered list with no start")
          is <- readItems(o.get("numbered"))
        } yield Block.Numbered(start.toInt, is)
      else if (has("quote"))
        o.get("quote").toRight("a quote with no blocks").flatMap(readBlocks).map(Block.Quote(_))
      else if (has("code"))
        for {
          lines <- array(o.get("code"), "a code block's lines")
            .flatMap(traverse(_)(l => string(Some(l), "a code block's line")))
        } yield Block.Code(o.get("language").flatMap(_.strOpt), lines)
      else if (has("table"))
        for {
          header <- array(o.get("table"), "a table's header")
            .flatMap(traverse(_)(c => readText(Some(c))))
          rows <- array(o.get("rows"), "a table's rows").flatMap(
            traverse(_)(r =>
              array(Some(r), "a table's row").flatMap(traverse(_)(c => readText(Some(c))))
            )
          )
        } yield Block.Table(header, rows)
      else if (has("rule")) Right(Block.Rule)
      else Left(s"a block of no kind: ${ujson.write(v)}")
    }

  private def text(t: Text[TopicNote]): ujson.Value = ujson.Arr.from(t.spans.map(span))

  private def readText(v: Option[ujson.Value]): Read[Text[TopicNote]] =
    array(v, "a text's spans").flatMap(traverse(_)(readSpan)).map(Text(_))

  private def span(s: Span[TopicNote]): ujson.Value = {
    val o = ujson.Obj("text" -> s.text)
    if (s.marks.nonEmpty)
      o("marks") = ujson.Arr.from(s.marks.toVector.map(mark).sortBy(ujson.write(_)))
    if (s.notes.nonEmpty) o("notes") = ujson.Arr.from(s.notes.map(note))
    o
  }

  private def readSpan(v: ujson.Value): Read[Span[TopicNote]] =
    for {
      o <- obj(v, "a span")
      t <- string(o.get("text"), "a span's text")
      marks <- o
        .get("marks")
        .fold(Right(Vector.empty))(m =>
          array(Some(m), "a span's marks").flatMap(traverse(_)(readMark))
        )
      notes <- o
        .get("notes")
        .fold(Right(Vector.empty))(n =>
          array(Some(n), "a span's notes").flatMap(traverse(_)(readNote))
        )
    } yield Span(t, marks.toSet, notes)

  private def mark(m: Mark): ujson.Value = m match {
    case Mark.Strong => "strong"
    case Mark.Emphasis => "emphasis"
    case Mark.Code => "code"
    case Mark.Link(href) => ujson.Obj("link" -> href)
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

  private def note(n: TopicNote): ujson.Value = n match {
    case TopicNote.Link(to) => ujson.Obj("link" -> TopicKey.value(to))
    case TopicNote.Claimed(c) => ujson.Obj("claim" -> claim(c))
  }

  private def readNote(v: ujson.Value): Read[TopicNote] =
    obj(v, "a note").flatMap(o =>
      (o.get("link"), o.get("claim")) match {
        case (Some(k), _) => topicKey(Some(k), "a link's topic").map(TopicNote.Link(_))
        case (_, Some(c)) => readClaim(c).map(TopicNote.Claimed(_))
        case _ => Left(s"a note of no kind: ${ujson.write(v)}")
      }
    )

  private def claim(c: Claim): ujson.Value = {
    val o = ujson.Obj(
      "id" -> ClaimId.value(c.id),
      "certainty" -> c.certainty.toString.toLowerCase,
      "relations" -> ujson.Arr.from(
        c.relations.map(r =>
          ujson.Obj(
            "kind" -> r.kind.toString.toLowerCase,
            "topic" -> TopicKey.value(r.to.topic),
            "claim" -> ClaimId.value(r.to.claim)
          )
        )
      ),
      "from" -> lineage(c.from)
    )
    c.key.foreach(k => o("key") = ClaimKey.value(k))
    o
  }

  private def readClaim(v: ujson.Value): Read[Claim] =
    for {
      o <- obj(v, "a claim")
      id <- string(o.get("id"), "a claim's id").flatMap(ClaimId.of)
      key <- o
        .get("key")
        .fold(Right(None))(k => string(Some(k), "a claim's key").flatMap(ClaimKey.of).map(Some(_)))
      certainty <- string(o.get("certainty"), "a claim's certainty").flatMap(c =>
        Certainty.values.find(_.toString.toLowerCase == c).toRight(s"no certainty: $c")
      )
      relations <- array(o.get("relations"), "a claim's relations").flatMap(
        traverse(_)(r =>
          for {
            ro <- obj(r, "a relation")
            kind <- string(ro.get("kind"), "a relation's kind").flatMap(k =>
              Relation.Kind.values.find(_.toString.toLowerCase == k).toRight(s"no relation: $k")
            )
            topic <- topicKey(ro.get("topic"), "a relation's topic")
            to <- string(ro.get("claim"), "a relation's claim").flatMap(ClaimId.of)
          } yield Relation(kind, ClaimRef(topic, to))
        )
      )
      from <- readLineage(o.get("from"))
    } yield Claim(id, key, certainty, relations, from)

  private def topicKey(v: Option[ujson.Value], what: String): Read[TopicKey] =
    string(v, what).flatMap(k =>
      DocKey.of(k).toOption.flatMap(TopicKey.read).toRight(s"$what is no topic's key: $k")
    )

  private def obj(v: ujson.Value, what: String): Read[collection.Map[String, ujson.Value]] =
    v.objOpt.toRight(s"$what is no object: ${ujson.write(v)}")

  private def array(v: Option[ujson.Value], what: String): Read[Vector[ujson.Value]] =
    v.flatMap(_.arrOpt).map(_.toVector).toRight(s"$what: no array")

  private def string(v: Option[ujson.Value], what: String): Read[String] =
    v.flatMap(_.strOpt).toRight(s"$what: no string")
}
