package grit.projects.artifact

import java.nio.charset.StandardCharsets

import grit.core.document.Document
import grit.core.message.Cut
import grit.projects.name.ArtifactKey
import grit.prose.form.{Block, Doc, Level, Mark, Span, Text}
import grit.prose.index.IndexText
import grit.prose.json.ProseJson
import grit.prose.markdown.Markdown

/** What an artifact says: prose, its references to other artifacts as notes on their spans, so
  * a rename or a move of what it cites breaks no sentence; non-empty, its stored form within
  * [[Body.StoredMost]].
  */
opaque type Body = Doc[ArtifactKey]

object Body {

  /** `artifact:`: what a markdown link's target starts with when it refers to an artifact. */
  val Scheme: String = "artifact:"

  /** 28,672: the most bytes its stored form holds as compact JSON, 4 KiB short of a document's
    * data ([[Document.DataMost]]), which its head shares.
    */
  val StoredMost: Int = Document.DataMost - 4096

  /** `markdown` as a body, its wrapped lines joined ([[Markdown.Newline.Space]]), a link to
    * `artifact:<key>` its reference to that artifact, or why not: blank or holding no block, a
    * reference that is no artifact's key, or a stored form over [[StoredMost]].
    */
  def read(markdown: String): Either[String, Body] =
    if (markdown.isBlank) Left("a body is blank")
    else {
      val parsed = Markdown.parse(markdown, Markdown.Newline.Space)
      val bad = parsed.mapSpans(s => Span(s.text, s.marks, referred(s))).notes.collect {
        case Referred(href) if ArtifactKey.read(href).isLeft => href
      }
      if (bad.nonEmpty) Left(s"a reference is no artifact's key: ${bad.mkString(", ")}")
      else {
        checked(parsed.mapSpans { s =>
          val keys = referred(s).flatMap(r => ArtifactKey.read(r.href).toOption)
          Span(s.text, s.marks.filter(m => !isReference(m)), keys)
        })
      }
    }

  /** `doc` as a body, or why not: it holds no block, or its stored form is over [[StoredMost]]. */
  private def checked(doc: Doc[ArtifactKey]): Either[String, Body] =
    if (doc.blocks.isEmpty) Left("a body is empty")
    else {
      val size = ujson.write(stored(doc)).getBytes(StandardCharsets.UTF_8).length
      if (size > StoredMost) Left(s"a body's stored form is $size bytes, over $StoredMost")
      else Right(doc)
    }

  /** As markdown, each reference a link to `artifact:<key>`, which [[read]] reads back as `b`. */
  def markdown(b: Body): String =
    Markdown.writeExact(b.mapSpans { s =>
      Span(s.text, s.marks ++ s.notes.map(k => Mark.Link(Scheme + k.value)), Vector.empty)
    })

  /** What search ranks it by. */
  def searchText(b: Body): String = IndexText.render(b.unnoted)

  /** `b` with its references to `keys` dropped, the text they were on kept. */
  def uncited(b: Body, keys: Set[ArtifactKey]): Body =
    b.mapSpans(s => Span(s.text, s.marks, s.notes.filterNot(keys)))

  /** The artifacts its text refers to, in reading order, each once. */
  def cites(b: Body): Vector[ArtifactKey] = b.notes.distinct

  /** 120: the most characters of a body's [[lead]]. */
  val LeadChars: Int = 120

  /** Its first paragraph as plain text, its references' text kept, cut at a word to `chars`
    * characters with `…` ([[grit.core.message.Cut.head]]); empty when it has no paragraph.
    */
  def lead(b: Body, chars: Int = LeadChars): String =
    b.blocks
      .collectFirst { case Block.Paragraph(text) => text.plain.trim }
      .fold("")(Cut.head(_, chars))

  /** The titles of its `##` headings, in order. */
  def sections(b: Body): Vector[String] = b.blocks.collect {
    case Block.Heading(level, text) if level.value == 2 => text.plain.trim
  }

  /** `Delta: `: what a `##` heading starts with when its section is a delta to the section named
    * by the rest.
    */
  val Delta: String = "Delta: "

  /** `into` with each of `deltas`' delta sections merged in, in order: a section is a `##`
    * heading and the blocks up to the next heading of rank 2 or less, and a delta, headed
    * `Delta: <name>`, replaces `into`'s section `<name>` or is added after its last; or why not:
    * a merged stored form over [[StoredMost]]. `into` itself when `deltas` holds none.
    */
  def merged(into: Body, deltas: Body): Either[String, Body] = {
    val changes = sectionsOf(deltas.blocks).collect {
      case Section(Some(title), blocks) if title.startsWith(Delta) =>
        Change(title.drop(Delta.length).trim, blocks.drop(1))
    }
    val doc = changes.foldLeft(into.blocks) { (blocks, change) =>
      val replacing = Block.Heading(Level.of(2), Text.plain(change.name)) +: change.blocks
      val sections = sectionsOf(blocks)
      if (sections.exists(_.title.contains(change.name)))
        sections.flatMap(s => if (s.title.contains(change.name)) replacing else s.blocks)
      else blocks ++ replacing
    }
    if (changes.isEmpty) Right(into) else checked(Doc(doc))
  }

  /** Blocks from a heading of rank 2 or less up to the next, or those before the first;
    * `title`, the heading's when it is of rank 2.
    */
  private final case class Section(title: Option[String], blocks: Vector[Block[ArtifactKey]])

  /** A delta: the section it replaces or adds, and the blocks under it. */
  private final case class Change(name: String, blocks: Vector[Block[ArtifactKey]])

  /** `blocks` cut before each heading of rank 2 or less. */
  private def sectionsOf(blocks: Vector[Block[ArtifactKey]]): Vector[Section] =
    blocks.foldLeft(Vector.empty[Section]) { (cut, block) =>
      block match {
        case Block.Heading(level, text) if level.value <= 2 =>
          cut :+ Section(Option.when(level.value == 2)(text.plain.trim), Vector(block))
        case _ =>
          cut.lastOption match {
            case Some(last) => cut.init :+ last.copy(blocks = last.blocks :+ block)
            case None => Vector(Section(None, Vector(block)))
          }
      }
    }

  /** As stored: prose's stored form ([[ProseJson]]), each reference by its key. */
  def stored(b: Body): ujson.Value = ProseJson.write(b, k => ujson.Str(k.value))

  /** What [[stored]] wrote, as the body, or why not: a stored form [[ProseJson.read]] refuses
    * (another version among them), a reference that is no artifact's key, no block, or a
    * stored form over [[StoredMost]].
    */
  def fromStored(json: ujson.Value): Either[String, Body] =
    ProseJson
      .read(
        json,
        {
          case ujson.Str(s) => ArtifactKey.read(s)
          case other => Left(s"a reference is not a key: $other")
        }
      )
      .flatMap(checked)

  /** A link's target where the link refers to an artifact. */
  private final case class Referred(href: String) extends caps.Pure

  private def isReference(m: Mark): Boolean = m match {
    case Mark.Link(href) => href.startsWith(Scheme)
    case _ => false
  }

  private def referred(s: Span[?]): Vector[Referred] = s.marks.toVector.collect {
    case Mark.Link(href) if href.startsWith(Scheme) => Referred(href.drop(Scheme.length))
  }
}
