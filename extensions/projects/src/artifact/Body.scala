package grit.projects.artifact

import java.nio.charset.StandardCharsets

import grit.core.document.Document
import grit.projects.name.ArtifactKey
import grit.prose.form.{Block, Doc, Mark, Span}
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

  /** `markdown` as a body, a link to `artifact:<key>` its reference to that artifact, or why
    * not: blank or holding no block, a reference that is no artifact's key, or a stored form
    * over [[StoredMost]].
    */
  def read(markdown: String): Either[String, Body] =
    if (markdown.isBlank) Left("a body is blank")
    else {
      val parsed = Markdown.parse(markdown)
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

  /** As markdown, each reference a link to `artifact:<key>`, and outside code each character
    * markdown could read as markup escaped, so [[read]] of it keeps every reference.
    */
  def markdown(b: Body): String =
    Markdown.write(b.mapSpans { s =>
      val text =
        if (s.marks(Mark.Code)) s.text
        else s.text.flatMap(c => if (Markup(c)) s"\\$c" else c.toString)
      Span(text, s.marks ++ s.notes.map(k => Mark.Link(Scheme + k.value)), Vector.empty)
    })

  /** What `Markdown.write` writes bare though [[Markdown.parse]] may read it as markup. */
  private val Markup: Set[Char] = Set('\\', '`', '[', ']', '<', '!', '*', '_')

  /** What search ranks it by. */
  def searchText(b: Body): String = IndexText.render(b.unnoted)

  /** The artifacts its text refers to, in reading order, each once. */
  def cites(b: Body): Vector[ArtifactKey] = b.notes.distinct

  /** The titles of its `##` headings, in order. */
  def sections(b: Body): Vector[String] = b.blocks.collect {
    case Block.Heading(level, text) if level.value == 2 => text.plain.trim
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
