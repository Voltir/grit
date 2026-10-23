package grit.formats.tree

import grit.core.files.Locus
import grit.prose.form.{Block, Doc, Level, Mark, Span, Text}

/** A deck's slides as one document, whichever kind of file they were read from. */
private[formats] object Deck {

  /** One slide as read: `number`, its place among all the deck's slides from 1; its title and
    * subtitle, when it has them; its body; its speaker notes; and whether the slideshow skips it.
    */
  final case class Slide(
      number: Int,
      title: Option[Text[Nothing]],
      subtitle: Option[Text[Nothing]],
      body: Vector[Block[Nothing]],
      notes: Vector[Block[Nothing]],
      skipped: Boolean
  )

  /** `slides` as sections, each headed by its slide's title, else "Slide n", every span on page
    * n: a skipped slide's first paragraph says so, its subtitle follows, then its body, then its
    * notes, when it has any, as a quote led by **Speaker notes**.
    */
  def doc(slides: Vector[Slide]): Doc[Locus] =
    Doc(slides.flatMap { s =>
      val heading = Block.Heading(Level.of(1), s.title.getOrElse(Text.plain(s"Slide ${s.number}")))
      val skipped =
        Option.when(s.skipped)(Block.Paragraph(marked("Skipped in the slideshow.", Mark.Emphasis)))
      val notes = Option.when(s.notes.nonEmpty)(
        Block.Quote(Block.Paragraph(marked("Speaker notes", Mark.Strong)) +: s.notes)
      )
      val blocks =
        Vector(heading) ++ skipped ++ s.subtitle.map(Block.Paragraph(_)) ++ s.body ++ notes
      val page = Vector(Locus.Page(s.number))
      Doc(blocks).mapSpans(sp => Span(sp.text, sp.marks, page)).blocks
    })

  private def marked(s: String, mark: Mark): Text[Nothing] = Text(Vector(Span(s, Set(mark))))
}
