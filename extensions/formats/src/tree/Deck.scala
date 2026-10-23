package grit.formats.tree

import grit.core.files.Locus
import grit.prose.form.{Block, Doc, Level, Mark, Span, Text}

/** A deck's slides as one document, whichever kind of file they were read from. */
private[formats] object Deck {

  /** One slide as read: `number`, its place among all the deck's slides from 1; its title and
    * subtitle, when it has them; its body; its speaker notes; whether the slideshow skips it;
    * and whether its author laid it out as a section's header (`divider`).
    */
  final case class Slide(
      number: Int,
      title: Option[Text[Nothing]],
      subtitle: Option[Text[Nothing]],
      body: Vector[Block[Nothing]],
      notes: Vector[Block[Nothing]],
      skipped: Boolean,
      divider: Boolean
  )

  /** `slides` as sections, each headed by its slide's title, else "Slide n", every span on page
    * n: a skipped slide's first paragraph says so, its subtitle follows, then its body, then its
    * notes, when it has any, as a quote led by **Speaker notes**. A divider's heading is level
    * 1 and the slides after it, up to the next, level 2; slides before the first divider, as in
    * a deck with none, level 1.
    */
  def doc(slides: Vector[Slide]): Doc[Locus] = {
    val under = slides.scanLeft(false)((after, s) => after || s.divider).drop(1)
    Doc(slides.zip(under).flatMap { (s, inSection) =>
      val level = Level.of(if (inSection && !s.divider) 2 else 1)
      val heading = Block.Heading(level, s.title.getOrElse(Text.plain(s"Slide ${s.number}")))
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
  }

  private def marked(s: String, mark: Mark): Text[Nothing] = Text(Vector(Span(s, Set(mark))))
}
