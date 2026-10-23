package grit.core.files

import grit.core.tool.Field

/** The arguments every tool that reads a file a range at a time takes for its range, each
  * optional: `pages`, `section` and `from`, as [[FileRange.parse]] reads them.
  */
object RangeArgs {

  /** A PDF's pages, or a deck's slides, to read: one page, or a run. */
  val Pages: Field[Option[String]] =
    Field
      .text("A PDF's pages, or a deck's slides, to read: one page, \"212\", or a run, \"212-214\".")
      .optional

  /** The heading of the section to read. */
  val Section: Field[Option[String]] =
    Field.text("The heading of the section to read, as the outline gives it.").optional

  /** Where to read on from. */
  val From: Field[Option[Int]] =
    Field
      .count("Where to read on from: the offset a read said to ask for.", 0, Int.MaxValue)
      .optional

  /** `near`: the name every reading tool gives [[Near]], which a piece's pointer sets. */
  val NearArgument: String = "near"

  /** Where a passage a piece's pointer, or a quote of it, names lies now. */
  val Near: Field[Option[String]] =
    Field
      .text(
        "The passage to read around: a piece's pointer, copied whole, or at least " +
          s"${Anchor.QuoteLeast} words quoted from the passage."
      )
      .optional
}
