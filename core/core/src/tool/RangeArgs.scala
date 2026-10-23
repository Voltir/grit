package grit.core.tool

import grit.core.files.FileRange

/** The arguments every tool that reads a file a range at a time takes for its range, each
  * optional: `pages`, `section` and `from`, as [[FileRange.parse]] reads them.
  */
object RangeArgs {

  /** A PDF's pages to read: one page, or a run. */
  val Pages: Field[Option[String]] =
    Field.text("A PDF's pages to read: one page, \"212\", or a run, \"212-214\".").optional

  /** The heading of the section to read. */
  val Section: Field[Option[String]] =
    Field.text("The heading of the section to read, as the outline gives it.").optional

  /** Where to read on from. */
  val From: Field[Option[Int]] =
    Field
      .count("Where to read on from: the offset a read said to ask for.", 0, Int.MaxValue)
      .optional

  /** Where a passage a piece's pointer names lies now. */
  val Near: Field[Option[String]] =
    Field
      .text("The passage to read around, as a piece's pointer gives it, copied whole.")
      .optional

  /** The range `pages`, `section`, `from` and `near` ask for ([[FileRange.Start]] when none is
    * given); refused as [[FileRange.parse]] refuses, naming the one argument given, or `pages`
    * when several were.
    */
  def range(
      pages: Option[String],
      section: Option[String],
      from: Option[Int],
      near: Option[String]
  ): Either[ArgsError, FileRange] =
    FileRange
      .parse(pages, section, from, near)
      .left
      .map(
        ArgsError.Refused(
          if (pages.nonEmpty) "pages"
          else if (section.nonEmpty) "section"
          else if (from.nonEmpty) "from"
          else "near",
          _
        )
      )
}
