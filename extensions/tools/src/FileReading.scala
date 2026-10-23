package grit.tools

import grit.core.files.{FileError, FileRange, Files}
import grit.core.id.FileHandle
import grit.core.store.Tx
import grit.core.tool.{
  Args,
  ArgsError,
  Believed,
  Field,
  Gate,
  Hosted,
  Outcome,
  RangeArgs,
  Retry,
  Tool,
  ToolName,
  ToolSpec
}
import grit.core.visibility.{Facts, Integrity, ResultPart}

/** `file_read`: more of a file the file area keeps, by the handle a read that kept it named
  * (`f41`), a range at a time, as the area's formats read it ([[Files.formats]]). Each call admits the file in one
  * transaction of the reads its turn hands it ([[grit.core.tool.Hosted.reading]]), as an item
  * of its source's result is read, then opens and reads it holding none. Its answer is one part
  * kept where the file came from and read by whoever read it there, its facts `{"file",
  * "covered", "complete"}`. A file expired, never kept, or not this turn's to read is answered
  * alike ([[FileError.Gone]]'s words); one no format reads, with the read's words.
  */
object FileReading {

  /** Its tool's name, `file_read`. */
  val Name: ToolName = ToolName("file_read")

  /** A read's arguments: the file, and what of it to read. */
  final case class Reading(file: FileHandle, range: FileRange)

  /** `file_read` over `files`. */
  def tool(files: Files^): Tool[Reading]^{files} =
    described.reading((asked, _, reads) =>
      reads
        .read((tx: Tx^) ?=> Right(files.admit(asked.file)))
        .left
        .map(FileError.Store(_))
        .flatten
        .flatMap(admitted =>
          files
            .open(admitted)(_.read(files.formats, asked.range))
            .map(_.map(read => (admitted.record.item, read)))
        ) match {
        case Left(error) => Outcome.Failed(error.message)
        case Right(Left(unread)) => Outcome.Failed(unread.message)
        case Right(Right((item, read))) =>
          Outcome.Done(
            "",
            parts = Some(
              Vector(
                ResultPart(
                  item.item,
                  item.readers,
                  s"${asked.file.written}: ${read.shown}",
                  Some(
                    Facts.of(
                      ujson.Obj(
                        "file" -> asked.file.written,
                        "covered" -> read.covered,
                        "complete" -> read.whole
                      )
                    )
                  )
                )
              )
            )
          )
      }
    )

  /** What the model is told of it. */
  private val described: Hosted[Reading] =
    Hosted(
      ToolSpec(
        Name,
        "Read any part of a file kept here, by its handle (such as f41), which the read that " +
          "kept it named. The whole file is kept, not only the part read then: with no other " +
          "argument, its outline and its start; ask for pages, a section by its heading, or " +
          "from an offset to read on, or near a passage as a piece's pointer gives it. Each " +
          "read says what it covers. Files are kept a while, " +
          "then must be read again from where they came from.",
        Args
          .of(
            (
              file = Field.text("The file's handle, such as f41, as the read that kept it said."),
              pages = RangeArgs.Pages,
              section = RangeArgs.Section,
              from = RangeArgs.From,
              near = RangeArgs.Near
            )
          )
          .refine(a =>
            for {
              file <- FileHandle
                .read(a.file.trim)
                .toRight(
                  ArgsError.Refused(
                    "file",
                    s"${a.file} is no file's handle; one is f and a number, such as f41."
                  )
                )
              range <- RangeArgs.range(a.pages, a.section, a.from, a.near)
            } yield Reading(file, range)
          ),
        retry = Retry.Rerun
      ),
      Gate.Free,
      r => (r.file.written +: Option(r.range.asked).filter(_.nonEmpty).toVector).mkString(" "),
      // A file's text is its source's, never grit's: believed as its item's service says.
      Believed.At(Integrity.Untrusted)
    )
}
