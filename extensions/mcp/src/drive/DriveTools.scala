package grit.mcp.drive

import java.io.IOException
import java.nio.file.{Files, Path}

import grit.core.clock.Clock
import grit.core.edge.{Route, ToolRequest}
import grit.core.files.{FileRange, FileRead, Formats, OpenFile}
import grit.core.tool.{
  Args,
  ArgsError,
  Believed,
  Field,
  Gate,
  Hosted,
  Outcome,
  Retry,
  Tool,
  ToolName,
  ToolSet,
  ToolSpec,
  Toolbox
}
import grit.core.visibility.Integrity
import grit.edge.{Run, Tools}

/** The requests a Drive edge is sent, run over `rest` wherever they were routed, as
  * [[Run.request]] runs a tool: [[DriveTools.List]], [[DriveTools.Search]] and
  * [[DriveTools.Read]], each answered as [[DriveItems.answer]] answers, or [[Outcome.Failed]]
  * with a [[DriveError]]'s words or why it was not placed. A read of a PDF, a Word, an Excel
  * or any other file Drive keeps the bytes of is by `formats`, of a copy written to `scratch`
  * and removed after (a copy left by a failed removal stays until the edge closes); Folders,
  * Slides and images are refused. The run's `said` hears each failure's cause ([[DriveError.logged]]), its tool named, and that a
  * search's drives could not be listed (its hits then name them by id) or a read's grants
  * could not be read (it is then believed untrusted).
  */
// `clock` is a parameter, rather than a capture-set parameter over `rest`, so the class's
// captures are in its signature (docs/capture-checking.md, "A capset parameter a class's body
// alone uses").
final class DriveTools(
    clock: Clock,
    rest: DriveRest^{clock},
    reading: DriveReading,
    formats: Formats,
    scratch: Path
) extends Tools {

  /** `file`'s `range`, read by `formats` from a copy of its bytes kept in `scratch` for this
    * read alone.
    */
  private def parsed(file: DriveFile, range: FileRange): Either[DriveError, FileRead] = {
    val kept =
      try Right(Files.createTempFile(scratch, "read-", ".part"))
      catch { case e: IOException => Left(DriveError.Unkept(e.getClass.getSimpleName)) }
    kept.flatMap { temp =>
      try
        rest.bytes(file, formats.limits.bytes, temp).flatMap { _ =>
          OpenFile
            .reading(temp, file.name, Some(file.mimeType), clock)(_.read(formats, range))
            .left
            .map(DriveError.Unkept(_))
            .flatMap(_.left.map(DriveError.Unread(_)))
        }
      finally {
        // A copy left behind is cleared with the scratch directory when the edge next opens.
        try Files.deleteIfExists(temp)
        catch { case _: IOException => () }
        ()
      }
    }
  }

  def run(route: Route, request: ToolRequest, said: String => Unit): Outcome = {
    // `e` told the operator after `what`, when it is not a person's own miss.
    def heard[A](what: String, e: Either[DriveError, A]): Either[DriveError, A] = {
      e.left.foreach(_.logged.foreach(why => said(s"$what: $why")))
      e
    }
    val search: Tool.Offered^{clock, said} = DriveTools.Search.over { s =>
      val drives = rest.drives() match {
        case Right(shared) => shared.names
        case Left(e) =>
          said(
            "drive_search: Drive's shared drives could not be listed, so hits name them by id: " +
              e.logged.getOrElse(e.message)
          )
          Map.empty
      }
      DriveTools.answer(
        reading,
        heard("drive_search", rest.search(s.text, s.folder)).map(DriveCalled.Searched(_, drives))
      )
    }
    val list: Tool.Offered^{clock, said} = DriveTools.List.over { folder =>
      DriveTools.answer(
        reading,
        folder match {
          case None => heard("drive_list", rest.drives()).map(DriveCalled.Drives(_))
          case Some(f) => heard("drive_list", rest.children(f)).map(DriveCalled.Listed(_))
        }
      )
    }
    val read: Tool.Offered^{clock, said} = DriveTools.Read.over { asked =>
      // Read after the file, as a person's grants are what its part is shown under.
      def grants: Option[Vector[Grant]] =
        heard(
          "drive_read: its grants could not be read, so it is believed untrusted",
          rest.grants(asked.file)
        ).toOption.flatten
      DriveTools.answer(
        reading,
        heard(
          "drive_read",
          rest.file(asked.file).flatMap { file =>
            file.kindOf match {
              case DriveKind.Document | DriveKind.Spreadsheet | DriveKind.Text =>
                rest.text(file).map(DriveCalled.Read(file, grants, _))
              case DriveKind.Folder | DriveKind.Slides | DriveKind.Image =>
                Left(DriveError.Unreadable(file.kindOf.words))
              case DriveKind.Pdf | DriveKind.Word | DriveKind.Excel | DriveKind.Other(_) =>
                parsed(file, asked.range).map(DriveCalled.Parsed(file, grants, _))
            }
          }
        )
      )
    }
    Toolbox.of[caps.CapSet^{clock, said}](list, search, read) match {
      case Right(box) => Run.request(request, box)
      // The three are named apart.
      case Left(repeated) =>
        Outcome.Failed(s"two Drive tools are named ${ToolName.value(repeated.name)}.")
    }
  }
}

object DriveTools {

  /** `called` as the model reads it, as [[DriveTools]] says. */
  private def answer(
      reading: DriveReading,
      called: Either[DriveError, DriveCalled]
  ): Outcome =
    called match {
      case Left(error) => Outcome.Failed(error.message)
      case Right(c) => DriveItems.answer(reading, c).fold(Outcome.Failed(_), identity)
    }

  /** `why`, a clause [[FileId.of]] gives, as the sentence a refused argument says. */
  private def sentence(why: String): String = s"${why.capitalize}."

  /** What Drive's tools' results are believed at where no service is named: a call at Drive's
    * service is believed at that service's output instead, as its items say.
    */
  private val AtNoService: Believed = Believed.At(Integrity.Untrusted)

  /** A search's arguments: its words, and the folder it is held to, if any. */
  final case class Searching(text: String, folder: Option[FileId])

  /** `drive_search`: the files shared whose text holds `text`, within `folder` when given. */
  val Search: Hosted[Searching] = new Hosted(
    ToolSpec(
      ToolName("drive_search"),
      "Search the Drive files shared here for those whose text holds these words. Each hit " +
        "gives the file's name, type and id, the folder it is in, and its shared drive. Files in " +
        "folders no one " +
        "has labelled yet are not shown.",
      Args
        .of(
          (
            text = Field.text("The words to look for."),
            folder =
              Field.text("A folder's id, from a search, to search only the files in it.").optional
          )
        )
        .refine { a =>
          if (a.text.trim.isEmpty)
            Left(ArgsError.Refused("text", "Give at least one word to search for."))
          else
            a.folder
              .map(FileId.of(_).map(Some(_)))
              .getOrElse(Right(None))
              .left
              .map(why => ArgsError.Refused("folder", DriveTools.sentence(why)))
              .map(Searching(a.text.trim, _))
        },
      Retry.Rerun
    ),
    Gate.Free,
    s => s.text,
    AtNoService
  )

  /** `drive_list`: the shared drives grit is a member of, or the files and folders directly in
    * one folder or shared drive, by its id.
    */
  val List: Hosted[Option[FileId]] = new Hosted(
    ToolSpec(
      ToolName("drive_list"),
      "List what is shared here from Drive: with no folder, the shared drives; with the id of " +
        "a folder or a shared drive, the files and folders directly in it. Each gives its name, " +
        "type and id, and when it last changed. Files in folders no one has labelled yet are " +
        "not shown.",
      Args
        .of(
          (folder =
            Field
              .text(
                "The id of a folder or a shared drive, from an earlier list or a search, to list " +
                  "what is directly in it."
              )
              .optional
          )
        )
        .refine(a =>
          a.folder
            .map(FileId.of(_).map(Some(_)))
            .getOrElse(Right(None))
            .left
            .map(why => ArgsError.Refused("folder", DriveTools.sentence(why)))
        ),
      Retry.Rerun
    ),
    Gate.Free,
    folder => folder.map(FileId.value).getOrElse("the shared drives"),
    AtNoService
  )

  /** A read's arguments: the file, and what of it to read. */
  final case class Reading(file: FileId, range: FileRange)

  /** `drive_read`: one file's text, by its id: a PDF, a Word file or an Excel file by `range`. */
  val Read: Hosted[Reading] = new Hosted(
    ToolSpec(
      ToolName("drive_read"),
      "Read a Drive file's text by its id, from a search: a Doc as markdown, a Sheet's first " +
        "sheet as CSV, a text file as it is, and a PDF, a Word file or an Excel file a part at a " +
        "time. For those, with no other argument it gives the file's outline (its headings, with " +
        "their pages) and its start; ask for pages, a section by its heading, or from an offset " +
        "to read on. Each read says what it covers.",
      Args
        .of(
          (
            file = Field.text("The file's id, from a search."),
            pages = Field
              .text("A PDF's pages to read: one page, \"212\", or a run, \"212-214\".")
              .optional,
            section =
              Field.text("The heading of the section to read, as the outline gives it.").optional,
            from = Field
              .count("Where to read on from: the offset a read said to ask for.", 0, Int.MaxValue)
              .optional
          )
        )
        .refine(a =>
          for {
            id <- FileId
              .of(a.file)
              .left
              .map(why => ArgsError.Refused("file", DriveTools.sentence(why)))
            range <- FileRange
              .parse(a.pages, a.section, a.from)
              .left
              // Named by the one argument given, or by pages when several were.
              .map(
                ArgsError.Refused(
                  if (a.section.nonEmpty && a.pages.isEmpty) "section"
                  else if (a.from.nonEmpty && a.pages.isEmpty) "from"
                  else "pages",
                  _
                )
              )
          } yield Reading(id, range)
        ),
      Retry.Rerun
    ),
    Gate.Free,
    r => (FileId.value(r.file) +: Option(r.range.asked).filter(_.nonEmpty).toVector).mkString(" "),
    AtNoService
  )

  /** The three, as an edge advertises them. */
  val Offered: ToolSet =
    ToolSet.of(Vector(List.entry, Search.entry, Read.entry)).getOrElse(ToolSet.Empty)
}
