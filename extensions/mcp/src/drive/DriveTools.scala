package grit.mcp.drive

import java.io.InputStream

import grit.core.clock.Clock
import grit.core.edge.{Route, ToolRequest}
import grit.core.files.{FileError, FileOrigin, FileRange, Incoming, Intake, PutKey, RangeArgs}
import grit.core.id.{CallSlot, EdgeName}
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
  * or any other file Drive keeps the bytes of is kept in `files` under its call
  * ([[grit.core.files.PutKey.of]]), so the call run again answers the file it kept, and read
  * there by the formats the area reads with ([[Intake.formats]]); one past the most they read is
  * refused before its bytes are asked for, and one that lies in no folder or that no one here may read is never fetched. A Doc,
  * a Sheet or a Slides deck is kept and read so in the first form Drive gives it in
  * ([[DriveKind.exportsAs]]) whose kind those formats read: a deck as the Slides API answers
  * it, else, or when the Slides API is not enabled ([[DriveError.SlidesOff]], `said` hears it),
  * as its PDF; while the API is off, every read of a deck makes one failing Slides request, so
  * heard, before it answers from the PDF the area keeps. Else a Doc or a Sheet is answered as Drive's text of it, and a Slides deck
  * refused. Folders and images are refused.
  * A file whose read meets its own refusal ([[DriveError.lasting]]: a format would not read it,
  * Drive flagged it or will not export it) is answered as unread, saying why
  * ([[DriveCalled.Unread]]), not as a failure. The run's `said` hears each failure's cause
  * ([[DriveError.logged]]), its tool named, and that a search's drives could not be listed (its
  * hits then name them by id) or a read's grants could not be read (it is then believed
  * untrusted).
  */
// `clock` is a parameter, rather than a capture-set parameter over `rest`, so the class's
// captures are in its signature (docs/capture-checking.md, "A capset parameter a class's body
// alone uses").
final class DriveTools(
    clock: Clock,
    rest: DriveRest^{clock},
    reading: DriveReading,
    files: Intake^
) extends Tools {

  /** `file`'s `range`, read by `files`' formats from the file it keeps under `slot`'s call, put
    * there from Drive when it keeps none: its bytes, or Drive's export of it `as` an office file.
    */
  private def kept(
      file: DriveFile,
      grants: Option[Vector[Grant]],
      range: FileRange,
      slot: CallSlot,
      as: Option[DriveExport]
  ): Either[DriveError, DriveCalled] =
    DriveItems.item(reading, file, grants) match {
      case None => Right(DriveCalled.Withheld(file))
      case Some(item) =>
        // Drive's own failure, kept beside the words the area is told, so it is answered and
        // logged as Drive's. Set only by this call's one put, on this thread.
        var refused: Option[DriveError] = None
        val incoming = new Incoming {
          def declared: Option[Long] = file.size
          def read[A](body: InputStream^ => A): Either[String, A] =
            as.fold(rest.media(file, files.formats.limits.bytes)(body))(a =>
              rest.exported(file, a)(body)
            ).left
              .map { e =>
                refused = Some(e)
                e.message
              }
        }
        files.put(
          PutKey.of(slot),
          file.name,
          Some(as.fold(file.mimeType)(_.mimeType)),
          FileOrigin(
            EdgeName(reading.service.name),
            FileId.value(file.id),
            // An export's revision is Drive's version of the file, which has no other, a deck's
            // own answer told from its PDF; a file Drive keeps the bytes of is named by when it
            // last changed.
            as.fold(file.changed.map(_.toString))(a =>
              file.version.map(v =>
                if (a == DriveExport.Deck) s"version $v, the Slides API's answer" else s"version $v"
              )
            ),
            clock.now()
          ),
          item,
          incoming
        )(_.read(files.formats, range)) match {
          case Left(FileError.Fetch(why)) =>
            Left(refused.getOrElse(DriveError.Unkept(FileError.Fetch(why))))
          case Left(error) => Left(DriveError.Unkept(error))
          case Right((_, Left(unread))) => Left(DriveError.Unread(unread))
          case Right((record, Right(read))) =>
            Right(DriveCalled.Parsed(file, grants, read, record.handle, files.keeps))
        }
    }

  def run(route: Route, request: ToolRequest, said: String => Unit): Outcome = {
    // `e` told the operator after `what`, when it is not a person's own miss.
    def heard[A](what: String, e: Either[DriveError, A]): Either[DriveError, A] = {
      e.left.foreach(_.logged.foreach(why => said(s"$what: $why")))
      e
    }
    val search: Tool.Offered^{clock, files, said} = DriveTools.Search.over { s =>
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
    val list: Tool.Offered^{clock, files, said} = DriveTools.List.over { folder =>
      DriveTools.answer(
        reading,
        folder match {
          case None => heard("drive_list", rest.drives()).map(DriveCalled.Drives(_))
          case Some(f) => heard("drive_list", rest.children(f)).map(DriveCalled.Listed(_))
        }
      )
    }
    val read: Tool.Offered^{clock, files, said} = DriveTools.Read.calling { (asked, slot) =>
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
            // A read refused for the file's own sake is answered as the file unread, so a
            // reader tells it from a failure to reach Drive or to keep the file.
            def readOrUnread(read: Either[DriveError, DriveCalled]) = read match {
              case Left(e) if e.lasting => Right(DriveCalled.Unread(file, grants, e))
              case other => other
            }
            // The forms in turn: the next only when the Slides API is off, the operator told.
            def inTurn(forms: Vector[DriveExport]): Either[DriveError, DriveCalled] =
              forms match {
                case as +: rest =>
                  kept(file, grants, asked.range, slot, Some(as)) match {
                    case Left(DriveError.SlidesOff) if rest.nonEmpty =>
                      DriveError.SlidesOff.logged
                        .foreach(why => said(s"drive_read: $why, so a deck is read as its PDF"))
                      inTurn(rest)
                    case other => other
                  }
                case _ => Left(DriveError.Unreadable(file.kindOf.words))
              }
            // A Doc, a Sheet or a Slides deck is read in the first form Drive gives it in that a
            // format here reads; a Doc or a Sheet, else, as Drive's text of it.
            file.kindOf.exportsAs.filter(as => files.formats.of(as.kind).nonEmpty) match {
              case forms if forms.nonEmpty => readOrUnread(inTurn(forms))
              case _ =>
                file.kindOf match {
                  case DriveKind.Document | DriveKind.Spreadsheet | DriveKind.Text =>
                    readOrUnread(rest.text(file).map(DriveCalled.Read(file, grants, _)))
                  case DriveKind.Folder | DriveKind.Slides | DriveKind.Image =>
                    Left(DriveError.Unreadable(file.kindOf.words))
                  case DriveKind.Pdf | DriveKind.Word | DriveKind.Excel | DriveKind.Other(_) =>
                    readOrUnread(kept(file, grants, asked.range, slot, None))
                }
            }
          }
        )
      )
    }
    Toolbox.of[caps.CapSet^{clock, files, said}](list, search, read) match {
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

  /** `drive_read`: one file's text, by its id: a PDF, a Word file, an Excel file or a Slides
    * deck by `range`.
    */
  val Read: Hosted[Reading] = new Hosted(
    ToolSpec(
      ToolName("drive_read"),
      "Read a Drive file's text by its id, from a search: a Doc as markdown, a Sheet's first " +
        "sheet as CSV, a text file as it is, and a PDF, a Word file, an Excel file or a Slides " +
        "deck a part at a time. For those, with no other argument it gives the file's outline (its headings, with " +
        "their pages) and its start; ask for pages, a section by its heading, or from an offset " +
        "to read on, or near a passage as a piece's pointer gives it. Each read says what it " +
        "covers.",
      Args
        .of(
          (
            file = Field.text("The file's id, from a search."),
            pages = RangeArgs.Pages,
            section = RangeArgs.Section,
            from = RangeArgs.From,
            near = RangeArgs.Near
          )
        )
        .refine(a =>
          for {
            id <- FileId
              .of(a.file)
              .left
              .map(why => ArgsError.Refused("file", DriveTools.sentence(why)))
            range <- FileRange.parse(a.pages, a.section, a.from, a.near)
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
