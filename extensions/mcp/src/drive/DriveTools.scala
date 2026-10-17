package grit.mcp.drive

import grit.core.clock.Clock
import grit.core.edge.{Route, ToolRequest}
import grit.core.tool.{
  Args,
  ArgsError,
  Field,
  Gate,
  Hosted,
  Outcome,
  Retry,
  Tool,
  ToolName,
  ToolSet,
  ToolSpec,
  Toolbox,
  Trimmed
}
import grit.edge.{Run, Tools}

/** The requests a Drive edge is sent, run over `rest` wherever they were routed, as
  * [[Run.request]] runs a tool: [[DriveTools.Search]] and [[DriveTools.Read]], each answered
  * [[Outcome.Done]] with the parts [[DriveItems.items]] cuts, trimmed as [[Trimmed.of]] trims
  * them, or [[Outcome.Failed]] with a [[DriveError]]'s words or why it was not placed. The
  * run's `said` hears each failure's cause ([[DriveError.logged]]), its tool named, and that a
  * search's drives could not be listed (its hits then name them by id) or a read's grants
  * could not be read (it is then believed untrusted).
  */
// `clock` is a parameter, rather than a capture-set parameter over `rest`, so the class's
// captures are in its signature (docs/capture-checking.md, "A capset parameter a class's body
// alone uses").
final class DriveTools(
    clock: Clock,
    rest: DriveRest^{clock},
    reading: DriveReading
) extends Tools {

  def run(route: Route, request: ToolRequest, said: String => Unit): Outcome = {
    // `e` told the operator after `what`, when it is not a person's own miss.
    def heard[A](what: String, e: Either[DriveError, A]): Either[DriveError, A] = {
      e.left.foreach(_.logged.foreach(why => said(s"$what: $why")))
      e
    }
    val search: Tool.Offered^{clock, said} = DriveTools.Search.over { s =>
      val drives = rest.drives() match {
        case Right(named) => named
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
    val read: Tool.Offered^{clock, said} = DriveTools.Read.over { id =>
      DriveTools.answer(
        reading,
        heard(
          "drive_read",
          for {
            file <- rest.file(id)
            text <- rest.text(file)
          } yield DriveCalled.Read(
            file,
            heard(
              "drive_read: its grants could not be read, so it is believed untrusted",
              rest.grants(id)
            ).toOption.flatten,
            text
          )
        )
      )
    }
    Toolbox.of[caps.CapSet^{clock, said}](search, read) match {
      case Right(box) => Run.request(request, box)
      // The two are named apart.
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
      case Right(c) =>
        DriveItems.items(reading, c) match {
          case Left(why) => Outcome.Failed(why)
          case Right(items) =>
            val trimmed = Trimmed.of(items.head, items.parts)
            Outcome.Done(trimmed.head, parts = trimmed.parts)
        }
    }

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
              .map(ArgsError.Refused("folder", _))
              .map(Searching(a.text.trim, _))
        },
      Retry.Rerun
    ),
    Gate.Free,
    s => s.text
  )

  /** `drive_read`: one file's text, by its id. */
  val Read: Hosted[FileId] = new Hosted(
    ToolSpec(
      ToolName("drive_read"),
      "Read a Drive file's text by its id, from a search: a Doc as markdown, a Sheet's first " +
        "sheet as CSV, or a text file as it is.",
      Args
        .of((file = Field.text("The file's id, from a search.")))
        .refine(a => FileId.of(a.file).left.map(ArgsError.Refused("file", _))),
      Retry.Rerun
    ),
    Gate.Free,
    id => FileId.value(id)
  )

  /** The two, as an edge advertises them. */
  val Offered: ToolSet =
    ToolSet.of(Vector(Search.entry, Read.entry)).getOrElse(ToolSet.Empty)
}
