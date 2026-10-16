package grit.mcp.drive

import grit.core.clock.Clock
import grit.core.edge.{Route, ToolRequest}
import grit.core.tool.{
  Args,
  ArgsError,
  Field,
  Fitted,
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
import grit.edge.{Run, Tools}

/** The requests a Drive edge is sent, run over `rest` wherever they were routed, as
  * [[Run.request]] runs a tool: [[DriveTools.Search]] and [[DriveTools.Read]], each answered
  * [[Outcome.Done]] with the parts [[DriveItems.items]] cuts, fitted as [[Fitted.of]] fits
  * them, or [[Outcome.Failed]] with a [[DriveError]]'s words or why it was not placed.
  */
// It logs nothing: the edge's Server is handed the log too, and separation checking lets no
// two of its arguments share one. Why an answer could not be read reaches the operator when the
// edge opens (DriveEdge), and the model is told only to try again.
// `clock` is a parameter, rather than a capture-set parameter over `rest`, so the class's
// captures are in its signature (docs/capture-checking.md, "A capset parameter a class's body
// alone uses").
final class DriveTools(
    clock: Clock,
    rest: DriveRest^{clock},
    reading: DriveReading
) extends Tools {

  def run(route: Route, request: ToolRequest): Outcome = {
    val search: Tool.Offered^{clock} = DriveTools.Search.over(s =>
      DriveTools.answer(
        reading,
        rest
          .search(s.text, s.folder)
          .map(DriveCalled.Searched(_, rest.drives().getOrElse(Map.empty)))
      )
    )
    val read: Tool.Offered^{clock} = DriveTools.Read.over { id =>
      DriveTools.answer(
        reading,
        for {
          file <- rest.file(id)
          text <- rest.text(file)
        } yield DriveCalled.Read(file, rest.grants(id).toOption.flatten, text)
      )
    }
    Toolbox.of[caps.CapSet^{clock}](search, read) match {
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
            val fitted = Fitted.of(items.head, items.parts)
            Outcome.Done(fitted.head, parts = fitted.parts)
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
