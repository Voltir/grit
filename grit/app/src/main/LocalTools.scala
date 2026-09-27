package grit.app.main

import java.nio.file.Path

import grit.core.edge.{Route, ToolRequest}
import grit.core.place.Directory
import grit.core.tool.{Outcome, ToolName}
import grit.edge.{Run, Tools}
import grit.host.{LocalEdits, LocalShell, LocalWorkspace}
import grit.tools.Coding

/** The coding tools this process's edge runs a request with, over the directory it was routed
  * to: `read`, `list` and `search` when `offered` is [[Main.ToolChoice.Read]], every coding
  * tool when it is [[Main.ToolChoice.All]]. A command sees the process's own environment,
  * not `.env`'s.
  */
private[main] final class LocalTools(offered: Main.ToolChoice) extends Tools {

  def run(route: Route, request: ToolRequest): Outcome = {
    val root = Path.of(Directory.value(route.root))
    val ws = new LocalWorkspace(root)
    def refused(name: ToolName): Outcome =
      Outcome.Failed(s"The coding tools offer ${ToolName.value(name)} twice; nothing ran.")
    offered match {
      case Main.ToolChoice.Read =>
        Coding.readOnly(ws).fold(d => refused(d.name), tools => Run.request(request, tools))
      case Main.ToolChoice.All =>
        Coding
          .all(ws, new LocalEdits(root), new LocalShell(root, sys.env))
          .fold(d => refused(d.name), tools => Run.request(request, tools))
    }
  }
}
