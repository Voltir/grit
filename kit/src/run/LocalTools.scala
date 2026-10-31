package grit.kit.run

import java.nio.file.Path

import grit.core.edge.{Route, ToolRequest}
import grit.core.place.Directory
import grit.core.tool.{Outcome, SetRefusal, ToolSet}
import grit.edge.{Run, Tools}
import grit.host.checkout.{LocalEdits, LocalShell, LocalWorkspace}
import grit.kit.deployment.Offered
import grit.tools.Coding

/** The coding tools this process's edge runs a request with, over the directory it was routed
  * to (a request routed to a service is `Failed`, and nothing runs): `read`, `list` and
  * `search` when `offered` is [[Offered.Read]], every coding tool when it is [[Offered.All]].
  * A command sees the process's own environment, not `.env`'s.
  */
final class LocalTools(offered: Offered) extends Tools {

  /** The tools [[run]] runs, as the engine offers them; `Left` names a tool Coding cannot offer
    * with the others.
    */
  def hosted: Either[SetRefusal, ToolSet] =
    ToolSet.of((offered match {
      case Offered.Read => Coding.readOnlyHosted
      case Offered.All => Coding.hosted
    }).map(_.entry))

  def run(route: Route, request: ToolRequest, said: String => Unit): Outcome = route match {
    case Route.Directory(dir) => over(Path.of(Directory.value(dir)), request)
    // This edge registers only directories, so a service route never reaches it.
    case Route.Service(service) =>
      Outcome.Failed(
        s"The coding tools act on a directory, not ${service.place.written}; nothing ran."
      )
  }

  private def over(root: Path, request: ToolRequest): Outcome = {
    val ws = new LocalWorkspace(root)
    def refused(why: SetRefusal): Outcome =
      Outcome.Failed(s"${why.refusing("The coding tools")}; nothing ran.")
    offered match {
      case Offered.Read =>
        Coding.readOnly(ws).fold(refused, tools => Run.request(request, tools))
      case Offered.All =>
        Coding
          .all(ws, new LocalEdits(root), new LocalShell(root, sys.env))
          .fold(refused, tools => Run.request(request, tools))
    }
  }
}
