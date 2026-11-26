package grit.kit.run

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import grit.core.clock.{Clock, Fresh}
import grit.core.edge.{Route, ToolRequest}
import grit.core.host.{Allowlisted, HostError, RepositoryInstructions}
import grit.core.place.{Directory, Repository}
import grit.core.tool.{Outcome, SetRefusal, ToolSet}
import grit.edge.desk.{Run, Tools}
import grit.host.checkout.{LocalEdits, LocalRuns, LocalShell, LocalWorkspace, Redaction}
import grit.host.git.WorkingCopies
import grit.kit.deployment.Offered
import grit.tools.{Coding, Versioning}

/** The coding tools this process's edge runs a request with, over the directory it was routed
  * to, or for a repository route in the working copy `copies` keeps for its run (a request
  * routed to a service, or to a repository `copies` does not keep, is `Failed`, and nothing
  * runs; so is one whose copy cannot be opened, with why): those `offered` hosts
  * ([[Offered.hosted]]), an allowlisted `run` starting only what its allowlist permits. A command
  * sees the process's own environment, not `.env`'s; what a tool shows is cleared by
  * `redaction`, the deployment's one. A command an allowlisted `run` left going is one of its
  * directory's runs, which every later request routed there reaches, whichever conversation
  * sent it, until [[close]]. With `pushing`, a repository route's `offer_pull_request` offers
  * its run's branch ([[grit.tools.Versioning]]); at a directory, that tool is no tool here.
  */
final class LocalTools(
    offered: Offered,
    redaction: Redaction,
    clock: Clock^,
    fresh: Fresh^,
    copies: Option[WorkingCopies^],
    pushing: Option[Pushing^] = None
) extends Tools
    with AutoCloseable
    with caps.SharedCapability {

  /** The repository's own instruction file ([[WorkingCopies.instructionFile]]); `Right(None)`
    * when these tools serve no repository.
    */
  def instructionFile: Either[HostError, Option[RepositoryInstructions]] = copies match {
    case Some(c) => c.instructionFile()
    case None => Right(None)
  }

  /** The repository whose runs these tools serve, each in its working copy. */
  def repository: Option[Repository] = copies match {
    case Some(c) => Some(c.repository)
    case None => None
  }

  /** Each directory's runs, made by its first allowlisted request. Only ever read or changed
    * under this object's lock.
    */
  private val runs: java.util.HashMap[Path, LocalRuns^] =
    new java.util.HashMap[Path, LocalRuns^]()

  /** The tools [[run]] runs, as the engine offers them; `Left` names a tool Coding cannot offer
    * with the others.
    */
  def hosted: Either[SetRefusal, ToolSet] =
    ToolSet.of(offered.hosted.map(_.entry))

  /** The tools [[run]] runs at its repository's place: [[hosted]]'s, then `offer_pull_request`
    * when its runs push ([[Pushing]]).
    */
  def hostedInRepository: Either[SetRefusal, ToolSet] =
    pushing match {
      case Some(p) => ToolSet.of(offered.hosted.map(_.entry) :+ p.described.entry)
      case None => hosted
    }

  def run(route: Route, request: ToolRequest, said: String => Unit): Outcome =
    if (closed.get()) Outcome.Failed("These coding tools are closed; nothing ran.")
    else {
      pruned()
      routed(route, request)
    }

  private def routed(route: Route, request: ToolRequest): Outcome = route match {
    case Route.Directory(dir) => over(Path.of(Directory.value(dir)), request)
    // This edge registers only directories, so a service route never reaches it.
    case Route.Service(service) =>
      Outcome.Failed(
        s"The coding tools act on a directory, not ${service.place.written}; nothing ran."
      )
    case Route.Repository(repository, work) =>
      copies match {
        case Some(kept) if kept.repository == repository =>
          // The run's driver holds its copy (WorkingCopies.held); opened gives the same one.
          kept
            .opened(work)
            .fold(
              e => Outcome.Failed(s"${e.message} Nothing ran."),
              copy =>
                pushing match {
                  case Some(p) if request.tool == Versioning.Name =>
                    Versioning
                      .offered(p.branch(work), p.described)
                      .fold(
                        why => Outcome.Failed(s"${why.refusing("The pull request tool")}."),
                        tools => Run.request(request, tools)
                      )
                  case _ => over(copy, request)
                }
            )
        case _ =>
          Outcome.Failed(
            s"This edge keeps no working copy of ${repository.place.written}; nothing ran."
          )
      }
  }

  private def over(root: Path, request: ToolRequest): Outcome = {
    val ws = new LocalWorkspace(root, redaction)
    def refused(why: SetRefusal): Outcome =
      Outcome.Failed(s"${why.refusing("The coding tools")}; nothing ran.")
    offered match {
      case Offered.Read =>
        Coding.readOnly(ws).fold(refused, tools => Run.request(request, tools))
      case Offered.All =>
        Coding
          .all(ws, new LocalEdits(root, redaction), new LocalShell(root, sys.env, redaction))
          .fold(refused, tools => Run.request(request, tools))
      case Offered.Allowlisted(allowlist) =>
        Coding
          .allowlisted(
            ws,
            new LocalEdits(root, redaction),
            Allowlisted.over(runsAt(root), allowlist)
          )
          .fold(refused, tools => Run.request(request, tools))
    }
  }

  /** Kills every command an allowlisted `run` left going, and removes their files; every later
    * request is `Failed`, and nothing runs.
    */
  def close(): Unit = {
    synchronized {
      closed.set(true)
      runs.values.forEach(_.close())
      runs.clear()
    }
  }

  /** Whether [[close]] was called. */
  private val closed = new java.util.concurrent.atomic.AtomicBoolean(false)

  /** The runs of each directory that is gone, a copy its sweep removed, closed and dropped. */
  private def pruned(): Unit = synchronized {
    val gone = runs.keySet.asScala.toVector.filterNot(Files.isDirectory(_))
    gone.foreach { root =>
      runs.get(root).close()
      val _ = runs.remove(root)
    }
  }

  /** `root`'s runs, made when it has none. */
  private def runsAt(root: Path): LocalRuns^ = synchronized {
    if (runs.containsKey(root)) runs.get(root)
    else {
      val made: LocalRuns^ = new LocalRuns(root, sys.env, redaction, clock, fresh)
      val _ = runs.put(root, made)
      made
    }
  }
}
