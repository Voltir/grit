package grit.core.edge

import grit.core.id.{EdgeId, PrincipalId}
import grit.core.place.{Place, WorkName}
import grit.core.store.Origin

/** An edge as it registered: its id, the principal it acts for, and the places it hosts. */
final case class Registration(edge: EdgeId, principal: PrincipalId, places: Set[Place])

/** Where a request may run, as [[Edges.authorize]] routed it. */
sealed trait Route

object Route {

  /** In the directory `root`: file and command tools act on it. */
  final case class Directory private[edge] (root: grit.core.place.Directory) extends Route

  /** At `service`'s place: its tools act on no directory. */
  final case class Service private[edge] (service: grit.core.place.Service) extends Route

  /** In the hosting edge's working copy of `repository` for the run `work`. */
  final case class Repository private[edge] (
      repository: grit.core.place.Repository,
      work: WorkName
  ) extends Route
}

/** Why an edge may not run a request. */
enum Refused {

  /** The request's workspace is not one of the places the edge registered. */
  case NotHosted(workspace: Place)

  /** The workspace is a place the edge registered where nothing runs for the request: neither
    * a directory nor a service, nor a repository whose run made it.
    */
  case NoRoute(workspace: Place)
}

object Edges {

  /** Whether `by` registered `request`'s workspace. */
  def hosts(request: ToolRequest, by: Registration): Either[Refused.NotHosted, Unit] =
    if (by.places.contains(request.workspace)) Right(())
    else Left(Refused.NotHosted(request.workspace))

  /** Whether `by` may run `request`, and where: an edge serves only the places it registered;
    * a directory place is routed to its directory, a service place to its service, and a
    * repository's place to the run `origin` names there, `origin` being that of the
    * conversation whose turn made `request` ([[Desk.origin]]). The one routing decision; the
    * permission model will plug in here.
    */
  def authorize(
      request: ToolRequest,
      by: Registration,
      origin: Option[Origin]
  ): Either[Refused, Route] =
    hosts(request, by).flatMap { _ =>
      val at = request.workspace
      at.directory
        .map(Route.Directory(_))
        .orElse(at.service.map(Route.Service(_)))
        .orElse(origin.collect {
          case Origin.Work(repository, work) if repository.place == at =>
            Route.Repository(repository, work)
        })
        .toRight(Refused.NoRoute(at))
    }
}
