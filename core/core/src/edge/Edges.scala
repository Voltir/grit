package grit.core.edge

import grit.core.id.{EdgeId, PrincipalId}
import grit.core.place.Place

/** An edge as it registered: its id, the principal it acts for, and the places it hosts. */
final case class Registration(edge: EdgeId, principal: PrincipalId, places: Set[Place])

/** Where a request may run, as [[Edges.authorize]] routed it. */
sealed trait Route

object Route {

  /** In the directory `root`: file and command tools act on it. */
  final case class Directory private[edge] (root: grit.core.place.Directory) extends Route

  /** At `service`'s place: its tools act on no directory. */
  final case class Service private[edge] (service: grit.core.place.Service) extends Route
}

/** Why an edge may not run a request. */
enum Refused {

  /** The request's workspace is not one of the places the edge registered. */
  case NotHosted(workspace: Place)

  /** The workspace is a place the edge registered, but neither a directory nor a service:
    * nothing runs there.
    */
  case NoRoute(workspace: Place)
}

object Edges {

  /** Whether `by` may run `request`, and where: an edge serves only the places it registered;
    * a directory place is routed to its directory, a service place to its service. The one
    * routing decision; the permission model will plug in here.
    */
  def authorize(request: ToolRequest, by: Registration): Either[Refused, Route] =
    if (!by.places.contains(request.workspace)) Left(Refused.NotHosted(request.workspace))
    else
      request.workspace.directory
        .map(Route.Directory(_))
        .orElse(request.workspace.service.map(Route.Service(_)))
        .toRight(Refused.NoRoute(request.workspace))
}
