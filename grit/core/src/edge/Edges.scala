package grit.core.edge

import grit.core.id.{EdgeId, PrincipalId}
import grit.core.place.{Directory, Place}

/** An edge as it registered: its id, the principal it acts for, and the places it hosts. */
final case class Registration(edge: EdgeId, principal: PrincipalId, places: Set[Place])

/** Where a request may run: the directory its tools act on. Only [[Edges.authorize]] makes
  * one.
  */
final case class Route private[edge] (root: Directory)

/** Why an edge may not run a request. */
enum Refused {

  /** The request's workspace is not one of the places the edge registered. */
  case NotHosted(workspace: Place)

  /** The workspace is a place the edge registered, but no directory to run tools over. */
  case NoDirectory(workspace: Place)
}

object Edges {

  /** Whether `by` may run `request`, and over which directory: an edge serves only the places
    * it registered, and only a directory place has a root. The one routing decision; the
    * permission model will plug in here.
    */
  def authorize(request: ToolRequest, by: Registration): Either[Refused, Route] =
    if (!by.places.contains(request.workspace)) Left(Refused.NotHosted(request.workspace))
    else request.workspace.directory.map(Route(_)).toRight(Refused.NoDirectory(request.workspace))
}
