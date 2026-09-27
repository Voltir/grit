package grit.core.edge

import grit.core.approval.Approval
import grit.core.id.{EdgeId, PrincipalId, ToolCallId}
import grit.core.message.AssistantBlock
import grit.core.model.NameRepair
import grit.core.place.{Directory, Place}
import grit.core.tool.{Bound, Outcome, Repairs, ToolName, Toolbox}

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

  /** `request` run by `tools`: its arguments read under its repairs, then run, free or
    * approved as its permit says. [[Outcome.Failed]] when this edge is too old for its
    * protocol, when no tool here has its name or its arguments do not read, or when its
    * permit does not match the tool's gate (a free permit for a tool that asks first, or the
    * reverse).
    */
  def run[C^](request: ToolRequest, tools: Toolbox[C]): Outcome =
    if (request.protocol > ToolRequest.Protocol)
      Outcome.Failed(s"This edge is too old for protocol ${request.protocol}; it did not run.")
    else {
      val call: AssistantBlock.ToolCall = AssistantBlock.ToolCall(
        ToolCallId(request.slot.key),
        ToolName.value(request.tool),
        request.arguments
      )
      tools.bind(call, Repairs(NameRepair.AsSent, request.repairs)) match {
        case Left(error) => error.outcome
        case Right(free: Bound.Free) =>
          if (request.permit == Permit.Free) free()
          else
            Outcome.Failed(
              s"${ToolName.value(request.tool)} does not ask first here; it did not run."
            )
        case Right(gated: Bound.Gated) =>
          if (request.permit == Permit.Approved) gated(Approval.Approved)
          else Outcome.Failed(s"${ToolName.value(request.tool)} asks first here; it did not run.")
      }
    }
}
