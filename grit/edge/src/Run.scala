package grit.edge

import grit.core.approval.Approval
import grit.core.edge.{Permit, ToolRequest}
import grit.core.id.ToolCallId
import grit.core.message.AssistantBlock
import grit.core.model.NameRepair
import grit.core.tool.{Bound, CallError, Outcome, Repairs, ToolName, Toolbox}

/** How an edge runs a request with its tools. */
object Run {

  /** `request` run by `tools`: its arguments read under its repairs, then run, free or
    * approved as its permit says. [[Outcome.Failed]] when this edge is too old for its
    * protocol, when no tool here has its name or its arguments do not read, or when its
    * permit does not match the tool's gate (a free permit for a tool that asks first, or the
    * reverse).
    */
  def request[C^](request: ToolRequest, tools: Toolbox[C]): Outcome =
    if (request.protocol > ToolRequest.Protocol)
      Outcome.Failed(s"This edge is too old for protocol ${request.protocol}; it did not run.")
    else {
      val call: AssistantBlock.ToolCall = AssistantBlock.ToolCall(
        ToolCallId(request.slot.key),
        ToolName.value(request.tool),
        request.arguments
      )
      val bound: Either[CallError, Bound^{C}] =
        tools.bind(call, Repairs(NameRepair.AsSent, request.repairs))
      bound match {
        case Left(error) => error.outcome
        case Right(free: Bound.Free) =>
          if (request.permit == Permit.Free) free()
          else Outcome.Failed(s"${ToolName.value(request.tool)} does not ask first here; it did not run.")
        case Right(gated: Bound.Gated) =>
          if (request.permit == Permit.Approved) gated(Approval.Approved)
          else Outcome.Failed(s"${ToolName.value(request.tool)} asks first here; it did not run.")
      }
    }
}
