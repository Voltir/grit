package grit.core.edge

import grit.core.id.{CallSlot, ConversationId, PrincipalId}
import grit.core.model.ArgRepair
import grit.core.place.Place
import grit.core.tool.{Retry, ToolName}

/** One call to a hosted tool, as its request row holds it (ADR 0017): made by the turn at
  * `slot` of `conversation`, for `principal`, addressed to the workspace `workspace` and never
  * to an edge; its `tool` with `arguments` as the model sent them, to be read under
  * `repairs`; how it was let through (`permit`), and what happens if its run is cut short
  * (`retry`); under protocol version `protocol`; `destination`, the place a writing tool's
  * call writes to, `None` for one that declares none: an edge's writing tool is told it,
  * never the call's own argument ([[grit.core.tool.Writes.Field]]).
  */
final case class ToolRequest(
    slot: CallSlot,
    protocol: Int,
    conversation: ConversationId,
    workspace: Place,
    principal: PrincipalId,
    tool: ToolName,
    permit: Permit,
    retry: Retry,
    arguments: ujson.Value,
    repairs: Set[ArgRepair],
    destination: Option[Place]
)

object ToolRequest {

  /** The protocol version requests are written under now. An edge that knows a lower one
    * answers a request of this one that it is too old.
    */
  val Protocol: Int = 1
}

/** How a call was let through before it was sent to an edge: its tool runs without asking,
  * or a person approved this call.
  */
enum Permit(val key: String) {
  case Free extends Permit("free")
  case Approved extends Permit("approved")
}

object Permit {

  /** The permit whose [[Permit.key]] is `key`; `None` for no permit's. */
  def of(key: String): Option[Permit] = Permit.values.find(_.key == key)
}
