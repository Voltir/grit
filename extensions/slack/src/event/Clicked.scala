package grit.slack.event

import grit.core.id.{ToolCallId, WorkflowId}

/** A click on an ask's card, as [[Events.clicked]] reads it: `user` of `team` clicked `button`
  * on the card asking about the gated call `call` of the turn whose workflow is `workflow`; the
  * card is rewritten, or its clicker answered, at `respondAt`.
  */
final case class Clicked(
    team: TeamId,
    user: UserId,
    workflow: WorkflowId,
    call: ToolCallId,
    button: Clicked.Button,
    respondAt: ResponseUrl
)

object Clicked {

  /** A card's buttons, each under the action id Slack sends back when it is clicked. */
  enum Button(val actionId: String) {

    /** Lets the call run. */
    case Apply extends Button("grit_apply")

    /** Declines the call. */
    case Cancel extends Button("grit_cancel")
  }

  /** What a card's buttons carry for `workflow`'s call `call`, which [[Events.clicked]] reads
    * back: those two ids alone, never what the call would change.
    */
  def value(workflow: WorkflowId, call: ToolCallId): String =
    ujson
      .Obj("workflow" -> WorkflowId.value(workflow), "call" -> ToolCallId.value(call))
      .render()
}
