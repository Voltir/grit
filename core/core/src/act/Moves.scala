package grit.core.act

import java.time.Instant

import grit.core.document.DocumentKeeper
import grit.core.durable.Journaled
import grit.core.message.Message
import grit.core.place.Service
import grit.core.provider.ModelRequest
import grit.core.store.{StoreError, Tx}
import grit.core.tool.ToolName
import grit.core.visibility.Label

/** A model's response to an ask, and the most what it holds can be: what the request was
  * built from was read at no more than `at`.
  */
final case class Asked(message: Message.Assistant, at: Label)

/** What a call came to. Never a person's decline or silence: a call whose tool asks first is
  * not sent under [[Gates.Closed]], and a planner's acting is closed.
  */
enum Called {

  /** The edge ran it: `text` is its answer, as the model would read it, holding nothing above
    * `at`.
    */
  case Done(text: String, at: Label)

  /** It did not run, or failed: `why`. Nothing was sent when no edge serves the place's tool,
    * the tool is not advertised there or asks first, or the acting may not read from that place,
    * send it the arguments, or write where they name (ADR 0031); no edge claimed it within
    * `grit.act.phase.Calling.ServeWithin`; or the tool failed.
    */
  case Failed(why: String)

  /** An edge claimed it and did not answer within `grit.act.phase.Calling.RunWithin` more: it
    * may have partly run.
    */
  case Interrupted
}

/** A planner's moves (ADR 0034), each made under its run's [[Acting]] at most once, as durable
  * steps named for it: a run resumed after a crash gets each move's result back without making
  * it again. So a run makes the same moves, by name, with the same inputs, in the same order,
  * every time: its code between moves reads only its `JobRun` and its moves' results, and a move
  * whose input differs on a rerun is [[MoveError.Diverged]]. Any move is refused, and not made,
  * when its name was used earlier in the run ([[MoveError.Repeated]]) or its kind's limit is
  * reached ([[MoveError.OverLimit]]).
  */
trait Moves {

  /** One response to `request` from the model the catalog in force assigns to summaries
    * (`grit.core.model.Policy.summary`), within its output budget; its cost recorded once,
    * under the run's conversation. The request goes to that model's provider, a third party: a
    * planner sends only what its deployment declared may go there. [[MoveError.Capped]] when the
    * allowance does not admit it, and no model is called; [[MoveError.Model]] when no catalog
    * reads or the provider fails after its retries.
    */
  def ask(name: MoveName, request: ModelRequest): Either[MoveError, Asked]

  /** `tool`, named as a model is offered it (an MCP server's tool under its server's prefix),
    * called with `arguments` at `service`'s place (ADR 0017), for the acting's principal, with
    * the retry the serving edge advertises for it. A tool that writes outside grit names its
    * destination in `arguments`' `to`, as a model's call does ([[grit.core.tool.Writes.Field]]).
    * Every refusal is a [[Called.Failed]], never a [[MoveError]].
    */
  def call(
      name: MoveName,
      service: Service,
      tool: ToolName,
      arguments: ujson.Obj
  ): Either[MoveError, Called]
}

/** [[Moves]], and the acting plugin's own documents. */
trait Keeping extends Moves {

  /** `body` over its plugin's documents, `at` the instant the move is made, in one transaction
    * opened for the acting's subject and committed with the move's record: so written at no
    * less than its floor. [[MoveError.Store]] when the store fails, another run wrote one of the
    * same keys meanwhile, or `body` returns `Left`: nothing it wrote is kept, and the move counts
    * as made.
    */
  def keep[A <: caps.Pure: Journaled](name: MoveName)(
      body: (DocumentKeeper, Instant) -> Tx^ ?-> Either[StoreError, A]
  ): Either[MoveError, A]
}
