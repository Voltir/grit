package grit.core.act

import java.time.Instant

import grit.core.document.DocumentKeeper
import grit.core.durable.Journaled
import grit.core.message.Message
import grit.core.place.Service
import grit.core.provider.ModelRequest
import grit.core.schema.{Conforming, JsonSchema, Typed}
import grit.core.store.{StoreError, Tx}
import grit.core.tool.ToolName
import grit.core.visibility.Label

/** What an ask poses, and what its reply reads as: `R`. Each shape is answered by its own
  * model and recorded as that model replied; `R` is read from the record on every run.
  */
sealed trait Posed[R]

object Posed {

  /** `request`, answered by the model in its own words. */
  final case class Text(request: ModelRequest) extends Posed[Message.Assistant]

  /** `system` then `messages`, answered as JSON that `reply`'s schema accepts and `reply`
    * reads. The model is offered one tool, [[ReplyTool]], whose parameters are the schema, and
    * must call it; the call's arguments are the reply. A reply that does not read (no such
    * call, a refusal by the schema, or by `reply`) is shown to the model with why and asked
    * again, up to [[Json.Repairs]] times. Every call is recorded, and all are admitted once,
    * before the first. A message holding a tool call may be refused by the provider, since no
    * other tool is offered.
    */
  final case class Json[T](system: String, messages: Vector[Message], reply: Typed[T])
      extends Posed[T]

  object Json {

    /** 1: how many times a reply that does not read is asked again, so up to two model calls. */
    val Repairs: Int = 1
  }

  /** `system` then `messages`, answered as JSON `schema` accepts. */
  def json(system: String, messages: Vector[Message], schema: JsonSchema): Posed[Conforming] =
    Json(system, messages, Typed.json(schema))

  /** `reply`: the one tool a [[Json]] ask offers. */
  val ReplyTool: String = "reply"
}

/** An ask's reply, and the most what it holds can be: what the ask was made from was read at
  * no more than `at`.
  */
final case class Asked[R](reply: R, at: Label)

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

  /** One reply to `posed`, from the model its shape names: for [[Posed.Text]] and
    * [[Posed.Json]], the model the catalog in force assigns to summaries
    * (`grit.core.model.Policy.summary`), within its output budget. Each model call's cost is
    * recorded once, under the run's conversation. The request goes to that model's provider, a
    * third party the deployment trusts with everything sent to it: nothing checks what goes
    * there. Admitted once, before its first call (a [[Posed.Json]] repair is not admitted
    * again). [[MoveError.Capped]] when the allowance does not admit it, and no model is called;
    * [[MoveError.Model]] when no catalog reads, the provider fails after its retries, or the
    * reply does not read ([[Posed.Json]]: after its repairs).
    */
  def ask[R](name: MoveName, posed: Posed[R]): Either[MoveError, Asked[R]]

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
