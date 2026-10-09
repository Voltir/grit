package grit.act.phase

import grit.core.message.Message
import grit.core.model.StrictSchemas
import grit.core.provider.{ModelRequest, ToolSchema, ToolUse}
import grit.core.schema.JsonSchema

/** A JSON ask's phases: its request, its reply read, and its one repair. */
object Shaping {

  /** `system`, `messages` and `schema` as the request that requires the model to call the one
    * tool `tool`, whose parameters are `schema`, described to the model as "Your reply: its
    * arguments are your answer."; reads only `strict`: sent `strict` when it is `Enforced` or
    * `WhenRequired`.
    */
  def request(
      system: String,
      messages: Vector[Message],
      tool: String,
      schema: JsonSchema,
      strict: StrictSchemas
  ): ModelRequest =
    ModelRequest(
      system,
      messages,
      Vector(ToolSchema(tool, Described, schema.json, strict = held(strict))),
      ToolUse.Required
    )

  /** Whether an upstream treating schemas as `strict` says holds a forced call to its schema. */
  private def held(strict: StrictSchemas): Boolean = strict match {
    case StrictSchemas.Enforced | StrictSchemas.WhenRequired => true
    case StrictSchemas.Ignored | StrictSchemas.Rejected => false
  }

  private val Described = "Your reply: its arguments are your answer."
}
