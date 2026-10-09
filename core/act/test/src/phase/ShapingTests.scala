package grit.act.phase

import grit.core.message.Message
import grit.core.model.StrictSchemas
import grit.core.provider.{ModelRequest, ToolUse}
import grit.core.schema.JsonSchema

import utest.*

/** A JSON ask's phases: its request, its reply read, and its one repair. */
object ShapingTests extends TestSuite {

  private val schemaJson: ujson.Obj = ujson.Obj(
    "type" -> "object",
    "properties" -> ujson.Obj(
      "count" -> ujson.Obj("type" -> "integer", "minimum" -> 1, "maximum" -> 5)
    ),
    "required" -> ujson.Arr("count"),
    "additionalProperties" -> false
  )

  private val schema: JsonSchema =
    JsonSchema.read(schemaJson).fold(e => throw new IllegalStateException(e.message), identity)

  private val asked: Vector[Message] = Vector(Message.User("how many?"))

  private def sent(strict: StrictSchemas): ModelRequest =
    Shaping.request("system", asked, "reply", schema, strict)

  val tests = Tests {
    test("the request requires a call of its one tool, whose parameters are the schema") {
      val got = sent(StrictSchemas.Ignored)
      (got.system, got.messages, got.use, got.tools.map(t => (t.name, t.parameters))) ==>
        ("system", asked, ToolUse.Required, Vector(("reply", schemaJson)))
    }

    test("strict is sent when the upstream holds it, under auto or only when required") {
      val strictness = StrictSchemas.values.toVector.map(s => s -> sent(s).tools.map(_.strict))
      strictness ==> Vector(
        StrictSchemas.Enforced -> Vector(true),
        StrictSchemas.WhenRequired -> Vector(true),
        StrictSchemas.Ignored -> Vector(false),
        StrictSchemas.Rejected -> Vector(false)
      )
    }
  }
}
