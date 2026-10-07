package grit.core.tool

import scala.collection.immutable.VectorMap

import grit.core.id.ToolCallId
import grit.core.message.AssistantBlock
import grit.core.place.Place
import grit.core.visibility.TestLabels.place

import utest.*

/** [[Hosted.advertised]] for an entry that writes outside grit: what the model is shown, and
  * what binding a call comes to.
  */
object HostedTests extends TestSuite {

  private val general: Place = place("slack:T/C1")
  private val board: Place = place("slack:T/C3")

  private val writes: Writes[Place] =
    Writes
      .of(
        VectorMap("general" -> general, "#general" -> general, "board" -> board),
        p => p,
        "The channel to post in."
      )
      .fold(e => throw new java.lang.AssertionError(e), _.placed)

  private val schema = ujson.Obj(
    "type" -> "object",
    "properties" -> ujson.Obj("text" -> ujson.Obj("type" -> "string")),
    "required" -> ujson.Arr("text")
  )

  private val entry =
    ToolSet.Entry(ToolName("post"), "Posts.", schema, false, Retry.Interrupt, Some(writes))

  private val box: Toolbox[{}] =
    Hosted.advertised(entry).toVector match {
      case Vector(offered) =>
        Toolbox.of(offered).fold(d => throw new java.lang.AssertionError(d.toString), identity)
      case none => throw new java.lang.AssertionError(s"not offered: $none")
    }

  private def call(args: ujson.Value): AssistantBlock.ToolCall =
    AssistantBlock.ToolCall(ToolCallId("c1"), "post", args)

  val tests = Tests {
    test(
      "a call of an advertised writing entry binds the place its `to` names as its destination, and its arguments without `to`"
    ) {
      box.bind(call(ujson.Obj("to" -> "#general", "text" -> "hi")), Repairs.All) match {
        case Right(b: Bound.Hosted) =>
          (b.destination, b.arguments, b.shown) ==>
            (Some(general), ujson.Obj("text" -> "hi"), """post {"text":"hi"}""")
        case other => throw new java.lang.AssertionError(s"not hosted: $other")
      }
    }

    test(
      "a call naming no place of the entry's, or none, is Unwritable with the names it may give"
    ) {
      val names = Vector("general", "#general", "board")
      box.bind(call(ujson.Obj("to" -> "random", "text" -> "hi")), Repairs.All).map(_.tool) ==>
        Left(CallError.Unwritable(ToolName("post"), Some("random"), names))
      box.bind(call(ujson.Obj("text" -> "hi")), Repairs.All).map(_.tool) ==>
        Left(CallError.Unwritable(ToolName("post"), None, names))
      // What the model reads: the names, and that nothing left grit.
      CallError.Unwritable(ToolName("post"), Some("random"), names).message ==>
        "The call to `post` was not run: `to` must be one of general, #general, board, the " +
        "places it may write to from here, not random. Nothing was sent."
    }

    test(
      "the model is shown `to` as a required choice among the entry's names, and the entry advertised is the entry"
    ) {
      val shown = box.schemas(strict = false).map(_.parameters)
      shown ==> Vector(
        ujson.Obj(
          "type" -> "object",
          "properties" -> ujson.Obj(
            "to" -> ujson.Obj(
              "type" -> "string",
              "enum" -> ujson.Arr("general", "#general", "board"),
              "description" -> "The channel to post in."
            ),
            "text" -> ujson.Obj("type" -> "string")
          ),
          "required" -> ujson.Arr("to", "text")
        )
      )
      box.set.tools ==> Vector(entry)
    }

    test("a writing tool whose own arguments declare `to` is refused") {
      val own = ToolSpec(
        ToolName("post"),
        "Posts.",
        Args.of((to = Field.text("Where."), text = Field.text("What.")))
      )
      Hosted.writing(own, Gate.Free, _.text, writes).map(_.name) ==>
        Left("the tool post's arguments declare to, which only its writes may name")
    }
  }
}
