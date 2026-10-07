package grit.core.tool

import scala.collection.immutable.VectorMap

import grit.core.id.{TestCallSlots, ToolCallId}
import grit.core.message.AssistantBlock
import grit.core.place.Place
import grit.core.visibility.TestLabels.place

import utest.*

/** [[Writing.over]]: an edge's writing tool, told the destination its request was checked to
  * write to.
  */
object WritingTests extends TestSuite {

  /** A destination as an edge declares it: its own id, and the place core labels it by. */
  private final case class Channel(id: String, at: Place) extends caps.Pure

  private val general = Channel("C1", place("slack:T/C1"))
  private val random = Channel("C2", place("slack:T/C2"))

  private val writes: Writes[Channel] =
    Writes
      .of(
        VectorMap("general" -> general, "#general" -> general, "random" -> random),
        _.at,
        "The channel to post in."
      )
      .fold(e => throw new java.lang.AssertionError(e), identity)

  private val writing: Writing[String, Channel] =
    Hosted
      .writing(
        ToolSpec(ToolName("post"), "Posts.", Args.of((text = Field.text("What."))).map(_.text)),
        Gate.Free,
        t => t,
        writes
      )
      .fold(e => throw new java.lang.AssertionError(e), identity)

  private val box: Toolbox[{}] =
    Toolbox
      .of(writing.over((text, to) => Outcome.Done(s"$text in ${to.id}")))
      .fold(d => throw new java.lang.AssertionError(d.toString), identity)

  private def call(args: ujson.Value): AssistantBlock.ToolCall =
    AssistantBlock.ToolCall(ToolCallId("c1"), "post", args)

  /** What `box` comes to for a request of `args` to `destination`, run when it binds. */
  private def ran(args: ujson.Value, destination: Option[Place]): Either[CallError, Outcome] =
    box.requested(call(args), Repairs.All, destination).map {
      case free: Bound.Free => free(TestCallSlots.First)
      case other => throw new java.lang.AssertionError(s"not free: $other")
    }

  val tests = Tests {
    test(
      "over's tool is told the destination placed at its request's, whatever its arguments hold for `to`"
    ) {
      ran(ujson.Obj("to" -> "random", "text" -> "hi"), Some(general.at)) ==>
        Right(Outcome.Done("hi in C1"))
      ran(ujson.Obj("text" -> "hi"), Some(random.at)) ==> Right(Outcome.Done("hi in C2"))
    }

    test(
      "a request holding no destination, or one at none of the tool's, is Misdirected and does not run"
    ) {
      ran(ujson.Obj("to" -> "general", "text" -> "hi"), None) ==>
        Left(CallError.Misdirected(ToolName("post"), None))
      ran(ujson.Obj("to" -> "general", "text" -> "hi"), Some(place("slack:T/C9"))) ==>
        Left(CallError.Misdirected(ToolName("post"), Some(place("slack:T/C9"))))
    }

    test("over's tool advertises what its description does") {
      box.set.tools ==> Vector(writing.entry)
      box.schemas(strict = true) ==> Vector(writing.schema(strict = true))
    }

    test("a writing tool cannot be run without being told its destination") {
      val error = assertCompileError("""writing.over((text: String) => Outcome.Done(text))""")
      assert(error.msg.contains("Found:"))
    }
  }
}
