package grit.tools

import grit.core.context.Label
import grit.core.id.ToolCallId
import grit.core.message.AssistantBlock
import grit.core.tool.{Bound, Outcome, Repairs, Tool, Toolbox}

import utest.*

/** [[About]]: the docs it ships, and which of them a call is answered with. */
object AboutTests extends TestSuite {

  private lazy val about: Tool[Option[About.Subject]] =
    About.load().fold(e => throw new java.lang.AssertionError(e), identity)

  private lazy val box =
    Toolbox.of(about).fold(d => throw new java.lang.AssertionError(s"duplicate $d"), identity)

  /** What a call to `about` with `args` is answered with. */
  private def asked(args: ujson.Obj): Outcome =
    box.bind(AssistantBlock.ToolCall(ToolCallId("c1"), "about", args), Repairs.All) match {
      case Right(b: Bound.Free) => b()
      case other => throw new java.lang.AssertionError(s"not free: $other")
    }

  private def shipped(key: String): String =
    scala.io.Source.fromResource(s"about/$key.md")(using scala.io.Codec.UTF8).mkString.trim

  val tests = Tests {
    test("about with no topic is the overview, and a topic is its doc alone") {
      asked(ujson.Obj()) ==> Outcome.Done(shipped("grit"))
      asked(ujson.Obj("topic" -> "markers")) ==> Outcome.Done(shipped("markers"))
      asked(ujson.Obj("topic" -> "places")) ==> Outcome.Done(shipped("places"))
    }

    test("the markers doc names every label grit writes") {
      Label.values.toVector.filterNot(l => shipped("markers").contains(l.tag)) ==> Vector.empty
    }

    test("about is free, and its topic is one of the subjects") {
      box.schemas(strict = false).map(_.name) ==> Vector("about")
      box
        .schemas(strict = false)
        .flatMap(_.parameters.obj.get("properties"))
        .map(
          _.obj.get("topic").flatMap(_.obj.get("enum"))
        ) ==> Vector(Some(ujson.Arr("grit", "memory", "markers", "periods", "places")))
      about.entry.asks ==> false
    }
  }
}
