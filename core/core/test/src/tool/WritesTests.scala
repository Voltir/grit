package grit.core.tool

import scala.collection.immutable.VectorMap

import grit.core.place.Place
import grit.core.visibility.TestLabels.place

import utest.*

object WritesTests extends TestSuite {

  /** A destination as an edge declares it: its own id, and the place core labels it by. */
  private final case class Channel(id: String, at: Place) extends caps.Pure

  private val general = Channel("C1", place("slack:T/C1"))
  private val random = Channel("C2", place("slack:T/C2"))
  private val board = Channel("C3", place("slack:T/C3"))

  private val writes: Writes[Channel] =
    Writes
      .of(
        VectorMap(
          "general" -> general,
          "#general" -> general,
          "random" -> random,
          "board" -> board
        ),
        _.at,
        "The channel to post in."
      )
      .fold(e => throw new java.lang.AssertionError(e), identity)

  val tests = Tests {
    test("narrowed keeps the names whose place is kept, in order, and none when none is left") {
      writes.narrowed(_ != random.at).map(_.to.keys.toVector) ==>
        Some(Vector("general", "#general", "board"))
      writes.narrowed(_ => false) ==> None
    }

    test("of refuses no names, and a blank name") {
      Writes.of(VectorMap.empty[String, Channel], _.at, "Where.") ==>
        Left("a writing tool names no destination")
      Writes.of(VectorMap("general" -> general, " " -> random), _.at, "Where.") ==>
        Left("a destination's name is blank")
    }

    test("a place resolves to the destination placed at it, a name to the one it names") {
      (writes.at(random.at), writes.at(place("slack:T/C9"))) ==> (Some(random), None)
      (writes.named("#general"), writes.named("C1")) ==> (Some(general), None)
    }

    test("placed carries each name to its destination's place") {
      writes.placed.to ==> VectorMap(
        "general" -> general.at,
        "#general" -> general.at,
        "random" -> random.at,
        "board" -> board.at
      )
    }

    test("a destination cannot be a capability") {
      val error = assertCompileError(
        """def f(ws: grit.core.host.Workspace^) = Writes.of(VectorMap("ws" -> ws), _ => Place.Everywhere, "")"""
      )
      assert(error.msg.contains("caps.Pure"))
    }

    test("writes are made only through of") {
      val applied = assertCompileError("""Writes(VectorMap("general" -> general), _.at, "")""")
      val made = assertCompileError("""new Writes(VectorMap("general" -> general), _.at, "")""")
      // Its apply is as private as its constructor, so the object takes no parameters.
      assert(
        applied.msg.contains("does not take parameters"),
        made.msg.contains("cannot be accessed")
      )
    }
  }
}
