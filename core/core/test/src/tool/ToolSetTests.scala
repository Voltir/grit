package grit.core.tool

import scala.collection.immutable.VectorMap

import grit.core.place.Place
import grit.core.store.Tx
import grit.core.visibility.TestLabels.place
import grit.core.visibility.{
  Clearance,
  Compartments,
  Label,
  Labelled,
  Level,
  RoomLabels,
  Visibility
}
import grit.dbos.sql.TestTx

import utest.*

/** [[ToolSet]]'s stored form and id: a turn's recorded first step names a set by its id, and
  * its replay reads the set back from that form.
  */
object ToolSetTests extends TestSuite {

  private val peek =
    ToolSet.Entry(
      ToolName("peek"),
      "Peeks.",
      ujson.Obj("type" -> "object"),
      asks = false,
      Retry.Rerun
    )

  /** A tool that posts to a channel named `general` or `#general`, or one named `random`. */
  private val post =
    ToolSet.Entry(
      ToolName("post"),
      "Posts.",
      ujson.Obj("type" -> "object", "properties" -> ujson.Obj("text" -> ujson.Obj())),
      asks = false,
      Retry.Interrupt,
      Writes
        .of[Place](
          VectorMap(
            "general" -> place("slack:T/C1"),
            "#general" -> place("slack:T/C1"),
            "random" -> place("slack:T/C2")
          ),
          p => p,
          "The channel."
        )
        .toOption
        .map(_.placed)
    )

  private val internal = Label.at(Level.Internal)

  /* `general`'s channel is internal, `random`'s public. */
  private val channels: Visibility =
    (for {
      labels <- RoomLabels
        .of(
          Vector(place("slack:T/C1") -> internal, place("slack:T/C2") -> Label.Public),
          Labelled.Mapped(Label.Public)
        )
        .left
        .map(_.written)
      compartments <- Compartments.of(Vector.empty).left.map(_.toString)
      v <- Visibility.of(compartments, labels, Vector.empty, Vector.empty).left.map(_.toString)
    } yield v).fold(e => throw new java.lang.AssertionError(e), identity)

  /** A transaction whose floor is `floor`, under [[channels]]. */
  private def floored(floor: Label): Tx = TestTx.fake(Clearance.of(floor), channels)

  private def set(entries: ToolSet.Entry*): ToolSet =
    ToolSet.of(entries.toVector).getOrElse(throw new java.lang.AssertionError("a duplicate"))

  val tests = Tests {
    test("a set's stored form, and its id, the hash of that form") {
      // Pinned: the id is recorded in every turn's history, so its form cannot drift. The
      // hash was computed outside grit: printf '%s' '<the form>' | sha256sum | cut -c1-16.
      val form =
        """{"tools":[{"name":"peek","does":"Peeks.","parameters":{"type":"object"},"asks":false,"retry":"rerun"}]}"""
      ujson.write(ToolSet.write(set(peek))) ==> form
      ToolSetId.value(set(peek).id) ==> "bc2b04f8a749b4ea"
    }

    test("a set reads back from its stored form, and a set differing in retry has another id") {
      val asking = peek.copy(asks = true, retry = Retry.Interrupt)
      ToolSet.read(ToolSet.write(set(peek, asking.copy(name = ToolName("poke"))))) ==>
        Right(set(peek, asking.copy(name = ToolName("poke"))))
      assert(set(peek).id != set(peek.copy(retry = Retry.Interrupt)).id)
    }

    test("a writing entry stores its names and their places, and reads back from them") {
      // Pinned: a recorded turn's set is read back from this form when it replays.
      ToolSet.write(set(post))("tools")(0).obj.get("writes").map(ujson.write(_)) ==> Some(
        """{"describe":"The channel.","to":[{"name":"general","place":"slack:T/C1"},""" +
          """{"name":"#general","place":"slack:T/C1"},{"name":"random","place":"slack:T/C2"}]}"""
      )
      ToolSet.read(ToolSet.write(set(peek, post))) ==> Right(set(peek, post))
    }

    test("a stored entry whose parameters declare the destination's argument is refused") {
      val declaring = peek.copy(parameters =
        ujson.Obj("type" -> "object", "properties" -> ujson.Obj(Writes.Field -> ujson.Obj()))
      )
      ToolSet.read(ToolSet.write(set(declaring))) ==>
        Left("the tool peek's parameters declare to, which only its writes may name")
    }

    test("a set naming a tool twice is refused, from entries and from its stored form") {
      ToolSet.of(Vector(peek, peek)) ==> Left(DuplicateName(ToolName("peek")))
      val twice = ujson.Obj(
        "tools" -> ujson.Arr(
          ToolSet.write(set(peek))("tools")(0),
          ToolSet.write(set(peek))("tools")(0)
        )
      )
      ToolSet.read(twice) ==> Left("a tool set names peek twice")
    }

    test("a writing entry is offerable less the places its transaction may not write to, or not") {
      val narrowed =
        post
          .offerable(sends = true)(using floored(internal))
          .flatMap(_.writes)
          .map(_.to.keys.toVector)
      narrowed ==> Some(Vector("general", "#general"))
      post.offerable(sends = true)(using floored(Label.at(Level.Confidential))) ==> None
    }

    test("an entry declaring no destination is offerable only where its arguments may be sent") {
      given Tx = floored(Label.Public)
      (peek.offerable(sends = true), peek.offerable(sends = false)) ==> (Some(peek), None)
    }
  }
}
