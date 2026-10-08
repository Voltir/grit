package grit.tools

import grit.core.context.SectionTag
import grit.core.id.{TestCallSlots, ToolCallId}
import grit.core.message.AssistantBlock
import grit.core.persona.Persona
import grit.core.tool.{Bound, Outcome, Repairs, Tool, Toolbox}

import utest.*

/** [[About]]: the docs it ships, and which of them a call is answered with. */
object AboutTests extends TestSuite {

  private def loaded(persona: Persona): Tool[Option[About.Subject]] =
    About.load(persona).fold(e => throw new java.lang.AssertionError(e), identity)

  private lazy val about: Tool[Option[About.Subject]] = loaded(Persona.Grit)

  private val pip: Persona =
    Persona.of("Pip").fold(e => throw new java.lang.AssertionError(e), identity)

  /** What a call to `about`, loaded for `persona`, with `args` is answered with. */
  private def asked(args: ujson.Obj, persona: Persona = Persona.Grit): Outcome =
    Toolbox
      .of(loaded(persona))
      .fold(d => throw new java.lang.AssertionError(s"duplicate $d"), identity)
      .bind(AssistantBlock.ToolCall(ToolCallId("c1"), "about", args), Repairs.All) match {
      case Right(b: Bound.Free) => b(TestCallSlots.First)
      case other => throw new java.lang.AssertionError(s"not free: $other")
    }

  private def shipped(key: String): String =
    scala.io.Source.fromResource(s"about/$key.md")(using scala.io.Codec.UTF8).mkString.trim

  val tests = Tests {
    test(
      "about with no topic is who the assistant is, then the overview; a topic is its doc alone"
    ) {
      asked(ujson.Obj()) ==> Outcome.Done("You are called grit.\n\n" + shipped("grit"))
      asked(ujson.Obj("topic" -> "markers")) ==> Outcome.Done(shipped("markers"))
      asked(ujson.Obj("topic" -> "places"), pip) ==> Outcome.Done(shipped("places"))
    }

    test(
      "a persona other than grit is told it is a persona running on grit, which the docs describe"
    ) {
      asked(ujson.Obj(), pip) ==> Outcome.Done(
        "You are called Pip, a persona of grit: grit is the harness you run on, described " +
          "below.\n\n" + shipped("grit")
      )
    }

    test("about says it answers who the assistant is and what grit is") {
      // Pinned: the model reads only this to know it can ask who it is.
      about.entry.does ==>
        "Who you are, and what grit, the harness you run on, is and how it works: the name you " +
        "are called by, grit's memory (no transcript), the [record], [afar] and [gap] labels, " +
        "periods and how they close, places and edges, and who may see what. Call it when the " +
        "person asks who you are or about grit. `topic` picks one part of grit; without it, who " +
        "you are and the overview."
    }

    test(
      "the security topic names Bell–LaPadula, reads it in plain words, and says where grit departs from it"
    ) {
      val security = asked(ujson.Obj("topic" -> "security"))
      // grit's model-facing wording, kept verbatim on purpose: these are the claims the model
      // must be able to quote when asked how grit decides who may see what.
      (
        security == Outcome.Done(shipped("security")),
        Vector(
          "grit follows Bell–LaPadula: no read up, no write down.",
          "No read up: a person sees only what they are cleared for.",
          "No write down: what is read in a room is never written somewhere less protected",
          "Compartments are Bell–LaPadula's categories.",
          "a room's members read everything said in it up to the room's label, whatever their " +
            "own clearance",
          "Writes out of grit."
        ).filterNot(shipped("security").replaceAll("\\s+", " ").contains)
      ) ==> (true, Vector())
    }

    test("the markers doc names every label grit writes") {
      SectionTag.values.toVector.filterNot(l => shipped("markers").contains(l.tag)) ==> Vector.empty
    }

    test("about is free, and its topic is one of the subjects") {
      val box =
        Toolbox.of(about).fold(d => throw new java.lang.AssertionError(s"duplicate $d"), identity)
      box.schemas(strict = false).map(_.name) ==> Vector("about")
      box
        .schemas(strict = false)
        .flatMap(_.parameters.obj.get("properties"))
        .map(
          _.obj.get("topic").flatMap(_.obj.get("enum"))
        ) ==> Vector(
        Some(ujson.Arr("grit", "memory", "markers", "periods", "places", "security"))
      )
      about.entry.asks ==> false
    }
  }
}
