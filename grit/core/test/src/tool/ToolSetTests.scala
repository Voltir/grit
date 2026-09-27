package grit.core.tool

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
  }
}
