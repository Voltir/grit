package grit.eval.harness.capture

import grit.core.document.Pointer
import grit.core.tool.ToolName

import utest.*

/** A call classified by the pointers current at capture and those its window held. */
object ReadingTests extends TestSuite {

  private val read = ToolName("github__get_file_contents")

  private def at(path: String): Pointer =
    Pointer.of(read, "path" -> ujson.Str(path)).fold(sys.error, identity)

  private def call(path: String) = ujson.Obj("path" -> path, "ref" -> "main")

  private def support(x: Double): Option[Support] = Support.read(x)

  val tests = Tests {
    test(
      "a call following a current pointer is indexed, and followed when its window held one too"
    ) {
      val current = Vector(at("README.md"), at("docs/a.md"))
      def reading(held: Vector[Pointer]) =
        Reading.of(ToolName.value(read), call("docs/a.md"), current, held, support(0.5))
      Vector(Vector.empty, Vector(at("README.md")), Vector(at("docs/a.md"))).map(reading) ==>
        Vector(
          Reading.Indexed(followed = false, support(0.5)),
          Reading.Indexed(followed = false, support(0.5)),
          Reading.Indexed(followed = true, support(0.5))
        )
    }

    test(
      "a call following only a held document no longer current is other, as is one following none or naming no tool"
    ) {
      val held = Vector(at("docs/a.md"))
      Vector(
        Reading.of(ToolName.value(read), call("docs/a.md"), Vector(at("README.md")), held, None),
        Reading.of(ToolName.value(read), call("docs/b.md"), held, held, None),
        Reading.of("not a tool name!", call("docs/a.md"), held, held, None)
      ) ==> Vector.fill(3)(Reading.Other)
    }

    test(
      "an indexed read is useful at the support line and over it, not under it nor without one"
    ) {
      Vector(
        Reading.Indexed(followed = false, support(0.3)),
        Reading.Indexed(followed = true, support(0.9)),
        Reading.Indexed(followed = true, support(0.29)),
        Reading.Indexed(followed = true, None),
        Reading.Other
      ).map(_.useful) ==> Vector(true, true, false, false, false)
    }
  }
}
