package grit.core.visibility

import grit.core.durable.Probes

import utest.*

/** A labeller decides by declared data and the item alone: one that holds a capability does
  * not compile, since an engine holds its deployment's labeller for as long as it runs.
  */
object LabellerCaptureTests extends TestSuite {

  private val prelude =
    """package outside
      |import grit.core.visibility.*
      |import grit.core.place.Place
      |trait Provider extends caps.SharedCapability { def call(): String }
      |""".stripMargin

  /* Its type left to inference, so only the labeller's own self type can refuse `p`. */
  private def labeller(decides: String): String =
    s"""def make(p: Provider^): Unit = {
       |    val made = new Labeller[Room] {
       |      def label(item: Room): Labelled = Labelled.Mapped($decides)
       |      def requires: Vector[Compartment] = Vector.empty
       |    }
       |    val _ = made.label(Room(Place.Everywhere, None))
       |  }""".stripMargin

  private val byItem = labeller("if (item.place.segments.isEmpty) Label.Public else Label.Public")
  private val calling = labeller("{ p.call(); Label.Public }")
  private lazy val compiled = Probes.batch(prelude, List(byItem, calling))

  val tests = Tests {
    test("one compile answers every probe") {
      assert(compiled.runs == 1)
    }

    test("control: a labeller deciding by the item alone compiles") {
      compiled.errors(byItem) ==> Nil
    }

    test("a labeller that calls a capability it holds is rejected") {
      val errs = compiled.errors(calling)
      assert(
        errs.exists(e =>
          e.contains("Reference `p` is not included in the allowed capture set {}") &&
            e.contains("self type") && e.contains("Labeller")
        )
      )
    }
  }
}
