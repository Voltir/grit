package grit.core.visibility

import grit.core.durable.Probes

import utest.*

/** A labeller decides by declared data and the item alone: one that holds a capability does
  * not compile, since an engine holds its deployment's labeller for as long as it runs.
  */
object LabellerCaptureTests extends TestSuite {

  private def probe(body: String): List[String] =
    Probes.errors(
      s"""package outside
         |import grit.core.visibility.*
         |import grit.core.place.Place
         |trait Provider extends caps.SharedCapability { def call(): String }
         |object Probe {
         |  $body
         |}
         |""".stripMargin
    )

  /* Its type left to inference, so only the labeller's own self type can refuse `p`. */
  private def labeller(decides: String): String =
    s"""def make(p: Provider^): Unit = {
       |    val made = new Labeller[Place] {
       |      def label(item: Place): Labelled = Labelled.Mapped($decides)
       |      def requires: Vector[Compartment] = Vector.empty
       |    }
       |    val _ = made.label(Place.Everywhere)
       |  }""".stripMargin

  val tests = Tests {
    test("control: a labeller deciding by the item alone compiles") {
      probe(labeller("if (item.segments.isEmpty) Label.Public else Label.Public")) ==> Nil
    }

    test("a labeller that calls a capability it holds is rejected") {
      val errs = probe(labeller("{ p.call(); Label.Public }"))
      assert(
        errs.exists(e =>
          e.contains("Reference `p` is not included in the allowed capture set {}") &&
            e.contains("self type") && e.contains("Labeller")
        )
      )
    }
  }
}
