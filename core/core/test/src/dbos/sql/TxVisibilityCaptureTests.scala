package grit.dbos.sql

import grit.core.durable.Probes

import utest.*

/** A transaction carries its deployment's [[grit.core.visibility.Visibility]] without adding
  * to its capture set, which holds only its connection: a visibility built from a labeller
  * that holds a capability, such as another connection, does not compile. Probed in this package
  * because `Tx.open` is `private[grit]`, by compiling sources against core with core's own
  * flags, since `assertCompileError` never reports a capture-checking error
  * (docs/capture-checking.md).
  */
object TxVisibilityCaptureTests extends TestSuite {

  private def probe(decides: String): List[String] =
    Probes.errors(
      s"""package grit.dbos.sql
         |import grit.core.place.Place
         |import grit.core.store.Tx
         |import grit.core.visibility.*
         |object Probe {
         |  def f(c: java.sql.Connection^, other: java.sql.Connection^): Unit = {
         |    val rooms = new Labeller[Room] {
         |      def label(item: Room): Labelled = Labelled.Mapped($decides)
         |      def requires: Vector[Compartment] = Vector.empty
         |    }
         |    val opened = Visibility
         |      .of(Compartments.Shipped, rooms, Vector.empty, Vector.empty)
         |      .map(v => Tx.open(c, Clearance.of(Label.Public), v, Recorded.Empty))
         |    val _ = opened
         |  }
         |}
         |""".stripMargin
    )

  val tests = Tests {
    test("control: a transaction opened with a visibility whose labeller decides by data") {
      probe("if (item.place.segments.isEmpty) Label.Public else Label.Public") ==> Nil
    }

    test("a visibility whose labeller holds a connection is rejected") {
      val errs = probe("{ other.isClosed(); Label.Public }")
      // Refused at the labeller's own self type; any layer refusing `other` holds the claim.
      assert(
        errs.exists(e =>
          e.contains("Reference `other` is not included in the allowed capture set {}") ||
            e.contains("capability `other` cannot flow into capture set {}")
        )
      )
    }
  }
}
