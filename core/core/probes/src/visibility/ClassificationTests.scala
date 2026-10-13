package grit.core.visibility

import grit.core.durable.Probes

import utest.*

/** A classification outside core is one a transaction read, never a label its holder picked:
  * typer errors, compiled in a probe package of its own against core.
  */
object ClassificationTests extends TestSuite {

  private val prelude =
    """package outside
      |import grit.core.store.Tx
      |import grit.core.visibility.*
      |""".stripMargin

  private val read =
    """def read(s: grit.core.place.Service)(using tx: Tx^): (Label, Boolean) = {
      |    val c: Classification = Tx.classification
      |    (c.label, Clearances.Empty.clears(Party.Service(s), c))
      |  }""".stripMargin
  private val ascribed = "val c: Classification = Label.Public"
  private val applied = "val c = Classification(Label.Public)"

  private lazy val compiled = Probes.batch(prelude, List(read, ascribed, applied))

  private def probe(body: String): List[String] = compiled.errors(body)

  val tests = Tests {
    test("two compiles answer every probe: the typer's refusals, then the control") {
      compiled.runs ==> 2
    }

    test("control: a transaction's classification is read, and checked against a party") {
      probe(read) ==> Nil
    }

    test("a label is not a classification outside core") {
      val errs = probe(ascribed)
      assert(
        errs.size == 1,
        errs.forall(e =>
          e.contains(
            "Found:    (grit.core.visibility.Label.Public : grit.core.visibility.Label)"
          ) &&
            e.contains("Required: grit.core.visibility.Classification")
        )
      )
    }

    test("a classification is not made from a label outside core") {
      probe(applied) ==> List(
        "object Classification in package grit.core.visibility does not take parameters"
      )
    }
  }
}
