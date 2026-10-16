package grit.core.visibility

import grit.core.durable.Probes

import utest.*

/** Confidentiality and integrity are one lattice at two scales, and never mixed: an integrity
  * where a label is wanted, or a label where an integrity is, does not compile.
  */
object AxesTests extends TestSuite {

  private val prelude =
    """package outside
      |import grit.core.visibility.*
      |""".stripMargin

  private val within =
    """val ok = (
      |  Label.Public.dominates(Label.at(Level.Internal)),
      |  Label.Public.join(Label.Public).meet(Label.Public),
      |  Integrity.Member.dominates(Integrity.Outside),
      |  Integrity.Member.meet(Integrity.Authority).join(Integrity.Untrusted)
      |)""".stripMargin
  private val integrityAsLabel = "val l: Label = Integrity.Member"
  private val labelAsIntegrity = "val i: Integrity = Label.Public"
  private val dominatesAcross = "val b = Label.Public.dominates(Integrity.Member)"
  private val meetAcross = "val m = Integrity.Member.meet(Label.Public)"
  private val equalAcross = "val e = Label.Public == Integrity.Untrusted"

  private lazy val compiled = Probes.batch(
    prelude,
    List(within, integrityAsLabel, labelAsIntegrity, dominatesAcross, meetAcross, equalAcross)
  )

  private def probe(body: String): List[String] = compiled.errors(body)

  private val IntegrityForLabel =
    """Found:    (grit.core.visibility.Integrity.Member : grit.core.visibility.Integrity)
      |Required: grit.core.visibility.Label""".stripMargin
  private val LabelForIntegrity =
    """Found:    (grit.core.visibility.Label.Public : grit.core.visibility.Label)
      |Required: grit.core.visibility.Integrity""".stripMargin

  val tests = Tests {
    test("control: each axis compares, joins and meets within itself") {
      probe(within) ==> Nil
    }

    test("an integrity is never a label, nor a label an integrity") {
      (probe(integrityAsLabel), probe(labelAsIntegrity)) ==> (
        List(IntegrityForLabel),
        List(LabelForIntegrity)
      )
    }

    test("an integrity and a label are never compared, joined or met with each other") {
      (probe(dominatesAcross), probe(meetAcross)) ==> (
        List(IntegrityForLabel),
        List(LabelForIntegrity)
      )
    }

    test("an integrity and a label are never tested for equality") {
      probe(equalAcross).count(_.contains("cannot be compared")) ==> 1
    }
  }
}
