package grit.tools

import grit.core.place.Place
import grit.core.store.Tx
import grit.core.tool.ToolSet
import grit.core.visibility.TestLabels.place
import grit.core.visibility.{
  Clearance,
  Compartments,
  DeclaredLabel,
  Label,
  Labelled,
  Level,
  RoomLabels,
  Visibility
}
import grit.dbos.sql.TestTx

import utest.*

/** [[Versioning]]'s tool as the engine offers it: where a turn may reach it. */
object VersioningTests extends TestSuite {

  private val forge: Place = place("service:github/acme/app")

  private val internal = Label.at(Level.Internal)

  private def entry: ToolSet.Entry =
    Versioning
      .hosted(forge, "acme/app")
      .fold(why => throw new java.lang.AssertionError(why), _.entry)

  /** A transaction on a public floor, places labelled as `labels` say, any other as
    * `otherwise`.
    */
  private def under(labels: Vector[(Place, DeclaredLabel)], otherwise: Labelled): Tx = {
    val visibility = (for {
      rooms <- RoomLabels.of(labels, otherwise).left.map(_.written)
      compartments <- Compartments.of(Vector.empty).left.map(_.toString)
      v <- Visibility.of(compartments, rooms, Vector.empty, Vector.empty).left.map(_.toString)
    } yield v).fold(e => throw new java.lang.AssertionError(e), identity)
    TestTx.fake(Clearance.of(Label.Public), visibility)
  }

  /** Whether the tool is offered to a turn that read at `read`, under `labels`. */
  private def offered(
      labels: Vector[(Place, DeclaredLabel)],
      read: Label,
      otherwise: Labelled = Labelled.Mapped(Label.Public)
  ): Boolean = {
    given Tx = under(labels, otherwise)
    entry.offerable(sends = true, Tx.locked(read)).isDefined
  }

  val tests = Tests {
    test(
      "offer_pull_request is offered after reading at the forge's label, withheld after reading above it"
    ) {
      (
        offered(Vector(forge -> internal), internal),
        offered(Vector(forge -> Label.Public), internal)
      ) ==> (true, false)
    }
    test("offer_pull_request is withheld from a forge place the deployment leaves unmapped") {
      val unmapped = Labelled.Unmapped(Label.Public)
      (
        offered(Vector.empty, Label.Public, unmapped),
        offered(Vector(forge -> Label.Public), Label.Public, unmapped)
      ) ==> (false, true)
    }
  }
}
