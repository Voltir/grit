package grit.core.store

import grit.core.place.{Place, Service}
import grit.core.visibility.*
import grit.dbos.sql.TestTx

import utest.*
import TestLabels.{compartment, place}

/** What a transaction may write or send outside grit, and read from a source, under the
  * deployment's labels (ADR 0031): no write down, no read up.
  */
object TxTests extends TestSuite {

  private val trial = compartment("trial")
  private val finance = compartment("finance")
  private val ops = compartment("ops")

  private def service(name: String): Service =
    Service.of(name).fold(e => throw new java.lang.AssertionError(e), identity)

  private val github = service("github")
  private val jira = service("jira")
  private val wiki = service("wiki")

  private val confidentialTrial = Label.at(Level.Confidential, trial)
  private val internal = Label.at(Level.Internal)
  private val internalFinance = Label.at(Level.Internal, finance)
  private val unmapped = Label.at(Level.Public, Compartment.Unmapped)

  private val room = place("slack:T/C-trial")
  private val sameLevel = place("slack:T/C-trial-two")
  private val above = place("slack:T/C-board")
  private val public = place("slack:T/C-general")
  private val undeclared = place("slack:T/C-ops")
  private val unplaced = place("slack:T/C-new")

  private val declared: Vector[(Place, Label)] = Vector(
    room -> confidentialTrial,
    sameLevel -> confidentialTrial,
    above -> Label.at(Level.Restricted, trial, finance),
    public -> Label.Public,
    github.place -> internal,
    jira.place -> Label.at(Level.Confidential)
  )

  /* The deployment's rooms: a declared table, but one place returns a compartment the
   * deployment does not declare, and anything else is unplaced. */
  private val rooms: Labeller[Place] = new Labeller[Place] {
    def label(item: Place): Labelled =
      if (item == undeclared) Labelled.Mapped(Label.at(Level.Internal, ops))
      else
        declared
          .collectFirst { case (p, l) if p == item => Labelled.Mapped(l) }
          .getOrElse(Labelled.Unmapped(Label.Public))
    def requires: Vector[Compartment] = Vector(trial, finance)
  }

  private val visibility: Visibility =
    (for {
      compartments <- Compartments.of(Vector(trial, finance)).left.map(_.toString)
      v <- Visibility
        .of(
          compartments,
          rooms,
          Vector.empty,
          Vector.empty,
          Vector(Trust(github, confidentialTrial))
        )
        .left
        .map(_.toString)
    } yield v).fold(e => throw new java.lang.AssertionError(e), identity)

  private def at(clearance: Clearance): Tx = TestTx.fake(clearance, visibility)

  /** In [[room]], for an asker cleared at `asker`. */
  private def inRoom(asker: Label): Tx = at(Clearance.inRoom(room, confidentialTrial, asker))

  val tests = Tests {
    test("in a confidential trial room, a place is written to only when its label is as high") {
      given Tx = inRoom(Label.Public)
      Vector(public, sameLevel, above).map(Tx.writesTo) ==> Vector(false, true, true)
    }

    test("an unplaced destination is never written to, whatever the floor") {
      val floors = Vector(Clearance.of(Label.Public), Clearance.of(unmapped))
      val written =
        (floors.map(at) :+ inRoom(Label.Public)).map(tx => Tx.writesTo(unplaced)(using tx))
      written ==> Vector(false, false, false)
      Tx.writable(unplaced)(using at(Clearance.of(unmapped))) ==> None
    }

    test("a place labelled with a compartment the deployment does not declare is not writable") {
      given Tx = at(Clearance.of(Label.Public))
      (Tx.writable(undeclared), Tx.writable(public)) ==> (None, Some(Label.Public))
    }

    test("a source is read from up to the room's label in its own room, elsewhere up to the meet") {
      given Tx = inRoom(internal)
      Vector(room, github.place, jira.place, unplaced).map(Tx.readsFrom) ==>
        Vector(true, true, false, false)
    }

    test("a service is sent to only when what it is trusted with is as high as the floor") {
      val floors = Vector(confidentialTrial, internal, internalFinance, Label.Public)
      // In a room at each floor, asked by someone cleared for nothing: the floor, not the read.
      val sends = floors.map { floor =>
        val tx = at(Clearance.inRoom(room, floor, Label.Public))
        (Tx.sendsTo(github)(using tx), Tx.sendsTo(wiki)(using tx))
      }
      sends ==> Vector((true, false), (true, false), (false, false), (true, true))
    }
  }
}
