package grit.core.tool

import grit.core.place.Place
import grit.core.store.Tx
import grit.core.visibility.*
import grit.dbos.sql.TestTx

import utest.*
import TestLabels.place

/** [[Outcome.readable]]: a service's result as one transaction may read it. */
object OutcomeReadableTests extends TestSuite {

  private val internal = Label.at(Level.Internal)
  private val confidential = Label.at(Level.Confidential)

  private val room = place("slack:T/C-staff")
  private val open = place("service:files/staff")
  private val board = place("service:files/board")

  private val labels: Map[Place, Label] =
    Map(room -> internal, open -> internal, board -> confidential)

  private val visibility: Visibility =
    (for {
      compartments <- Compartments.of(Vector.empty).left.map(_.toString)
      v <- Visibility
        .of(
          compartments,
          new Labeller[Room] {
            def label(item: Room): Labelled =
              labels.get(item.place).fold(Labelled.Unmapped(Label.Public))(Labelled.Mapped(_))
            def requires: Vector[Compartment] = Vector.empty
          },
          Vector.empty,
          Vector.empty
        )
        .left
        .map(_.toString)
    } yield v).fold(e => throw new java.lang.AssertionError(e), identity)

  /* A reader in the staff room, labelled internal, for an asker cleared for internal alone. */
  private given Tx = TestTx.fake(Clearance.inRoom(room, internal, internal), visibility)

  private val staff = Provenance(open, Integrity.Member)
  private val minutes = Provenance(board, Integrity.Member)

  val tests = Tests {
    test(
      "a part at a place labelled above the reader's clearance is left out, leaving no trace; one it reads follows the text, its provenance among the items, its label in what was read"
    ) {
      Outcome
        .Done(
          "Found:",
          None,
          Accounted.of(Vector.empty),
          Some(
            Vector(
              ResultPart(minutes, Readers.AtPlace, "board-minutes.md"),
              ResultPart(staff, Readers.AtPlace, "rota.md")
            )
          )
        )
        .readable ==> Outcome.Read(
        Outcome.Done("Found:\nrota.md", None, Accounted.of(Vector(staff)), None),
        internal,
        Outcome.Stated.None
      )
    }

    test(
      "a cut result none of whose parts is read says NoneShown, as one with no parts does; one not cut reads as it is"
    ) {
      val hidden = Some(Vector(ResultPart(minutes, Readers.AtPlace, "board-minutes.md")))
      val whole = Outcome.Done("rota.md", None, Accounted.of(Vector(staff)))
      Vector(
        Outcome.Done("Found:", parts = hidden).readable,
        Outcome.Done("Found:", parts = Some(Vector.empty)).readable,
        whole.readable
      ) ==> Vector(
        Outcome.Read(Outcome.Done("Found:\nNothing was found."), Label.Public, Outcome.Stated.None),
        Outcome.Read(Outcome.Done("Found:\nNothing was found."), Label.Public, Outcome.Stated.None),
        Outcome.Read(whole, Label.Public, Outcome.Stated.None)
      )
    }

    test(
      "a part's facts are kept with it and withheld with it; the answer's own facts reach every reader"
    ) {
      val head = Facts.of(ujson.Obj("complete" -> true))
      def facts(name: String) = Facts.of(ujson.Obj("name" -> name))
      val cut = Outcome
        .Done(
          "Found:",
          None,
          Accounted.of(Vector.empty),
          Some(
            Vector(
              ResultPart(minutes, Readers.AtPlace, "board-minutes.md", Some(facts("minutes"))),
              ResultPart(staff, Readers.AtPlace, "rota.md", Some(facts("rota"))),
              ResultPart(staff, Readers.AtPlace, "notes.md"),
              ResultPart(staff, Readers.AtPlace, "plan.md", Some(facts("plan")))
            )
          ),
          Some(head)
        )
      val whole = Outcome.Done("rota.md", facts = Some(head))
      (cut.readable, whole.readable) ==> (
        Outcome.Read(
          Outcome.Done(
            "Found:\nrota.md\nnotes.md\nplan.md",
            None,
            Accounted.of(Vector(staff, staff, staff)),
            None
          ),
          internal,
          Outcome.Stated(Some(head), Vector(facts("rota"), facts("plan")))
        ),
        Outcome.Read(
          Outcome.Done("rota.md"),
          Label.Public,
          Outcome.Stated(Some(head), Vector.empty)
        )
      )
    }
  }
}
