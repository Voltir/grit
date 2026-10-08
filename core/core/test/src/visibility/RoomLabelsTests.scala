package grit.core.visibility

import utest.*
import TestLabels.{compartment, place}

object RoomLabelsTests extends TestSuite {

  private val trial = Label.at(Level.Internal, compartment("trial"))
  private val team = Label.at(Level.Internal)
  private val ops = Label.at(Level.Confidential, compartment("ops"))
  private val pub = Label.at(Level.Confidential)

  private def unreported(text: String): Room = Room(place(text), None)

  private val rooms = RoomLabels
    .of(
      Vector(
        place("slack:acme") -> team,
        place("slack:acme/#trial") -> trial,
        place("slack:acme/#trial/ops") -> ops
      ),
      Labelled.Unmapped(Label.Public)
    )
    .fold(p => throw new java.lang.AssertionError(p.written), identity)

  /* As `rooms`, with public rooms at `pub`. */
  private val opening = RoomLabels
    .of(
      Vector(place("slack:acme") -> team, place("slack:acme/#trial") -> trial),
      Labelled.Unmapped(Label.Public),
      Some(Labelled.Mapped(pub))
    )
    .fold(p => throw new java.lang.AssertionError(p.written), identity)

  val tests = Tests {
    test(
      "a room of unreported access takes the label declared at the longest declared place it is within"
    ) {
      rooms.label(unreported("slack:acme/#trial/1712.3")) ==> Labelled.Mapped(trial)
      rooms.label(unreported("slack:acme/#trial/ops/9")) ==> Labelled.Mapped(ops)
      rooms.label(unreported("slack:acme/#general")) ==> Labelled.Mapped(team)
    }

    test("a room of unreported access within no declared place takes otherwise, kept at unmapped") {
      rooms.label(unreported("slack:other/#x")) ==> Labelled.Unmapped(Label.Public)
      rooms.label(unreported("slack:other/#x")).label ==>
        Label.at(Level.Public, Compartment.Unmapped)
      RoomLabels.Public.label(unreported("fs:/home/nick")) ==> Labelled.Mapped(Label.Public)
    }

    test("a label declared at a room's own place wins over its access") {
      (
        opening.label(Room(place("slack:acme/#trial"), Some(RoomAccess.Open))),
        opening.label(Room(place("slack:acme/#trial"), Some(RoomAccess.Invited)))
      ) ==> (Labelled.Mapped(trial), Labelled.Mapped(trial))
    }

    test("an open room not declared at its own place takes open, over any enclosing declaration") {
      (
        opening.label(Room(place("slack:acme/#general"), Some(RoomAccess.Open))),
        opening.label(Room(place("slack:other/#x"), Some(RoomAccess.Open)))
      ) ==> (Labelled.Mapped(pub), Labelled.Mapped(pub))
    }

    test(
      "an invited room not declared at its own place is unmapped, over any enclosing declaration"
    ) {
      opening.label(Room(place("slack:acme/#private"), Some(RoomAccess.Invited))) ==>
        Labelled.Unmapped(Label.Public)
      opening.label(Room(place("slack:acme/#private"), Some(RoomAccess.Invited))).label ==>
        Label.at(Level.Public, Compartment.Unmapped)
    }

    test("open is otherwise when not given") {
      rooms.label(Room(place("slack:other/#x"), Some(RoomAccess.Open))) ==>
        Labelled.Unmapped(Label.Public)
    }

    test("a room labeller requires every compartment its labels hold, open's among them") {
      rooms.requires.toSet ==>
        Set(compartment("trial"), compartment("ops"), Compartment.Unmapped)
      RoomLabels
        .of(Vector.empty, Labelled.Mapped(Label.Public), Some(Labelled.Mapped(ops)))
        .map(_.requires) ==> Right(Vector(compartment("ops")))
    }

    test("a place declared twice is refused") {
      RoomLabels.of(
        Vector(place("slack:acme") -> team, place("slack:acme") -> trial),
        Labelled.Mapped(Label.Public)
      ) ==> Left(place("slack:acme"))
    }
  }
}
