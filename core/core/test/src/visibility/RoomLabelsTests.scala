package grit.core.visibility

import utest.*
import TestLabels.{compartment, place}

object RoomLabelsTests extends TestSuite {

  private val trial = Label.at(Level.Internal, compartment("trial"))
  private val team = Label.at(Level.Internal)
  private val ops = Label.at(Level.Confidential, compartment("ops"))

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

  val tests = Tests {
    test("a room takes the label declared at the longest declared place it is within") {
      rooms.label(place("slack:acme/#trial/1712.3")) ==> Labelled.Mapped(trial)
      rooms.label(place("slack:acme/#trial/ops/9")) ==> Labelled.Mapped(ops)
      rooms.label(place("slack:acme/#general")) ==> Labelled.Mapped(team)
    }

    test("a room within no declared place takes otherwise, kept at unmapped") {
      rooms.label(place("slack:other/#x")) ==> Labelled.Unmapped(Label.Public)
      rooms.label(place("slack:other/#x")).label ==>
        Label.at(Level.Public, Compartment.Unmapped)
      RoomLabels.Public.label(place("fs:/home/nick")) ==> Labelled.Mapped(Label.Public)
    }

    test("a room labeller requires every compartment its labels hold") {
      rooms.requires.toSet ==>
        Set(compartment("trial"), compartment("ops"), Compartment.Unmapped)
    }

    test("a place declared twice is refused") {
      RoomLabels.of(
        Vector(place("slack:acme") -> team, place("slack:acme") -> trial),
        Labelled.Mapped(Label.Public)
      ) ==> Left(place("slack:acme"))
    }
  }
}
