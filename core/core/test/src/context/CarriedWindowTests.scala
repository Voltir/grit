package grit.core.context

import grit.core.id.{EntrySeq, TurnSeq}
import grit.core.message.Tokens

import utest.*

object CarriedWindowTests extends TestSuite {

  private def drawn(seq: Long): Window = Window(Vector(EntrySeq(seq)))

  val tests = Tests {
    test(
      "after a boundary the carried window is its part alone; after an append, the parts before it then it"
    ) {
      val first = CarriedWindow.Empty.after(Projection.Boundary(drawn(1), Tokens(10)), TurnSeq(1))
      val second = first.after(Projection.Appended(drawn(2), Tokens(4)), TurnSeq(2))
      val cleared = second.after(Projection.Boundary(drawn(3), Tokens(7)), TurnSeq(3))
      second.parts.map(p => (p.window, p.cost, p.turn)) ==> Vector(
        (drawn(1), Tokens(10), TurnSeq(1)),
        (drawn(2), Tokens(4), TurnSeq(2))
      )
      cleared.parts.map(p => (p.window, p.cost, p.turn)) ==> Vector(
        (drawn(3), Tokens(7), TurnSeq(3))
      )
    }
  }
}
