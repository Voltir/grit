package grit.core.visibility

import utest.*
import TestLabels.{compartment, place}

object ClearanceTests extends TestSuite {

  private val trialLabel = Label.at(Level.Internal, compartment("trial"))
  private val room = place("slack:acme/#trial")
  private val other = place("slack:acme/#elsewhere")
  // Declared under the room (a thread, a nested directory): a room of its own, not the room.
  private val under = place("slack:acme/#trial/1712.3")

  /* An asker cleared for internal alone, in a room labelled internal in `trial`. */
  private val uncleared = Clearance.inRoom(room, trialLabel, Label.at(Level.Internal))

  val tests = Tests {
    test("an uncleared asker reads what was recorded in its own room up to the room's label") {
      assert(
        uncleared.reads(Item.InRoom(room), trialLabel),
        !uncleared.reads(Item.InRoom(room), Label.at(Level.Confidential, compartment("trial")))
      )
    }

    test("an uncleared asker reads its room's label nowhere else: another room, or one under it") {
      assert(
        !uncleared.reads(Item.InRoom(other), trialLabel),
        !uncleared.reads(Item.InRoom(under), trialLabel)
      )
    }

    test("an uncleared asker reads a document kept in its room, and none kept elsewhere") {
      assert(
        uncleared.reads(Item.Kept(Some(room)), trialLabel),
        !uncleared.reads(Item.Kept(Some(other)), trialLabel),
        !uncleared.reads(Item.Kept(Some(under)), trialLabel),
        !uncleared.reads(Item.Kept(None), trialLabel)
      )
    }

    test(
      "beyond its room, a reader reads up to the room's label met with the asker's, and no higher"
    ) {
      assert(
        uncleared.reads(Item.InRoom(other), Label.at(Level.Internal)),
        uncleared.reads(Item.Kept(None), Label.at(Level.Internal)),
        !uncleared.reads(Item.Kept(None), Label.at(Level.Confidential))
      )
      uncleared.everywhere ==> Label.at(Level.Internal)
      val cleared =
        Clearance.inRoom(room, trialLabel, Label.at(Level.Restricted, compartment("trial")))
      assert(
        cleared.reads(Item.InRoom(other), trialLabel),
        cleared.reads(Item.Kept(None), trialLabel)
      )
    }

    test("a clearance of one label reads every item it dominates, wherever, and no room's more") {
      val c = Clearance.of(trialLabel)
      assert(
        c.reads(Item.InRoom(room), trialLabel),
        c.reads(Item.Kept(None), trialLabel),
        !c.reads(Item.InRoom(room), Label.at(Level.Confidential, compartment("trial"))),
        !c.reads(Item.Kept(Some(room)), Label.at(Level.Internal, compartment("ops")))
      )
    }

    test(
      "an item in or kept in a direct message's room is read only by a reader whose own room it is, up to its own label, whatever it reads everywhere"
    ) {
      val dm = place("direct:slack/T/U")
      val another = place("direct:slack/T/V")
      val top = Label.at(Level.Restricted, compartment("trial"))
      val inChannel = Clearance.inRoom(room, top, top)
      val inDm = Clearance.inRoom(dm, trialLabel, top)
      val inAnother = Clearance.inRoom(another, top, top)
      Vector(inChannel, Clearance.of(top), inDm, inAnother).map(c =>
        (c.reads(Item.InRoom(dm), trialLabel), c.reads(Item.Kept(Some(dm)), trialLabel))
      ) ==> Vector((false, false), (false, false), (true, true), (false, false))
      inDm.reads(Item.InRoom(dm), top) ==> false
    }

    test("maintenance reads a direct message's items, as every other row") {
      Maintenance
        .clearance(Label.at(Level.Restricted))
        .reads(Item.InRoom(place("direct:slack/T/U")), Label.at(Level.Internal)) ==> true
    }

    test("a reader in a room writes at the room's label; one in none at its label") {
      uncleared.floor ==> trialLabel
      Clearance.of(Label.at(Level.Internal)).floor ==> Label.at(Level.Internal)
      Maintenance.clearance(trialLabel).floor ==> trialLabel
    }
  }
}
