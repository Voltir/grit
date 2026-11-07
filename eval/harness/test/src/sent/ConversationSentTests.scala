package grit.eval.harness.sent

import grit.core.id.{EntryId, EntrySeq}
import grit.core.message.Tokens
import grit.core.store.Payload

import utest.*

/** A conversation's turns as sent ([[ConversationSent]]): which windows were drawn at a boundary. */
object ConversationSentTests extends TestSuite {

  val tests = Tests {
    test(
      "a window carrying nothing after it, or recorded before windows were carried, was drawn at a boundary"
    ) {
      def window(carry: Option[Option[EntryId]]) =
        Payload.Window(
          Vector(EntrySeq(1)),
          Vector.empty,
          carry = carry.map(after => Payload.Carry(after, Tokens(0), None, None))
        )
      (
        ConversationSent.boundary(window(Some(None))),
        ConversationSent.boundary(window(None)),
        ConversationSent.boundary(window(Some(Some(EntryId("w")))))
      ) ==> (true, true, false)
    }
  }
}
