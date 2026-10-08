package grit.core.visibility

import grit.core.identity.{Principal, TestAccounts}

import utest.*

/** [[Explanation.text]]: an explanation in the words its asker reads. */
object ExplanationTextTests extends TestSuite {

  import Explained.{Above, After, Before, Dana, Named, confidentialTrial, explain, names}

  val tests = Tests {
    test(
      "the rendered text of a sealed or raised thread names no compartment or group above it, and no account's or realm's name"
    ) {
      val sealedText = Explanation.text(Explained.sealedThread)
      val raisedText = Explanation.text(Explained.raisedThread)
      val fullText = Explanation.text(Explained.fullClearance)
      (
        names(sealedText, Above*),
        names(raisedText, Above*),
        Vector(sealedText, raisedText, fullText).flatMap(names(_, Named*))
      ) ==> (Vector(), Vector(), Vector())
    }

    test("the rendered text shows each account by kind and how it is the person's") {
      Explanation.text(Explained.fullClearance) ==>
        """This conversation is labelled [level: confidential, in: {finance, trial}].
          |
          |From anywhere else, I read for you up to [level: confidential, in: {finance, trial}].
          |
          |I know you by a slack account, linked to you by an email a trusted source confirmed, a full member of its source; a test account, your own.
          |
          |Your groups, each with what it clears you for:
          |- trial: [level: confidential, in: {trial}], through your slack account and through your test account
          |- leadership: [level: confidential, in: {finance, trial}], through your slack account
          |- staff: [level: internal], as a full member of a slack source
          |
          |How it works: what is said in a room is read there, by its members, up to the room's label. Anything said elsewhere is read here up to this room's label met with your clearance. A label is shown as its level, then the compartments it is in.
          |
          |I never say whether anything is hidden from you.""".stripMargin
    }

    test(
      "a group the person was added to through grit says so, by the kind of the account added"
    ) {
      val told = explain(
        After,
        Some(Dana),
        confidentialTrial,
        Recorded(
          Map.empty,
          Map(TestLabels.group("trial") -> Set(TestAccounts.account("slack:T/U-dana")))
        )
      )
      Explanation.text(told).linesIterator.toVector.filter(_.startsWith("- trial")) ==> Vector(
        "- trial: [level: confidential, in: {trial}], through your slack account and through " +
          "your test account and added through grit for your slack account"
      )
    }

    test("no one, and grit, are told as such") {
      (
        Explanation.text(explain(Before, None, Label.Public)).linesIterator.toVector.lift(4),
        Explanation
          .text(explain(Before, Some(Principal.Grit), Label.Public))
          .linesIterator
          .toVector
          .lift(4)
      ) ==> (
        Some("No one I know asked this, so I tell no one's clearance."),
        Some("This turn is my own, not a person's, so there is no person's clearance to tell.")
      )
    }
  }
}
