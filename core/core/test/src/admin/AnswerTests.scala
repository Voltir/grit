package grit.core.admin

import grit.core.identity.TestAccounts
import grit.core.visibility.TestLabels.{compartment, group, place}
import grit.core.visibility.{
  Compartment,
  Compartments,
  Explained,
  Label,
  Level,
  RoomAccess
}

import utest.*

object AnswerTests extends TestSuite {

  private val trial = compartment("trial")
  private val acme = compartment("acme")
  private val finance = compartment("finance")
  private val compartments: Compartments =
    Compartments
      .of(Vector(trial, acme, finance))
      .fold(c => throw new java.lang.AssertionError(c), identity)

  private val ops = place("slack:acme/C1")
  private val bo = TestAccounts.account("slack:T1/U-bo")
  private val internal = Label.at(Level.Internal)
  private val confidential = Label.at(Level.Confidential, trial)

  val tests = Tests {
    test(
      "a label shown says whether it is set, declared or a default, and when access is unknown"
    ) {
      Vector(
        Answer.label(confidential, Answer.Source.Set, Some(RoomAccess.Invited)),
        Answer.label(internal, Answer.Source.Declared, Some(RoomAccess.Open)),
        Answer.label(internal, Answer.Source.Default, Some(RoomAccess.Open)),
        Answer.label(
          Label.at(Level.Public, Compartment.Unmapped),
          Answer.Source.Default,
          Some(RoomAccess.Invited)
        ),
        Answer.label(internal, Answer.Source.Default, None)
      ).map(_.text) ==> Vector(
        "This room is labelled [level: confidential, in: {trial}], set through grit.",
        "This room is labelled [level: internal], as the deployment declares it.",
        "This room is labelled [level: internal], the default for a public room.",
        "This room is labelled [level: public, in: {unmapped}], the default for a private " +
          "room.",
        "This room is labelled [level: internal], the deployment's default. Its access is not " +
          "yet known."
      )
    }

    test(
      "a direct message's label is shown as its person's clearance, another's clearance as its label, and one's own in its explanation's words"
    ) {
      val e = Explained.fullClearance
      (
        Answer.direct(internal),
        Answer.theirs(confidential),
        Answer.clearance(e).text.linesIterator.toVector.headOption
      ) ==> (
        Answer.Shown(
          "This is a direct message: its label is your clearance, [level: internal], and it " +
            "cannot be changed."
        ),
        Answer.Shown("Their clearance is [level: confidential, in: {trial}]."),
        Some(
          "You are cleared for up to [level: confidential, in: {finance, trial}], wherever you ask."
        )
      )
    }

    test("a label changed says what it was, and that conversations begun before keep theirs") {
      val set = new Change.Relabel(ops, internal, Change.To.Set(confidential))
      val unset = new Change.Relabel(ops, confidential, Change.To.Default(internal))
      (Answer.relabelled(set), Answer.relabelled(unset)) ==> (
        Answer.Changed(
          set,
          "This room is now labelled [level: confidential, in: {trial}]; it was " +
            "[level: internal]. Conversations begun before keep theirs."
        ),
        Answer.Changed(
          unset,
          "This room is back to its default label, [level: internal]; it was " +
            "[level: confidential, in: {trial}]. Conversations begun before keep theirs."
        )
      )
    }

    test("quiet says what it stops, and what it does not") {
      Vector(
        Answer.quieted(new Change.Quiet(ops, true)),
        Answer.quieted(new Change.Quiet(ops, false))
      )
        .map(_.text) ==> Vector(
        "This room is quiet: I post nothing here unasked, and still answer when mentioned.",
        "This room is no longer quiet: I may post here unasked."
      )
    }

    test("clear and remove show the person's clearance after") {
      val clear = new Change.Clear(bo, trial)
      val remove = new Change.Remove(bo, trial)
      (
        Answer.cleared(clear, Label.at(Level.Internal, acme, trial)),
        Answer.removed(remove, Label.at(Level.Internal, acme), Vector.empty)
      ) ==> (
        Answer.Changed(
          clear,
          "Cleared for trial. Their clearance is now [level: internal, in: {acme, trial}]."
        ),
        Answer.Changed(
          remove,
          "Removed from trial. Their clearance is now [level: internal, in: {acme}]."
        )
      )
    }

    test("a remove says which declared memberships still clear the person for it") {
      Answer
        .removed(
          new Change.Remove(bo, trial),
          Label.at(Level.Internal, trial),
          Vector(group("trial-leads"), group("t1-members"))
        )
        .text ==>
        "Removed from trial. Their clearance is now [level: internal, in: {trial}]. They are " +
        "still cleared for trial through trial-leads and t1-members, as the deployment " +
        "declares: only a change to the deployment takes that away."
    }

    test(
      "compartments shows an administrator every one, a steward also those they steward, and " +
        "anyone else those they are cleared for"
    ) {
      val cleared = Label.at(Level.Internal, trial, Compartment.Unmapped)
      Vector(
        Answer.compartments(compartments, administers = true, Set.empty, Label.Public),
        Answer.compartments(compartments, administers = false, Set(finance), cleared),
        Answer.compartments(compartments, administers = false, Set.empty, cleared),
        Answer.compartments(compartments, administers = false, Set.empty, internal)
      ).map(_.text) ==> Vector(
        "Every compartment: acme, finance and trial.",
        "You are cleared for trial. You steward finance.",
        "You are cleared for trial.",
        "You are cleared for no compartment."
      )
    }

    test("a refusal reads as its line, and help as the usage") {
      (Answer.Refused(Refusal.PublicRoom).text, Answer.Help.text) ==> (
        "This channel is public: make it private to label it, or ask an administrator.",
        Command.Usage
      )
    }
  }
}
