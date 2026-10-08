package grit.core.admin

import grit.core.identity.{Evidence, Held, Principal, TestAccounts}
import grit.core.place.Place
import grit.core.visibility.TestLabels.{compartment, place}
import grit.core.visibility.{Compartment, Compartments, Label, Level, RoomAccess}

import utest.*

object AuthorityTests extends TestSuite {

  private val trial = compartment("trial")
  private val acme = compartment("acme")
  private val finance = compartment("finance")
  private val compartments: Compartments =
    Compartments
      .of(Vector(trial, acme, finance))
      .fold(c => throw new java.lang.AssertionError(c), identity)

  /** `trial` and `acme` have groups of their own, granted them; `finance` has none. */
  private val ownGroups: Set[Compartment] = Set(trial, acme)

  private val ops = place("slack:acme/C1")
  private val dm = place("direct:slack/T1/U-ana")
  private val bo = TestAccounts.account("slack:T1/U-bo")

  /** A person whose one account a trusted realm attests a full member, or not. */
  private def person(member: Boolean): Principal = {
    val account = TestAccounts.account("slack:T1/U-ana")
    Principal.Person(
      TestAccounts.principalId(account),
      Set(Held(account, Evidence.Vouched, member))
    )
  }
  private val member = person(member = true)
  private val guest = person(member = false)

  /** Who decides, and with what standing: anyone, an administrator, or a steward of some. */
  private enum By {
    case Member
    case Administrator
    case Steward(of: Compartment*)
  }

  private def decide(
      change: Change,
      by: By,
      access: Option[RoomAccess] = Some(RoomAccess.Invited)
  ): Either[Refusal, Change] = {
    val (administers, stewards) = by match {
      case By.Member => (false, Set.empty[Compartment])
      case By.Administrator => (true, Set.empty[Compartment])
      case By.Steward(of*) => (false, of.toSet)
    }
    Authority
      .decide(change, member, access, administers, stewards, compartments, ownGroups)
      .map(_.change)
  }

  private def relabel(from: Label, to: Label, room: Place = ops): Change =
    Change.Relabel(room, from, Change.To.Set(to))

  private val first = Label.at(Level.Public, Compartment.Unmapped)
  private def at(level: Level, cs: Compartment*): Label = Label.at(level, cs*)

  val tests = Tests {
    test("in a private room, a full member raises a label freely; no one else may") {
      val raise = relabel(at(Level.Internal, trial), at(Level.Confidential, trial, acme))
      (
        decide(raise, By.Member),
        Authority
          .decide(raise, guest, Some(RoomAccess.Invited), false, Set.empty, compartments, ownGroups)
          .map(_.change),
        Authority
          .decide(
            raise,
            Principal.Grit,
            Some(RoomAccess.Invited),
            true,
            Set.empty,
            compartments,
            ownGroups
          )
          .map(_.change)
      ) ==> (Right(raise), Left(Refusal.NotVouched), Left(Refusal.NotVouched))
    }

    test("in a public room, or one whose access is unreported, only an administrator relabels") {
      val raise = relabel(at(Level.Internal), at(Level.Confidential, trial))
      val unlabel =
        Change.Relabel(ops, at(Level.Confidential, trial), Change.To.Default(at(Level.Internal)))
      Vector(
        decide(raise, By.Member, Some(RoomAccess.Open)),
        decide(raise, By.Steward(trial), None),
        decide(unlabel, By.Steward(trial), Some(RoomAccess.Open)),
        decide(raise, By.Administrator, Some(RoomAccess.Open)),
        decide(unlabel, By.Administrator, None)
      ) ==> Vector(
        Left(Refusal.PublicRoom),
        Left(Refusal.PublicRoom),
        Left(Refusal.PublicRoom),
        Right(raise),
        Right(unlabel)
      )
    }

    test("removing a compartment from a private room needs a steward of each one removed") {
      val dropTrial = relabel(at(Level.Confidential, trial, acme), at(Level.Confidential, acme))
      val dropBoth = relabel(at(Level.Confidential, trial, acme), at(Level.Confidential))
      Vector(
        decide(dropTrial, By.Steward(trial)),
        decide(dropTrial, By.Steward(acme)),
        decide(dropTrial, By.Member),
        decide(dropBoth, By.Steward(trial)),
        decide(dropBoth, By.Steward(trial, acme)),
        decide(dropBoth, By.Administrator)
      ) ==> Vector(
        Right(dropTrial),
        Left(Refusal.NotSteward(trial)),
        Left(Refusal.NotSteward(trial)),
        Left(Refusal.NotSteward(acme)),
        Right(dropBoth),
        Right(dropBoth)
      )
    }

    test("lowering a private room's level needs an administrator, whoever stewards it") {
      val lower = relabel(at(Level.Confidential, trial), at(Level.Internal, trial))
      val unlabel =
        Change.Relabel(ops, at(Level.Confidential, trial), Change.To.Default(first))
      Vector(
        decide(lower, By.Steward(trial)),
        decide(lower, By.Administrator),
        decide(unlabel, By.Steward(trial)),
        decide(unlabel, By.Administrator)
      ) ==> Vector(
        Left(Refusal.NotAdministrator),
        Right(lower),
        Left(Refusal.NotAdministrator),
        Right(unlabel)
      )
    }

    test(
      "a private room's first label needs an administrator, or a steward of every compartment " +
        "it holds; one holding none, an administrator; one keeping unmapped is a raise, free " +
        "to a member"
    ) {
      val both = relabel(first, at(Level.Confidential, trial, acme))
      val none = relabel(first, at(Level.Internal))
      Vector(
        decide(both, By.Steward(trial, acme)),
        decide(both, By.Steward(trial)),
        decide(both, By.Member),
        decide(both, By.Administrator),
        decide(none, By.Steward(trial, acme)),
        decide(none, By.Administrator),
        decide(relabel(first, at(Level.Confidential, Compartment.Unmapped, trial)), By.Member)
      ) ==> Vector(
        Right(both),
        Left(Refusal.NotSteward(acme)),
        Left(Refusal.NotSteward(acme)),
        Right(both),
        Left(Refusal.NotAdministrator),
        Right(none),
        Right(relabel(first, at(Level.Confidential, Compartment.Unmapped, trial)))
      )
    }

    test("clearing or removing a person for a compartment needs its steward or an administrator") {
      val clear = Change.Clear(bo, trial)
      val remove = Change.Remove(bo, trial)
      Vector(
        decide(clear, By.Steward(trial)),
        decide(remove, By.Steward(trial)),
        decide(clear, By.Steward(acme)),
        decide(remove, By.Member),
        decide(clear, By.Administrator),
        decide(remove, By.Administrator)
      ) ==> Vector(
        Right(clear),
        Right(remove),
        Left(Refusal.NotSteward(trial)),
        Left(Refusal.NotSteward(trial)),
        Right(clear),
        Right(remove)
      )
    }

    test(
      "a compartment not declared is refused, and clear for one with no group of its own " +
        "granted it"
    ) {
      val legal = compartment("legal")
      Vector(
        decide(relabel(at(Level.Internal), at(Level.Internal, legal)), By.Administrator),
        decide(Change.Clear(bo, legal), By.Administrator),
        decide(Change.Remove(bo, legal), By.Administrator),
        decide(Change.Clear(bo, finance), By.Administrator),
        decide(Change.Remove(bo, finance), By.Steward(finance))
      ) ==> Vector(
        Left(Refusal.Undeclared(legal)),
        Left(Refusal.Undeclared(legal)),
        Left(Refusal.Undeclared(legal)),
        Left(Refusal.NoOwnGroup(finance)),
        Right(Change.Remove(bo, finance))
      )
    }

    test("quiet is free to a full member, in a public room too; never in a direct message") {
      Vector(
        decide(Change.Quiet(ops, true), By.Member, Some(RoomAccess.Open)),
        decide(Change.Quiet(ops, false), By.Member, None),
        decide(Change.Quiet(dm, true), By.Administrator),
        decide(relabel(Label.Public, at(Level.Internal), room = dm), By.Administrator)
      ) ==> Vector(
        Right(Change.Quiet(ops, true)),
        Right(Change.Quiet(ops, false)),
        Left(Refusal.InDirectMessage),
        Left(Refusal.InDirectMessage)
      )
    }

    test("each refusal reads as one line for its asker") {
      Vector(
        Refusal.NotVouched,
        Refusal.NotAdministrator,
        Refusal.PublicRoom,
        Refusal.NotSteward(trial),
        Refusal.Undeclared(trial),
        Refusal.NoOwnGroup(trial),
        Refusal.InDirectMessage,
        Refusal.NotForYou
      ).map(_.text) ==> Vector(
        "Only a person grit knows as a full member of a source it trusts can change this.",
        "Only an administrator can make this change.",
        "This channel is public: make it private to label it, or ask an administrator.",
        "Only a steward of trial, or an administrator, can make this change.",
        "trial is not a compartment here: compartments lists the ones you can name.",
        "No one is cleared for trial through grit: no group of its own is declared, granted it.",
        "A direct message's label is its person's clearance, and it is never quiet: neither " +
          "can be changed.",
        "Only an administrator can see another person's clearance."
      )
    }
  }
}
