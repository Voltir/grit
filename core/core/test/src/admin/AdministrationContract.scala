package grit.core.admin

import java.time.Instant

import grit.core.identity.{Account, Evidence, Held, Principal, Realm, TestAccounts}
import grit.core.place.Place
import grit.core.store.{Origin, Tx}
import grit.core.visibility.TestLabels.{compartment, group, place}
import grit.core.visibility.{
  Compartment,
  Compartments,
  Grant,
  Group,
  Label,
  Labelled,
  Level,
  RoomAccess,
  RoomLabels,
  Steward,
  Visibility
}
import grit.dbos.sql.TestTx

import utest.*

/** The contract every [[Administration]] keeps, run against the in-memory fake in core and the
  * SQL administration in grit.dbos: what each command answers, what it keeps, and who may make
  * which change.
  */
abstract class AdministrationContract extends TestSuite {
  import AdministrationContract.*

  /** An administration under [[Declared]], over a store that has seen no account and recorded
    * nothing of any room.
    */
  protected def fresh(): Administration

  /** Has `account` vouched a full member by [[T1]], as its realm's attestation would. */
  protected def member(a: Administration, account: Account): Unit

  /** Has the store `a` reads see `account`, as a first message through it would, no realm
    * vouching it a member.
    */
  protected def seen(a: Administration, account: Account): Unit

  /** Whether the store `a` reads has seen `account`. */
  protected def known(a: Administration, account: Account): Boolean

  /** Has `room`'s access reported as `access`, as its edge would. */
  protected def reported(a: Administration, room: Place, access: RoomAccess): Unit

  /** Every audit row kept, oldest first. */
  protected def kept(a: Administration): Vector[Kept]

  /** A fresh administration, every one of [[Members]] a full member and [[gus]] seen, with
    * [[privateRoom]] reported invited and [[publicRoom]] open.
    */
  private def arranged(): Administration = {
    val a = fresh()
    Members.foreach(member(a, _))
    seen(a, gus)
    reported(a, privateRoom, RoomAccess.Invited)
    reported(a, publicRoom, RoomAccess.Open)
    a
  }

  private def run(a: Administration, by: Account, room: Place, command: Command, at: Int = 0) =
    a.run(by, room, command, t(at)).fold(e => throw new java.lang.AssertionError(s"$e"), identity)

  val tests = Tests {
    test(
      "a room's label is shown with where it comes from: declared, an access's default, set through grit; a direct message's as its person's clearance"
    ) {
      val a = arranged()
      (
        run(a, mia, declaredRoom, Command.ShowLabel),
        run(a, mia, privateRoom, Command.ShowLabel),
        run(a, mia, publicRoom, Command.ShowLabel),
        run(a, mia, newRoom, Command.ShowLabel),
        run(a, tess, direct, Command.ShowLabel),
        run(a, mia, newRoom, Command.Help)
      ) ==> (
        Answer.label(internal, Answer.Source.Declared, None),
        Answer.label(unmapped, Answer.Source.Default, Some(RoomAccess.Invited)),
        Answer.label(internal, Answer.Source.Default, Some(RoomAccess.Open)),
        Answer.label(Label.Public, Answer.Source.Default, None),
        Answer.direct(internalTrial),
        Answer.Help
      )
      kept(a) ==> Vector()
    }

    test(
      "a first label by a steward of its compartment and a member's raise after are each kept with one audit row, and shown as set; repeating the raise keeps nothing"
    ) {
      val a = arranged()
      val first = new Change.Relabel(privateRoom, unmapped, Change.To.Set(internalTrial))
      val raise = new Change.Relabel(privateRoom, internalTrial, Change.To.Set(confidentialTrial))
      (
        run(a, tess, privateRoom, Command.SetLabel(internalTrial), 1),
        run(a, mia, privateRoom, Command.SetLabel(confidentialTrial), 2),
        run(a, mia, privateRoom, Command.SetLabel(confidentialTrial), 3),
        run(a, mia, privateRoom, Command.ShowLabel)
      ) ==> (
        Answer.relabelled(first),
        Answer.relabelled(raise),
        Answer.relabelled(
          new Change.Relabel(privateRoom, confidentialTrial, Change.To.Set(confidentialTrial))
        ),
        Answer.label(confidentialTrial, Answer.Source.Set, Some(RoomAccess.Invited))
      )
      kept(a) ==> Vector(Kept(tess, t(1), first), Kept(mia, t(2), raise))
    }

    test(
      "a change refused keeps nothing: a guest's, a member's relabel of a public room, a first label by no steward, an undeclared compartment, a direct message's"
    ) {
      val a = arranged()
      val acme = compartment("acme")
      (
        run(a, gus, privateRoom, Command.SetLabel(Label.at(Level.Confidential))),
        run(a, mia, publicRoom, Command.SetLabel(Label.at(Level.Confidential))),
        run(a, mia, newRoom, Command.SetLabel(Label.at(Level.Confidential))),
        run(a, mia, privateRoom, Command.SetLabel(internalTrial)),
        run(a, mia, privateRoom, Command.SetLabel(Label.at(Level.Internal, acme))),
        run(a, mia, direct, Command.Quiet(true)),
        run(a, mia, privateRoom, Command.ShowLabel)
      ) ==> (
        Answer.Refused(Refusal.NotVouched),
        Answer.Refused(Refusal.PublicRoom),
        Answer.Refused(Refusal.PublicRoom),
        Answer.Refused(Refusal.NotSteward(trial)),
        Answer.Refused(Refusal.Undeclared(acme)),
        Answer.Refused(Refusal.InDirectMessage),
        Answer.label(unmapped, Answer.Source.Default, Some(RoomAccess.Invited))
      )
      kept(a) ==> Vector()
    }

    test(
      "only an administrator lowers a level or relabels a public room, and unlabel sets a room back to its default"
    ) {
      val a = arranged()
      val first = new Change.Relabel(privateRoom, unmapped, Change.To.Set(confidentialTrial))
      val lowered = new Change.Relabel(privateRoom, confidentialTrial, Change.To.Set(internalTrial))
      val raised = new Change.Relabel(publicRoom, internal, Change.To.Set(confidential))
      val unset = new Change.Relabel(publicRoom, confidential, Change.To.Default(internal))
      (
        run(a, tess, privateRoom, Command.SetLabel(confidentialTrial), 1),
        run(a, mia, privateRoom, Command.SetLabel(internalTrial), 2),
        run(a, ada, privateRoom, Command.SetLabel(internalTrial), 3),
        run(a, ada, publicRoom, Command.SetLabel(confidential), 4),
        run(a, mia, publicRoom, Command.Unlabel, 5),
        run(a, ada, publicRoom, Command.Unlabel, 6),
        run(a, ada, publicRoom, Command.ShowLabel)
      ) ==> (
        Answer.relabelled(first),
        Answer.Refused(Refusal.NotAdministrator),
        Answer.relabelled(lowered),
        Answer.relabelled(raised),
        Answer.Refused(Refusal.PublicRoom),
        Answer.relabelled(unset),
        Answer.label(internal, Answer.Source.Default, Some(RoomAccess.Open))
      )
      kept(a) ==> Vector(
        Kept(tess, t(1), first),
        Kept(ada, t(3), lowered),
        Kept(ada, t(4), raised),
        Kept(ada, t(6), unset)
      )
    }

    test("quiet and speak are free to any member, and repeating either keeps nothing") {
      val a = arranged()
      val on = new Change.Quiet(publicRoom, true)
      val off = new Change.Quiet(publicRoom, false)
      (
        run(a, mia, publicRoom, Command.Quiet(true), 1),
        run(a, max, publicRoom, Command.Quiet(true), 2),
        run(a, mia, publicRoom, Command.Quiet(false), 3),
        run(a, mia, publicRoom, Command.Quiet(false), 4),
        run(a, mia, newRoom, Command.Quiet(false), 5)
      ) ==> (
        Answer.quieted(on),
        Answer.quieted(on),
        Answer.quieted(off),
        Answer.quieted(off),
        Answer.quieted(new Change.Quiet(newRoom, false))
      )
      kept(a) ==> Vector(Kept(mia, t(1), on), Kept(mia, t(3), off))
    }

    test(
      "a clear shows the person's clearance after it and clears them from then on; a remove shows it after, with any declared group still clearing them; repeats keep nothing"
    ) {
      val a = arranged()
      val clear = new Change.Clear(mia, trial)
      val remove = new Change.Remove(mia, trial)
      val viaLeads = new Change.Clear(max, finance)
      (
        run(a, tess, privateRoom, Command.Clear(mia, trial), 1),
        run(a, ada, privateRoom, Command.Clearance(Some(mia))),
        run(a, tess, privateRoom, Command.Clear(mia, trial), 2),
        run(a, tess, privateRoom, Command.Remove(mia, trial), 3),
        run(a, ada, privateRoom, Command.Remove(tess, trial), 4),
        run(a, fin, privateRoom, Command.Clear(max, finance), 5)
      ) ==> (
        Answer.cleared(clear, internalTrial),
        Answer.theirs(internalTrial),
        Answer.cleared(clear, internalTrial),
        Answer.removed(remove, Label.Public, Vector()),
        Answer.removed(new Change.Remove(tess, trial), internalTrial, Vector(group("trial"))),
        Answer.cleared(viaLeads, confidentialFinance)
      )
      kept(a) ==> Vector(
        Kept(tess, t(1), clear),
        Kept(tess, t(3), remove),
        Kept(fin, t(5), viaLeads)
      )
    }

    test(
      "people added through grit never steward: one cleared for a compartment whose own group stewards it can neither clear another nor give a room its first label"
    ) {
      val a = arranged()
      (
        run(a, tess, privateRoom, Command.Clear(mia, trial), 1),
        run(a, mia, privateRoom, Command.Clear(max, trial), 2),
        run(a, mia, privateRoom, Command.Remove(tess, trial), 3),
        run(a, mia, privateRoom, Command.SetLabel(internalTrial), 4)
      ) ==> (
        Answer.cleared(new Change.Clear(mia, trial), internalTrial),
        Answer.Refused(Refusal.NotSteward(trial)),
        Answer.Refused(Refusal.NotSteward(trial)),
        Answer.Refused(Refusal.NotSteward(trial))
      )
      kept(a) ==> Vector(Kept(tess, t(1), new Change.Clear(mia, trial)))
    }

    test(
      "another's clearance is shown only to an administrator; one's own is shown in full, wherever asked"
    ) {
      val a = arranged()
      val tesss = Answer.clearance(explained(tess, internalTrial))
      (
        run(a, mia, publicRoom, Command.Clearance(Some(tess))),
        run(a, ada, publicRoom, Command.Clearance(Some(tess))),
        run(a, tess, publicRoom, Command.Clearance(Some(tess))),
        run(a, tess, publicRoom, Command.Clearance(None)),
        run(a, tess, direct, Command.Clearance(None))
      ) ==> (Answer.Refused(Refusal.NotForYou), Answer.theirs(internalTrial), tesss, tesss, tesss)
    }

    test(
      "an asker never seen is no one: their clearance is public and explained as no one's, any change is refused, and asking keeps no account"
    ) {
      val a = arranged()
      (
        run(a, una, publicRoom, Command.Clearance(None)),
        run(a, una, publicRoom, Command.Quiet(true)),
        known(a, una),
        kept(a)
      ) ==> (
        Answer.clearance(Tx.explain(None, Label.Public)(using TestTx.inForce(Declared))),
        Answer.Refused(Refusal.NotVouched),
        false,
        Vector()
      )
    }

    test(
      "an administrator asking the clearance of an account never seen is told public, and the account is still unseen"
    ) {
      val a = arranged()
      (run(a, ada, publicRoom, Command.Clearance(Some(una))), known(a, una)) ==>
        (Answer.theirs(Label.Public), false)
    }

    test(
      "compartments shows an administrator every one, a steward also those they steward, anyone else those they are cleared for"
    ) {
      val a = arranged()
      val cs = Declared.compartments
      (
        run(a, ada, publicRoom, Command.Compartments),
        run(a, fin, publicRoom, Command.Compartments),
        run(a, tess, publicRoom, Command.Compartments),
        run(a, mia, publicRoom, Command.Compartments)
      ) ==> (
        Answer.compartments(cs, true, Set.empty, Label.Public),
        Answer.compartments(cs, false, Set(finance), Label.Public),
        Answer.compartments(cs, false, Set(trial), internalTrial),
        Answer.compartments(cs, false, Set.empty, Label.Public)
      )
    }
  }
}

object AdministrationContract {

  /** An audit row: who made `change`, and when. */
  final case class Kept(by: Account, at: Instant, change: Change)

  def t(seconds: Int): Instant = Instant.parse("2026-10-08T09:00:00Z").plusSeconds(seconds)

  /** The realm whose full members may make changes: every account here is of it. */
  val T1: Realm = Realm.of("slack", "T1").fold(e => throw new java.lang.AssertionError(e), identity)

  val trial: Compartment = compartment("trial")
  val finance: Compartment = compartment("finance")

  val internal: Label = Label.at(Level.Internal)
  val confidential: Label = Label.at(Level.Confidential)
  val internalTrial: Label = Label.at(Level.Internal, trial)
  val confidentialTrial: Label = Label.at(Level.Confidential, trial)
  val confidentialFinance: Label = Label.at(Level.Confidential, finance)
  val unmapped: Label = Label.at(Level.Public, Compartment.Unmapped)

  /** An administrator, declared by account. */
  val ada: Account = TestAccounts.account("slack:T1/U-ada")

  /** A declared member of trial's own group, which stewards it. */
  val tess: Account = TestAccounts.account("slack:T1/U-tess")

  /** A declared member of finance-leads, which stewards finance. */
  val fin: Account = TestAccounts.account("slack:T1/U-fin")

  /** Members, in no declared group. */
  val mia: Account = TestAccounts.account("slack:T1/U-mia")
  val max: Account = TestAccounts.account("slack:T1/U-max")

  /** Seen, and no realm vouches a member. */
  val gus: Account = TestAccounts.account("slack:T1/U-gus")

  /** Never seen. */
  val una: Account = TestAccounts.account("slack:T1/U-una")

  val Members: Vector[Account] = Vector(ada, tess, fin, mia, max)

  /** A private room, a public one, one the deployment declares internal (its access not
    * reported), one never heard of, and tess's direct message's.
    */
  val privateRoom: Place = place("slack:T1/C-private")
  val publicRoom: Place = place("slack:T1/C-public")
  val declaredRoom: Place = place("slack:T1/C-declared")
  val newRoom: Place = place("slack:T1/C-new")
  val direct: Place = Origin.Direct(TestAccounts.sourced("slack:T1/U-tess"), "1.0").room

  /** trial and finance declared; [[declaredRoom]] internal, public rooms internal, any other
    * public; administrators `admins` ([[ada]]); trial's own group ([[tess]]) granted
    * internal·trial and its steward; finance's own group (no one) granted confidential·finance,
    * stewarded by `finance-leads` ([[fin]]).
    */
  val Declared: Visibility =
    (for {
      compartments <- Compartments.of(Vector(trial, finance)).left.map(_.toString)
      rooms <- RoomLabels
        .of(
          Vector(declaredRoom -> internal),
          Labelled.Mapped(Label.Public),
          Some(Labelled.Mapped(internal))
        )
        .left
        .map(_.written)
      v <- Visibility
        .of(
          compartments,
          rooms,
          Vector(
            Group(group("admins"), Set(ada)),
            Group(group("trial"), Set(tess)),
            Group(group("finance"), Set.empty),
            Group(group("finance-leads"), Set(fin))
          ),
          Vector(
            Grant(group("trial"), internalTrial),
            Grant(group("finance"), confidentialFinance)
          ),
          administrators = Some(group("admins")),
          stewards =
            Vector(Steward(trial, group("trial")), Steward(finance, group("finance-leads")))
        )
        .left
        .map(_.toString)
    } yield v).fold(e => throw new java.lang.AssertionError(e), identity)

  /** What `account`, a full member by its own account alone, is told of their clearance asked
    * at `cleared`, under [[Declared]] with nothing recorded.
    */
  def explained(account: Account, cleared: Label) =
    Tx.explain(
      Some(
        Principal.Person(
          TestAccounts.principalId(account),
          Set(Held(account, Evidence.Home, member = true))
        )
      ),
      cleared
    )(using TestTx.inForce(Declared))
}
