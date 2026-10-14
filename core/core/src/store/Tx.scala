package grit.core.store

import grit.core.identity.{Account, Held, Principal}
import grit.core.model.Grade
import grit.core.place.{Place, Service}
import grit.core.visibility.{
  Classification,
  Clearance,
  Explanation,
  Integrity,
  Item,
  Label,
  Labelled,
  Party,
  Recorded,
  Room,
  RoomAccess,
  Visibility
}

/** Capability to read and write inside one database transaction, opened for a
  * [[grit.core.visibility.Subject]]: what it reads of labelled rows, and the least label it
  * writes at, are its [[Tx.clearance]]. Obtained from an opener; valid only within its
  * callback, which the type system enforces.
  *
  * It carries the labels in force: the deployment's declaration ([[Visibility]]) and what is
  * recorded beside it ([[Recorded]]), read when it opened, on its own connection. A change
  * recorded by another transaction is in force from the next transaction opened after it
  * commits.
  */
opaque type Tx = Tx.Opened

object Tx {

  /* A class, so `Tx^{c}` keeps the connection's capture set; the clearance, the visibility and
   * what is recorded are pure, so they add nothing to it. */
  private[store] final class Opened(
      val connection: java.sql.Connection^,
      val clearance: Clearance,
      val visibility: Visibility,
      val recorded: Recorded
  )

  /** `c`'s transaction, read and written at `clearance`, under the deployment's `visibility`
    * and what `recorded` holds, as read on `c`: the one way to make a `Tx`.
    */
  private[grit] def open(
      c: java.sql.Connection^,
      clearance: Clearance,
      visibility: Visibility,
      recorded: Recorded
  ): Tx^{c} =
    new Opened(c, clearance, visibility, recorded)

  private[grit] def connection(tx: Tx^): java.sql.Connection^{tx} = tx.connection

  /** The clearance it was opened at: what stores filter and floor by. */
  def clearance(tx: Tx^): Clearance = tx.clearance

  /** The deployment's declaration it was opened under. */
  def visibility(tx: Tx^): Visibility = tx.visibility

  /** What it reads of documents and plugins' data: every label this dominates. For choosing
    * which variant of derived data to serve, never for enforcing: a read of a variant above it
    * returns nothing, so a wrong choice cannot read up. Under posting and a job's run it is
    * also the floor what is written is kept at.
    */
  def cleared(tx: Tx^): Label = tx.clearance.everywhere

  /** The least label it writes at: its clearance's floor, its own room's label when it has
    * one. What a row made from its own room's content is kept at.
    */
  def floor(tx: Tx^): Label = tx.clearance.floor

  /** The label conversations in `room` are created at now, in this order: a direct message's
    * room ([[Place.direct]]) [[grit.core.visibility.Compartments.top]], since its person's
    * clearance needs the opener (ADR 0032); the label set for it through grit; otherwise what
    * the deployment's labeller gives it as a [[Room]] with its reported access ([[access]]). A
    * compartment not declared is kept at [[grit.core.visibility.Compartment.Unmapped]] instead.
    * Conversations already created keep theirs.
    */
  def roomLabel(room: Place)(using tx: Tx^): Label =
    if (room.direct) tx.visibility.compartments.top
    else tx.visibility.compartments.admit(labelled(room).label)

  /** The label conversations in `room` would be created at were none set for it through
    * grit: [[roomLabel]] but for its second step. What `unlabel` sets a room back to.
    */
  def defaultLabel(room: Place)(using tx: Tx^): Label =
    if (room.direct) tx.visibility.compartments.top
    else tx.visibility.compartments.admit(declared(room).label)

  /** Whether a label is set for `room` through grit, so [[roomLabel]] is that one. */
  def labelSet(room: Place)(using tx: Tx^): Boolean =
    tx.recorded.rooms.get(room).exists(_.label.isDefined)

  /** `room`'s access as its edge last reported it; `None` when no edge has reported one. */
  def access(room: Place)(using tx: Tx^): Option[RoomAccess] =
    tx.recorded.rooms.get(room).flatMap(_.access)

  /** Whether `room` is quiet: nothing is posted there unasked, so [[writable]] is `None` for
    * it.
    */
  def quiet(room: Place)(using tx: Tx^): Boolean =
    tx.recorded.rooms.get(room).exists(_.quiet)

  /** The least grade `room`'s turns are routed to, set through grit by an administrator; `None`
    * when none is. A floor within what a turn may be routed to
    * ([[grit.core.route.Routing.floored]]), never above it.
    */
  def baseline(room: Place)(using tx: Tx^): Option[Grade] =
    tx.recorded.rooms.get(room).flatMap(_.baseline)

  /** `principal`'s clearance: grit's, [[grit.core.visibility.Compartments.top]]; a person's,
    * the join of the grants of every declared group they are in, by the deployment's
    * declaration or added through grit; [[Label.Public]] in none.
    */
  def clearanceOf(principal: Principal)(using tx: Tx^): Label =
    principal match {
      case Principal.Grit => tx.visibility.compartments.top
      case Principal.Person(_, held) =>
        memberships(held).map(_.label).foldLeft(Label.Public)(_.join(_))
    }

  /** `principal`'s integrity, as their words enter grit before their room's cap: grit's,
    * [[Integrity.top]]; the local operator's and a declared administrator's, `Authority`; a
    * steward's, `Authority` in the compartments they steward; a person attested in a realm,
    * `Member`; anyone else, `Outside`.
    */
  def integrityOf(principal: Principal)(using tx: Tx^): Integrity =
    principal match {
      case Principal.Grit => Integrity.top(tx.visibility.compartments)
      case Principal.Person(_, held) =>
        val v = tx.visibility
        val in = declaredIn(principal).map(_.group).toSet
        val authority = Option.when(
          held.exists(_.account == Account.Local) || v.administrators.exists(in.contains)
        )(Integrity.at(Integrity.Level.Authority))
        val stewarded = v.stewards
          .filter(s => in.contains(s.group))
          .map(s => Integrity.at(Integrity.Level.Authority, s.compartment))
        (authority.toVector ++ stewarded)
          .foldLeft(if (principal.vouched) Integrity.Member else Integrity.Outside)(_.join(_))
    }

  /** The groups the deployment declares `principal` in, by an account it names or as a full
    * member of a realm it names, in its order; never one they were only added to through grit.
    * Read through [[explain]] at the top label, which every grant is under, so the one rule for
    * who is in a group is its.
    */
  private[core] def declaredIn(principal: Principal)(using tx: Tx^): Vector[Explanation.In] =
    explain(Some(principal), tx.visibility.compartments.top).groups
      .filter(i => i.through.nonEmpty || i.members.nonEmpty)

  /** What `asker` (`None`: no one asked) is told of their clearance in a room labelled `room`
    * ([[Explanation]]); each group says through which kinds of account they are in it by
    * declaration, as a realm's full member, or added through grit.
    */
  def explain(asker: Option[Principal], room: Label)(using tx: Tx^): Explanation = {
    val (who, in) = asker match {
      case None => (Explanation.Asker.Nobody, Vector.empty)
      case Some(Principal.Grit) => (Explanation.Asker.Grit, Vector.empty)
      case Some(Principal.Person(_, held)) =>
        val shown = held.toVector
          .map(h => Explanation.Shown(Explanation.Namespace.of(h.account), h.evidence, h.member))
          .sortBy(s => (s.namespace, s.evidence.ordinal, s.member))
        (Explanation.Asker.Person(shown), memberships(held).filter(i => room.dominates(i.label)))
    }
    Explanation(room, room.meet(asker.fold(Label.Public)(clearanceOf)), who, in)
  }

  /** The label `to` is written at outside grit: the label in force there ([[roomLabel]]) when
    * it is set through grit or the deployment's labeller maps it explicitly, with only declared
    * compartments; `None` otherwise, and then nothing writes to it. `None` for a quiet room
    * ([[quiet]]) and for a direct message's room ([[Place.direct]]): only its own turns write
    * there. What is offered or sent outside grit; reads of grit's own rows are filtered by each
    * row's own label.
    */
  def writable(to: Place)(using tx: Tx^): Option[Label] =
    if (to.direct || quiet(to)) None
    else
      labelled(to) match {
        case Labelled.Mapped(l) if tx.visibility.compartments.admit(l) == l => Some(l)
        case _ => None
      }

  /** Whether what was read at `at` may be written to `to` outside grit: `to` is [[writable]]
    * and its label dominates `at` (ADR 0030, no write down). `at` is [[classification]], or a
    * routed turn's [[locked]] classification.
    */
  def writesTo(to: Place, at: Classification)(using tx: Tx^): Boolean =
    writable(to).exists(_.dominates(at.label))

  /** Whether what `source` contains may be read in this transaction: as anything recorded in a
    * room at `source` is read ([[Clearance.reads]]), at the label in force there
    * ([[roomLabel]]), an unplaced one at [[grit.core.visibility.Compartment.Unmapped]] (no read
    * up). A transaction's own room is read up to its own label; any other place up to
    * `everywhere`. What is offered or sent outside grit; reads of grit's own rows are filtered
    * by each row's own label.
    */
  def readsFrom(source: Place)(using tx: Tx^): Boolean =
    tx.clearance.reads(Item.InRoom(source), roomLabel(source))

  /** What this transaction knows is classified at: [[floor]]. */
  def classification(using tx: Tx^): Classification = Classification(floor(tx))

  /** What a turn routed for `bound` sends and writes out at in this transaction: `bound`
    * joined with [[classification]], so never below what the transaction knows. A routed turn
    * holds its routed classification while it runs (tranquility, ADR 0037): a floor that fell
    * since does not lower what it may send to.
    */
  def locked(bound: Label)(using tx: Tx^): Classification =
    Classification(floor(tx).join(bound))

  /** `Right` when what this transaction knows is classified at most `bound`
    * ([[classification]]); otherwise `StoreError.Invalid`, naming both and saying to ask
    * again: what it knows rose since `bound` was read.
    */
  def within(bound: Label)(using tx: Tx^): Either[StoreError, Unit] = {
    val known = classification.label
    Either.cond(
      bound.dominates(known),
      (),
      StoreError.Invalid(
        s"a read at ${Label.shown(known)} is above the ${Label.shown(bound)} its calls were " +
          "routed for, which changed since: ask again"
      )
    )
  }

  /** Whether what was read at `at` may be sent to `party`, as this transaction's deployment
    * clears it ([[grit.core.visibility.Clearances.clears]]). `at` is [[classification]], or a
    * routed turn's [[locked]] classification.
    */
  def clears(party: Party, at: Classification)(using tx: Tx^): Boolean =
    tx.visibility.clearances.clears(party, at)

  /** Whether what was read at `at` may be sent to `service` as a call's arguments ([[clears]]
    * its party). What the call returns is checked by [[readsFrom]] of its place.
    */
  def sendsTo(service: Service, at: Classification)(using tx: Tx^): Boolean =
    clears(Party.Service(service), at)

  /** `room`'s label before its compartments are admitted: the one set through grit, else the
    * deployment's labeller's for it with its reported access.
    */
  private def labelled(room: Place)(using tx: Tx^): Labelled =
    tx.recorded.rooms.get(room).flatMap(_.label).fold(declared(room))(Labelled.Mapped(_))

  /** `room`'s label from the deployment's labeller, with its reported access, before its
    * compartments are admitted.
    */
  private def declared(room: Place)(using tx: Tx^): Labelled =
    tx.visibility.rooms.label(Room(room, access(room)))

  /** Each declared group `held`'s person is in, in the deployment's order, with the join of
    * its grants and the kinds of the accounts and realms that put them in it.
    */
  private def memberships(held: Set[Held])(using tx: Tx^): Vector[Explanation.In] = {
    val v = tx.visibility
    v.groups.flatMap { g =>
      val through = held.filter(h => g.accounts.contains(h.account))
      val members = g.realms.filter(r => held.exists(h => h.member && r.holds(h.account)))
      val listed = tx.recorded.added.getOrElse(g.name, Set.empty)
      val added = held.filter(h => listed.contains(h.account))
      Option.when(through.nonEmpty || members.nonEmpty || added.nonEmpty)(
        Explanation.In(
          g.name,
          v.grants.filter(_.group == g.name).map(_.label).foldLeft(Label.Public)(_.join(_)),
          through.map(h => Explanation.Namespace.of(h.account)),
          members.map(Explanation.Namespace.of),
          added.map(h => Explanation.Namespace.of(h.account))
        )
      )
    }
  }
}
