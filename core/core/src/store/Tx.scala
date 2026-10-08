package grit.core.store

import grit.core.identity.{Held, Principal}
import grit.core.place.{Place, Service}
import grit.core.visibility.{
  Clearance,
  Explanation,
  Item,
  Label,
  Labelled,
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

  /** `room`'s access as its edge last reported it; `None` when no edge has reported one. */
  def access(room: Place)(using tx: Tx^): Option[RoomAccess] =
    tx.recorded.rooms.get(room).flatMap(_.access)

  /** Whether `room` is quiet: nothing is posted there unasked, so [[writable]] is `None` for
    * it.
    */
  def quiet(room: Place)(using tx: Tx^): Boolean =
    tx.recorded.rooms.get(room).exists(_.quiet)

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

  /** Whether what this transaction knows may be written to `to` outside grit: `to` is
    * [[writable]] and its label dominates [[floor]] (ADR 0030, no write down).
    */
  def writesTo(to: Place)(using tx: Tx^): Boolean =
    writable(to).exists(_.dominates(floor(tx)))

  /** Whether what `source` contains may be read in this transaction: as anything recorded in a
    * room at `source` is read ([[Clearance.reads]]), at the label in force there
    * ([[roomLabel]]), an unplaced one at [[grit.core.visibility.Compartment.Unmapped]] (no read
    * up). A transaction's own room is read up to its own label; any other place up to
    * `everywhere`. What is offered or sent outside grit; reads of grit's own rows are filtered
    * by each row's own label.
    */
  def readsFrom(source: Place)(using tx: Tx^): Boolean =
    tx.clearance.reads(Item.InRoom(source), roomLabel(source))

  /** Whether what this transaction knows may be sent to `service` as a call's arguments: what
    * the deployment trusts `service` with ([[Visibility.trusted]], public when it declares no
    * trust) dominates [[floor]]. What the call returns is checked by [[readsFrom]] of its place.
    */
  def sendsTo(service: Service)(using tx: Tx^): Boolean =
    tx.visibility.trusted(service).dominates(floor(tx))

  /** `room`'s label before its compartments are admitted: the one set through grit, else the
    * deployment's labeller's for it with its reported access.
    */
  private def labelled(room: Place)(using tx: Tx^): Labelled = {
    val kept = tx.recorded.rooms.get(room)
    kept.flatMap(_.label) match {
      case Some(set) => Labelled.Mapped(set)
      case None => tx.visibility.rooms.label(Room(room, kept.flatMap(_.access)))
    }
  }

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
