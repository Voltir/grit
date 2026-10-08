package grit.core.visibility

import grit.core.identity.{Held, Principal}
import grit.core.place.{Place, Service}

/** What a deployment injects into core about who may see what (ADR 0030): its
  * compartments, the labeller of its rooms, its groups and what each group's members are
  * cleared for, and what it trusts each outside service with.
  */
final case class Visibility private (
    compartments: Compartments,
    rooms: Labeller[Room],
    groups: Vector[Group],
    grants: Vector[Grant],
    trusts: Vector[Trust],
    administrators: Option[GroupName]
) {

  /** `principal`'s clearance: grit's is [[Compartments.top]]; a person's, the join of the
    * grants of every group they are in ([[Group]]), or [[Label.Public]].
    */
  def cleared(principal: Principal): Label =
    principal match {
      case Principal.Grit => compartments.top
      case Principal.Person(_, held) =>
        memberships(held).map(_.label).foldLeft(Label.Public)(_.join(_))
    }

  /** What `asker` (`None`: no one asked) is told of their clearance in a room labelled `room`
    * ([[Explanation]]).
    */
  def explain(asker: Option[Principal], room: Label): Explanation = {
    val (who, in) = asker match {
      case None => (Explanation.Asker.Nobody, Vector.empty)
      case Some(Principal.Grit) => (Explanation.Asker.Grit, Vector.empty)
      case Some(Principal.Person(_, held)) =>
        val shown = held.toVector
          .map(h => Explanation.Shown(Explanation.Namespace.of(h.account), h.evidence, h.member))
          .sortBy(s => (s.namespace, s.evidence.ordinal, s.member))
        (Explanation.Asker.Person(shown), memberships(held).filter(i => room.dominates(i.label)))
    }
    Explanation(room, room.meet(asker.fold(Label.Public)(cleared)), who, in)
  }

  /** Each group `held`'s person is in, in the deployment's order, with the join of its grants
    * and the kinds of the accounts and realms that put them in it.
    */
  private def memberships(held: Set[Held]): Vector[Explanation.In] =
    groups.flatMap { g =>
      val through = held.filter(h => g.accounts.contains(h.account))
      val members = g.realms.filter(r => held.exists(h => h.member && r.holds(h.account)))
      Option.when(through.nonEmpty || members.nonEmpty)(
        Explanation.In(
          g.name,
          grants.filter(_.group == g.name).map(_.label).foldLeft(Label.Public)(_.join(_)),
          through.map(h => Explanation.Namespace.of(h.account)),
          members.map(Explanation.Namespace.of)
        )
      )
    }

  /** The label a conversation in `room` ([[grit.core.store.Origin.room]]) is created at: the
    * one [[rooms]] gives it as a room whose access is not reported, a compartment not declared kept at [[Compartment.Unmapped]]
    * instead ([[Compartments.admit]]). A direct message's room ([[Place.direct]]) is
    * [[Compartments.top]]: its label is its person's clearance, which only a transaction can
    * resolve (ADR 0032), so anything labelling it without one fails high.
    */
  def roomLabel(room: Place): Label =
    if (room.direct) compartments.top else compartments.admit(rooms.label(Room(room, None)).label)

  /** What `service` is trusted with: its declared trust's label; [[Label.Public]] when none is
    * declared.
    */
  def trusted(service: Service): Label =
    trusts.find(_.service == service).fold(Label.Public)(_.label)
}

object Visibility {

  /** The declared compartment `group` is named for: the one whose own group it is. */
  private def ownGroupOf(compartments: Compartments, group: GroupName): Option[Compartment] =
    compartments.declared.find(c => Compartment.name(c) == GroupName.value(group))

  /** No compartment but unmapped, every room public, no group, no service trusted above
    * public: every label is public, so every reader reads what it read before labels existed.
    */
  val Shipped: Visibility =
    new Visibility(
      Compartments.Shipped,
      RoomLabels.Public,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      None
    )

  /** These, or the first mistake: a room declared within `direct` (`DirectDeclared`); a
    * compartment `compartments` does not declare, named by a declared room label,
    * `rooms.requires`, a grant or a trust (`Undeclared`, saying where); two groups of one name;
    * a grant to a group not declared; a service trusted twice; or `administrators` naming no
    * declared group (`NoSuchGroup`), or a compartment's own group, the one named as it
    * (`AdministersCompartment`), which people cleared for it through grit join. `administrators`' declared
    * members alone may make the changes reserved to administrators; with none, no one may.
    */
  def of(
      compartments: Compartments,
      rooms: Labeller[Room],
      groups: Vector[Group],
      grants: Vector[Grant],
      trusts: Vector[Trust] = Vector.empty,
      administrators: Option[GroupName] = None
  ): Either[VisibilityRefusal, Visibility] = {
    def undeclared(namer: Namer, label: Label): Option[VisibilityRefusal] =
      compartments.undeclared(label).map(VisibilityRefusal.Undeclared(namer, _))
    val declaredRooms = rooms match {
      case r: RoomLabels =>
        r.declared.map((place, label) => (Namer.RoomAt(place), label)) :+
          (Namer.OtherRooms, r.otherwise.label) :+ (Namer.OpenRooms, r.open.label)
      case _ => Vector.empty
    }
    val names = groups.map(_.name)
    val direct = rooms match {
      case r: RoomLabels =>
        r.declared.collectFirst {
          case (place, _) if place.direct => VisibilityRefusal.DirectDeclared(place)
        }
      case _ => None
    }
    val refusal =
      direct
        .orElse(declaredRooms.flatMap(undeclared(_, _)).headOption)
        .orElse(
          rooms.requires
            .find(c => !compartments.declared.contains(c))
            .map(VisibilityRefusal.Undeclared(Namer.Rooms, _))
        )
        .orElse(grants.flatMap(g => undeclared(Namer.Granted(g.group), g.label)).headOption)
        .orElse(names.diff(names.distinct).headOption.map(VisibilityRefusal.GroupTwice(_)))
        .orElse(
          grants
            .find(g => !names.contains(g.group))
            .map(g => VisibilityRefusal.NoSuchGroup(g.group))
        )
        .orElse(trusts.flatMap(t => undeclared(Namer.Trusted(t.service), t.label)).headOption)
        .orElse {
          val services = trusts.map(_.service)
          services.diff(services.distinct).headOption.map(VisibilityRefusal.TrustedTwice(_))
        }
        .orElse(administrators.flatMap { g =>
          if (!names.contains(g)) Some(VisibilityRefusal.NoSuchGroup(g))
          else ownGroupOf(compartments, g).map(_ => VisibilityRefusal.AdministersCompartment(g))
        })
    refusal.toLeft(new Visibility(compartments, rooms, groups, grants, trusts, administrators))
  }
}

/** What in a [[Visibility]] names a compartment. */
enum Namer {

  /** The rooms' labeller, by its [[Labeller.requires]]. */
  case Rooms

  /** The label a [[RoomLabels]] declares at `place`. */
  case RoomAt(place: Place)

  /** A [[RoomLabels]]' `otherwise`. */
  case OtherRooms

  /** A [[RoomLabels]]' `open`. */
  case OpenRooms

  /** A grant to `group`. */
  case Granted(group: GroupName)

  /** The trust of `service`. */
  case Trusted(service: Service)
}

/** Why a deployment's [[Visibility]] is refused. */
enum VisibilityRefusal {

  /** `compartment`, named by `by`, is not among the deployment's compartments. */
  case Undeclared(by: Namer, compartment: Compartment)

  /** Two groups are named `name`. */
  case GroupTwice(name: GroupName)

  /** A grant is to `group`, which no declared group is named. */
  case NoSuchGroup(group: GroupName)

  /** `group`, the administrators group, is named as a declared compartment: people added to that
    * compartment through grit would administer.
    */
  case AdministersCompartment(group: GroupName)

  /** Two trusts name `service`. */
  case TrustedTwice(service: Service)

  /** `place`, a room the deployment's [[RoomLabels]] declares, is within `direct`: a direct
    * message's room is labelled at its person's clearance, never by the labeller.
    */
  case DirectDeclared(place: Place)
}

/** The deployment trusts `service` with `label`: what a turn may send it as a call's arguments
  * is at most this ([[grit.core.store.Tx.sendsTo]]). What it contains is its place's label,
  * given as any room's.
  */
final case class Trust(service: Service, label: Label)
