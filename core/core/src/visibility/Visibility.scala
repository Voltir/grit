package grit.core.visibility

import grit.core.id.PrincipalId
import grit.core.place.{Place, Service}

/** What a deployment injects into core about who may see what (ADR 0030): its
  * compartments, the labeller of its rooms, its groups and what each group's members are
  * cleared for, and what it trusts each outside service with.
  */
final case class Visibility private (
    compartments: Compartments,
    rooms: Labeller[Place],
    groups: Vector[Group],
    grants: Vector[Grant],
    trusts: Vector[Trust]
) {

  /** `person`'s clearance: the join of the grants of every group they are in, or
    * [[Label.Public]]; for [[PrincipalId.Grit]], [[Compartments.top]].
    */
  def cleared(person: PrincipalId): Label =
    if (person == PrincipalId.Grit) compartments.top
    else {
      val in = groups.filter(_.members.contains(person)).map(_.name).toSet
      grants.filter(g => in.contains(g.group)).map(_.label).foldLeft(Label.Public)(_.join(_))
    }

  /** The label a conversation in `room` ([[grit.core.store.Origin.room]]) is created at: the
    * one [[rooms]] gives it, a compartment not declared kept at [[Compartment.Unmapped]]
    * instead ([[Compartments.admit]]).
    */
  def roomLabel(room: Place): Label = compartments.admit(rooms.label(room).label)

  /** What `service` is trusted with: its declared trust's label; [[Label.Public]] when none is
    * declared.
    */
  def trusted(service: Service): Label =
    trusts.find(_.service == service).fold(Label.Public)(_.label)
}

object Visibility {

  /** No compartment but unmapped, every room public, no group, no service trusted above
    * public: every label is public, so every reader reads what it read before labels existed.
    */
  val Shipped: Visibility =
    new Visibility(
      Compartments.Shipped,
      RoomLabels.Public,
      Vector.empty,
      Vector.empty,
      Vector.empty
    )

  /** These, or the first mistake: a compartment `compartments` does not declare, named by a
    * declared room label, `rooms.requires`, a grant or a trust (`Undeclared`, saying where);
    * two groups of one name; a grant to a group not declared; or a service trusted twice.
    */
  def of(
      compartments: Compartments,
      rooms: Labeller[Place],
      groups: Vector[Group],
      grants: Vector[Grant],
      trusts: Vector[Trust] = Vector.empty
  ): Either[VisibilityRefusal, Visibility] = {
    def undeclared(namer: Namer, label: Label): Option[VisibilityRefusal] =
      compartments.undeclared(label).map(VisibilityRefusal.Undeclared(namer, _))
    val declaredRooms = rooms match {
      case r: RoomLabels =>
        r.declared.map((place, label) => (Namer.RoomAt(place), label)) :+
          (Namer.OtherRooms, r.otherwise.label)
      case _ => Vector.empty
    }
    val names = groups.map(_.name)
    val refusal =
      declaredRooms
        .flatMap(undeclared(_, _))
        .headOption
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
    refusal.toLeft(new Visibility(compartments, rooms, groups, grants, trusts))
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

  /** Two trusts name `service`. */
  case TrustedTwice(service: Service)
}

/** The deployment trusts `service` with `label`: what a turn may send it as a call's arguments
  * is at most this ([[grit.core.store.Tx.sendsTo]]). What it contains is its place's label,
  * given as any room's.
  */
final case class Trust(service: Service, label: Label)
