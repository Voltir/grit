package grit.core.visibility

import grit.core.place.{Place, Service}

/** What a deployment injects into core about who may see what (ADR 0030): its
  * compartments, the labeller of its rooms, its groups and what each group's members are
  * cleared for, and what it trusts each outside service with. What a room is labelled and a
  * person is cleared for in force is read on a transaction ([[grit.core.store.Tx.roomLabel]],
  * [[grit.core.store.Tx.clearanceOf]]), which adds what is recorded through grit.
  */
final case class Visibility private (
    compartments: Compartments,
    rooms: Labeller[Room],
    groups: Vector[Group],
    grants: Vector[Grant],
    trusts: Vector[Trust],
    administrators: Option[GroupName],
    stewards: Vector[Steward]
) {

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
      None,
      Vector.empty
    )

  /** These, or the first mistake: a room declared within `direct` (`DirectDeclared`); a
    * compartment `compartments` does not declare, named by a declared room label,
    * `rooms.requires`, a grant or a trust (`Undeclared`, saying where); two groups of one name;
    * a grant to a group not declared; a service trusted twice; or `administrators` naming no
    * declared group (`NoSuchGroup`), or a compartment's own group, the one named as it
    * (`AdministersCompartment`), which people cleared for it through grit join.
    * `administrators`' declared members alone may make the changes
    * [[grit.core.admin.Authority]] reserves to administrators; with none, no one may. A
    * steward of a compartment not declared (`Undeclared`, by [[Namer.Stewarded]]), of
    * [[Compartment.Unmapped]] (`StewardsUnmapped`), through a group not declared
    * (`NoSuchGroup`) or through another compartment's own group (`StewardsThroughCompartment`),
    * or one compartment stewarded twice (`StewardedTwice`), is refused too.
    */
  def of(
      compartments: Compartments,
      rooms: Labeller[Room],
      groups: Vector[Group],
      grants: Vector[Grant],
      trusts: Vector[Trust] = Vector.empty,
      administrators: Option[GroupName] = None,
      stewards: Vector[Steward] = Vector.empty
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
        .orElse(stewards.flatMap { st =>
          val c = st.compartment
          if (c == Compartment.Unmapped) Some(VisibilityRefusal.StewardsUnmapped)
          else if (!compartments.declared.contains(c))
            Some(VisibilityRefusal.Undeclared(Namer.Stewarded(c), c))
          else if (!names.contains(st.group)) Some(VisibilityRefusal.NoSuchGroup(st.group))
          else
            ownGroupOf(compartments, st.group)
              .filter(_ != c)
              .map(_ => VisibilityRefusal.StewardsThroughCompartment(c, st.group))
        }.headOption)
        .orElse {
          val stewarded = stewards.map(_.compartment)
          stewarded.diff(stewarded.distinct).headOption.map(VisibilityRefusal.StewardedTwice(_))
        }
    refusal.toLeft(
      new Visibility(compartments, rooms, groups, grants, trusts, administrators, stewards)
    )
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

  /** A [[Steward]] of `compartment`. */
  case Stewarded(compartment: Compartment)
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

  /** `compartment` is stewarded through `group`, the own group of another compartment. Name the
    * compartment's own group, or a group no compartment is named for.
    */
  case StewardsThroughCompartment(compartment: Compartment, group: GroupName)

  /** [[Compartment.Unmapped]] is grit's, and no one stewards it. */
  case StewardsUnmapped

  /** Two stewards are declared for `compartment`. */
  case StewardedTwice(compartment: Compartment)

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
