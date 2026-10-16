package grit.core.visibility

import grit.core.place.{Namespace, Place, Service}

/** What a deployment injects into core about who may see what (ADR 0030): its
  * compartments, the labeller of its rooms, its groups and what each group's members are
  * cleared for, and what each party it sends to is cleared for; and what may be believed
  * (ADR 0036): the caps on what is said at its places ([[Cap]]). What a room is labelled and a
  * person is cleared for in force is read on a transaction ([[grit.core.store.Tx.roomLabel]],
  * [[grit.core.store.Tx.clearanceOf]]), which adds what is recorded through grit.
  */
final case class Visibility private (
    compartments: Compartments,
    rooms: Labeller[Room],
    groups: Vector[Group],
    grants: Vector[Grant],
    clearances: Clearances,
    administrators: Option[GroupName],
    stewards: Vector[Steward],
    private val caps: PlaceMap[Integrity],
    private val invited: Vector[Place]
) {

  /** Whether `room` lies strictly within a place the deployment declares invited: a service's
    * place it shares only with the people it names, each room of it read as invited when no
    * edge reports its access.
    */
  def invites(room: Place): Boolean =
    invited.exists(p => room != p && room.within(p))

  /** The cap on what is said at `place` ([[Cap]]): the declared one at the deepest place `place`
    * is or lies within; `None` when no declared cap covers it.
    */
  def capOf(place: Place): Option[Integrity] = caps.covering(place)

  /** What `service`'s results are believed at: the cap at its place ([[capOf]]), else
    * [[Cap.Undeclared]].
    */
  def outputOf(service: Service): Integrity = capOf(service.place).getOrElse(Cap.Undeclared)

  /** What a result of `service` is believed at, given what its extension read of its items
    * (`items`): [[outputOf]] when unread or holding no item; untrusted when unaccountable; else
    * the meet, over its items, of each one's `vouched` met with the cap at its place ([[capOf]],
    * [[Cap.Undeclared]] when none), and untrusted for an item placed outside `service`'s place.
    */
  def believed(service: Service, items: Accounted): Integrity = items match {
    case Accounted.Unread => outputOf(service)
    case Accounted.Unaccountable => Integrity.Untrusted
    case Accounted.Items(provenances) =>
      provenances
        .map { p =>
          if (!p.place.within(service.place)) Integrity.Untrusted
          else p.vouched.meet(capOf(p.place).getOrElse(Cap.Undeclared))
        }
        .reduceOption(_.meet(_))
        .getOrElse(outputOf(service))
  }
}

object Visibility {

  /** The declared compartment `group` is named for: the one whose own group it is. */
  private def ownGroupOf(compartments: Compartments, group: GroupName): Option[Compartment] =
    compartments.declared.find(c => GroupName.own(c) == group)

  /** No compartment but unmapped, every room public, no group, no party cleared above
    * public: every label is public, so every reader reads what it read before labels existed.
    */
  val Shipped: Visibility =
    new Visibility(
      Compartments.Shipped,
      RoomLabels.Public,
      Vector.empty,
      Vector.empty,
      Clearances.Empty,
      None,
      Vector.empty,
      PlaceMap.Empty,
      Vector.empty
    )

  /** These, or the first mistake: a room declared within `direct` (`DirectDeclared`); a
    * compartment `compartments` does not declare, named by a declared room label,
    * `rooms.requires`, a grant or a clearance (`Undeclared`, saying where); two groups of one
    * name; a grant to a group not declared; a party cleared twice (`ClearedTwice`); or
    * `administrators` naming no declared group (`NoSuchGroup`), or a compartment's own group,
    * the one named as it (`AdministersCompartment`), which people cleared for it through grit
    * join.
    * `administrators`' declared members alone may make the changes
    * [[grit.core.admin.Authority]] reserves to administrators; with none, no one may. A
    * steward of a compartment not declared (`Undeclared`, by [[Namer.Stewarded]]), through a
    * group not declared
    * (`NoSuchGroup`) or through another compartment's own group (`StewardsThroughCompartment`),
    * or one compartment stewarded twice (`StewardedTwice`), is refused too. So is a cap at a
    * directory under `fs`, a direct message's room or everywhere (`CapOutside`), a place capped
    * twice (`CappedTwice`), or a cap naming a compartment `compartments` does not declare
    * (`Undeclared`, by [[Namer.CapOf]]).
    * `invited` are places a service shares only with the people it names ([[invites]]); one
    * that is not under a service's own place (`service:<name>`, or a place within it) is
    * refused (`InvitedOutside`).
    */
  def of(
      compartments: Compartments,
      rooms: Labeller[Room],
      groups: Vector[Group],
      grants: Vector[Grant],
      clearances: Vector[Cleared] = Vector.empty,
      administrators: Option[GroupName] = None,
      stewards: Vector[Steward] = Vector.empty,
      caps: Vector[Cap] = Vector.empty,
      invited: Vector[Place] = Vector.empty
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
    val cleared = Clearances.of(clearances).left.map(VisibilityRefusal.ClearedTwice(_))
    val capped =
      PlaceMap.of(caps.map(c => (c.place, c.integrity))).left.map(VisibilityRefusal.CappedTwice(_))
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
        .orElse(
          clearances.flatMap(c => undeclared(Namer.Cleared(c.party), c.clearance)).headOption
        )
        .orElse(cleared.left.toOption)
        .orElse(administrators.flatMap { g =>
          if (!names.contains(g)) Some(VisibilityRefusal.NoSuchGroup(g))
          else ownGroupOf(compartments, g).map(_ => VisibilityRefusal.AdministersCompartment(g))
        })
        .orElse(stewards.flatMap { st =>
          val c = st.compartment
          if (!compartments.declared.contains(c))
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
        .orElse(caps.collectFirst {
          case c
              if c.place == Place.Everywhere || c.place.direct ||
                c.place.within(Place.under(Namespace.Fs, Vector.empty)) =>
            VisibilityRefusal.CapOutside(c.place)
        })
        .orElse(capped.left.toOption)
        .orElse(invited.collectFirst {
          case p
              if p.segments.headOption.forall(_ != Namespace.Service.key) || p.segments.size < 2 =>
            VisibilityRefusal.InvitedOutside(p)
        })
        .orElse(caps.flatMap { cap =>
          Integrity
            .compartments(cap.integrity)
            .find(c => !compartments.declared.contains(c))
            .map(VisibilityRefusal.Undeclared(Namer.CapOf(cap.place), _))
        }.headOption)
    for {
      _ <- refusal.toLeft(())
      c <- cleared
      m <- capped
    } yield new Visibility(
      compartments,
      rooms,
      groups,
      grants,
      c,
      administrators,
      stewards,
      m,
      invited
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

  /** The clearance of `party`. */
  case Cleared(party: Party)

  /** A [[Steward]] of `compartment`. */
  case Stewarded(compartment: Compartment)

  /** The [[Cap]] declared at `place`. */
  case CapOf(place: Place)
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

  /** Two stewards are declared for `compartment`. */
  case StewardedTwice(compartment: Compartment)

  /** Two clearances name `party`. */
  case ClearedTwice(party: Party)

  /** Two caps are declared at `place`. */
  case CappedTwice(place: Place)

  /** A [[Cap]] is declared at `place`: a directory under `fs`, a direct message's room or
    * everywhere, each holding a person's own conversations with grit, which no cap lowers.
    */
  case CapOutside(place: Place)

  /** `place`, a room the deployment's [[RoomLabels]] declares, is within `direct`: a direct
    * message's room is labelled at its person's clearance, never by the labeller.
    */
  case DirectDeclared(place: Place)

  /** `place` is declared invited, and is no service's place, or is the service namespace
    * itself: only a service's own places are shared with grit by invitation, and a room
    * anywhere else reports its own access.
    */
  case InvitedOutside(place: Place)
}
