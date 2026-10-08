package grit.core.admin

import java.time.Instant

import grit.core.identity.{Account, Principal}
import grit.core.place.Place
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.{
  Compartment,
  Explanation,
  GroupName,
  Label,
  Level,
  Room,
  RoomLabels,
  Visibility
}

/** Changes to who may see what that people make through grit (ADR 0033), and what they read
  * of them. An edge calls it as it calls the inbox; nothing else writes these settings.
  */
trait Administration extends caps.SharedCapability {

  /** Runs `command`, asked through `by` in `room` at `at`, in one transaction, against the
    * labels in force then and whom `by` is linked to then. `ShowLabel`, `Clearance`,
    * `Compartments` and `Help` only read. Any other is decided by [[Authority.decide]], and
    * when allowed is kept with one audit row naming `by`, `at` and the change with what it
    * replaced; repeating a change keeps nothing, and is answered as any change is. Whether `by`
    * administers, and which compartments they steward, are read here from the deployment's
    * declared groups, never from people added through grit. `by` should be attested first
    * ([[grit.core.edge.Attesting.before]]): an account never vouched is no full member and can
    * only read. A clearance shown is the asker's own in full, wherever asked, and another's
    * only to an administrator ([[Refusal.NotForYou]]); an edge shows every [[Answer]] to its
    * asker alone. `Left` only when the database failed; then nothing changed.
    */
  def run(by: Account, room: Place, command: Command, at: Instant): Either[StoreError, Answer]
}

object Administration {

  /** What `command`, asked in `room` by `by` (whom its account is linked to now; `None` for
    * one never seen), comes to under the labels in force: `Left`, its answer, when it only
    * reads or is refused; `Right`, the change [[Authority.decide]] allowed, which an
    * implementation of [[Administration.run]] keeps and then answers by [[answer]]. `named` is
    * whom the account a `Clearance` names is linked to now (`None` for one never seen, and for
    * any other command).
    */
  private[grit] def decide(
      by: Option[Principal],
      room: Place,
      command: Command,
      named: Option[Principal]
  )(using tx: Tx^): Either[Answer, Authority.Allowed] = {
    val v = Tx.visibility(tx)
    val roles = by.fold(Roles.None)(Roles.of)
    def cleared(p: Option[Principal]): Label = p.fold(Label.Public)(Tx.clearanceOf)
    val own = Left(Answer.clearance(Tx.explain(by, cleared(by))))
    def change(c: Change): Either[Answer, Authority.Allowed] =
      by.toRight(Refusal.NotVouched)
        .flatMap(p =>
          Authority.decide(
            c,
            p,
            Tx.access(room),
            roles.administers,
            roles.stewards,
            v.compartments,
            ownGroups(v)
          )
        )
        .left
        .map(Answer.Refused(_))
    command match {
      case Command.ShowLabel => Left(shown(room, by))
      case Command.Help => Left(Answer.Help)
      case Command.Compartments =>
        Left(Answer.compartments(v.compartments, roles.administers, roles.stewards, cleared(by)))
      case Command.Clearance(None) => own
      case Command.Clearance(Some(_)) =>
        (by, named) match {
          case (Some(Principal.Person(asker, _)), Some(Principal.Person(of, _))) if asker == of =>
            own
          case _ if roles.administers => Left(Answer.theirs(cleared(named)))
          case _ => Left(Answer.Refused(Refusal.NotForYou))
        }
      case Command.SetLabel(label) =>
        change(Change.Relabel(room, Tx.roomLabel(room), Change.To.Set(label)))
      case Command.Unlabel =>
        change(Change.Relabel(room, Tx.roomLabel(room), Change.To.Default(Tx.defaultLabel(room))))
      case Command.Quiet(on) => change(Change.Quiet(room, on))
      case Command.Clear(person, c) => change(Change.Clear(person, c))
      case Command.Remove(person, c) => change(Change.Remove(person, c))
    }
  }

  /** The answer to `kept`, a change made, read under the labels in force after it; `person` is
    * whom a `Clear`'s or `Remove`'s account is linked to now (`None` for any other change, and
    * for an account never seen).
    */
  private[grit] def answer(kept: Authority.Allowed, person: Option[Principal])(using
      tx: Tx^
  ): Answer = {
    val after = person.fold(Label.Public)(Tx.clearanceOf)
    kept.change match {
      case c: Change.Relabel => Answer.relabelled(c)
      case c: Change.Quiet => Answer.quieted(c)
      case c: Change.Clear => Answer.cleared(c, after)
      case c: Change.Remove =>
        val holds = Label.at(Level.Public, c.compartment)
        val still = person.fold(Vector.empty[GroupName])(p =>
          declaredIn(p).filter(_.label.dominates(holds)).map(_.group)
        )
        Answer.removed(c, after, still)
    }
  }

  /** What `principal` may do beyond any full member: administer, and steward `stewards`. */
  private final case class Roles(administers: Boolean, stewards: Set[Compartment])

  private object Roles {
    val None: Roles = Roles(administers = false, Set.empty)

    /** `principal`'s roles, from the groups the deployment declares them in alone. */
    def of(principal: Principal)(using tx: Tx^): Roles = {
      val v = Tx.visibility(tx)
      val in = declaredIn(principal).map(_.group).toSet
      Roles(
        v.administrators.exists(in.contains),
        v.stewards.filter(s => in.contains(s.group)).map(_.compartment).toSet
      )
    }
  }

  /** The groups the deployment declares `principal` in, by an account it names or as a full
    * member of a realm it names, in its order; never one they were only added to through grit.
    * Read through [[Tx.explain]] at the top label, which every grant is under, so the one rule
    * for who is in a group is its.
    */
  private def declaredIn(principal: Principal)(using tx: Tx^): Vector[Explanation.In] =
    Tx.explain(Some(principal), Tx.visibility(tx).compartments.top)
      .groups
      .filter(i => i.through.nonEmpty || i.members.nonEmpty)

  /** The declared compartments whose own group ([[GroupName.own]]) the deployment declares,
    * granted it: those a person may be cleared for through grit.
    */
  private def ownGroups(v: Visibility): Set[Compartment] =
    v.compartments.declared.filter { c =>
      val own = GroupName.own(c)
      v.groups.exists(_.name == own) &&
      v.grants.exists(g => g.group == own && g.label.dominates(Label.at(Level.Public, c)))
    }

  /** `room`'s label as `ShowLabel` shows it to `by`: a direct message's as its person's
    * clearance; any other's with where it comes from.
    */
  private def shown(room: Place, by: Option[Principal])(using tx: Tx^): Answer =
    if (room.direct) Answer.direct(by.fold(Label.Public)(Tx.clearanceOf))
    else {
      val access = Tx.access(room)
      val source =
        if (Tx.labelSet(room)) Answer.Source.Set
        else
          Tx.visibility(tx).rooms match {
            case r: RoomLabels =>
              if (r.declares(Room(room, access))) Answer.Source.Declared else Answer.Source.Default
            // A labeller of the deployment's own making declares every label it gives.
            case _ => Answer.Source.Declared
          }
      Answer.label(Tx.roomLabel(room), source, access)
    }
}
