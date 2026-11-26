package grit.core.act

import grit.core.agent.Subagent
import grit.core.id.TurnRef
import grit.core.message.{Cost, SpendCap}
import grit.core.place.{Place, Repository}
import grit.core.store.Origin
import grit.core.visibility.{Integrity, Label}

/** How an acting is narrowed for a subagent a turn starts: what it may do, from its parent's. */
object Delegation {

  /** The acting a child of type `as`, briefed at `brief`, acts under, given its turn: it acts as
    * `parent` acts for, its gates [[Gates.Closed]] (nobody reads a child's conversation to
    * approve its calls), believed at `parent`'s integrity met with `brief`, its asks held to
    * `as.spendCap`. Refused by the first of `Below`, `Above`, `NotWorking` and
    * `OverCap` that holds. `parentAt` is the parent's classification, `room` the label the
    * child's room may receive, `served` the repositories a coding child of this parent's is served
    * in, and `spent` what it has spent, read only when `parent`'s allowance is `Capped`.
    */
  def narrowed(
      parent: Acting,
      as: Subagent,
      brief: Integrity,
      parentAt: Label,
      room: Label,
      served: Set[Repository],
      spent: Cost
  ): Either[Refused, TurnRef -> Acting] = {
    val believed = Delegation.believed(parent, brief)
    for {
      _ <- Either.cond(believed.dominates(as.requires), (), Refused.Below(as.requires, believed))
      _ <- Either.cond(room.dominates(parentAt), (), Refused.Above(parentAt, room))
      _ <- working(as.tools, served)
      _ <- within(parent.allowance, as.spendCap, spent)
    } yield (turn: TurnRef) =>
      Acting(
        turn,
        parent.actsFor,
        Allowance.Capped(as.spendCap),
        Gates.Closed,
        believed
      )
  }

  /** The repositories served to a coding child of a parent from `origin`, in a process whose edges
    * host `hosts`: those, and a run of work's own repository, whose edge its driver serves; none
    * to a spawned child's, which starts no child.
    */
  def served(origin: Origin, hosts: Set[Repository]): Set[Repository] = origin match {
    case Origin.Spawned(_, _, _) => Set.empty
    case Origin.Work(repository, _) => hosts + repository
    case _ => hosts
  }

  /** What a child `parent` briefs at `brief` is believed at: the meet of the two. */
  def believed(parent: Acting, brief: Integrity): Integrity = parent.integrity.meet(brief)

  /** Why a child may not be made under its parent. */
  enum Refused extends caps.Pure {

    /** The type requires `needs`; the child would be believed at `has`, which does not dominate
      * it.
      */
    case Below(needs: Integrity, has: Integrity)

    /** `room` does not dominate `classification`, the parent's: a child made at its room's label
      * would write what it was told down to it. Labels neither of which dominates the other are
      * refused so too.
      */
    case Above(classification: Label, room: Label)

    /** A `Subagent.Tools.Coding` type over `repository`, which is not among those served to a
      * coding child of its parent's.
      */
    case NotWorking(repository: Repository)

    /** `asked` is more than `cap` less `spent`, or some of `spent` was not priced
      * (`Cost.AtLeast`), so what is left is unknown.
      */
    case OverCap(asked: SpendCap, cap: SpendCap, spent: Cost)

    /** Its room is a direct message's, or a thread in one: no child is made within one. */
    case Direct(room: Place)
  }

  private def working(tools: Subagent.Tools, served: Set[Repository]): Either[Refused, Unit] =
    tools match {
      case Subagent.Tools.Services(_) => Right(())
      case Subagent.Tools.Coding(_, repository, _) =>
        Either.cond(served.contains(repository), (), Refused.NotWorking(repository))
    }

  private def within(allowance: Allowance, asked: SpendCap, spent: Cost): Either[Refused, Unit] =
    allowance match {
      case Allowance.Admitted | Allowance.Daily(_) => Right(())
      case Allowance.Capped(cap) =>
        spent match {
          case Cost.Exact(usd) if cap.usd - usd >= asked.usd => Right(())
          case _ => Left(Refused.OverCap(asked, cap, spent))
        }
    }
}
