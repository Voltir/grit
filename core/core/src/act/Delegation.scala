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
    * `parent` acts for and at its gates, believed at `parent`'s integrity met with `brief`, its
    * asks held to `as.spendCap`. Refused by the first of `Below`, `Above`, `NotWorking` and
    * `OverCap` that holds. `parentAt` is the parent's classification, `room` the label the
    * child's room may receive, `origin` the parent conversation's, and `spent` what it has spent,
    * read only when `parent`'s allowance is `Capped`.
    */
  def narrowed(
      parent: Acting,
      as: Subagent,
      brief: Integrity,
      parentAt: Label,
      room: Label,
      origin: Origin,
      spent: Cost
  ): Either[Refused, TurnRef -> Acting] = {
    val believed = Delegation.believed(parent, brief)
    for {
      _ <- Either.cond(believed.dominates(as.requires), (), Refused.Below(as.requires, believed))
      _ <- Either.cond(room.dominates(parentAt), (), Refused.Above(parentAt, room))
      _ <- working(as.tools, origin)
      _ <- within(parent.allowance, as.spendCap, spent)
    } yield (turn: TurnRef) =>
      Acting(
        turn,
        parent.actsFor,
        Allowance.Capped(as.spendCap),
        parent.gates,
        believed
      )
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

    /** A `Subagent.Tools.Coding` type over `repository`, its parent's origin not an
      * `Origin.Work` in it.
      */
    case NotWorking(repository: Repository)

    /** `asked` is more than `cap` less `spent`, or some of `spent` was not priced
      * (`Cost.AtLeast`), so what is left is unknown.
      */
    case OverCap(asked: SpendCap, cap: SpendCap, spent: Cost)

    /** Its room is a direct message's, or a thread in one: no child is made within one. */
    case Direct(room: Place)
  }

  private def working(tools: Subagent.Tools, origin: Origin): Either[Refused, Unit] =
    (tools, origin) match {
      case (Subagent.Tools.Services(_), _) => Right(())
      case (Subagent.Tools.Coding(_, repository, _), Origin.Work(in, _)) if in == repository =>
        Right(())
      case (Subagent.Tools.Coding(_, repository, _), _) => Left(Refused.NotWorking(repository))
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
