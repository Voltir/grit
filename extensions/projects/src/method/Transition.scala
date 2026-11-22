package grit.projects.method

import grit.core.id.{AgentType, ClaimHolder, HolderForm}
import grit.projects.artifact.ArtifactHead
import grit.projects.name.{CheckName, Handle, KindName, RoleName, StateName}

/** A kind of artifact: the state each starts in, its other states, those of them it ends in,
  * the `##` sections its body must hold, and what it may sit under.
  */
final case class ArtifactKind(
    name: KindName,
    first: StateName,
    states: Vector[StateName],
    ends: Set[StateName],
    sections: Vector[String],
    nesting: Nesting = Nesting.Anywhere
) extends caps.Pure {

  /** `first`, then `states`. */
  def all: Vector[StateName] = first +: states
}

object ArtifactKind {

  /** Whether `head` is in an end state of its kind among `kinds`; false for a kind none is. */
  def ends(kinds: Vector[ArtifactKind], head: ArtifactHead): Boolean =
    kinds.exists(k => k.name == head.kind && k.ends(head.state))
}

/** What an artifact of a kind may sit under: any artifact or none, none, or an artifact of one
  * of `kinds`.
  */
enum Nesting extends caps.Pure {
  case Anywhere
  case Root
  case Under private[Nesting] (kinds: Set[KindName])
}

object Nesting {

  /** Under one of `kinds`, or why not: none given. */
  def under(kinds: Set[KindName]): Either[String, Nesting] =
    if (kinds.isEmpty) Left("a nesting under kinds names no kind") else Right(new Under(kinds))
}

/** A move an artifact of `kind` may make from `from` to `to`: allowed only when each check
  * holds and `movers` includes its mover, then doing its action.
  */
final case class Transition(
    kind: KindName,
    from: StateName,
    to: StateName,
    checks: Vector[CheckName],
    action: TransitionAction,
    movers: Movers = Movers.Anyone
) extends caps.Pure

/** Who may make a transition: anyone, or only a [[Mover.Person]]. */
enum Movers extends caps.Pure {
  case Anyone, Person
}

/** What a transition does besides moving its artifact: one of grit's, never a method's code. */
enum TransitionAction extends caps.Pure {
  case Nothing

  /** Merges the moved artifact's deltas into the one artifact of kind `into` its head cites,
    * whatever the order of its cites, as OpenSpec's archive merges a change's spec deltas into
    * its spec: each `##` section headed `Delta: <name>` ([[grit.projects.artifact.Body.merged]])
    * replaces that artifact's section `<name>`, or is added after its last; that artifact is kept
    * unchanged when there is no delta. Refused in the move ([[MoveRefusal]]) when its head cites
    * no artifact of kind `into`, when it cites several, or when the merged body is over its cap.
    */
  case MergeDeltas(into: KindName)

  /** How [[Method.digest]] names it, fixed whatever its Scala spelling: `Nothing`, or
    * `MergeDeltas(<into>)`.
    */
  def digested: String = this match {
    case Nothing => "Nothing"
    case MergeDeltas(into) => s"MergeDeltas(${KindName.value(into)})"
  }
}

/** What `role` is shown working on an artifact: it, and what it cites to `depth` links, of
  * `kinds`; whole, whatever it cites, each artifact of the project whose handle is one of
  * `carried`; never what the project's conversations said. Claimed only for `holders`.
  */
final case class RoleView private (
    role: RoleName,
    kinds: Set[KindName],
    depth: Int,
    carried: Set[Handle],
    covers: Cover,
    holders: Holders
) extends caps.Pure

/** What a claim as a view's role covers: its artifact alone, or what sits under it too, at any
  * depth.
  */
enum Cover extends caps.Pure {
  case Alone, Under
}

/** Who may hold a claim in a role: anyone; a session's holder, never an agent's or another's
  * ([[ClaimHolder.form]]); or only an agent's of one of `types`.
  */
sealed trait Holders extends caps.Pure {

  /** Whether a claim in a role of these holders may be made for `holder`. */
  def admits(holder: ClaimHolder): Boolean = (this, ClaimHolder.form(holder)) match {
    case (Holders.Anyone, _) => true
    case (Holders.Session, HolderForm.Session) => true
    case (Holders.Session, HolderForm.Agent(_) | HolderForm.Other) => false
    case (Holders.Agents(types), HolderForm.Agent(Some(t))) => types.contains(t)
    case (Holders.Agents(_), HolderForm.Session | HolderForm.Agent(None) | HolderForm.Other) =>
      false
  }
}

object Holders {
  case object Anyone extends Holders
  case object Session extends Holders

  /** An agent type its harness names with capitals, or with more than [[AgentType.Longest]]
    * characters (`Explore`), is kept in its holder by its hash, so no `types` names it.
    */
  final case class Agents private (types: Set[AgentType]) extends Holders

  object Agents {

    /** These types, or `None` when there are none. */
    def of(types: Set[AgentType]): Option[Agents] = Option.when(types.nonEmpty)(new Agents(types))
  }
}

object RoleView {

  /** 3: the most links a role view follows. */
  val Deepest: Int = 3

  /** This view, or why not: `depth` under 0 or over [[Deepest]], or `kinds` empty. */
  def of(
      role: RoleName,
      kinds: Set[KindName],
      depth: Int,
      carried: Set[Handle] = Set.empty,
      covers: Cover = Cover.Alone,
      holders: Holders = Holders.Anyone
  ): Either[String, RoleView] =
    if (depth < 0 || depth > Deepest)
      Left(s"a role view's depth is $depth, not 0 to $Deepest")
    else if (kinds.isEmpty) Left("a role view shows no kind")
    else Right(new RoleView(role, kinds, depth, carried, covers, holders))
}
