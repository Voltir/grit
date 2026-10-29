package grit.projects.method

import grit.projects.artifact.ArtifactHead
import grit.projects.name.{CheckName, KindName, LocalKey, RoleName, StateName}

/** A kind of artifact: the state each starts in, its other states, those of them it ends in,
  * and the `##` sections its body must hold.
  */
final case class ArtifactKind(
    name: KindName,
    first: StateName,
    states: Vector[StateName],
    ends: Set[StateName],
    sections: Vector[String]
) extends caps.Pure {

  /** `first`, then `states`. */
  def all: Vector[StateName] = first +: states
}

object ArtifactKind {

  /** Whether `head` is in an end state of its kind among `kinds`; false for a kind none is. */
  def ends(kinds: Vector[ArtifactKind], head: ArtifactHead): Boolean =
    kinds.exists(k => k.name == head.kind && k.ends(head.state))
}

/** A move an artifact of `kind` may make from `from` to `to`: allowed only when each check
  * holds, then doing its action.
  */
final case class Transition(
    kind: KindName,
    from: StateName,
    to: StateName,
    checks: Vector[CheckName],
    action: TransitionAction
) extends caps.Pure

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
}

/** What `role` is shown working on an artifact: it, and what it cites to `depth` links, of
  * `kinds`; whole, whatever it cites, each artifact of the project whose local key is one of
  * `carried`; never what the project's conversations said.
  */
final case class RoleView private (
    role: RoleName,
    kinds: Set[KindName],
    depth: Int,
    carried: Set[LocalKey]
) extends caps.Pure

object RoleView {

  /** 3: the most links a role view follows. */
  val Deepest: Int = 3

  /** This view, or why not: `depth` under 0 or over [[Deepest]], or `kinds` empty. */
  def of(
      role: RoleName,
      kinds: Set[KindName],
      depth: Int,
      carried: Set[LocalKey] = Set.empty
  ): Either[String, RoleView] =
    if (depth < 0 || depth > Deepest)
      Left(s"a role view's depth is $depth, not 0 to $Deepest")
    else if (kinds.isEmpty) Left("a role view shows no kind")
    else Right(new RoleView(role, kinds, depth, carried))
}
