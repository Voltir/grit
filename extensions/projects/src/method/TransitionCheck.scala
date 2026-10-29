package grit.projects.method

import grit.projects.artifact.{Artifact, ArtifactHead, Body}
import grit.projects.name.{CheckName, StateName}

/** A compiled check a transition may name: whether `judged` may move, over its project's heads
  * and its method's kinds. Deterministic: the same artifact, heads and kinds give the same
  * answer, whenever and wherever asked (a bench replay and a live move agree only so). A change
  * to what it decides is a new `version`.
  */
trait TransitionCheck extends caps.Pure {
  def name: CheckName
  def version: Int

  /** `Left` saying what failed, in words the mover can act on. */
  def holds(
      judged: Artifact,
      heads: Vector[ArtifactHead],
      kinds: Vector[ArtifactKind]
  ): Either[String, Unit]
}

/** The checks grit compiles in. */
object TransitionChecks {

  /** `children-ended`: every artifact whose parent it is is in an end state of its kind. */
  object ChildrenEnded extends Builtin("children-ended") {
    def holds(
        judged: Artifact,
        heads: Vector[ArtifactHead],
        kinds: Vector[ArtifactKind]
    ): Either[String, Unit] = {
      val open =
        heads.filter(h => h.parent.contains(judged.head.key) && !ArtifactKind.ends(kinds, h))
      if (open.isEmpty) Right(())
      else
        Left(
          s"not ended yet: ${open.map(h => s"${h.key.value} (${StateName.value(h.state)})").mkString(", ")}"
        )
    }
  }

  /** `cited-accepted`: some artifact citing it is in a state named `accepted`. */
  object CitedAccepted extends Builtin("cited-accepted") {
    def holds(
        judged: Artifact,
        heads: Vector[ArtifactHead],
        kinds: Vector[ArtifactKind]
    ): Either[String, Unit] =
      if (
        heads
          .exists(h => h.cites.contains(judged.head.key) && StateName.value(h.state) == "accepted")
      )
        Right(())
      else Left(s"nothing citing ${judged.head.key.value} is accepted")
  }

  /** `sections-present`: its body holds a `##` section of each name its kind lists. */
  object SectionsPresent extends Builtin("sections-present") {
    def holds(
        judged: Artifact,
        heads: Vector[ArtifactHead],
        kinds: Vector[ArtifactKind]
    ): Either[String, Unit] = {
      val held = Body.sections(judged.body).toSet
      val missing = kinds.filter(_.name == judged.head.kind).flatMap(_.sections).filterNot(held)
      if (missing.isEmpty) Right(())
      else Left(s"its body lacks ${missing.map(s => s"## $s").mkString(", ")}")
    }
  }

  /** Every check grit compiles in, each at version 1. */
  val all: Vector[TransitionCheck] = Vector(ChildrenEnded, CitedAccepted, SectionsPresent)

  sealed abstract class Builtin(named: String) extends TransitionCheck {
    val name: CheckName = CheckName.builtin(named)
    val version: Int = 1
  }
}
