package grit.projects.method

import grit.projects.artifact.{Artifact, ArtifactHead, Body, Project}
import grit.projects.name.{
  ArtifactKey,
  CheckName,
  KindName,
  MethodName,
  Pin,
  ProjectKey,
  RoleName,
  StateName
}

/** Names and a small method for the method package's suites. */
object MethodFixtures {

  def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  def kind(s: String): KindName = right(KindName.of(s))
  def state(s: String): StateName = right(StateName.of(s))
  def check(s: String): CheckName = right(CheckName.of(s))
  def role(s: String): RoleName = right(RoleName.of(s))
  def key(s: String): ArtifactKey = right(ArtifactKey.read(s))

  val method: MethodName = right(MethodName.of("grit"))

  /** A change: proposed, then building, then done; its body holds `## Why`. */
  val change: ArtifactKind =
    ArtifactKind(
      kind("change"),
      state("proposed"),
      Vector(state("building"), state("done")),
      Set(state("done")),
      Vector("Why")
    )

  /** A note: open, then closed. */
  val note: ArtifactKind = ArtifactKind(
    kind("note"),
    state("open"),
    Vector(state("closed")),
    Set(state("closed")),
    Vector.empty
  )

  def move(k: ArtifactKind, from: String, to: String, checks: String*): Transition =
    Transition(k.name, state(from), state(to), checks.toVector.map(check), TransitionAction.Nothing)

  val transitions: Vector[Transition] = Vector(
    move(change, "proposed", "building"),
    move(change, "building", "done", "sections-present"),
    move(note, "open", "closed")
  )

  val views: Vector[RoleView] = Vector(right(RoleView.of(role("builder"), Set(change.name), 1)))

  def head(
      k: String,
      kindOf: ArtifactKind,
      in: String,
      parent: Option[String] = None,
      cites: Vector[String] = Vector.empty
  ): ArtifactHead =
    ArtifactHead(key(k), kindOf.name, state(in), k, parent.map(key), cites.map(key), None)

  def artifact(h: ArtifactHead, body: String): Artifact = Artifact(h, right(Body.read(body)))

  /** Project `p` of `artifacts`, pinned to a digest no method has: the bench reads no pin. */
  def project(artifacts: Artifact*): Project =
    right(
      Project.of(right(ProjectKey.of("p")), right(Pin.of(method, 1, "0f" * 32)), artifacts.toVector)
    )
}
