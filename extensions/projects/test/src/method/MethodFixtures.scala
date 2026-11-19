package grit.projects.method

import grit.projects.artifact.Pin
import grit.projects.artifact.{Artifact, ArtifactHead, Body, Project, Rank}
import grit.projects.name.{
  ArtifactKey,
  ArtifactNumber,
  ArtifactRef,
  CheckName,
  Handle,
  KindName,
  MethodName,
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

  /** The handles test artifacts are named by; an artifact named `Names(i)` is numbered `i + 1`, so
    * a key written `p/<name>` reads as one id in every suite.
    */
  val Names: Vector[String] = Vector(
    "a",
    "b",
    "c",
    "d",
    "e",
    "f",
    "g",
    "h",
    "j",
    "k",
    "m",
    "n",
    "q",
    "x",
    "z",
    "zz",
    "auth",
    "bare",
    "billing",
    "call",
    "epic",
    "far",
    "look",
    "near",
    "other",
    "plan",
    "review",
    "why",
    "tfa",
    "tfa-design",
    "tfa-task",
    "p0",
    "rules",
    "process",
    "idea",
    "live",
    "plans",
    "plan-c",
    "plan-c5",
    "n01",
    "r",
    "more",
    "none",
    "i",
    "l",
    "o",
    "s",
    "t",
    "u",
    "v",
    "w",
    "y"
  )

  /** `s`, `<project>/<n>` or `<project>/<name>` of [[Names]], as an id. */
  def key(s: String): ArtifactKey = right(ids(right(ArtifactRef.read(s))))

  /** `ref` as an id: itself, or a name of [[Names]] numbered by its place there. */
  val ids: ArtifactRef -> Either[String, ArtifactKey] = {
    case ArtifactRef.Id(k) => Right(k)
    case ArtifactRef.Named(p, h) =>
      Names.indexOf(Handle.value(h)) match {
        case -1 => Left(s"${Handle.value(h)} is not among MethodFixtures.Names")
        case i => ArtifactNumber.of(i + 1).map(ArtifactKey.of(p, _))
      }
  }

  /** The handle of `key`, an id of [[Names]], if it has one. */
  def handleOf(key: ArtifactKey): Option[Handle] =
    Names.lift(ArtifactNumber.value(key.number) - 1).map(n => right(Handle.of(n)))

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

  /** [[transitions]], a change made done only by people. */
  val peopleFinish: Vector[Transition] =
    transitions.map(t => if (t.to == state("done")) t.copy(movers = Movers.Person) else t)

  val views: Vector[RoleView] = Vector(right(RoleView.of(role("builder"), Set(change.name), 1)))

  def head(
      k: String,
      kindOf: ArtifactKind,
      in: String,
      parent: Option[String] = None,
      cites: Vector[String] = Vector.empty
  ): ArtifactHead =
    ArtifactHead(
      key(k),
      handleOf(key(k)),
      kindOf.name,
      state(in),
      k,
      parent.map(key),
      Rank.Middle,
      cites.map(key),
      None
    )

  def artifact(h: ArtifactHead, body: String): Artifact = Artifact(h, right(Body.read(body, ids)))

  /** Project `p` of `artifacts`, pinned to `m` as it is. */
  def pinnedTo(m: Method, artifacts: Artifact*): Project =
    right(
      Project.of(
        right(ProjectKey.of("p")),
        right(Pin.of(m.name, m.version, m.digest)),
        artifacts.toVector
      )
    )

  /** Project `p` of `artifacts`, pinned to a digest no method has: the bench reads no pin. */
  def project(artifacts: Artifact*): Project =
    right(
      Project.of(
        right(ProjectKey.of("p")),
        right(Pin.read(method, 1, "0f" * 32)),
        artifacts.toVector
      )
    )
}
