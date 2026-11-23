package grit.projects.plugin

import java.time.Instant

import scala.annotation.tailrec

import grit.core.claim.{Claim, ClaimRole, Claimed, Claims}
import grit.core.document.{Document, DocumentVersion}
import grit.core.id.{ClaimHolder, DocKey, SessionId}
import grit.core.identity.Principal
import grit.core.store.Tx
import grit.projects.artifact.{Artifact, Project}
import grit.projects.method.{Cover, Method}
import grit.projects.name.{ArtifactKey, ProjectKey, StateName}
import grit.projects.render.AnswerText

import ProjectArgs.*
import ProjectCommands.*
import Stop.stored

private[plugin] object ProjectCovers {

  /** [[Claims.claim]] of `document`, the artifact `args` names, as `args` asks, worked by
    * `session`, by `by`, at its label.
    */
  private[plugin] def claimedFor(
      claims: Claims,
      document: Document,
      args: Claiming,
      session: Option[SessionId],
      by: Principal.Person,
      at: Instant
  )(using Tx^)(using Locked): Either[Stop, Claimed] =
    stored(claims.claim(document.key, args.holder, args.role, session, by, document.label, at))

  /** What claim `c` holds, named as [[ArtifactHead.called]] names it when it is an artifact of
    * `project`, matched by its document's key; else its subject as stored.
    */
  private[plugin] def subjectOf(project: Loaded, c: Claim): String =
    project.documents
      .collectFirst { case (d, a) if d.key == c.subject => a.head.called }
      .getOrElse(DocKey.value(c.subject))

  /** That `key`'s project has no artifact `key`. */
  private[plugin] def lacking(key: ArtifactKey): Stop =
    Stop.Refused(s"project ${ProjectKey.value(key.project)} has no artifact ${key.value}")

  /** How a refusal over claims says to release `what` first: `release <what> with project_release
    * first`.
    */
  private[plugin] def releaseFirst(what: String): String =
    s"release $what with project_release first"

  /** Nothing, or a refusal naming each of `changed` that a claim live at `at` holds, at any
    * label, and the holder it is claimed for.
    */
  private[plugin] def unclaimed(claims: Claims, changed: Vector[(Document, Artifact)], at: Instant)(
      using Tx^
  ): Either[Stop, Unit] =
    stored(claims.live(at)).flatMap { live =>
      claimedOf(live, changed).map((artifact, holder) =>
        s"${artifact.head.called} is claimed for ${ClaimHolder.value(holder)}"
      ) match {
        case Vector() => Right(())
        case Vector(one) => Left(Stop.Refused(s"$one: ${releaseFirst("it")}"))
        case many => Left(Stop.Refused(s"${many.mkString("; ")}: ${releaseFirst("them")}"))
      }
    }

  /** Each of `documents` that a claim of `live` holds, in the order given, with the holder of the
    * first such claim.
    */
  private[plugin] def claimedOf(
      live: Vector[Claim],
      documents: Vector[(Document, Artifact)]
  ): Vector[(Artifact, ClaimHolder)] =
    documents.flatMap((d, a) => live.find(_.subject == d.key).map(c => a -> c.holder))

  /** Whether a claim as `role` covers under `method`: its role's view covers; a role with no view
    * covers nothing.
    */
  private[plugin] def covers(method: Method, role: ClaimRole): Boolean =
    method.views.exists(v => v.covers == Cover.Under && v.role == role)

  /** Each of `live` that covers under `method`, with its artifact's id among `ids`. */
  private[plugin] def covering(
      method: Method,
      live: Vector[Claim],
      ids: Map[DocKey, ArtifactKey]
  ): Vector[(claim: Claim, key: ArtifactKey)] =
    live.filter(c => covers(method, c.role)).flatMap(c => ids.get(c.subject).map(k => (c, k)))

  /** Whether `above` is `below`, or sits above it by parent, in `project`. */
  private[plugin] def atOrAbove(
      project: Project,
      above: ArtifactKey,
      below: ArtifactKey
  ): Boolean = {
    @tailrec def climbs(k: Option[ArtifactKey]): Boolean = k match {
      case Some(at) if at == above => true
      case Some(at) => climbs(project.artifacts.find(_.head.key == at).flatMap(_.head.parent))
      case None => false
    }
    climbs(Some(below))
  }

  /** Each pair of `claims` for two holders whose artifacts overlap in `project`, the higher
    * first.
    */
  private[plugin] def overlapping(
      project: Project,
      claims: Vector[(claim: Claim, key: ArtifactKey)]
  ): Vector[((claim: Claim, key: ArtifactKey), (claim: Claim, key: ArtifactKey))] =
    for {
      upper <- claims
      lower <- claims
      if upper.claim.holder != lower.claim.holder && upper.key != lower.key &&
        atOrAbove(project, upper.key, lower.key)
    } yield (upper, lower)

  /** How `key` is named in `project`: its head's [[ArtifactHead.called]], or its id. */
  private[plugin] def calledIn(project: Project, key: ArtifactKey): String =
    project.artifacts.find(_.head.key == key).fold(key.value)(_.head.called)

  /** Of `entered`, artifacts of `project` each with when it entered its state, those no claim
    * among `live` holds or covers, and on which a claim as `role` for `holder` is [[uncovered]]:
    * the first to enter first, ties by number.
    */
  private[plugin] def oldestFirst(
      method: Method,
      project: Project,
      ids: Map[DocKey, ArtifactKey],
      live: Vector[Claim],
      role: ClaimRole,
      holder: ClaimHolder,
      entered: Vector[(ArtifactKey, Instant)]
  ): Vector[ArtifactKey] = {
    val held = live.flatMap(c => ids.get(c.subject)).toSet
    val covered = covering(method, live, ids).map(_.key)
    entered
      .filter((key, _) =>
        !held(key) && !covered.exists(atOrAbove(project, _, key)) &&
          uncovered(method, project, ids, live, key, role, holder).isRight
      )
      .sortBy((key, when) => (when, key.number))
      .map(_._1)
  }

  /** When an artifact whose kept versions are `kept`, newest first, each read by `read`, last
    * entered `state`: when the version that moved it there was written, or, when `kept` no
    * longer holds that version, the earliest it holds in `state`. A withdrawal, or a version
    * `read` refuses, counts as leaving `state`; `None` when the newest is not in it.
    */
  private[plugin] def entered(
      kept: Vector[DocumentVersion],
      read: Document -> Either[String, Artifact],
      state: StateName
  ): Option[Instant] =
    kept
      .takeWhile {
        case DocumentVersion.Holding(d) => read(d).exists(_.head.state == state)
        case DocumentVersion.Withdrawal(_, _) => false
      }
      .lastOption
      .collect { case DocumentVersion.Holding(d) => d.written }

  /** Nothing, or why a claim on `key` as `role` for `holder` may not be made in `project`: it
    * would cover, and another holder's covering claim among `live` overlaps it.
    */
  private[plugin] def uncovered(
      method: Method,
      project: Project,
      ids: Map[DocKey, ArtifactKey],
      live: Vector[Claim],
      key: ArtifactKey,
      role: ClaimRole,
      holder: ClaimHolder
  ): Either[Stop, Unit] =
    covering(method, live, ids)
      .filter(c => covers(method, role) && c.claim.holder != holder)
      .find(c => atOrAbove(project, c.key, key) || atOrAbove(project, key, c.key)) match {
      case None => Right(())
      case Some(other) =>
        val c = other.claim
        val side = if (atOrAbove(project, other.key, key)) "under" else "above"
        Left(
          Stop.Refused(
            s"${calledIn(project, key)} is $side ${calledIn(project, other.key)}, which is claimed " +
              s"as ${ClaimRole.value(c.role)} for ${ClaimHolder.value(c.holder)} by ${accounts(c.by)} " +
              s"since ${c.made}; that claim covers everything under it, and so would this one; " +
              s"${AnswerText.lapsing(c)}; project_release frees it"
          )
        )
    }

  /** Nothing, or why `after` may not be kept over `before`: in it two holders' covering claims
    * among `live` overlap that did not in `before`. An overlap `before` held already is let stand.
    */
  private[plugin] def stillUncovered(
      method: Method,
      before: Option[Project],
      after: Project,
      ids: Map[DocKey, ArtifactKey],
      live: Vector[Claim]
  ): Either[Stop, Unit] = {
    val claims = covering(method, live, ids)
    def keys(p: Project) = overlapping(p, claims).map((u, l) => (u.key, l.key)).toSet
    val had = before.fold(Set.empty)(keys)
    overlapping(after, claims).find((u, l) => !had((u.key, l.key))) match {
      case None => Right(())
      case Some((upper, lower)) =>
        def as(c: Claim) = s"as ${ClaimRole.value(c.role)} for ${ClaimHolder.value(c.holder)}"
        Left(
          Stop.Refused(
            s"${calledIn(after, lower.key)} would sit under ${calledIn(after, upper.key)}, and " +
              s"their covering claims, ${as(lower.claim)} and ${as(upper.claim)}, would overlap; " +
              releaseFirst("one")
          )
        )
    }
  }
}
