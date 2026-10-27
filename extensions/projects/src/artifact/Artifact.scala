package grit.projects.artifact

import grit.projects.name.{ArtifactKey, KindName, StateName}

/** An artifact's head: everything but its body. */
final case class ArtifactHead(
    key: ArtifactKey,
    kind: KindName,
    state: StateName,
    title: String,
    parent: Option[ArtifactKey],
    cites: Vector[ArtifactKey],
    by: Option[String]
) extends caps.Pure

/** An artifact: its head and its body. */
final case class Artifact(head: ArtifactHead, body: Body) extends caps.Pure
