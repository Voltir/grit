package grit.projects.artifact

import grit.core.document.Document
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
final case class Artifact(head: ArtifactHead, body: Body) extends caps.Pure {

  /** The artifacts it cites, in its head or its body, each once, its head's first. */
  def cites: Vector[ArtifactKey] = (head.cites ++ Body.cites(body)).distinct
}

object Artifact {

  /** `document` as an artifact, as [[ArtifactJson.write]] wrote it, or why not: its key is no
    * artifact's, or its data is not an artifact's head and stored body (a body
    * [[Body.fromStored]] refuses among them).
    */
  def of(document: Document): Either[String, Artifact] = ArtifactJson.read(document)
}
