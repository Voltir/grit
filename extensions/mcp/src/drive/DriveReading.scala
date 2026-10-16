package grit.mcp.drive

import grit.core.identity.{Domain, Email, Identities}
import grit.core.place.{Namespace, Place, Service}
import grit.core.visibility.Integrity

/** How a deployment's Drive at `service` is read (ADR 0036): `domain`, a domain the deployment
  * claims, whose addresses are its people; `self`, the deployment's own address, never counted
  * among a file's writers; `writers`, accounts that are not people (another service's), each
  * believed at its integrity when it may change a file.
  */
final case class DriveReading private (
    service: Service,
    domain: Domain,
    self: Email,
    writers: Map[Email, Integrity]
) {

  /** Where the files lying within `parent`, a folder or a shared drive, are kept:
    * `service:{name}/{parent}`.
    */
  def place(parent: FileId): Place =
    Place.under(Namespace.Service, Vector(service.name, FileId.value(parent)))
}

object DriveReading {

  /** These, or why not: `domain` not one `identities` claims; a writer in `domain`, whose
    * people are believed as people; or `self` among the writers.
    */
  def of(
      service: Service,
      domain: Domain,
      identities: Identities,
      self: Email,
      writers: Map[Email, Integrity]
  ): Either[String, DriveReading] = {
    val inDomain = writers.keys.filter(Email.domain(_) == domain).map(Email.value).toVector.sorted
    if (!identities.domains.contains(domain))
      Left(s"Drive's domain ${Domain.value(domain)} is not one the deployment claims")
    else if (inDomain.nonEmpty)
      Left(
        s"a Drive writer in ${Domain.value(domain)} is one of its people: ${inDomain.mkString(", ")}"
      )
    else if (writers.keys.exists(Email.value(_) == Email.value(self)))
      Left(s"the deployment's own address ${Email.value(self)} is never a Drive writer")
    else Right(DriveReading(service, domain, self, writers))
  }
}

/** A Drive tool's exchange, as [[DriveItems]] reads it. */
enum DriveCalled {

  /** A search and its hits. */
  case Searched(hits: Hits)

  /** A file read: what Drive says of it, its grants (`None` when they were not read whole), and
    * its text.
    */
  case Read(file: DriveFile, grants: Option[Vector[Grant]], text: String)
}
