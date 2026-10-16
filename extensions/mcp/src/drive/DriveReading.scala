package grit.mcp.drive

import grit.core.admin.PlaceNames
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

  /** How a person names, in a command, where a folder's or a shared drive's files are kept
    * ([[place]]): by its link as Drive gives it (`https://drive.google.com/drive/folders/{id}`,
    * with `/u/{n}/` before `folders` or not, and a query after or not; or
    * `https://drive.google.com/open?id={id}`), its scheme written or not; or by its id alone, of
    * at least [[DriveReading.ShortestId]] characters. A file's link, a shorter word and any
    * other site's link name none.
    */
  def names: PlaceNames = new PlaceNames {
    def place(word: String): Option[Place] = {
      val linked = word match {
        case DriveReading.FolderLink(id) => Some(id)
        case DriveReading.OpenLink(id) => Some(id)
        case id if id.length >= DriveReading.ShortestId => Some(id)
        case _ => None
      }
      linked.flatMap(id => FileId.of(id).toOption).map(DriveReading.this.place)
    }
  }
}

object DriveReading {

  /** The fewest characters an id read alone has: 19, a shared drive's, the shortest Drive
    * gives; so a word such as a compartment's name is never read as one.
    */
  val ShortestId: Int = 19

  /** A folder's or a shared drive's link, its id the group. */
  private val FolderLink =
    "(?:https?://)?drive\\.google\\.com/drive/(?:u/\\d+/)?folders/([A-Za-z0-9_-]+)/?(?:[?#].*)?".r

  /** Drive's older link to a file or folder by id, its id the group. */
  private val OpenLink = "(?:https?://)?drive\\.google\\.com/open\\?id=([A-Za-z0-9_-]+)(?:&.*)?".r

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
