package grit.mcp.drive

import java.time.Instant

import grit.core.identity.{Domain, Email}

/** What Drive says of one file or folder: `parent`, the folder or shared drive it lies in, when
  * Drive names one it shares with grit; `drive`, the shared drive holding it (`None` in a
  * person's My Drive); `owners`, empty in a shared drive; `changed`, when it last changed
  * (`modifiedTime`), when Drive says.
  */
final case class DriveFile(
    id: FileId,
    name: String,
    mimeType: String,
    parent: Option[FileId],
    drive: Option[FileId],
    owners: Vector[Email],
    changed: Option[Instant]
) {

  /** Where it lies: its parent, else its shared drive; `None` for a file with neither. */
  def within: Option[FileId] = parent.orElse(drive)

  def folder: Boolean = mimeType == DriveFile.Folder

  /** Its type in words a person reads: "a Doc", "a PDF", "a folder"; "a file of type …" for
    * one with no plainer name.
    */
  def kind: String = mimeType match {
    case DriveFile.Document => "a Doc"
    case DriveFile.Spreadsheet => "a Sheet"
    case DriveFile.Folder => "a folder"
    case "application/pdf" => "a PDF"
    case "application/vnd.google-apps.presentation" => "Slides"
    case other if other.startsWith("image/") => "an image"
    case other if other.startsWith("text/") => "a text file"
    case other => s"a file of type $other"
  }
}

object DriveFile {

  /** A folder's type, a shared drive's root's included. */
  val Folder: String = "application/vnd.google-apps.folder"

  /** A Google Doc's type. */
  val Document: String = "application/vnd.google-apps.document"

  /** A Google Sheet's type. */
  val Spreadsheet: String = "application/vnd.google-apps.spreadsheet"
}

/** A search's first page of hits, in Drive's order; `incomplete` when Drive has more than these
  * or searched only some of the drives shared with grit.
  */
final case class Hits(files: Vector[DriveFile], incomplete: Boolean)

/** A grant's role, as `permissions.list` names it. */
enum Role derives CanEqual {
  case Owner, Organizer, FileOrganizer, Writer, Commenter, Reader

  /** Whether it may change a file's text: owner, organizer, file organizer or writer. */
  def writes: Boolean = this match {
    case Owner | Organizer | FileOrganizer | Writer => true
    case Commenter | Reader => false
  }
}

/** One grant on a file, as `permissions.list` gives it. */
enum Grant derives CanEqual {

  /** To one account: a person's, or another service's. */
  case Person(email: Email, role: Role)

  /** To a group's address, whoever is in it. */
  case Group(email: Email, role: Role)

  /** To everyone in `domain`. */
  case Everyone(domain: Domain, role: Role)

  /** To anyone with the link. */
  case Anyone(role: Role)

  def role: Role
}
