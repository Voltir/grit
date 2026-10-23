package grit.mcp.drive

import java.time.Instant
import java.time.format.DateTimeParseException

import grit.core.identity.{Domain, Email}
import grit.core.json.Fields

/** Drive's v3 REST answers as grit reads them, asked for with [[DriveJson]]'s fields; each `Left`
  * says in words what was not in the form Drive documents.
  */
object DriveJson {

  /** The fields a file is asked for with. */
  val FileFields: String =
    "id,name,mimeType,parents,driveId,owners(emailAddress),modifiedTime,size,version"

  /** The fields a search is asked for with. */
  val SearchFields: String = s"nextPageToken,incompleteSearch,files($FileFields)"

  /** The fields shared drives are listed with. */
  val DriveFields: String = "nextPageToken,drives(id,name)"

  /** A `drives.list` answer: each drive's name, by its id; incomplete when it names a next page,
    * which is not read.
    */
  def drives(answer: ujson.Value): Either[String, SharedDrives] =
    for {
      o <- obj(answer, "a list of drives")
      listed <- o.get("drives") match {
        case None => Right(Vector.empty)
        case Some(ujson.Arr(ds)) => Right(ds.toVector)
        case Some(_) => Left("a listing's drives are not a list")
      }
      named <- listed.foldLeft[Either[String, Map[FileId, String]]](Right(Map.empty)) { (read, d) =>
        for {
          kept <- read
          id <- Fields("a drive", d).str("id").flatMap(FileId.of)
          name <- Fields("a drive", d).str("name")
        } yield kept + (id -> name)
      }
    } yield SharedDrives(named, incomplete = o.contains("nextPageToken"))

  /** The fields a file's grants are asked for with. */
  val GrantFields: String = "nextPageToken,permissions(type,role,emailAddress,domain)"

  /** A `files.list` answer: incomplete when it names a next page or `incompleteSearch` true. */
  def hits(answer: ujson.Value): Either[String, Hits] =
    for {
      o <- obj(answer, "a search")
      files <- o.get("files").map(_.arrOpt) match {
        case None => Left("a search names no files")
        case Some(None) => Left("a search's files are not a list")
        case Some(Some(fs)) =>
          fs.toVector.foldLeft[Either[String, Vector[DriveFile]]](Right(Vector.empty)) {
            (read, f) => read.flatMap(kept => file(f).map(kept :+ _))
          }
      }
      incomplete <- o.get("incompleteSearch") match {
        case None => Right(false)
        case Some(ujson.Bool(b)) => Right(b)
        case Some(_) => Left("a search's incompleteSearch is not a boolean")
      }
    } yield Hits(files, incomplete || o.contains("nextPageToken"))

  /** A `files.get` answer, or one of a search's files: its first parent, when it names one, and
    * refused when it names more than one.
    */
  def file(answer: ujson.Value): Either[String, DriveFile] =
    for {
      o <- obj(answer, "a file")
      f = Fields("a file", answer)
      id <- f.str("id").flatMap(FileId.of)
      name <- f.str("name")
      mimeType <- f.str("mimeType")
      parent <- o.get("parents") match {
        case None => Right(None)
        case Some(ujson.Arr(ps)) =>
          ps.toVector match {
            case Vector() => Right(None)
            case Vector(ujson.Str(p)) => FileId.of(p).map(Some(_))
            case Vector(_) => Left("a file's parent is not a string")
            case _ => Left("a file names more than one parent")
          }
        case Some(_) => Left("a file's parents are not a list")
      }
      drive <- o.get("driveId") match {
        case None => Right(None)
        case Some(ujson.Str(d)) => FileId.of(d).map(Some(_))
        case Some(_) => Left("a file's driveId is not a string")
      }
      owners <- o.get("owners") match {
        case None => Right(Vector.empty)
        case Some(ujson.Arr(os)) =>
          os.toVector.foldLeft[Either[String, Vector[Email]]](Right(Vector.empty)) {
            (read, owner) =>
              read.flatMap(kept =>
                Fields("an owner", owner)
                  .str("emailAddress")
                  .flatMap(Email.of)
                  .map(kept :+ _)
              )
          }
        case Some(_) => Left("a file's owners are not a list")
      }
      changed <- o.get("modifiedTime") match {
        case None => Right(None)
        case Some(ujson.Str(t)) =>
          try Right(Some(Instant.parse(t)))
          catch { case _: DateTimeParseException => Left("a file's modifiedTime is not a time") }
        case Some(_) => Left("a file's modifiedTime is not a time")
      }
      // Drive writes a size as a string of digits, and gives none for its own apps' files.
      size <- o.get("size") match {
        case None => Right(None)
        case Some(ujson.Str(s)) =>
          s.toLongOption.filter(_ >= 0).map(Some(_)).toRight("a file's size is not a number")
        case Some(_) => Left("a file's size is not a number")
      }
      // A string of digits too, an int64.
      version <- o.get("version") match {
        case None => Right(None)
        case Some(ujson.Str(v)) =>
          v.toLongOption.filter(_ >= 0).map(Some(_)).toRight("a file's version is not a number")
        case Some(_) => Left("a file's version is not a number")
      }
    } yield DriveFile(id, name, mimeType, parent, drive, owners, changed, size, version)

  /** A `permissions.list` answer's grants; `None` when it names a next page, so it was not read
    * whole.
    */
  def grants(answer: ujson.Value): Either[String, Option[Vector[Grant]]] =
    for {
      o <- obj(answer, "a file's grants")
      listed <- o.get("permissions") match {
        case None => Left("a file's grants name no permissions")
        case Some(ps) => ps.arrOpt.toRight("a file's permissions are not a list")
      }
      read <- listed.toVector.foldLeft[Either[String, Vector[Grant]]](Right(Vector.empty)) {
        (read, g) => read.flatMap(kept => grant(g).map(kept :+ _))
      }
    } yield Option.unless(o.contains("nextPageToken"))(read)

  /** The reasons an error answer gives, in order: Drive's (`error.errors[].reason`), then
    * Google's newer form's (`error.details[].reason`); none when it names none.
    */
  def reasons(answer: ujson.Value): Vector[String] = {
    val error = answer.objOpt.flatMap(_.get("error")).flatMap(_.objOpt)
    Vector("errors", "details").flatMap(key =>
      error
        .flatMap(_.get(key))
        .flatMap(_.arrOpt)
        .toVector
        .flatten
        .flatMap(_.objOpt.flatMap(_.get("reason")).flatMap(_.strOpt))
    )
  }

  private def grant(answer: ujson.Value): Either[String, Grant] = {
    def str(k: String, what: String) = Fields(what, answer).str(k)
    for {
      kind <- str("type", "a grant")
      role <- str("role", "a grant").flatMap(role)
      read <- kind match {
        case "user" =>
          str("emailAddress", "a user's grant").flatMap(Email.of).map(Grant.Person(_, role))
        case "group" =>
          str("emailAddress", "a group's grant").flatMap(Email.of).map(Grant.Group(_, role))
        case "domain" =>
          str("domain", "a domain's grant").flatMap(Domain.of).map(Grant.Everyone(_, role))
        case "anyone" => Right(Grant.Anyone(role))
        case other => Left(s"a grant's type $other is not one Drive documents")
      }
    } yield read
  }

  private def role(name: String): Either[String, Role] = name match {
    case "owner" => Right(Role.Owner)
    case "organizer" => Right(Role.Organizer)
    case "fileOrganizer" => Right(Role.FileOrganizer)
    case "writer" => Right(Role.Writer)
    case "commenter" => Right(Role.Commenter)
    case "reader" => Right(Role.Reader)
    case other => Left(s"a grant's role $other is not one Drive documents")
  }

  private def obj(
      v: ujson.Value,
      what: String
  ): Either[String, collection.Map[String, ujson.Value]] =
    v.objOpt.toRight(s"$what is not a JSON object")
}
