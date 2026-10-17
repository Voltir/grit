package grit.mcp.drive

import grit.core.identity.{Domain, Email}

/** Drive's v3 REST answers as grit reads them, asked for with [[DriveJson]]'s fields; each `Left`
  * says in words what was not in the form Drive documents.
  */
object DriveJson {

  /** The fields a file is asked for with. */
  val FileFields: String = "id,name,mimeType,parents,driveId,owners(emailAddress)"

  /** The fields a search is asked for with. */
  val SearchFields: String = s"nextPageToken,incompleteSearch,files($FileFields)"

  /** The fields shared drives are listed with. */
  val DriveFields: String = "nextPageToken,drives(id,name)"

  /** A `drives.list` answer: each drive's name, by its id; a drive on a later page is not read.
    */
  def drives(answer: ujson.Value): Either[String, Map[FileId, String]] =
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
          drive <- obj(d, "a drive")
          id <- str(drive, "id", "a drive").flatMap(FileId.of)
          name <- str(drive, "name", "a drive")
        } yield kept + (id -> name)
      }
    } yield named

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
      id <- str(o, "id", "a file").flatMap(FileId.of)
      name <- str(o, "name", "a file")
      mimeType <- str(o, "mimeType", "a file")
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
                obj(owner, "an owner")
                  .flatMap(str(_, "emailAddress", "an owner"))
                  .flatMap(Email.of)
                  .map(kept :+ _)
              )
          }
        case Some(_) => Left("a file's owners are not a list")
      }
    } yield DriveFile(id, name, mimeType, parent, drive, owners)

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

  /** The reasons an error answer gives (`error.errors[].reason`), in order; none when it names
    * none.
    */
  def reasons(answer: ujson.Value): Vector[String] =
    answer.objOpt
      .flatMap(_.get("error"))
      .flatMap(_.objOpt)
      .flatMap(_.get("errors"))
      .flatMap(_.arrOpt)
      .toVector
      .flatten
      .flatMap(_.objOpt.flatMap(_.get("reason")).flatMap(_.strOpt))

  private def grant(answer: ujson.Value): Either[String, Grant] =
    for {
      o <- obj(answer, "a grant")
      kind <- str(o, "type", "a grant")
      role <- str(o, "role", "a grant").flatMap(role)
      read <- kind match {
        case "user" =>
          str(o, "emailAddress", "a user's grant").flatMap(Email.of).map(Grant.Person(_, role))
        case "group" =>
          str(o, "emailAddress", "a group's grant").flatMap(Email.of).map(Grant.Group(_, role))
        case "domain" =>
          str(o, "domain", "a domain's grant").flatMap(Domain.of).map(Grant.Everyone(_, role))
        case "anyone" => Right(Grant.Anyone(role))
        case other => Left(s"a grant's type $other is not one Drive documents")
      }
    } yield read

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

  private def str(
      o: collection.Map[String, ujson.Value],
      key: String,
      what: String
  ): Either[String, String] =
    o.get(key) match {
      case None => Left(s"$what names no $key")
      case Some(v) => v.strOpt.toRight(s"$what's $key is not a string")
    }
}
