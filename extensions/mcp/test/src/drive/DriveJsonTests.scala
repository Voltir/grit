package grit.mcp.drive

import grit.core.identity.{Domain, Email}

import utest.*

/** [[DriveJson]] over Drive's recorded answers, and over the shapes they do not show. */
object DriveJsonTests extends TestSuite {

  private def email(e: String): Email =
    Email.of(e).fold(why => throw new java.lang.AssertionError(why), identity)

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private def file(fields: (String, ujson.Value)*): ujson.Obj =
    ujson.Obj.from(
      Vector[(String, ujson.Value)](
        "id" -> "file-1",
        "name" -> "name-1",
        "mimeType" -> "text/plain"
      ) ++ fields
    )

  val tests = Tests {
    test(
      "every recorded hit names its drive, and a shared drive's top file has the drive's id as its parent"
    ) {
      val hits = right(DriveJson.hits(Recorded.json("search-fulltext")))
      val top = hits.files.find(f => FileId.value(f.id) == "file-4")
      (
        hits.files.size,
        hits.files.forall(_.drive.nonEmpty),
        top.map(f => (f.parent, f.drive, f.mimeType)),
        hits.incomplete
      ) ==> (
        17,
        true,
        Some((Some(Recorded.id("drive-a")), Some(Recorded.id("drive-a")), "application/pdf")),
        false
      )
    }

    test("a drive's root reads as a folder with no parent, lying within its drive") {
      val root = right(DriveJson.file(Recorded.json("metadata-drive-a")))
      (root.folder, root.parent, root.within, root.owners) ==>
        (true, None, Some(Recorded.id("drive-a")), Vector.empty)
    }

    test(
      "a file's grants read every recorded grant, the account's own among them, each with its role"
    ) {
      DriveJson.grants(Recorded.json("permissions-drive-b")) ==> Right(
        Some(
          Vector(
            Grant.Person(email("person-1@example.com"), Role.Organizer),
            Grant.Person(email("person-2@example.com"), Role.Organizer),
            Grant.Person(email("person-3@example.com"), Role.Organizer),
            Grant.Person(email("writer-bot-1@project.iam.example"), Role.FileOrganizer),
            Grant.Person(email("self@project.iam.example"), Role.Commenter)
          )
        )
      )
    }

    test(
      "a group, a domain and anyone read as their own grants; a further page leaves them unread"
    ) {
      val listed = ujson.Arr(
        ujson.Obj("type" -> "group", "role" -> "writer", "emailAddress" -> "team@example.com"),
        ujson.Obj("type" -> "domain", "role" -> "reader", "domain" -> "example.com"),
        ujson.Obj("type" -> "anyone", "role" -> "commenter")
      )
      (
        DriveJson.grants(ujson.Obj("permissions" -> listed)),
        DriveJson.grants(ujson.Obj("permissions" -> listed, "nextPageToken" -> "p2"))
      ) ==> (
        Right(
          Some(
            Vector(
              Grant.Group(email("team@example.com"), Role.Writer),
              Grant.Everyone(right(Domain.of("example.com")), Role.Reader),
              Grant.Anyone(Role.Commenter)
            )
          )
        ),
        Right(None)
      )
    }

    test("a grant of a type or role Drive does not document is refused, not skipped") {
      val grants = (g: ujson.Obj) => DriveJson.grants(ujson.Obj("permissions" -> ujson.Arr(g)))
      (
        grants(ujson.Obj("type" -> "robot", "role" -> "writer")),
        grants(ujson.Obj("type" -> "anyone", "role" -> "editor"))
      ) ==> (
        Left("a grant's type robot is not one Drive documents"),
        Left("a grant's role editor is not one Drive documents")
      )
    }

    test("a search naming a next page, or saying it searched only some drives, is incomplete") {
      (
        DriveJson
          .hits(ujson.Obj("files" -> ujson.Arr(), "nextPageToken" -> "p2"))
          .map(_.incomplete),
        DriveJson
          .hits(ujson.Obj("files" -> ujson.Arr(), "incompleteSearch" -> true))
          .map(_.incomplete)
      ) ==> (Right(true), Right(true))
    }

    test("a search with one file not in Drive's form is refused whole, not read without it") {
      DriveJson.hits(
        ujson.Obj("files" -> ujson.Arr(file(), file("id" -> "file-2", "name" -> ujson.Num(2))))
      ) ==> Left("a file's name is not a string")
    }

    test("a search or a file's grants naming no list says so, apart from one not a list") {
      (
        DriveJson.hits(ujson.Obj()),
        DriveJson.hits(ujson.Obj("files" -> "file-1")),
        DriveJson.grants(ujson.Obj()),
        DriveJson.grants(ujson.Obj("permissions" -> "anyone"))
      ) ==> (
        Left("a search names no files"),
        Left("a search's files are not a list"),
        Left("a file's grants name no permissions"),
        Left("a file's permissions are not a list")
      )
    }

    test("a file naming no name says so, apart from one whose name is not a string") {
      DriveJson.file(ujson.Obj("id" -> "file-1", "mimeType" -> "text/plain")) ==>
        Left("a file names no name")
    }

    test("a file naming two parents, or an id holding a slash, is refused") {
      (
        DriveJson.file(file("parents" -> ujson.Arr("folder-1", "folder-2"))),
        DriveJson.file(file("id" -> "folder-1/x"))
      ) ==> (
        Left("a file names more than one parent"),
        Left("a Drive file's id holds only letters, digits, - and _, not folder-1/x")
      )
    }

    test(
      "a file says when it last changed, as Drive gives it; none when Drive does not, and a time in another form is refused"
    ) {
      (
        DriveJson.file(file("modifiedTime" -> "2026-10-01T09:30:15.123Z")).map(_.changed),
        DriveJson.file(file()).map(_.changed),
        DriveJson.file(file("modifiedTime" -> "yesterday")).map(_.changed),
        DriveJson.file(file("modifiedTime" -> 3)).map(_.changed)
      ) ==> (
        Right(Some(java.time.Instant.parse("2026-10-01T09:30:15.123Z"))),
        Right(None),
        Left("a file's modifiedTime is not a time"),
        Left("a file's modifiedTime is not a time")
      )
    }

    test("a file in My Drive reads its owners' addresses") {
      DriveJson
        .file(file("owners" -> ujson.Arr(ujson.Obj("emailAddress" -> "Person-1@Example.com"))))
        .map(_.owners) ==>
        Right(Vector(email("person-1@example.com")))
    }

    test(
      "a listing of shared drives names each by its id; a drive with no name, or none listed, is refused"
    ) {
      (
        DriveJson.drives(Recorded.json("drives-list")),
        DriveJson.drives(ujson.Obj("drives" -> ujson.Arr(ujson.Obj("id" -> "drive-a")))),
        DriveJson.drives(ujson.Obj("drives" -> "drive-a")),
        DriveJson.drives(ujson.Obj())
      ) ==> (
        Right(Map(Recorded.id("drive-a") -> "name-1", Recorded.id("drive-b") -> "name-2")),
        Left("a drive names no name"),
        Left("a listing's drives are not a list"),
        Right(Map.empty)
      )
    }

    test("an error answer's reasons are read in order") {
      (
        DriveJson.reasons(Recorded.json("error-export-folder")),
        DriveJson.reasons(Recorded.json("error-no-file"))
      ) ==>
        (Vector("fileNotExportable"), Vector("notFound"))
    }
  }
}
