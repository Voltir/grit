package grit.mcp.drive

import grit.core.files.Unread
import grit.core.identity.Email

import utest.*

/** [[DriveRest]] against [[FakeDriveServer]]: what it sends, and how it reads Drive's failures. */
object DriveRestTests extends TestSuite {

  private def withFake[A](body: (FakeDriveServer, DriveRest^) => A): A = {
    val fake = FakeDriveServer.start()
    try body(fake, fake.rest())
    finally fake.stop()
  }

  private def error(status: Int, reason: String): String =
    ujson.write(
      ujson.Obj(
        "error" -> ujson.Obj("code" -> status, "errors" -> ujson.Arr(ujson.Obj("reason" -> reason)))
      )
    )

  private val file1 = Recorded.id("file-1")

  val tests = Tests {
    test(
      "a search's words are quoted so they cannot widen it, and a folder narrows it to that folder's files"
    ) {
      withFake { (fake, rest) =>
        val _ = rest.search("it's a \\ test", Some(Recorded.id("folder-3")))
        fake.requests.lastOption.map((path, q) =>
          (
            path,
            q.get("q"),
            q.get("corpora"),
            q.get("includeItemsFromAllDrives"),
            q.get("pageSize"),
            q.get("fields")
          )
        ) ==> Some(
          (
            "files",
            Some(
              "fullText contains 'it\\'s a \\\\ test' and trashed = false and 'folder-3' in parents"
            ),
            Some("allDrives"),
            Some("true"),
            Some("20"),
            Some(DriveJson.SearchFields)
          )
        )
      }
    }

    test(
      "a listing asks for one folder's own files, none in the trash, a page of ListMost, folders first; a next page makes it incomplete"
    ) {
      withFake { (fake, rest) =>
        val whole = rest.children(Recorded.id("folder-3")).map(_.incomplete)
        val asked = fake.requests.lastOption.map((path, q) =>
          (
            path,
            q.get("q"),
            q.get("corpora"),
            q.get("includeItemsFromAllDrives"),
            q.get("pageSize"),
            q.get("orderBy"),
            q.get("fields")
          )
        )
        val more = ujson.write(
          ujson.Obj.from(
            Recorded.json("made-children-folder-3").obj.toSeq :+ ("nextPageToken" -> ujson.Str("t"))
          )
        )
        fake.answering("files", 200, more)
        (whole, asked, rest.children(Recorded.id("folder-3")).map(_.incomplete)) ==> (
          Right(false),
          Some(
            (
              "files",
              Some("'folder-3' in parents and trashed = false"),
              Some("allDrives"),
              Some("true"),
              Some("100"),
              Some("folder,name"),
              Some(DriveJson.SearchFields)
            )
          ),
          Right(true)
        )
      }
    }

    test(
      "a 403 for a rate or quota limit is busy, as are a 429 and a 5xx; any other 403, and a 404, are hidden"
    ) {
      withFake { (fake, rest) =>
        def answered(status: Int, reason: String) = {
          fake.answering("files/file-1", status, error(status, reason))
          rest.file(file1)
        }
        Vector(
          answered(403, "userRateLimitExceeded"),
          answered(403, "rateLimitExceeded"),
          answered(403, "dailyLimitExceeded"),
          answered(403, "quotaExceeded"),
          answered(429, "rateLimitExceeded"),
          answered(503, "backendError"),
          answered(403, "insufficientFilePermissions"),
          answered(404, "notFound")
        ) ==> Vector(
          Left(DriveError.Busy(403)),
          Left(DriveError.Busy(403)),
          Left(DriveError.Busy(403)),
          Left(DriveError.Busy(403)),
          Left(DriveError.Busy(429)),
          Left(DriveError.Busy(503)),
          Left(DriveError.Hidden),
          Left(DriveError.Hidden)
        )
      }
    }

    test(
      "shared drives are listed a page of 100 at a time, asking supportsAllDrives only of files"
    ) {
      withFake { (fake, rest) =>
        val _ = rest.drives()
        val _ = rest.file(Recorded.id("file-1"))
        fake.requests.map((path, q) => (path, q - "fields")) ==> Vector(
          "drives" -> Map("pageSize" -> "100"),
          "files/file-1" -> Map("supportsAllDrives" -> "true")
        )
      }
    }

    test("a refused token is no token, and what the model reads of it names no setting") {
      withFake { (fake, rest) =>
        fake.answering("files/file-1", 401, error(401, "authError"))
        rest.file(file1).left.map(e => (e, e.message)) ==> Left(
          (
            DriveError.NoToken("Drive refused the token it was given"),
            "Drive access isn't working just now; tell an administrator."
          )
        )
      }
    }

    test("a file Drive will not export is unreadable, in words; a PDF's text is not asked for") {
      withFake { (fake, rest) =>
        fake.answering("files/file-1/export", 403, Recorded.text("error-export-folder.json"))
        val doc = rest.file(file1)
        val pdf = rest.file(Recorded.id("file-4"))
        val asked = fake.requests.size
        (
          doc.flatMap(rest.text).left.map(_.message),
          pdf.flatMap(rest.text),
          fake.requests.size - asked
        ) ==> (
          Left(
            "Docs, Sheets, PDFs, Word and Excel files, and text files in Drive can be read " +
              "here, not this file."
          ),
          Left(DriveError.Unreadable("a PDF")),
          1
        )
      }
    }

    test("a file Drive says is larger than read here is refused before its bytes are asked for") {
      withFake { (fake, rest) =>
        val big = rest.file(Recorded.id("file-4")).map(_.copy(size = Some(30L << 20)))
        val asked = fake.requests.size
        (
          big.flatMap(rest.media(_, 20L << 20)(_.read())),
          fake.requests.drop(asked)
        ) ==> (Left(DriveError.Unread(Unread.TooLarge(30L << 20, 20L << 20))), Vector())
      }
    }

    test("a file's bytes are streamed whole, one of no size Drive gives past the most read too") {
      withFake { (fake, rest) =>
        val content = Vector.tabulate(3000)(i => (i % 256).toByte)
        fake.answeringBytes("files/file-4?alt=media", 200, content)
        val file = rest.file(Recorded.id("file-4")).map(_.copy(size = None))
        file.flatMap(rest.media(_, 1000)(_.readAllBytes().toVector == content)) ==> Right(true)
      }
    }

    test("a file Drive has flagged as possibly harmful is refused in words") {
      withFake { (fake, rest) =>
        fake.answering("files/file-4?alt=media", 403, error(403, "cannotDownloadAbusiveFile"))
        rest
          .file(Recorded.id("file-4"))
          .flatMap(rest.media(_, 1L << 20)(_.read()))
          .left
          .map(_.message) ==>
          Left("Drive has flagged this file as possibly harmful, so it isn't read here.")
      }
    }

    test("a Doc's export loses its byte-order mark, and nothing else") {
      withFake { (fake, rest) =>
        val recorded = Recorded.text("export-doc.txt")
        fake.answering("files/file-1/export", 200, recorded)
        (recorded.startsWith("\uFEFF"), rest.file(file1).flatMap(rest.text)) ==>
          (true, Right(recorded.drop(1)))
      }
    }

    test("a text file's content is read as it is, its byte-order mark dropped") {
      withFake { (fake, rest) =>
        val content = Recorded.text("made-file-text.txt")
        (
          content.startsWith("\uFEFF"),
          rest.file(Recorded.id("file-text")).flatMap(rest.text),
          fake.requests.lastOption.map((path, q) => (path, q.get("alt")))
        ) ==> (true, Right(content.drop(1)), Some(("files/file-text", Some("media"))))
      }
    }

    test("an answer longer than MaxRead keeps only its first whole lines within it") {
      withFake { (fake, rest) =>
        val line = "text text text text text text text\n"
        fake.answering("files/file-1/export", 200, line * (DriveRest.MaxRead / line.length + 10))
        // One byte a character: the most whole lines within MaxRead bytes.
        rest.file(file1).flatMap(rest.text).map(t => (t.length, t.endsWith("\n"))) ==>
          Right(((DriveRest.MaxRead / line.length) * line.length, true))
      }
    }

    test(
      "over the recordings, a search's hits, the shared drives and a file's grants are what Drive answered"
    ) {
      withFake { (_, rest) =>
        def email(e: String) =
          Email.of(e).fold(w => throw new java.lang.AssertionError(w), identity)
        (
          rest.search("text", None).map(h => (h.files.map(f => FileId.value(f.id)), h.incomplete)),
          rest
            .drives()
            .map(d => (d.names.map((id, name) => (FileId.value(id), name)), d.incomplete)),
          rest.grants(file1)
        ) ==> (
          Right(
            (
              Vector(13, 4, 2, 14, 18, 7, 6, 10, 15, 16, 12, 9, 8, 3, 5, 11, 1).map(n =>
                s"file-$n"
              ),
              false
            )
          ),
          Right((Map("drive-a" -> "name-1", "drive-b" -> "name-2"), false)),
          Right(
            Some(
              Vector(
                Grant.Person(email("person-1@example.com"), Role.Organizer),
                Grant.Person(email("person-2@example.com"), Role.Organizer),
                Grant.Person(email("person-3@example.com"), Role.Organizer),
                Grant.Person(email("self@project.iam.example"), Role.Writer)
              )
            )
          )
        )
      }
    }
  }
}
