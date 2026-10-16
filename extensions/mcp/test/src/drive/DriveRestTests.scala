package grit.mcp.drive

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
      "a 403 for a rate or quota limit is busy, as are a 429 and a 5xx; any other 403, and a 404, are hidden"
    ) {
      withFake { (fake, rest) =>
        def answered(status: Int, reason: String) = {
          fake.answering("files/file-1", status, error(status, reason))
          rest.file(file1)
        }
        Vector(
          answered(403, "userRateLimitExceeded"),
          answered(403, "dailyLimitExceeded"),
          answered(429, "rateLimitExceeded"),
          answered(503, "backendError"),
          answered(403, "insufficientFilePermissions"),
          answered(404, "notFound")
        ) ==> Vector(
          Left(DriveError.Busy(403)),
          Left(DriveError.Busy(403)),
          Left(DriveError.Busy(429)),
          Left(DriveError.Busy(503)),
          Left(DriveError.Hidden),
          Left(DriveError.Hidden)
        )
      }
    }

    test("a refused token is no token, and what the model reads of it names no setting") {
      withFake { (fake, rest) =>
        fake.answering("files/file-1", 401, error(401, "authError"))
        rest.file(file1).left.map(e => (e, e.message)) ==> Left(
          (
            DriveError.NoToken("Drive refused grit's token"),
            "grit could not get access to Drive just now; an administrator should check grit's Drive key."
          )
        )
      }
    }

    test("a file Drive will not export is unreadable, in words; a PDF is not asked for at all") {
      withFake { (fake, rest) =>
        fake.answering("files/file-1/export", 403, Recorded.text("error-export-folder.json"))
        val doc = rest.file(file1)
        val pdf = rest.file(Recorded.id("file-4"))
        val asked = fake.requests.size
        (
          doc.flatMap(rest.text).left.map(_.message),
          pdf.flatMap(rest.text).left.map(_.message),
          fake.requests.size - asked
        ) ==> (
          Left("grit can read Docs, Sheets and text files in Drive, not this file."),
          Left("grit can read Docs, Sheets and text files in Drive, not a PDF."),
          1
        )
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

    test("an answer longer than MaxRead keeps only its first whole lines within it") {
      withFake { (fake, rest) =>
        val line = "text text text text text text text\n"
        fake.answering("files/file-1/export", 200, line * (DriveRest.MaxRead / line.length + 10))
        rest
          .file(file1)
          .flatMap(rest.text)
          .map(t => (t.length <= DriveRest.MaxRead, t.endsWith("\n"), t.length % line.length)) ==>
          Right((true, true, 0))
      }
    }
  }
}
