package grit.mcp.drive

import utest.*

/** [[Neutral]]: a Drive answer's stand-ins, what is kept, and what is replaced. */
object NeutralTests extends TestSuite {

  private val Folder = "1FolderIdOfTwentyFiveChars"
  private val Drive = "0ADriveIdOfNineteen"
  private val Doc = "1DocIdOfTwentyFiveCharsXY"

  private def neutral = new Neutral(
    domain = "acme.test",
    self = "reader@proj.iam.gserviceaccount.com",
    writers = Set("engine@proj.iam.gserviceaccount.com"),
    folders = Set(Folder),
    drives = Vector(Drive)
  )

  /** A `tools/call` result whose text block is `inner`. */
  private def answer(inner: ujson.Value): ujson.Obj =
    ujson.Obj(
      "content" -> ujson.Arr(ujson.Obj("type" -> "text", "text" -> ujson.write(inner))),
      "isError" -> false
    )

  /** The text block of `result`, read back as JSON. */
  private def inner(result: ujson.Obj): ujson.Value = ujson.read(result("content")(0)("text").str)

  val tests = Tests {
    test("ids, titles and text become stand-ins, one per value, and types, times and shape stay") {
      val files = ujson.Obj(
        "files" -> ujson.Arr(
          ujson.Obj(
            "id" -> Folder,
            "title" -> "Layoffs 2027",
            "mimeType" -> "application/vnd.google-apps.folder",
            "parentId" -> Drive,
            "modifiedTime" -> "2026-10-01T12:00:00.000Z"
          ),
          ujson.Obj(
            "id" -> Doc,
            "title" -> "Layoffs 2027",
            "mimeType" -> "application/vnd.google-apps.document",
            "parentId" -> Folder,
            "contentSnippet" -> "secret words",
            "webViewLink" -> s"https://docs.google.com/document/d/$Doc/edit",
            "size" -> 42
          )
        )
      )
      val out = neutral.result(answer(files))
      assert(out("isError") == ujson.False)
      assert(out("content")(0)("type").str == "text")
      assert(
        inner(out) == ujson.Obj(
          "files" -> ujson.Arr(
            ujson.Obj(
              "id" -> "folder-1",
              "title" -> "title-1",
              "mimeType" -> "application/vnd.google-apps.folder",
              "parentId" -> "drive-a",
              "modifiedTime" -> "2026-10-01T12:00:00.000Z"
            ),
            ujson.Obj(
              "id" -> "file-1",
              "title" -> "title-1",
              "mimeType" -> "application/vnd.google-apps.document",
              "parentId" -> "folder-1",
              "contentSnippet" -> "contentSnippet-1",
              "webViewLink" -> "webViewLink-1",
              "size" -> 42
            )
          )
        )
      )
    }
    test("a file's size, a string of digits as Drive writes it, stays") {
      val files = ujson.Obj("files" -> ujson.Arr(ujson.Obj("id" -> Doc, "size" -> "12345")))
      inner(neutral.result(answer(files))) ==>
        ujson.Obj("files" -> ujson.Arr(ujson.Obj("id" -> "file-1", "size" -> "12345")))
    }

    test("addresses become stand-ins by whose they are, and a role or a kind of grant stays") {
      val permissions = ujson.Obj(
        "permissions" -> ujson.Arr(
          ujson.Obj(
            "id" -> "01234567890123456789",
            "role" -> "organizer",
            "type" -> "user",
            "emailAddress" -> "jane@acme.test",
            "displayName" -> "Jane Doe"
          ),
          ujson.Obj("role" -> "writer", "type" -> "user", "emailAddress" -> "x@gmail.com"),
          ujson.Obj(
            "role" -> "writer",
            "type" -> "user",
            "emailAddress" -> "reader@proj.iam.gserviceaccount.com"
          ),
          ujson.Obj(
            "role" -> "fileOrganizer",
            "type" -> "user",
            "emailAddress" -> "engine@proj.iam.gserviceaccount.com"
          ),
          ujson.Obj(
            "role" -> "reader",
            "type" -> "user",
            "emailAddress" -> "other@other.iam.gserviceaccount.com"
          ),
          ujson.Obj("role" -> "commenter", "type" -> "user", "emailAddress" -> "j@sub.acme.test"),
          ujson.Obj("role" -> "reader", "type" -> "domain", "domain" -> "acme.test"),
          ujson.Obj("role" -> "organizer", "type" -> "user", "emailAddress" -> "jane@acme.test")
        )
      )
      val out = inner(neutral.result(answer(permissions)))
      assert(
        out == ujson.Obj(
          "permissions" -> ujson.Arr(
            ujson.Obj(
              "id" -> "permission-1",
              "role" -> "organizer",
              "type" -> "user",
              "emailAddress" -> "person-1@example.com",
              "displayName" -> "displayName-1"
            ),
            ujson.Obj(
              "role" -> "writer",
              "type" -> "user",
              "emailAddress" -> "outside-1@gmail.example"
            ),
            ujson.Obj(
              "role" -> "writer",
              "type" -> "user",
              "emailAddress" -> "self@project.iam.example"
            ),
            ujson.Obj(
              "role" -> "fileOrganizer",
              "type" -> "user",
              "emailAddress" -> "writer-bot-1@project.iam.example"
            ),
            ujson.Obj(
              "role" -> "reader",
              "type" -> "user",
              "emailAddress" -> "service-1@project.iam.example"
            ),
            ujson.Obj(
              "role" -> "commenter",
              "type" -> "user",
              "emailAddress" -> "outside-2@elsewhere.example"
            ),
            ujson.Obj("role" -> "reader", "type" -> "domain", "domain" -> "example.com"),
            ujson.Obj(
              "role" -> "organizer",
              "type" -> "user",
              "emailAddress" -> "person-1@example.com"
            )
          )
        )
      )
    }
    test("a text block that is not JSON, and a string of no known shape, are replaced whole") {
      val result = ujson.Obj(
        "content" -> ujson.Arr(ujson.Obj("type" -> "text", "text" -> "Dear Jane, the plan.")),
        "structuredContent" -> ujson.Obj("fileContent" -> "the plan", "note" -> "Acme Q3 2026")
      )
      val out = neutral.result(result)
      assert(
        out == ujson.Obj(
          "content" -> ujson.Arr(ujson.Obj("type" -> "text", "text" -> "text-1")),
          "structuredContent" -> ujson.Obj("fileContent" -> "fileContent-1", "note" -> "note-1")
        )
      )
    }
    test(
      "an exported text keeps its lines and marks, each word replaced, an address and a URL too"
    ) {
      val exported =
        "# Layoffs 2027\n\n| who | when |\n|---|---|\nsee jane@acme.test, https://x.io/a?b=1\n"
      assert(
        neutral.body(exported) ==
          "# text text\n\n| text | text |\n|---|---|\ntext text@text.text, text://text.text/text?text=text\n"
      )
    }
    test("an error's reason and its domain of reasons stay, and its message is replaced") {
      val error = ujson.Obj(
        "error" -> ujson.Obj(
          "code" -> 403,
          "message" -> s"Export only supports Docs Editors files: $Doc",
          "errors" -> ujson.Arr(
            ujson.Obj(
              "domain" -> "global",
              "reason" -> "fileNotExportable",
              "message" -> "Export only"
            )
          )
        )
      )
      assert(
        neutral.result(error) == ujson.Obj(
          "error" -> ujson.Obj(
            "code" -> 403,
            "message" -> "message-1",
            "errors" -> ujson.Arr(
              ujson.Obj(
                "domain" -> "global",
                "reason" -> "fileNotExportable",
                "message" -> "message-2"
              )
            )
          )
        )
      )
    }
  }
}
