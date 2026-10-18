package grit.mcp.drive

import utest.*

/** [[DriveCapture]]: a re-capture records every field the client reads, and the recordings
  * hold each one Drive gives.
  */
object DriveCaptureTests extends TestSuite {

  /** `fields`' top-level names, a parenthesised group kept whole. */
  private def names(fields: String): Set[String] =
    fields.split(",(?![^()]*\\))").toSet

  /** The recordings of files: the searches' hits and each `files.get`. */
  private val Recordings: Vector[String] = Vector(
    "search-files",
    "search-folders",
    "search-fulltext",
    "metadata-drive-a",
    "metadata-drive-b",
    "metadata-file-in-folder",
    "metadata-file-top",
    "metadata-folder"
  )

  /** Whether Drive gives `field` to `file`, as its v3 reference says: `owners` only outside a
    * shared drive, `driveId` only within one, `parents` to all but a drive's root; the rest
    * always.
    */
  private def gives(field: String, file: ujson.Obj): Boolean = {
    def has(key: String) = file.value.contains(key)
    field match {
      case "owners" => !has("driveId")
      case "driveId" => !has("owners")
      case "parents" => file.value.get("id") != file.value.get("driveId")
      case _ => true
    }
  }

  val tests = Tests {
    test("a re-capture asks Drive for every field the client asks for") {
      assert(DriveCapture.SearchFields == DriveJson.SearchFields)
      assert(DriveCapture.DriveFields == DriveJson.DriveFields)
      assert(names(DriveJson.FileFields).diff(names(DriveCapture.MetadataFields)) == Set.empty)
    }

    test("every recorded file holds each field the client asks for that Drive gives it") {
      val asked = names(DriveJson.FileFields).map(_.takeWhile(_ != '('))
      val lacking = for {
        name <- Recordings
        recorded = Recorded.json(name)
        file <- recorded.obj
          .get("files")
          .flatMap(_.arrOpt)
          .map(_.toVector)
          .getOrElse(Vector(recorded))
        field <- asked.toVector.sorted
        if gives(field, file.obj) && !file.obj.contains(field)
      } yield s"$name: ${file.obj.get("id").flatMap(_.strOpt).getOrElse("?")} lacks $field"
      lacking ==> Vector()
    }
  }
}
