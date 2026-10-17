package grit.mcp.drive

import utest.*

/** [[DriveCapture]]: a re-capture records every field the client reads. */
object DriveCaptureTests extends TestSuite {

  /** `fields`' top-level names, a parenthesised group kept whole. */
  private def names(fields: String): Set[String] =
    fields.split(",(?![^()]*\\))").toSet

  val tests = Tests {
    test("a re-capture asks Drive for every field the client asks for") {
      assert(DriveCapture.SearchFields == DriveJson.SearchFields)
      assert(DriveCapture.DriveFields == DriveJson.DriveFields)
      assert(names(DriveJson.FileFields).diff(names(DriveCapture.MetadataFields)) == Set.empty)
    }
  }
}
