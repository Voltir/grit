package grit.app.main

import scala.concurrent.duration.DurationInt

import grit.core.files.Files

import utest.*

/** How long the file area keeps a file (`GRIT_FILES_EXPIRY`). */
object FilesExpiryTests extends TestSuite {

  val tests = Tests {
    test(
      "GRIT_FILES_EXPIRY: unset is the area's default; a duration of at least a minute, or an error"
    ) {
      (
        Main.filesExpiry(Map.empty),
        Main.filesExpiry(Map("GRIT_FILES_EXPIRY" -> "10m")),
        Main.filesExpiry(Map("GRIT_FILES_EXPIRY" -> "30s")),
        Main.filesExpiry(Map("GRIT_FILES_EXPIRY" -> "10"))
      ) ==> (
        Right(Files.Expiry),
        Right(10.minutes),
        Left("GRIT_FILES_EXPIRY must be at least a minute"),
        Left(
          "GRIT_FILES_EXPIRY: not a duration: write a whole number and s, m, h or d, as 30s or 3m"
        )
      )
    }
  }
}
