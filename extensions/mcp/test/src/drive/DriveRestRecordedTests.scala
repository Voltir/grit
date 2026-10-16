package grit.mcp.drive

import grit.core.identity.Email

/** [[DriveRestContract]] over Drive's recorded answers (extensions/mcp/README.md), served by
  * [[FakeDriveServer]].
  */
object DriveRestRecordedTests extends DriveRestContract {

  private val fake = FakeDriveServer.start()

  override def utestAfterAll(): Unit = fake.stop()

  protected def rest: DriveRest^ = fake.rest()
  protected def words: String = "text"
  protected def topFile: (FileId, FileId) = (Recorded.id("file-4"), Recorded.id("drive-a"))
  protected def folderFile: (FileId, FileId) = (Recorded.id("file-1"), Recorded.id("folder-3"))
  protected def doc: FileId = Recorded.id("file-1")
  protected def sheet: FileId = Recorded.id("file-11")
  protected def folder: FileId = Recorded.id("folder-2")
  protected def missing: FileId = Recorded.id("file-404")
  protected def self: Email =
    Email.of("self@project.iam.example").fold(e => throw new java.lang.AssertionError(e), identity)
}
