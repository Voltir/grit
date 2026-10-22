package grit.core.files

/** An [[Intake]] for an edge's tests that keep no file: every put refused as a machine that keeps
  * none would refuse it ([[FileError.Disk]]), reading nothing.
  */
final class NoIntake extends Intake {

  def keeps: scala.concurrent.duration.FiniteDuration = Files.Expiry

  def formats: Formats = PagedText.formats

  def put[A](
      key: PutKey,
      name: String,
      claimed: Option[String],
      origin: FileOrigin,
      item: FileItem,
      incoming: Incoming^
  )(body: OpenFile^ => A): Either[FileError, (FileRecord, A)] =
    Left(FileError.Disk("this test keeps no files"))
}
