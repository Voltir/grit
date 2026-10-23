package grit.knowledge.drive

import grit.core.id.FileHandle
import grit.core.tool.Outcome

/** A Drive file as a read of it answered: its entry, the leading whole lines of its text the
  * answer gave, whether that is all of it (`complete`), and, when a format read it, the handle
  * the file area keeps all of it under (`file`).
  */
final case class DriveOpened(
    entry: DriveEntry,
    text: String,
    complete: Boolean,
    file: Option[FileHandle]
) extends caps.Pure

object DriveOpened {

  /** `stated`, a read's facts as Drive's tools write them (their form pinned by the goldens
    * under `test/resources/drive/facts`), or why not: no part or more than one, or its part
    * missing a key or giving it in another form, named with its key, or a `file` no handle.
    */
  def read(stated: Outcome.Stated): Either[String, DriveOpened] =
    stated.parts match {
      case Vector(part) =>
        val facts = part.json
        for {
          entry <- DriveEntry.read(facts, 1)
          o <- Fields.obj(facts, "part 1")
          text <- Fields.str(o, "text", "part 1")
          complete <- Fields.bool(o, "complete", "part 1")
          kept <- Fields.optStr(o, "file", "part 1")
          file <- kept match {
            case None => Right(None)
            case Some(h) =>
              FileHandle.read(h).map(Some(_)).toRight(s"part 1's `file` is no handle: $h")
          }
        } yield DriveOpened(entry, text, complete, file)
      case parts => Left(s"a read states one part, not ${parts.size}")
    }
}
