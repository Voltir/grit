package grit.mcp.client

import java.nio.file.{Files, NoSuchFileException, Path}

import scala.util.control.NonFatal

/** The files a [[Credential]] reads a key from: a service account's, at the path its variable
  * names.
  */
trait KeyFiles {

  /** The text of the file at `path`, or why not, never quoting it: "no file is at its path",
    * or "its file cannot be read" naming the cause's kind.
    */
  def read(path: Path): Either[String, String]
}

object KeyFiles {

  /** This machine's files, read as UTF-8. */
  object Local extends KeyFiles {
    def read(path: Path): Either[String, String] =
      try Right(Files.readString(path))
      catch {
        case _: NoSuchFileException => Left("no file is at its path")
        case NonFatal(e) => Left(s"its file cannot be read (${e.getClass.getSimpleName})")
      }
  }
}
