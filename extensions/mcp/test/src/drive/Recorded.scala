package grit.mcp.drive

import java.nio.charset.StandardCharsets.UTF_8

import grit.core.identity.{Domain, Email, Identities}
import grit.core.place.Service
import grit.core.visibility.Integrity

/** Drive's recorded answers, `test/resources/drive/` (extensions/mcp/README.md). */
object Recorded {

  /** `drive/{name}` as recorded, read as UTF-8 (a byte-order mark kept). */
  def text(name: String): String = {
    val in = Option(getClass.getResourceAsStream(s"/drive/$name"))
      .getOrElse(throw new java.lang.AssertionError(s"no recording drive/$name"))
    try new String(in.readAllBytes(), UTF_8)
    finally in.close()
  }

  /** `drive/{name}.json` as JSON. */
  def json(name: String): ujson.Value = ujson.read(text(s"$name.json"))

  /** How the recordings are read: as a deployment of `service:drive` claiming `example.com`,
    * its own address `self@project.iam.example`, declaring `writer-bot-1@project.iam.example` a
    * writer at member.
    */
  val reading: DriveReading = readingAt(Integrity.Member)

  /** [[reading]], its writer declared at `writer` instead. */
  def readingAt(writer: Integrity): DriveReading = {
    def right[A](e: Either[String, A]): A =
      e.fold(why => throw new java.lang.AssertionError(why), identity)
    val example = right(Domain.of("example.com"))
    right(
      DriveReading
        .of(
          right(Service.of("drive")),
          example,
          right(Identities.of(Vector.empty, Set(example)).left.map(_.toString)),
          right(Email.of("self@project.iam.example")),
          Map(right(Email.of("writer-bot-1@project.iam.example")) -> writer)
        )
        .left
        .map(_.message)
    )
  }

  /** `id` as a [[FileId]], for an id a test names. */
  def id(id: String): FileId =
    FileId.of(id).fold(e => throw new java.lang.AssertionError(e), identity)
}
