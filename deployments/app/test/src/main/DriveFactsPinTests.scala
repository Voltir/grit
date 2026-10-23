package grit.app.main

import java.nio.charset.StandardCharsets.UTF_8

import grit.core.clock.Clock
import grit.core.files.{FileRange, FileRead, Files, Kind, Unread}
import grit.core.id.FileHandle
import grit.core.tool.Outcome
import grit.mcp.drive.{DriveCalled, DriveError, DriveItems, DriveRest, FakeDriveServer, Recorded}

import utest.*

/** Drive's facts as a deployment's knowledge plugin reads them: what grit.mcp's `DriveItems`
  * answers over Drive's recorded answers, held equal to the goldens knowledge's own tests read
  * (`test/resources/drive/facts`), since knowledge may not name grit.mcp. Each golden is what a
  * reader keeping every part states: `{"answer"?:…,"parts":[…]}`.
  */
object DriveFactsPinTests extends TestSuite {

  private def golden(name: String): ujson.Value = {
    val in = Option(getClass.getResourceAsStream(s"/drive/facts/$name.json"))
      .getOrElse(throw new java.lang.AssertionError(s"no golden drive/facts/$name.json"))
    try ujson.read(new String(in.readAllBytes(), UTF_8))
    finally in.close()
  }

  /** The facts of `called`'s answer, every part kept. */
  private def stated(called: Either[?, DriveCalled]): ujson.Value =
    called.flatMap(DriveItems.answer(Recorded.reading, _)) match {
      case Right(Outcome.Done(_, _, _, parts, facts)) =>
        val o = ujson.Obj()
        facts.foreach(f => o("answer") = f.json)
        o("parts") = ujson.Arr.from(parts.toVector.flatten.flatMap(_.facts).map(_.json))
        o
      case other => throw new java.lang.AssertionError(other.toString)
    }

  private def over[A](body: DriveRest^ => A): A = {
    val fake = FakeDriveServer.start()
    try body(fake.rest(Clock.system()))
    finally fake.stop()
  }

  val tests = Tests {
    test("each golden of Drive's facts is what Drive's tools answer over its recordings") {
      over { rest =>
        val read = for {
          file <- rest.file(Recorded.id("file-text"))
          text <- rest.text(file)
          grants <- rest.grants(file.id)
        } yield DriveCalled.Read(file, grants, text)
        // A PDF's middle pages, as a format's read of them is shown: not the whole file.
        val parsed = for {
          file <- rest.file(Recorded.id("file-4"))
          grants <- rest.grants(file.id)
        } yield DriveCalled.Parsed(
          file,
          grants,
          FileRead("[page 2]\nTwo.", "pages 2–3 of 4", FileRange.pages(4, 4), None, whole = false),
          FileHandle.of(41).getOrElse(throw new java.lang.AssertionError("a handle")),
          Files.Expiry
        )
        // A PDF a format found damaged.
        val unread = for {
          file <- rest.file(Recorded.id("file-4"))
          grants <- rest.grants(file.id)
        } yield DriveCalled.Unread(file, grants, DriveError.Unread(Unread.Damaged(Kind.Pdf)))
        Vector(
          stated(rest.drives().map(DriveCalled.Drives(_))),
          stated(rest.children(Recorded.id("drive-a")).map(DriveCalled.Listed(_))),
          stated(rest.children(Recorded.id("folder-3")).map(DriveCalled.Listed(_))),
          stated(read),
          stated(parsed),
          stated(unread)
        ) ==> Vector(
          "drives",
          "listing-drive-a",
          "listing-folder-3",
          "read-file-text",
          "read-file-pdf",
          "read-file-unread"
        ).map(golden)
      }
    }
  }
}
