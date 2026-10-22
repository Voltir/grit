package grit.tools

import java.io.{ByteArrayInputStream, InputStream}
import java.time.Instant

import scala.concurrent.duration.*

import grit.core.clock.SetClock
import grit.core.files.{
  FileArea,
  FileItem,
  FileOrigin,
  InMemoryFileCatalog,
  Incoming,
  PagedText,
  PutKey
}
import grit.core.flow.TestTainted
import grit.core.host.TempBlobs
import grit.core.id.{EdgeName, FileHandle, TestCallSlots, ToolCallId}
import grit.core.identity.Email
import grit.core.message.AssistantBlock
import grit.core.place.Place
import grit.core.store.{Db, Jot, StoreError, Tx}
import grit.core.tool.{ArgsError, Bound, CallError, Outcome, Repairs, Toolbox}
import grit.core.visibility.{
  Accounted,
  Clearance,
  Integrity,
  Provenance,
  Readers,
  Subject,
  Visibility
}
import grit.dbos.sql.TestTx

import utest.*

/** [[FileReading]]: `file_read` over an area of in-memory rows and temporary blobs. */
object FileReadingTests extends TestSuite {

  private val Start = Instant.parse("2026-10-22T09:00:00Z")
  private val drive =
    Place.read("service:drive/folder").fold(e => throw new java.lang.AssertionError(e), identity)
  private val shared = FileItem(Provenance(drive, Integrity.Member), Readers.AtPlace)
  private val anasOwn = FileItem(
    Provenance(drive, Integrity.Member),
    Readers.Originator(
      Email.of("ana@example.com").fold(e => throw new java.lang.AssertionError(e), identity)
    )
  )

  private def jot: Jot = new Jot {
    def write[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using TestTx.fake)
  }

  /** `body` over an area of 10-minute files, dated by a clock at [[Start]]. */
  private def area[A](body: (FileArea, SetClock) => A): A =
    TempBlobs.around { blobs =>
      val clock = new SetClock(Start)
      body(
        new FileArea(
          new InMemoryFileCatalog,
          blobs,
          jot,
          clock,
          PagedText.formats,
          FileArea.Bounds(10.minutes, 1024, 4096)
        ),
        clock
      )
    }

  /** The handle `pages` were kept under, as a file of [[PagedText]]'s, for `item`. */
  private def kept(a: FileArea, n: Int, item: FileItem, pages: String*): FileHandle = {
    val incoming = new Incoming {
      def declared: Option[Long] = None
      def read[A](body: InputStream^ => A): Either[String, A] =
        Right(body(new ByteArrayInputStream(("%PDF-" + pages.mkString("\f")).getBytes("UTF-8"))))
    }
    a.put(
      PutKey.read(s"tool:t:0:$n"),
      "minutes.pdf",
      None,
      FileOrigin(EdgeName("drive"), "d", None, Start),
      item,
      incoming
    )(_.bytes)
      .fold(e => throw new java.lang.AssertionError(e.toString), _._1.handle)
  }

  /** `file_read` with `arguments`, bound as a turn binds a call, or why not. */
  private def bound(a: FileArea, arguments: ujson.Value) =
    Toolbox
      .of(FileReading.tool(a))
      .fold(d => throw new java.lang.AssertionError(s"duplicate $d"), identity)
      .bind(
        TestTainted.authority(AssistantBlock.ToolCall(ToolCallId("c1"), "file_read", arguments)),
        Repairs.All
      )

  /** What `file_read` with `arguments` comes to, read by a turn that reads every place. */
  private def called(a: FileArea, arguments: ujson.Value): Outcome = {
    val at = TestCallSlots.First
    val store: Db^ = new Db {
      def read[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
        body(using TestTx.fake(Clearance.of(Visibility.Shipped.compartments.top)))
    }
    bound(a, arguments) match {
      case Right(r: Bound.Reading) =>
        r.through(store.as(Subject.Turn(at.turn))) match {
          case b: Bound.Free => b(at)
          case other => throw new java.lang.AssertionError(s"not free: $other")
        }
      case other => throw new java.lang.AssertionError(s"not reading: $other")
    }
  }

  val tests = Tests {
    test("the model is told a handle keeps the whole file, not only the part read before") {
      area { (a, _) =>
        FileReading.tool(a).spec.does ==>
          "Read any part of a file kept here, by its handle (such as f41), which the read that " +
          "kept it named. The whole file is kept, not only the part read then: with no other " +
          "argument, its outline and its start; ask for pages, a section by its heading, or " +
          "from an offset to read on. Each read says what it covers. Files are kept a while, " +
          "then must be read again from where they came from."
      }
    }

    test(
      "a kept file is read by its handle, a range at a time, shown to the turn as read where it came from"
    ) {
      area { (a, _) =>
        val h = kept(a, 1, shared, "One.", "Two.", "Three.")
        called(a, ujson.Obj("file" -> h.written, "pages" -> "2")) ==> Outcome.Done(
          s"${h.written}: Read: page 2 of 3.\n\n[page 2]\nTwo.\n\n[To read on, ask for pages=\"3\".]",
          None,
          Accounted.of(Vector(shared.item))
        )
      }
    }
    test("a file expired, never kept, or not this turn's to read is answered alike, in words") {
      area { (a, clock) =>
        val own = kept(a, 1, anasOwn, "Mine.")
        val ours = kept(a, 2, shared, "Ours.")
        val unread = called(a, ujson.Obj("file" -> own.written))
        clock.at = Start.plusSeconds(601)
        // The same words for each: nothing says which file was never this turn's.
        val gone =
          Outcome.Failed(
            "That file is no longer kept (files are kept for 10 minutes); read it again from " +
              "where it came from."
          )
        (
          unread,
          called(a, ujson.Obj("file" -> ours.written)),
          called(a, ujson.Obj("file" -> "f999"))
        ) ==>
          (gone, gone, gone)
      }
    }
    test("a handle that is not one is refused, naming the argument, before anything is read") {
      area { (a, _) =>
        bound(a, ujson.Obj("file" -> "minutes.pdf")).left.toOption ==> Some(
          CallError.BadArgs(
            FileReading.Name,
            ArgsError.Refused(
              "file",
              "minutes.pdf is no file's handle; one is f and a number, such as f41."
            ),
            """{"file":"minutes.pdf"}"""
          )
        )
      }
    }
  }
}
