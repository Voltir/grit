package grit.mcp.drive

import java.util.concurrent.ConcurrentLinkedQueue

import scala.jdk.CollectionConverters.*

import grit.core.admin.InMemoryAdministration
import grit.core.clock.Clock
import grit.core.document.InMemoryDocuments
import grit.core.edge.{
  Attesting,
  EdgeRefusal,
  EdgeStores,
  Edges,
  InMemoryAcknowledgements,
  InMemoryDeliveries,
  InMemoryEdges,
  InMemoryJoins,
  InMemoryOrigins,
  OutcomeJson,
  Permit,
  Registration,
  Route,
  ServedEdge,
  ToolRequest,
  Variable
}
import grit.core.files.{FileArea, InMemoryFileCatalog, Intake, NoIntake, PagedText}
import grit.core.host.TempBlobs
import grit.core.id.{CallSlot, ConversationId, EdgeId, EdgeName, PrincipalId, TurnRef, TurnSeq}
import grit.core.inbox.InMemoryInbox
import grit.core.place.Service
import grit.core.review.InMemoryReviews
import grit.core.spend.Budget
import grit.core.store.{InMemoryVoucher, Jot, StoreError, Tx}
import grit.core.tool.{Outcome, Retry, ToolName}
import grit.core.visibility.{Integrity, Subject}
import grit.dbos.sql.TestTx
import grit.edge.PlaceFragments
import grit.mcp.client.Credential

import utest.*

/** [[DriveEdge]] opened over in-memory stores, and [[DriveTools]] run, against
  * [[FakeDriveServer]].
  */
object DriveEdgeTests extends TestSuite {

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private object FakeJot extends Jot {
    def write[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using TestTx.fake)
  }

  private val Token = Variable("FAKE_DRIVE_TOKEN")
  private val Env = Map(Variable.value(Token) -> FakeDriveServer.Token)
  private val Drive: Service = right(Service.of("drive"))

  private val reading: DriveReading = Recorded.reading

  private def withFake[A](body: FakeDriveServer => A): A = {
    val fake = FakeDriveServer.start()
    try body(fake)
    finally fake.stop()
  }

  /** What [[DriveEdge]] opened over `fake` with `env` did: its result, the edges it registered
    * in, and its log; closed after.
    */
  private def opened(
      fake: FakeDriveServer,
      env: Map[String, String]
  ): (Either[EdgeRefusal, Unit], InMemoryEdges, Vector[String]) = {
    val edges = new InMemoryEdges
    val inbox = InMemoryInbox.fresh(Budget(java.time.ZoneOffset.UTC, None))
    val stores = EdgeStores(
      inbox,
      InMemoryAdministration.none(),
      InMemoryJoins.none(),
      inbox.principals,
      new InMemoryDeliveries,
      inbox.ledger,
      new InMemoryAcknowledgements,
      InMemoryReviews.over(inbox),
      new InMemoryOrigins(inbox.conversations),
      (new InMemoryDocuments).reviews.of,
      FakeJot,
      edges,
      new Attesting(InMemoryVoucher.none(), FakeJot, _ => ()),
      new NoIntake
    )
    val log = new ConcurrentLinkedQueue[String]()
    val edge = right(
      DriveEdge.serving(
        Credential.static(Token),
        reading,
        PagedText.formats,
        api = fake.api.toString
      )
    )
    val result = edge.open(stores, env, Clock.system(), line => { val _ = log.add(line) }) match {
      case Left(refusal) => Left(refusal)
      case Right(open) => Right(open.close())
    }
    (result, edges, log.asScala.toVector)
  }

  /** A request of `tool` with `arguments`, addressed to Drive's place. */
  private def request(tool: String, arguments: ujson.Value): ToolRequest = {
    val turn = TurnRef(ConversationId("c"), TurnSeq.First)
    ToolRequest(
      CallSlot.of(turn, 0, 0).getOrElse(throw new java.lang.AssertionError()),
      ToolRequest.Protocol,
      turn.conversationId,
      Drive.place,
      PrincipalId.Grit,
      right(ToolName.of(tool)),
      Permit.Free,
      Retry.Rerun,
      arguments,
      Set.empty,
      None
    )
  }

  /** `tool` run with `arguments` by [[DriveTools]] over `fake`, keeping what it reads in
    * `files`, telling `said` what it says.
    */
  private def run(
      fake: FakeDriveServer,
      tool: String,
      arguments: ujson.Value,
      said: String => Unit = _ => (),
      files: Intake^ = new NoIntake
  ): Outcome = {
    val q = request(tool, arguments)
    val route: Route = Edges
      .authorize(q, Registration(EdgeId("e"), PrincipalId.Grit, Set(q.workspace)))
      .fold(r => throw new java.lang.AssertionError(r.toString), identity)
    val clock = Clock.system()
    new DriveTools(clock, fake.rest(clock), reading, PagedText.formats, files).run(route, q, said)
  }

  /** `body` over a file area whose catalog is in memory and bytes in a temporary directory,
    * kept as `bounds` says.
    */
  private def withArea[A](bounds: FileArea.Bounds = FileArea.Bounds.Default)(
      body: FileArea^ => A
  ): A =
    TempBlobs.around(blobs =>
      body(new FileArea(new InMemoryFileCatalog, blobs, FakeJot, Clock.system(), bounds))
    )

  /** How many of `fake`'s requests asked for a file's bytes. */
  private def media(fake: FakeDriveServer): Int =
    fake.requests.count(_._2.get("alt").contains("media"))

  val tests = Tests {
    test(
      "what the model is told of Drive says how a folder is labelled, by its link or id, and names no place as grit writes one"
    ) {
      (
        DriveEdge.Instructions.contains("`label internal at <the folder's link or id>`"),
        DriveEdge.Instructions.contains("service:")
      ) ==> (true, false)
    }

    test(
      "what the model cannot be told of a failed run reaches the operator's log, naming no argument; a person's own miss, nothing; a read whose grants were not read, that it is believed untrusted"
    ) {
      withFake { fake =>
        val log = Vector.newBuilder[String]
        fake.answering("files/file-1", 200, "not json")
        fake.answering("files/file-4", 404, Recorded.text("error-no-file.json"))
        fake.answering("drives", 500, "{}")
        fake.answering("files/file-2/permissions", 503, "{}")
        val outcomes = Vector(
          run(fake, "drive_read", ujson.Obj("file" -> "file-1"), log += _),
          run(fake, "drive_read", ujson.Obj("file" -> "file-2"), log += _),
          run(fake, "drive_read", ujson.Obj("file" -> "file-4"), log += _),
          run(fake, "drive_search", ujson.Obj("text" -> "plan"), log += _)
        ).map {
          case Outcome.Done(_, _, _, parts, _) => s"done, ${parts.map(_.size)}"
          case other => other.toString
        }
        (outcomes, log.result()) ==> (
          Vector(
            "Failed(Drive's answer couldn't be read; try again later.)",
            "done, Some(1)",
            "Failed(That file isn't shared here, or doesn't exist.)",
            "done, Some(17)"
          ),
          Vector(
            "drive_read: Drive's answer could not be read: an answer that is not JSON",
            "drive_read: its grants could not be read, so it is believed untrusted: Drive answered 503",
            "drive_search: Drive's shared drives could not be listed, so hits name them by id: Drive answered 500"
          )
        )
      }
    }

    test(
      "the edge is named for its service, needs its credential's variable, and is never asked; blank instructions or an API that is no URL are refused"
    ) {
      val edge = right(DriveEdge.serving(Credential.static(Token), reading, PagedText.formats))
      (
        (edge.name, edge.needs, edge.asks),
        DriveEdge
          .serving(Credential.static(Token), reading, PagedText.formats, instructions = " ")
          .map(_ => ()),
        DriveEdge
          .serving(Credential.static(Token), reading, PagedText.formats, api = "drive/v3")
          .map(_ => ())
      ) ==> (
        (EdgeName("drive"), Vector(Token), ServedEdge.Asks.Never),
        Left("the instructions declared for service:drive are blank"),
        Left("Drive's API drive/v3 is not an absolute URL")
      )
    }

    test(
      "opened, it advertises drive_list, drive_search and drive_read at the service's place, its instructions the Place layer, logged"
    ) {
      withFake { fake =>
        val (result, edges, log) = opened(fake, Env)
        (
          result,
          edges.adverts.toVector.map((at, advert) => (at._2, advert.tools, advert.instructions)),
          DriveTools.Offered.tools.map(e => ToolName.value(e.name)),
          log
        ) ==> (
          Right(()),
          Vector(
            (
              Drive.place,
              DriveTools.Offered.id,
              PlaceFragments.declared(Drive.place, DriveEdge.Instructions).toSeq.toVector.map(_.id)
            )
          ),
          Vector("drive_list", "drive_search", "drive_read"),
          Vector("service:drive: serving drive_list, drive_search, drive_read")
        )
      }
    }

    test(
      "open is refused, naming the key's variable, when Drive does not take the token; unset, nothing is sent"
    ) {
      withFake { fake =>
        fake.answering("files", 401, "{}")
        (opened(fake, Env)._1, opened(fake, Map.empty)._1, fake.requests.size) ==> (
          Left(
            EdgeRefusal.Refused(
              "Drive gave no access (Drive refused the token it was given); check FAKE_DRIVE_TOKEN"
            )
          ),
          Left(EdgeRefusal.Missing(Token)),
          1
        )
      }
    }

    test(
      "open is refused, telling the operator the cause, when Drive's answer cannot be read, Drive is busy, or it cannot be reached"
    ) {
      withFake { fake =>
        fake.answering("files", 200, "not json")
        val malformed = opened(fake, Env)._1
        fake.answering("files", 503, "{}")
        val busy = opened(fake, Env)._1
        fake.stop()
        (malformed, busy, opened(fake, Env)._1) ==> (
          Left(EdgeRefusal.Refused("Drive's answer could not be read: an answer that is not JSON")),
          Left(EdgeRefusal.Refused("Drive answered 503")),
          Left(EdgeRefusal.Refused("Drive could not be reached: ConnectException"))
        )
      }
    }

    test(
      "a search is answered with its hits as parts, each at the place it lies within; one held to a folder, with that folder's alone"
    ) {
      withFake { fake =>
        def placed(arguments: ujson.Obj): Vector[(String, String)] =
          run(fake, "drive_search", arguments) match {
            case Outcome.Done(_, _, _, Some(parts), _) =>
              parts.map(p => (p.text, p.item.place.written))
            case other => throw new java.lang.AssertionError(other.toString)
          }
        val open = placed(ujson.Obj("text" -> "plan"))
        val held = placed(ujson.Obj("text" -> "plan", "folder" -> "folder-3"))
        def at(name: String, in: Vector[(String, String)]) =
          in.collectFirst { case (text, place) if text.startsWith(s"- $name ") => place }
        (
          open.size,
          at("name-22", open),
          at("name-13", open),
          held.map(_._2).distinct,
          held.map(_._1.drop(2).takeWhile(_ != ' '))
        ) ==> (
          17,
          Some("service:drive/folder-4"),
          Some("service:drive/drive-a"),
          Vector("service:drive/folder-3"),
          Vector("name-19", "name-18", "name-17", "name-12", "name-10")
        )
      }
    }

    test(
      "a listing with no folder names the shared drives at the service's place; one of a drive, its files at the drive's place and its folders at the service's"
    ) {
      withFake { fake =>
        def placed(arguments: ujson.Obj): Vector[(String, String)] =
          run(fake, "drive_list", arguments) match {
            case Outcome.Done(_, _, _, Some(parts), _) =>
              parts.map(p => (p.text, p.item.place.written))
            case other => throw new java.lang.AssertionError(other.toString)
          }
        val drive = placed(ujson.Obj("folder" -> "drive-a"))
        val asked = fake.requests.lastOption.flatMap(_._2.get("q"))
        (placed(ujson.Obj()), drive.size, drive.take(2), asked) ==> (
          Vector(
            ("- name-1 (a shared drive), id drive-a", "service:drive"),
            ("- name-2 (a shared drive), id drive-b", "service:drive")
          ),
          7,
          Vector(
            (
              "- name-5 (a folder), id folder-3, last changed 2026-09-01T10:00:00Z",
              "service:drive"
            ),
            (
              "- name-13 (a PDF), id file-4, last changed 2026-09-02T10:00:00Z",
              "service:drive/drive-a"
            )
          ),
          Some("'drive-a' in parents and trashed = false")
        )
      }
    }

    test("an answer's facts reach the request's row") {
      withFake { fake =>
        val row = OutcomeJson.answer(run(fake, "drive_list", ujson.Obj("folder" -> "drive-a")))
        (row.obj.get("facts"), row("parts").arr.headOption.flatMap(_.obj.get("facts"))) ==> (
          Some(ujson.Obj("complete" -> true)),
          Some(
            ujson.Obj(
              "id" -> "folder-3",
              "kind" -> "folder",
              "name" -> "name-5",
              "changed" -> "2026-09-01T10:00:00Z",
              "within" -> "drive-a"
            )
          )
        )
      }
    }

    test(
      "a read is answered with its name and text, believed at the meet over its writers; a file not shared is the error's words"
    ) {
      withFake { fake =>
        (
          run(fake, "drive_read", ujson.Obj("file" -> "file-1")) match {
            case Outcome.Done(_, _, _, Some(Vector(part)), _) => (part.item.vouched, part.text)
            case other => throw new java.lang.AssertionError(other.toString)
          },
          run(fake, "drive_read", ujson.Obj("file" -> "file-404"))
        ) ==> (
          // Its name, type and change time, then the export's lines, the last one's newline
          // ending it.
          (
            Integrity.Member,
            "name-10 (a Doc), last changed 2026-09-08T10:00:00Z:\n" +
              Recorded.text("export-doc.md").stripSuffix("\n")
          ),
          Outcome.Failed("That file isn't shared here, or doesn't exist.")
        )
      }
    }

    test(
      "a text file is read end to end, its byte-order mark dropped, believed by its own grants"
    ) {
      withFake { fake =>
        run(fake, "drive_read", ujson.Obj("file" -> "file-text")) match {
          case Outcome.Done(_, _, _, Some(Vector(part)), _) =>
            // Its own grants name a writer outside the domain; its drive's name none.
            (part.item.vouched, part.text) ==>
              (
                Integrity.Outside,
                "name-30 (a text file), last changed 2026-09-13T10:00:00Z:\ntext text.\ntext text, text."
              )
          case other => throw new java.lang.AssertionError(other.toString)
        }
      }
    }

    test(
      "a PDF read by its pages says which of how many it shows, how to read on, and its facts say so"
    ) {
      withFake { fake =>
        fake.answeringBytes(
          "files/file-4?alt=media",
          200,
          PagedText.bytes("One.", "Two.", "Three.", "Four.")
        )
        withArea()(area =>
          run(fake, "drive_read", ujson.Obj("file" -> "file-4", "pages" -> "2-3"), files = area)
        ) match {
          case Outcome.Done(_, _, _, Some(Vector(part)), _) =>
            (
              part.text.linesIterator.drop(1).mkString("\n"),
              part.facts.map(f => (f.json("covered"), f.json("complete")))
            ) ==> (
              "Read: pages 2–3 of 4.\n\n[page 2]\nTwo.\n\n[page 3]\nThree.\n\n" +
                "[To read on, ask for pages=\"4\".]",
              Some((ujson.Str("pages 2–3 of 4"), ujson.Bool(false)))
            )
          case other => throw new java.lang.AssertionError(other.toString)
        }
      }
    }

    test(
      "a PDF read is kept under a handle its answer names, and the call run again answers that handle without asking Drive for its bytes again"
    ) {
      withFake { fake =>
        fake.answeringBytes("files/file-4?alt=media", 200, PagedText.bytes("One.", "Two."))
        withArea() { area =>
          def read(): (String, Option[ujson.Value]) =
            run(fake, "drive_read", ujson.Obj("file" -> "file-4"), files = area) match {
              case Outcome.Done(head, _, _, Some(Vector(part)), _) =>
                (head, part.facts.flatMap(_.json.obj.get("file")))
              case other => throw new java.lang.AssertionError(other.toString)
            }
          val first = read()
          (first, read(), media(fake)) ==> (
            (
              "The Drive file asked for, kept here as f1 for 24 hours; file_read reads more of " +
                "it by that handle:",
              Some(ujson.Str("f1"))
            ),
            first,
            1
          )
        }
      }
    }

    test(
      "a file Drive says is larger than a file kept here may be is refused before its bytes are asked for"
    ) {
      withFake { fake =>
        fake.answeringBytes("files/file-4?alt=media", 200, PagedText.bytes("One."))
        withArea(FileArea.Bounds.Default.copy(maxFile = 1000L)) { area =>
          (run(fake, "drive_read", ujson.Obj("file" -> "file-4"), files = area), media(fake)) ==> (
            Outcome.Failed("That file is 1 MB, over the 1 MB a file kept here may be."),
            0
          )
        }
      }
    }

    test("an id that is not one, or blank words, are refused before Drive is asked") {
      withFake { fake =>
        val failed = Vector(
          run(fake, "drive_read", ujson.Obj("file" -> "../x")),
          run(fake, "drive_search", ujson.Obj("text" -> "plan", "folder" -> "a b")),
          run(fake, "drive_search", ujson.Obj("text" -> " ")),
          run(fake, "drive_list", ujson.Obj("folder" -> "a/b"))
        )
        (failed, fake.requests) ==> (
          Vector(
            Outcome.Failed(
              "The call to `drive_read` was not run: A Drive file's id holds only letters, " +
                "digits, - and _, not ../x. You sent: {\"file\":\"../x\"}"
            ),
            Outcome.Failed(
              "The call to `drive_search` was not run: A Drive file's id holds only letters, " +
                "digits, - and _, not a b. You sent: {\"text\":\"plan\",\"folder\":\"a b\"}"
            ),
            Outcome.Failed(
              "The call to `drive_search` was not run: Give at least one word to search for. " +
                "You sent: {\"text\":\" \"}"
            ),
            Outcome.Failed(
              "The call to `drive_list` was not run: A Drive file's id holds only letters, " +
                "digits, - and _, not a/b. You sent: {\"folder\":\"a/b\"}"
            )
          ),
          Vector()
        )
      }
    }
  }
}
