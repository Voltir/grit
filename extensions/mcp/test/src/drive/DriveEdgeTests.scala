package grit.mcp.drive

import java.util.concurrent.ConcurrentLinkedQueue

import scala.jdk.CollectionConverters.*

import grit.core.admin.InMemoryAdministration
import grit.core.clock.Clock
import grit.core.edge.{
  Attesting,
  EdgeRefusal,
  EdgeStores,
  Edges,
  InMemoryAcknowledgements,
  InMemoryDeliveries,
  InMemoryEdges,
  InMemoryJoins,
  Permit,
  Registration,
  Route,
  ToolRequest,
  Variable
}
import grit.core.id.{CallSlot, ConversationId, EdgeId, EdgeName, PrincipalId, TurnRef, TurnSeq}
import grit.core.identity.{Domain, Email, Identities}
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
  private val Example: Domain = right(Domain.of("example.com"))

  private val reading: DriveReading = right(
    DriveReading.of(
      Drive,
      Example,
      Identities
        .of(Vector.empty, Set(Example))
        .fold(e => throw new java.lang.AssertionError(e), identity),
      right(Email.of("self@project.iam.example")),
      Map(right(Email.of("writer-bot-1@project.iam.example")) -> Integrity.Member)
    )
  )

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
      FakeJot,
      edges,
      new Attesting(InMemoryVoucher.none(), FakeJot, _ => ())
    )
    val log = new ConcurrentLinkedQueue[String]()
    val edge = right(DriveEdge.serving(Credential.static(Token), reading, api = fake.api.toString))
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

  /** `tool` run with `arguments` by [[DriveTools]] over `fake`, telling `said` what it says. */
  private def run(
      fake: FakeDriveServer,
      tool: String,
      arguments: ujson.Value,
      said: String => Unit = _ => ()
  ): Outcome = {
    val q = request(tool, arguments)
    val route: Route = Edges
      .authorize(q, Registration(EdgeId("e"), PrincipalId.Grit, Set(q.workspace)))
      .fold(r => throw new java.lang.AssertionError(r.toString), identity)
    val clock = Clock.system()
    new DriveTools(clock, fake.rest(clock), reading).run(route, q, said)
  }

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
          case Outcome.Done(_, _, _, parts) => s"done, ${parts.map(_.size)}"
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
      "the edge is named for its service, needs its credential's variable, and answers no ask; blank instructions or an API that is no URL are refused"
    ) {
      val edge = right(DriveEdge.serving(Credential.static(Token), reading))
      (
        (edge.name, edge.needs, edge.answersAsks),
        DriveEdge.serving(Credential.static(Token), reading, instructions = " ").map(_ => ()),
        DriveEdge.serving(Credential.static(Token), reading, api = "drive/v3").map(_ => ())
      ) ==> (
        (EdgeName("drive"), Vector(Token), false),
        Left("the instructions declared for service:drive are blank"),
        Left("Drive's API drive/v3 is not an absolute URL")
      )
    }

    test(
      "opened, it advertises drive_search and drive_read at the service's place, its instructions the Place layer, logged"
    ) {
      withFake { fake =>
        val (result, edges, log) = opened(fake, Env)
        (
          result,
          edges.adverts.toVector.map((at, advert) => (at._2, advert.tools, advert.instructions)),
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
          Vector("service:drive: serving drive_search, drive_read")
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
              "Drive gave grit no access (Drive refused grit's token); check FAKE_DRIVE_TOKEN"
            )
          ),
          Left(EdgeRefusal.Missing(Token)),
          1
        )
      }
    }

    test("a search is answered with its hits as parts, each at the place it lies within") {
      withFake { fake =>
        run(fake, "drive_search", ujson.Obj("text" -> "plan")) match {
          case Outcome.Done(_, _, _, Some(parts)) =>
            (
              parts.size,
              parts.map(_.item.place).distinct.size > 1,
              parts.forall(_.item.vouched == Integrity.Member)
            ) ==>
              (17, true, true)
          case other => throw new java.lang.AssertionError(other.toString)
        }
      }
    }

    test(
      "a read is answered with its name and text, believed at the meet over its writers; a file not shared is the error's words"
    ) {
      withFake { fake =>
        (
          run(fake, "drive_read", ujson.Obj("file" -> "file-1")) match {
            case Outcome.Done(_, _, _, Some(Vector(part))) =>
              (part.item.vouched, part.text.startsWith("name-10 (a Doc):\n"))
            case other => throw new java.lang.AssertionError(other.toString)
          },
          run(fake, "drive_read", ujson.Obj("file" -> "file-404"))
        ) ==> (
          (Integrity.Member, true),
          Outcome.Failed("That file isn't shared here, or doesn't exist.")
        )
      }
    }

    test("an id that is not one, or blank words, are refused before Drive is asked") {
      withFake { fake =>
        val failed = Vector(
          run(fake, "drive_read", ujson.Obj("file" -> "../x")),
          run(fake, "drive_search", ujson.Obj("text" -> "plan", "folder" -> "a b")),
          run(fake, "drive_search", ujson.Obj("text" -> " "))
        ).map {
          case Outcome.Failed(_) => true
          case _ => false
        }
        (failed, fake.requests) ==> (Vector(true, true, true), Vector())
      }
    }
  }
}
