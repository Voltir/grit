package grit.edge

import scala.concurrent.duration.FiniteDuration

import grit.core.edge.{Desk, DeskError, InMemoryEdges, Permit, Registration, Route, ToolRequest}
import grit.core.id.{CallSlot, ConversationId, EdgeId, PrincipalId, TurnRef, TurnSeq}
import grit.core.place.{Directory, Place}
import grit.core.prompt.Fragment
import grit.core.tool.{Outcome, Retry, ToolName, ToolSet}
import grit.dbos.sql.TestTx

import utest.*

/** [[Server]] over the in-memory edges: what an edge claims, runs and answers. */
object ServerTests extends TestSuite {

  private val api =
    Place.of(Directory.of("/work/api").getOrElse(throw new java.lang.AssertionError()))
  private val web =
    Place.of(Directory.of("/work/web").getOrElse(throw new java.lang.AssertionError()))

  private def request(index: Int, at: Place, retry: Retry = Retry.Rerun): ToolRequest = {
    val turn = TurnRef(ConversationId("c"), TurnSeq.First)
    ToolRequest(
      CallSlot.of(turn, 0, index).getOrElse(throw new java.lang.AssertionError()),
      ToolRequest.Protocol,
      turn.conversationId,
      at,
      PrincipalId.Local,
      ToolName("read"),
      Permit.Free,
      retry,
      ujson.Obj(),
      Set.empty,
      None
    )
  }

  /** Tools that answer with the directory they ran over, and count their runs. */
  private final class Counting extends Tools {
    @caps.unsafe.untrackedCaptures
    var runs = Vector.empty[String]
    def note(key: String): Unit = runs = runs :+ key
    def run(route: Route, request: ToolRequest): Outcome = {
      note(request.slot.key)
      route match {
        case Route.Directory(root) => Outcome.Done(s"ran in ${Directory.value(root)}")
        case Route.Service(service) => Outcome.Done(s"ran at ${service.place.written}")
      }
    }
  }

  private def inline(run: () => Unit): Unit = run()

  private def quiet(said: String): Unit = ()

  /** What a server said, in order. */
  private final class Said {
    @caps.unsafe.untrackedCaptures
    var lines = Vector.empty[String]
    def hear(line: String): Unit = lines = lines :+ line
  }

  val tests = Tests {
    test("an open request in a registered place is claimed, run over its directory, and answered") {
      val edges = new InMemoryEdges
      val q = request(0, api)
      edges.dispatch(Vector(q))(using TestTx.fake)
      val tools = new Counting
      new Server(edges.desk(Set(api)), tools, inline, quiet).pass() ==> 1
      edges.answers ==> Vector((q.slot, Outcome.Done("ran in /work/api")))
    }

    test("a request outside the registered places is never claimed, whatever the desk lists") {
      // A desk that lists a request in another place, as a desk with a wrong query would.
      val claims = new Counting
      val lost = request(0, web)
      val desk = new Desk {
        val registration = Registration(EdgeId("e"), PrincipalId.Local, Set(api))
        def await(within: FiniteDuration): Boolean = false
        def open(): Either[DeskError, Vector[ToolRequest]] = Right(Vector(lost))
        def claim(q: ToolRequest): Either[DeskError, Boolean] = {
          claims.note(q.slot.key)
          Right(true)
        }
        def answer(slot: CallSlot, outcome: Outcome): Either[DeskError, Boolean] = Right(true)
        def orphans(): Either[DeskError, Vector[ToolRequest]] = Right(Vector.empty)
        def advertise(
            place: Place,
            tools: ToolSet,
            instructions: Vector[Fragment]
        ): Either[DeskError, Unit] = Right(())
      }
      val tools = new Counting
      new Server(desk, tools, inline, quiet).pass() ==> 0
      (claims.runs, tools.runs) ==> (Vector(), Vector())
    }

    test("a refused request is said by its slot and why, never its arguments") {
      val lost = request(0, web).copy(arguments = ujson.Obj("text" -> "the secret plan"))
      val desk = new Desk {
        val registration = Registration(EdgeId("e"), PrincipalId.Local, Set(api))
        def await(within: FiniteDuration): Boolean = false
        def open(): Either[DeskError, Vector[ToolRequest]] = Right(Vector(lost))
        def claim(q: ToolRequest): Either[DeskError, Boolean] = Right(true)
        def answer(slot: CallSlot, outcome: Outcome): Either[DeskError, Boolean] = Right(true)
        def orphans(): Either[DeskError, Vector[ToolRequest]] = Right(Vector.empty)
        def advertise(
            place: Place,
            tools: ToolSet,
            instructions: Vector[Fragment]
        ): Either[DeskError, Unit] = Right(())
      }
      val said = new Said
      new Server(desk, new Counting, inline, said.hear).pass() ==> 0
      said.lines ==> Vector(s"refused tool:c:0:0:0: NotHosted($web)")
    }

    test("a request whose run fails, or throws, is answered so and nothing is said of it") {
      val edges = new InMemoryEdges
      val secret = ujson.Obj("text" -> "the secret plan")
      val failed = request(0, api).copy(arguments = secret)
      val thrown = request(1, api).copy(arguments = secret)
      edges.dispatch(Vector(failed, thrown))(using TestTx.fake)
      val tools = new Tools {
        def run(route: Route, q: ToolRequest): Outcome =
          if (q.slot == failed.slot) Outcome.Failed(s"bad arguments: ${q.arguments}")
          else throw new IllegalStateException(s"bad arguments: ${q.arguments}")
      }
      val said = new Said
      new Server(edges.desk(Set(api)), tools, inline, said.hear).pass() ==> 2
      edges.answers.map(_._2) ==> Vector(
        Outcome.Failed("bad arguments: {\"text\":\"the secret plan\"}"),
        Outcome.Failed("The edge failed running it: bad arguments: {\"text\":\"the secret plan\"}")
      )
      said.lines ==> Vector()
    }

    test("a request another edge claimed first is not run here") {
      val edges = new InMemoryEdges
      val q = request(0, api)
      edges.dispatch(Vector(q))(using TestTx.fake)
      edges.desk(Set(api)).claim(q) ==> Right(true)
      val tools = new Counting
      new Server(edges.desk(Set(api)), tools, inline, quiet).pass() ==> 0
      tools.runs ==> Vector()
    }

    test("an orphan declared Rerun is run once more by the edge that finds it") {
      val edges = new InMemoryEdges
      val q = request(0, api, Retry.Rerun)
      edges.dispatch(Vector(q))(using TestTx.fake)
      val dead = edges.desk(Set(api))
      dead.claim(q) ==> Right(true)
      edges.kill(dead.registration.edge)
      val tools = new Counting
      val server = new Server(edges.desk(Set(api)), tools, inline, quiet)
      (server.pass(), server.pass()) ==> (1, 0)
      (tools.runs, edges.answers) ==> (
        Vector(q.slot.key),
        Vector((q.slot, Outcome.Done("ran in /work/api")))
      )
    }

    test("an orphan declared Interrupt is answered Interrupted, and never run") {
      val edges = new InMemoryEdges
      val q = request(0, api, Retry.Interrupt)
      edges.dispatch(Vector(q))(using TestTx.fake)
      val dead = edges.desk(Set(api))
      dead.claim(q) ==> Right(true)
      edges.kill(dead.registration.edge)
      val tools = new Counting
      new Server(edges.desk(Set(api)), tools, inline, quiet).pass() ==> 0
      (tools.runs, edges.answers) ==> (Vector(), Vector((q.slot, Outcome.Interrupted)))
    }
  }
}
