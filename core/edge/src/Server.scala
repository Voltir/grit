package grit.edge

import java.util.concurrent.atomic.AtomicBoolean

import scala.concurrent.duration.*
import scala.util.control.NonFatal

import grit.core.edge.{Desk, DeskError, Desks, EdgeRefusal, Edges, Route, ServedEdge, ToolRequest}
import grit.core.id.PrincipalId
import grit.core.place.Place
import grit.core.store.StoreError
import grit.core.tool.{Outcome, ToolName, ToolSet}

/** What an edge runs a request with: its tools where [[Edges.authorize]] routed the request
  * ([[Route]]). Never throws: every failure is an [[Outcome]].
  */
trait Tools {

  /** `request` run where `route` says; `said` hears what an operator would want to know of the
    * run that the model is not told (why an answer could not be read, say), never its arguments
    * or its answer.
    */
  def run(route: Route, request: ToolRequest, said: String => Unit): Outcome
}

/** Serves `desk`'s requests (ADR 0017): each request [[Edges.authorize]] allows is claimed,
  * run by `tools` over its route through `runner` (a virtual thread each, so a round's reads
  * run together), and answered; one another edge claimed first is skipped. Orphans in the
  * registered places are settled on every pass ([[Desk.orphans]]), and those that rerun are
  * run. `said` hears what an operator would want to know: an answer that came too late, a
  * request refused, and what `tools` say of a run, each named by its slot, never by its
  * arguments or its answer. `tools` are lent `said` for each run rather than given it: under
  * separation checking no capability the server's `said` holds may also be hidden in `tools`.
  */
final class Server(desk: Desk^, tools: Tools^, runner: (() => Unit) => Unit, said: String => Unit)
    extends AutoCloseable {

  private val serving = new AtomicBoolean(false)

  /** One pass: the orphans, then every open request; how many runs it started. */
  def pass(): Int = {
    val reruns =
      desk.orphans().fold(e => { said(s"orphans unread: ${e.why}"); Vector.empty }, identity)
    val opened =
      desk.open().fold(e => { said(s"requests unread: ${e.why}"); Vector.empty }, identity)
    val rerun = reruns.map(q => start(q)).count(identity)
    val claimed = opened.map { q =>
      Edges.authorize(q, desk.registration) match {
        case Left(refused) =>
          said(s"refused ${q.slot.key}: $refused")
          false
        case Right(_) =>
          desk.claim(q) match {
            case Right(true) => start(q)
            case Right(false) => false
            case Left(e) =>
              said(s"claim of ${q.slot.key} failed: ${e.why}")
              false
          }
      }
    }
    rerun + claimed.count(identity)
  }

  /** Runs `q`, claimed by this desk, and answers it; false when it has no route here. */
  private def start(q: ToolRequest): Boolean =
    Edges.authorize(q, desk.registration) match {
      case Left(refused) =>
        said(s"refused ${q.slot.key}: $refused")
        false
      case Right(route) =>
        runner { () =>
          val outcome =
            try tools.run(route, q, line => said(s"${q.slot.key}: $line"))
            catch {
              case NonFatal(e) => Outcome.Failed(s"The edge failed running it: ${e.getMessage}")
            }
          desk.answer(q.slot, outcome) match {
            case Right(true) => ()
            case Right(false) =>
              said(s"late answer to ${q.slot.key}, dropped: the turn stopped waiting")
            case Left(e) => said(s"answer to ${q.slot.key} not written: ${e.why}")
          }
        }
        true
    }

  /** Serves on a daemon thread of its own until [[close]]: a pass whenever the desk is woken,
    * and at least every [[Server.Poll]].
    */
  def serve(): Unit =
    if (serving.compareAndSet(false, true)) {
      val thread = new Thread(() => {
        while (serving.get()) {
          try {
            val _ = pass()
            val _ = desk.await(Server.Poll)
          } catch { case NonFatal(e) => said(s"serving failed: ${e.getMessage}") }
        }
      })
      thread.setName("grit-edge")
      thread.setDaemon(true)
      thread.start()
    }

  def close(): Unit = serving.set(false)
}

object Server {

  /** The longest an idle edge waits before looking again, whatever it is told: 1 second. */
  val Poll: FiniteDuration = 1.second

  /** The set an edge advertised at its place, and the tools that serve it. */
  final class Advertised(val offered: ToolSet, val tools: Tools^)

  /** `place` registered for grit itself ([[PrincipalId.Grit]]) through `desks` and served:
    * `advertise`, given the desk, advertises there and gives the set it advertised, logged in
    * one line ("{place}: serving a, b"), and the tools serving it, run by a [[Server]] of their
    * own, each request on a virtual thread, what the server says logged after the place. The
    * edge delivers nothing; closing it stops the server, and the desk stays open. `Failed` when
    * the desk cannot be registered or `advertise` is refused.
    */
  def open(desks: Desks^, place: Place, log: String => Unit)(
      advertise: (desk: Desk^) => Either[DeskError, Advertised^{desk, caps.any}]
  ): Either[EdgeRefusal, ServedEdge.Open^{desks, log, advertise, caps.any}] =
    desks.register(PrincipalId.Grit, Set(place)) match {
      case Left(e) => Left(EdgeRefusal.Failed(s"its desk could not be registered: ${e.why}"))
      case Right(desk) =>
        advertise(desk) match {
          case Left(e) => Left(EdgeRefusal.Failed(s"its tools could not be advertised: ${e.why}"))
          case Right(advertised) =>
            log(
              s"${place.written}: serving " +
                advertised.offered.tools.map(e => ToolName.value(e.name)).mkString(", ")
            )
            val server = new Server(
              desk,
              advertised.tools,
              run => { val _ = Thread.ofVirtual().start(() => run()) },
              said => log(s"${place.written}: $said")
            )
            server.serve()
            Right(new ServedEdge.Open {
              def deliver(): Either[StoreError, Int] = Right(0)
              def close(): Unit = server.close()
            })
        }
    }
}
