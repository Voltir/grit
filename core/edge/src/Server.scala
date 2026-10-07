package grit.edge

import java.util.concurrent.atomic.AtomicBoolean

import scala.concurrent.duration.*
import scala.util.control.NonFatal

import grit.core.edge.{Desk, Edges, Route, ToolRequest}
import grit.core.tool.Outcome

/** What an edge runs a request with: its tools where [[Edges.authorize]] routed the request
  * ([[Route]]). Never throws: every failure is an [[Outcome]].
  */
trait Tools {
  def run(route: Route, request: ToolRequest): Outcome
}

/** Serves `desk`'s requests (ADR 0017): each request [[Edges.authorize]] allows is claimed,
  * run by `tools` over its route through `runner` (a virtual thread each, so a round's reads
  * run together), and answered; one another edge claimed first is skipped. Orphans in the
  * registered places are settled on every pass ([[Desk.orphans]]), and those that rerun are
  * run. `said` hears what an operator would want to know: an answer that came too late, a
  * request refused, each named by its slot, never by its arguments or its answer.
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
            try tools.run(route, q)
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
}
