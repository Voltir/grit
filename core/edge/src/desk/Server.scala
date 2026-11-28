package grit.edge.desk

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

  /** `request` run where `route` says, a repository route in that run's working copy; `said` hears what an operator would want to know of the
    * run that the model is not told (why an answer could not be read, say), never its arguments
    * or its answer.
    */
  def run(route: Route, request: ToolRequest, said: String => Unit): Outcome
}

/** Serves `desk`'s requests (ADR 0017): each request [[Edges.authorize]] allows is claimed,
  * run by `tools` over its route through `runner` (a virtual thread each, so a round's reads
  * run together), and answered; one another edge claimed first is skipped. The calls of one
  * round whose tool `readOnly` does not hold for are run in one `runner` job, one after
  * another in the order the reply made them, so that no two of them race on the checkout. Orphans in the
  * registered places are settled on every pass ([[Desk.orphans]]), and those that rerun are
  * run. `said` hears what an operator would want to know: an answer that came too late, a
  * request refused, and what `tools` say of a run, each named by its slot, never by its
  * arguments or its answer. `tools` are lent `said` for each run rather than given it: under
  * separation checking no capability the server's `said` holds may also be hidden in `tools`.
  */
final class Server(
    desk: Desk^,
    tools: Tools^,
    readOnly: ToolName -> Boolean,
    runner: (() => Unit) => Unit,
    said: String => Unit
) extends AutoCloseable {

  private val serving = new AtomicBoolean(false)

  /** One pass: the orphans, then every open request; how many runs it started. */
  def pass(): Int = {
    val reruns =
      desk.orphans().fold(e => { said(s"orphans unread: ${e.why}"); Vector.empty }, identity)
    val opened =
      desk.open().fold(e => { said(s"requests unread: ${e.why}"); Vector.empty }, identity)
    val claimed = opened.filter { q =>
      Edges.hosts(q, desk.registration) match {
        case Left(refused) =>
          said(s"refused ${q.slot.key}: $refused")
          false
        case Right(()) =>
          desk.claim(q) match {
            case Right(true) => true
            case Right(false) => false
            case Left(e) =>
              said(s"claim of ${q.slot.key} failed: ${e.why}")
              false
          }
      }
    }
    val (reads, changes) = (reruns ++ claimed).partition(q => readOnly(q.tool))
    // Calls that change the checkout race when run together: `git add` and `git commit` made in
    // one reply fought over git's index lock (grit/801).
    val inTurn = changes
      .groupBy(q => (q.slot.turn, q.slot.round))
      .values
      .toVector
      .map(_.sortBy(_.slot.index))
    reads.map(q => start(Vector(q))).sum + inTurn.map(start).sum
  }

  /** Runs `qs`, claimed by this desk, in one job, one after another, and answers each; how many
    * it runs. One with no route here is answered failed, since its claim is held and no other
    * edge will answer it.
    */
  private def start(qs: Vector[ToolRequest]): Int = {
    // Local, so the run below captures the desk and `said`, not the server and its runner.
    def answered(q: ToolRequest, outcome: Outcome): Unit =
      desk.answer(q.slot, outcome) match {
        case Right(true) => ()
        case Right(false) =>
          said(s"late answer to ${q.slot.key}, dropped: the turn stopped waiting")
        case Left(e) => said(s"answer to ${q.slot.key} not written: ${e.why}")
      }
    val routed = qs.flatMap { q =>
      // Only a repository's place routes by its conversation's origin (Edges.authorize).
      val origin =
        if (q.workspace.repository.isEmpty) None
        else
          desk
            .origin(q)
            .fold(
              e => {
                said(s"origin of ${q.slot.key} unread: ${e.why}")
                None
              },
              identity
            )
      Edges.authorize(q, desk.registration, origin) match {
        case Left(refused) =>
          said(s"refused ${q.slot.key}: $refused")
          answered(q, Outcome.Failed(Server.unrouted(q)))
          None
        case Right(route) => Some((q, route))
      }
    }
    if (routed.nonEmpty)
      runner { () =>
        routed.foreach { (q, route) =>
          val outcome =
            try tools.run(route, q, line => said(s"${q.slot.key}: $line"))
            catch {
              case NonFatal(e) => Outcome.Failed(s"The edge failed running it: ${e.getMessage}")
            }
          answered(q, outcome)
        }
      }
    routed.size
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

  /** What the model is told of a request claimed here that has no route: that nothing ran. */
  def unrouted(q: ToolRequest): String =
    s"This edge runs nothing at ${q.workspace.written} for this conversation; nothing ran."

  /** A [[Server]]'s `runner`: each run on a virtual thread of its own, named `named` (an
    * unnamed virtual thread's name is empty, the default).
    */
  def onVirtualThreads(named: String = ""): (() => Unit) -> Unit =
    run => { val _ = Thread.ofVirtual().name(named).start(() => run()) }

  /** Whether a tool of `sets` named so is [[ToolSet.Entry.readOnly]] in each set that offers
    * it; false for a name none offers.
    */
  def readOnlyIn(sets: Vector[ToolSet]): ToolName -> Boolean = {
    val named = sets.flatMap(_.tools)
    tool => {
      val offered = named.filter(_.name == tool)
      offered.nonEmpty && offered.forall(_.readOnly)
    }
  }

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
              readOnlyIn(Vector(advertised.offered)),
              onVirtualThreads(),
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
