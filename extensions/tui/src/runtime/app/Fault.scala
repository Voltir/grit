package grit.tui.runtime.app

/** A throwable the loop caught in an app's own code, as plain data: which part of the app
  * threw, what it was, and where. The loop survives it -- the screen keeps the last frame
  * that painted and says what failed, and input goes on being handled, so the app's quit
  * key still quits -- and hands it to the [[Host]] to log.
  */
final case class Fault(stage: Fault.Stage, error: String, trace: Vector[String])

object Fault {

  /** The part of the app that threw. */
  enum Stage {

    /** `update`, on a message: the message is dropped, the state is as it was. */
    case Update

    /** `view`, or painting the tree it returned: the last good frame stays up. */
    case View

    /** A handler in the tree, on an input: the input is dropped. */
    case Handler
  }

  /** How many stack frames a fault keeps. */
  val TraceFrames = 24

  /** `e`, thrown in `stage`. */
  def of(stage: Stage, e: Throwable): Fault =
    Fault(stage, e.toString, e.getStackTrace.toVector.take(TraceFrames).map(_.toString))

  /** Whether the loop may carry on past `e`: everything but an interrupt, which is the
    * thread being told to stop, and running out of memory, after which nothing is sure.
    * A linkage error -- a class that failed to load mid-render -- is survivable: the
    * rest of the app still runs.
    */
  def survivable(e: Throwable): Boolean = e match {
    case _: InterruptedException | _: OutOfMemoryError => false
    case _ => true
  }

  /** The line the screen shows for `f`: what threw first, so a narrow screen still says
    * it.
    */
  def line(f: Fault): String = {
    val (what, after) = f.stage match {
      case Stage.Update => ("update", "the message was dropped")
      case Stage.View => ("view", "this is the last screen that painted")
      case Stage.Handler => ("an input handler", "the input was dropped")
    }
    s" ✗ $what failed: ${f.error} · $after · any key dismisses"
  }
}
