package grit.core.durable

import grit.core.id.WorkflowId
import grit.core.visibility.Subject

import utest.*

/** How one run of a workflow settled, as [[DurableContract]] sees it. */
enum Settled {

  /** The workflow returned `output`, now or on an earlier run. */
  case Returned(output: String)

  /** The workflow failed with `error`, now or on an earlier run. */
  case Threw(error: Throwable)

  /** The process died inside a step ([[DurableRuntime.crash]]); nothing was recorded for
    * that step or for the workflow's outcome.
    */
  case Crashed
}

/** A durable runtime [[DurableContract]] runs against: the in-memory fake, or DBOS on
  * Postgres. Each implements "the process died inside a step" its own way.
  */
trait DurableRuntime {

  /** A workflow id no earlier test on this runtime has used. */
  def freshId(): WorkflowId

  /** Runs workflow `id` with `body` and waits for it to settle. The first call starts it;
    * after a [[Settled.Crashed]] the next call restarts the process, so the workflow is
    * resumed as recovery would resume it, with `body` as the new build's code.
    */
  def run(id: WorkflowId)(body: WorkflowId => Durable^ ?=> String): Settled

  /** The names of every operation recorded for `id`, in order. */
  def recordedSteps(id: WorkflowId): Vector[String]

  /** Every piece written to the stream `key` of `id`, in order, across its runs. */
  def streamed(id: WorkflowId, key: String): Vector[String]

  /** Called from a step body: the process dies there. */
  def crash(): Nothing

  /** The attempted and recorded step names, if `error` is this runtime's report of a step
    * that ran under a different name than the one recorded at its position.
    */
  def unexpectedStep(error: Throwable): Option[(String, String)]

  /** Sends `message` to workflow `id` on `topic`, from outside any workflow, as an edge
    * would; a repeated `key` is ignored. Throws what the runtime throws for a refused send.
    */
  def send(id: WorkflowId, topic: String, message: String, key: Option[String]): Unit

  /** The workflow named, if `error` is this runtime's refusal of a send to a workflow that
    * has never started.
    */
  def noSuchWorkflow(error: Throwable): Option[WorkflowId]

  /** The messages sent to `id` on `topic` that it has not received, oldest first. */
  def unreceived(id: WorkflowId, topic: String): Vector[String]
}

// Outside the class: a case class nested in it is path-dependent, so a codec lambda that
// builds one captures `DurableContract.this` and is no longer pure (docs/capture-checking.md).
object DurableContract {
  final case class Picked(ids: Vector[String])

  val pickedJournal: Journaled[Picked] = Journaled.json[Picked](
    p => ujson.Obj("ids" -> ujson.Arr.from(p.ids.map(ujson.Str(_)))),
    v =>
      v.objOpt.flatMap(_.get("ids")).flatMap(_.arrOpt) match {
        case Some(ids) => Right(Picked(ids.flatMap(_.strOpt).toVector))
        case None => Left("expected {ids: [...]}")
      }
  )
}

/** A scenario of [[DurableContract]] a runtime may be listed as failing. */
enum Divergence {

  /** A stream write outside a step, which DBOS records as an operation of its own. */
  case WriteOutsideStep
}

/** The semantics grit relies on from every [[Durable]] runtime, run against each: DBOS's,
  * which [[InMemoryDurable]] stands in for in other modules' tests.
  */
abstract class DurableContract extends TestSuite {
  import DurableContract.*

  /** The runtime under test, shared by every test of the suite. */
  def runtime: DurableRuntime

  /** Scenarios this runtime is known to get wrong, by [[Divergence]]. Each must still fail,
    * so a runtime brought into line shows up as a failure until it is taken off the list.
    */
  def divergences: Set[Divergence] = Set.empty

  private def scenario(d: Divergence)(body: => Unit): Unit =
    if (!divergences.contains(d)) body
    else {
      val held =
        try { body; true }
        catch { case _: java.lang.AssertionError => false }
      assert(!held)
    }

  /** Step bodies, and the workflow body around them, that count their own executions. */
  final class Counts {
    @caps.unsafe.untrackedCaptures
    var body = 0
    @caps.unsafe.untrackedCaptures
    var a = 0
    @caps.unsafe.untrackedCaptures
    var b = 0
    @caps.unsafe.untrackedCaptures
    var crashInB = false
  }

  private def twoSteps(rt: DurableRuntime, counts: Counts)(using d: Durable^): String = {
    counts.body += 1
    val a = d.step("a") { () => counts.a += 1; "a" }
    val b = d.step("b") { () =>
      counts.b += 1
      if (counts.crashInB) rt.crash()
      "b"
    }
    a + b
  }

  /** The error a settled run threw, or a failed assertion naming what it did instead. */
  private def threw(settled: Settled): Throwable = settled match {
    case Settled.Threw(e) => e
    case other => throw new java.lang.AssertionError(s"expected the run to throw, it $other")
  }

  private def writeOutsideStep(): Unit = {
    val rt = runtime
    val id = rt.freshId()
    def wf(using d: Durable^): String = {
      val out = d.stream("k")
      out.write("p1")
      d.step("a") { () => "a" }
    }
    rt.run(id)(_ => wf) ==> Settled.Returned("a")
    rt.streamed(id, "k") ==> Vector("p1")
    rt.recordedSteps(id) ==> Vector("DBOS.writeStream", "a")
  }

  val tests = Tests {
    test("a workflow that returned is not run again") {
      val rt = runtime
      val id = rt.freshId()
      val counts = new Counts
      rt.run(id)(_ => twoSteps(rt, counts)) ==> Settled.Returned("ab")
      rt.run(id)(_ => twoSteps(rt, counts)) ==> Settled.Returned("ab")
      (counts.body, counts.a, counts.b) ==> (1, 1, 1)
    }

    test("after a crash, completed steps replay and the crashed step runs again") {
      val rt = runtime
      val id = rt.freshId()
      val counts = new Counts
      counts.crashInB = true
      rt.run(id)(_ => twoSteps(rt, counts)) ==> Settled.Crashed
      rt.recordedSteps(id) ==> Vector("a")
      counts.crashInB = false
      rt.run(id)(_ => twoSteps(rt, counts)) ==> Settled.Returned("ab")
      (counts.body, counts.a, counts.b) ==> (2, 1, 2)
    }

    test("a step that threw fails the workflow, and a later run runs neither it nor the body") {
      val rt = runtime
      val id = rt.freshId()
      val counts = new Counts
      def wf(using d: Durable^): String = {
        counts.body += 1
        d.step("boom") { () => counts.a += 1; throw new IllegalStateException("boom") }
      }
      val first = threw(rt.run(id)(_ => wf))
      val second = threw(rt.run(id)(_ => wf))
      (first.getClass, first.getMessage) ==> (classOf[IllegalStateException], "boom")
      (second.getClass, second.getMessage) ==> (classOf[IllegalStateException], "boom")
      (counts.body, counts.a) ==> (1, 1)
    }

    test("a step renamed between runs is rejected") {
      val rt = runtime
      val id = rt.freshId()
      val counts = new Counts
      counts.crashInB = true
      rt.run(id)(_ => twoSteps(rt, counts)) ==> Settled.Crashed
      val e = threw(rt.run(id) { _ => d ?=> d.step("renamed") { () => "x" } })
      rt.unexpectedStep(e) ==> Some(("renamed", "a"))
    }

    test("workflows journal independently") {
      val rt = runtime
      val counts = new Counts
      rt.run(rt.freshId())(_ => twoSteps(rt, counts))
      rt.run(rt.freshId())(_ => twoSteps(rt, counts))
      (counts.a, counts.b) ==> (2, 2)
    }

    test("the body receives its workflow id, and a step may capture it") {
      val rt = runtime
      val id = rt.freshId()
      def wf(workflowId: WorkflowId)(using d: Durable^): String =
        d.step("echo") { () => WorkflowId.value(workflowId) }
      rt.run(id)(wf) ==> Settled.Returned(WorkflowId.value(id))
    }

    test("a transaction step is journaled like any other") {
      val rt = runtime
      val id = rt.freshId()
      val counts = new Counts
      counts.crashInB = true
      def wf(using d: Durable^): String =
        d.transact("write", Subject.Public) { counts.a += 1; "written" } + d.step("after") { () =>
          if (counts.crashInB) rt.crash()
          "!"
        }
      rt.run(id)(_ => wf) ==> Settled.Crashed
      counts.crashInB = false
      rt.run(id)(_ => wf) ==> Settled.Returned("written!")
      rt.recordedSteps(id) ==> Vector("write", "after")
      counts.a ==> 1
    }

    test("a JSON-journaled output round-trips") {
      val rt = runtime
      def wf(using d: Durable^): String =
        d.step("pick") { () => Picked(Vector("e1", "e2")) }(using pickedJournal).ids.mkString(",")
      rt.run(rt.freshId())(_ => wf) ==> Settled.Returned("e1,e2")
    }

    test("an output its codec cannot read back fails on the first run, not only on replay") {
      val lossy = new Journaled[String] {
        def encode(a: String): String = a
        def decode(s: String): Either[String, String] = Left("unreadable")
      }
      val rt = runtime
      val e = threw(rt.run(rt.freshId()) { _ => d ?=> d.step("s") { () => "x" }(using lossy) })
      e.getClass ==> classOf[UnreadableJournal]
    }

    test("patch: a fresh workflow records the marker and takes the new branch") {
      val rt = runtime
      val id = rt.freshId()
      def wf(using d: Durable^): String =
        (if (d.patch("p1")) d.step("new") { () => "new" } else d.step("a") { () => "a" }) +
          d.step("b") { () => "b" }
      rt.run(id)(_ => wf) ==> Settled.Returned("newb")
      rt.recordedSteps(id) ==> Vector("DBOS.patch-p1", "new", "b")
    }

    test("patch: a workflow that passed the change replays its old branch") {
      val rt = runtime
      val id = rt.freshId()
      val counts = new Counts
      counts.crashInB = true
      rt.run(id)(_ => twoSteps(rt, counts)) ==> Settled.Crashed
      def patched(using d: Durable^): String =
        (if (d.patch("p1")) d.step("new") { () => "new" }
         else d.step("a") { () => counts.a += 1; "a" }) +
          d.step("b") { () => "b" }
      rt.run(id)(_ => patched) ==> Settled.Returned("ab")
      counts.a ==> 1
      rt.recordedSteps(id) ==> Vector("a", "b")
    }

    test("deprecatePatch replays a marked history and marks nothing new") {
      val rt = runtime
      val id = rt.freshId()
      def patched(using d: Durable^): String = {
        val branch = if (d.patch("p1")) "new" else "old"
        d.step(branch) { () => branch } + d.step("b") { () => rt.crash() }
      }
      def deprecated(using d: Durable^): String = {
        d.deprecatePatch("p1")
        d.step("new") { () => "new" } + d.step("b") { () => "b" }
      }
      rt.run(id)(_ => patched) ==> Settled.Crashed
      rt.run(id)(_ => deprecated) ==> Settled.Returned("newb")
      rt.recordedSteps(id) ==> Vector("DBOS.patch-p1", "new", "b")
      val fresh = rt.freshId()
      rt.run(fresh)(_ => deprecated) ==> Settled.Returned("newb")
      rt.recordedSteps(fresh) ==> Vector("new", "b")
    }

    test("a stream write inside a step is kept, and is not a step") {
      val rt = runtime
      val id = rt.freshId()
      def wf(using d: Durable^): String = {
        val out = d.stream("k")
        d.step("a") { () => out.write("p1"); "a" } + d.step("b") { () => "b" }
      }
      rt.run(id)(_ => wf) ==> Settled.Returned("ab")
      rt.streamed(id, "k") ==> Vector("p1")
      rt.recordedSteps(id) ==> Vector("a", "b")
    }

    test("a stream write in a step cut short by a crash is written again, after the first") {
      val rt = runtime
      val id = rt.freshId()
      val counts = new Counts
      counts.crashInB = true
      def wf(using d: Durable^): String = {
        val out = d.stream("k")
        d.step("a") { () =>
          counts.a += 1
          out.write(s"run ${counts.a}")
          if (counts.crashInB) rt.crash()
          "a"
        }
      }
      rt.run(id)(_ => wf) ==> Settled.Crashed
      counts.crashInB = false
      rt.run(id)(_ => wf) ==> Settled.Returned("a")
      rt.streamed(id, "k") ==> Vector("run 1", "run 2")
    }

    test("a stream write outside a step is recorded as an operation of its own") {
      scenario(Divergence.WriteOutsideStep)(writeOutsideStep())
    }

    test("send: a message to a workflow that has not started is refused") {
      val rt = runtime
      val id = rt.freshId()
      val refused =
        try { rt.send(id, "t", "early", None); None }
        catch { case e: Exception => rt.noSuchWorkflow(e) }
      refused ==> Some(id)
    }

    test("a recorded error rethrows on replay without running, with a message or without") {
      val rt = runtime
      val id = rt.freshId()
      val counts = new Counts
      counts.crashInB = true
      def wf(using d: Durable^): String = {
        def attempt(name: String, message: Option[String]): String =
          try
            d.step(name) { () =>
              counts.a += 1
              throw message.fold(new IllegalStateException())(new IllegalStateException(_))
            }
          catch { case e: IllegalStateException => s"caught ${Option(e.getMessage)}" }
        val said = attempt("said", Some("boom"))
        val silent = attempt("silent", None)
        val last = d.step("last") { () =>
          counts.b += 1
          if (counts.crashInB) rt.crash()
          "last"
        }
        s"$said; $silent; $last"
      }
      rt.run(id)(_ => wf) ==> Settled.Crashed
      counts.crashInB = false
      rt.run(id)(_ => wf) ==> Settled.Returned("caught Some(boom); caught None; last")
      (counts.a, counts.b) ==> (2, 2)
    }

    test("recv: the oldest message on its topic, once; none when none was sent") {
      val rt = runtime
      val id = rt.freshId()
      val counts = new Counts
      counts.crashInB = true
      def body(using d: Durable^): String = {
        d.step("ready") { () => if (counts.crashInB) rt.crash(); "ready" }
        val waits = scala.concurrent.duration.FiniteDuration(100, "ms")
        Vector(d.recv("t", waits), d.recv("t", waits), d.recv("t", waits)).mkString(",")
      }
      // Started, and so known to the runtime, before anything is sent to it.
      rt.run(id)(_ => body) ==> Settled.Crashed
      rt.send(id, "t", "first", None)
      rt.send(id, "t", "second", None)
      rt.send(id, "other", "elsewhere", None)
      counts.crashInB = false
      rt.run(id)(_ => body) ==> Settled.Returned("Some(first),Some(second),None")
      rt.unreceived(id, "other") ==> Vector("elsewhere")
      rt.recordedSteps(id) ==>
        ("ready" +: Vector.fill(3)(Vector("DBOS.recv", "DBOS.sleep")).flatten)
    }

    test("recv: a replay returns what was received, and receives nothing more") {
      val rt = runtime
      val id = rt.freshId()
      val counts = new Counts
      counts.crashInB = true
      def body(using d: Durable^): String = {
        d.step("ready") { () => counts.a += 1; if (counts.a == 1) rt.crash(); "ready" }
        val got = d.recv("t", scala.concurrent.duration.FiniteDuration(100, "ms"))
        d.step("after") { () => if (counts.crashInB) rt.crash(); "after" }
        got.getOrElse("none")
      }
      rt.run(id)(_ => body) ==> Settled.Crashed
      rt.send(id, "t", "yes", None)
      rt.run(id)(_ => body) ==> Settled.Crashed
      rt.send(id, "t", "later", None)
      counts.crashInB = false
      rt.run(id)(_ => body) ==> Settled.Returned("yes")
      rt.unreceived(id, "t") ==> Vector("later")
    }

    test("send: a second message under the same key is ignored") {
      val rt = runtime
      val id = rt.freshId()
      def body(using d: Durable^): String = d.step("ready") { () => rt.crash() }
      rt.run(id)(_ => body) ==> Settled.Crashed
      rt.send(id, "t", "approved", Some("k"))
      rt.send(id, "t", "declined", Some("k"))
      rt.unreceived(id, "t") ==> Vector("approved")
    }
  }
}
