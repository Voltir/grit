package grit.core.durable

import grit.core.id.WorkflowId

import utest.*

object DurableTests extends TestSuite {

  private val id = WorkflowId("c1:1")

  /** Step bodies that count their own executions. */
  final class Counts {
    @caps.unsafe.untrackedCaptures
    var a = 0
    @caps.unsafe.untrackedCaptures
    var b = 0
    @caps.unsafe.untrackedCaptures
    var crashInB = false
  }

  private def twoSteps(counts: Counts)(using d: Durable^): String = {
    val a = d.step("a") { () => counts.a += 1; "a" }
    val b = d.step("b") { () =>
      counts.b += 1
      if (counts.crashInB) throw new InMemoryDurable.Crash
      "b"
    }
    a + b
  }

  private final case class Picked(ids: Vector[String])

  private val pickedJournal: Journaled[Picked] = Journaled.json[Picked](
    p => ujson.Obj("ids" -> ujson.Arr.from(p.ids.map(ujson.Str(_)))),
    v =>
      v.objOpt.flatMap(_.get("ids")).flatMap(_.arrOpt) match {
        case Some(ids) => Right(Picked(ids.flatMap(_.strOpt).toVector))
        case None => Left("expected {ids: [...]}")
      }
  )

  val tests = Tests {
    test("a workflow that returned is not run again") {
      val durable = new InMemoryDurable
      val counts = new Counts
      durable.run(id)(_ => twoSteps(counts)) ==> "ab"
      durable.run(id)(_ => twoSteps(counts)) ==> "ab"
      (counts.a, counts.b) ==> (1, 1)
    }

    test("after a crash, completed steps replay and the crashed step runs again") {
      val durable = new InMemoryDurable
      val counts = new Counts
      counts.crashInB = true
      assertThrows[InMemoryDurable.Crash](durable.run(id)(_ => twoSteps(counts)))
      durable.recordedSteps(id) ==> Vector("a")
      counts.crashInB = false
      durable.run(id)(_ => twoSteps(counts)) ==> "ab"
      (counts.a, counts.b) ==> (1, 2)
    }

    test("a step that threw rethrows on every later run without running") {
      val durable = new InMemoryDurable
      @caps.unsafe.untrackedCaptures
      var runs = 0
      def wf(using d: Durable^): String =
        d.step("boom") { () => runs += 1; throw new IllegalStateException("boom") }
      assertThrows[IllegalStateException](durable.run(id)(_ => wf))
      assertThrows[IllegalStateException](durable.run(id)(_ => wf))
      runs ==> 1
    }

    test("a step renamed between runs is rejected") {
      val durable = new InMemoryDurable
      val counts = new Counts
      counts.crashInB = true
      assertThrows[InMemoryDurable.Crash](durable.run(id)(_ => twoSteps(counts)))
      val e = assertThrows[InMemoryDurable.UnexpectedStep] {
        durable.run(id) { _ => d ?=> d.step("renamed") { () => "x" } }
      }
      (e.name, e.recorded) ==> ("renamed", "a")
    }

    test("workflows journal independently") {
      val durable = new InMemoryDurable
      val counts = new Counts
      durable.run(id)(_ => twoSteps(counts))
      durable.run(WorkflowId("c1:2"))(_ => twoSteps(counts))
      (counts.a, counts.b) ==> (2, 2)
    }

    test("the body receives its workflow id, and a step may capture it") {
      val durable = new InMemoryDurable
      def wf(workflowId: WorkflowId)(using d: Durable^): String =
        d.step("echo") { () => WorkflowId.value(workflowId) }
      durable.run(id)(wf) ==> "c1:1"
    }

    test("a transaction step is journaled like any other") {
      val durable = new InMemoryDurable
      @caps.unsafe.untrackedCaptures
      var runs = 0
      def wf(using d: Durable^): String =
        d.transact("write") { runs += 1; "written" }
      durable.run(id)(_ => wf)
      durable.recordedSteps(id) ==> Vector("write")
      runs ==> 1
    }

    test("a JSON-journaled output round-trips") {
      val durable = new InMemoryDurable
      def wf(using d: Durable^): String =
        d.step("pick") { () => Picked(Vector("e1", "e2")) }(using pickedJournal).ids.mkString(",")
      durable.run(id)(_ => wf) ==> "e1,e2"
    }

    test("an output its codec cannot read back fails on the first run, not only on replay") {
      val lossy = new Journaled[String] {
        def encode(a: String): String = a
        def decode(s: String): Either[String, String] = Left("unreadable")
      }
      val durable = new InMemoryDurable
      assertThrows[UnreadableJournal] {
        durable.run(id) { _ => d ?=> d.step("s") { () => "x" }(using lossy) }
      }
    }

    test("patch: a fresh workflow records the marker and takes the new branch") {
      val durable = new InMemoryDurable
      def wf(using d: Durable^): String =
        (if (d.patch("p1")) d.step("new") { () => "new" } else d.step("a") { () => "a" }) +
          d.step("b") { () => "b" }
      durable.run(id)(_ => wf) ==> "newb"
      durable.recordedSteps(id) ==> Vector("DBOS.patch-p1", "new", "b")
    }

    test("patch: a workflow that passed the change replays its old branch") {
      val durable = new InMemoryDurable
      val counts = new Counts
      counts.crashInB = true
      assertThrows[InMemoryDurable.Crash](durable.run(id)(_ => twoSteps(counts)))
      def patched(using d: Durable^): String =
        (if (d.patch("p1")) d.step("new") { () => "new" }
         else d.step("a") { () => counts.a += 1; "a" }) +
          d.step("b") { () => "b" }
      durable.run(id)(_ => patched) ==> "ab"
      counts.a ==> 1
      durable.recordedSteps(id) ==> Vector("a", "b")
    }

    test("deprecatePatch replays a marked history and marks nothing new") {
      val durable = new InMemoryDurable
      def patched(using d: Durable^): String = {
        val branch = if (d.patch("p1")) "new" else "old"
        d.step(branch) { () => branch } + d.step("b") { () => throw new InMemoryDurable.Crash }
      }
      def deprecated(using d: Durable^): String = {
        d.deprecatePatch("p1")
        d.step("new") { () => "new" } + d.step("b") { () => "b" }
      }
      assertThrows[InMemoryDurable.Crash](durable.run(id)(_ => patched))
      durable.run(id)(_ => deprecated) ==> "newb"
      durable.recordedSteps(id) ==> Vector("DBOS.patch-p1", "new", "b")
      val fresh = WorkflowId("c1:2")
      durable.run(fresh)(_ => deprecated) ==> "newb"
      durable.recordedSteps(fresh) ==> Vector("new", "b")
    }

    test("replay: a body that follows the history passes") {
      val history = Vector(InMemoryDurable.Step("a", Some("a"), None))
      new InMemoryDurable().replay(id, history)(_ => twoSteps(new Counts)) ==> Right("ab")
    }

    test("replay: a renamed step, an unreadable output, or an early end fails") {
      val counts = new Counts
      val renamed = Vector(InMemoryDurable.Step("x", Some("x"), None))
      assert(new InMemoryDurable().replay(id, renamed)(_ => twoSteps(counts)).isLeft)
      def picked(using d: Durable^): String =
        d.step("pick") { () => Picked(Vector("e1")) }(using pickedJournal).ids.mkString
      val garbage = Vector(InMemoryDurable.Step("pick", Some("not json"), None))
      assert(new InMemoryDurable().replay(id, garbage)(_ => picked).isLeft)
      val longer = Vector(
        InMemoryDurable.Step("a", Some("a"), None),
        InMemoryDurable.Step("b", Some("b"), None),
        InMemoryDurable.Step("c", Some("c"), None)
      )
      new InMemoryDurable().replay(id, longer)(_ => twoSteps(counts)) ==>
        Left("ended after 2 of 3 recorded steps; next was 'c'")
    }

    test("Unit is not a step output") {
      val err = assertCompileError("summon[Journaled[Unit]]")
      assert(err.msg.contains("return a value describing what it did"))
    }

    test("recv: the oldest message on its topic, once; none when none was sent") {
      val durable = new InMemoryDurable
      durable.send(id, "t", "first")
      durable.send(id, "t", "second")
      durable.send(id, "other", "elsewhere")
      def body(using d: Durable^): String = {
        val waits = scala.concurrent.duration.FiniteDuration(1, "s")
        Vector(d.recv("t", waits), d.recv("t", waits), d.recv("t", waits)).mkString(",")
      }
      val got = durable.run(id)(_ => body)
      got ==> "Some(first),Some(second),None"
      durable.unreceived(id, "other") ==> Vector("elsewhere")
      durable.recordedSteps(id) ==>
        Vector.fill(3)(Vector(InMemoryDurable.Recv, InMemoryDurable.Sleep)).flatten
    }

    test("recv: a replay returns what was received, and receives nothing more") {
      val durable = new InMemoryDurable
      durable.send(id, "t", "yes")
      def body(using d: Durable^): String =
        d.recv("t", scala.concurrent.duration.FiniteDuration(1, "s")).getOrElse("none")
      durable.run(id)(_ => body) ==> "yes"
      val history = durable.history(id)
      durable.send(id, "t", "later")
      durable.replay(id, history)(_ => body) ==> Right("yes")
      durable.unreceived(id, "t") ==> Vector("later")
      durable.replay(id, history.map(s => s.copy(output = None)))(_ => body) ==> Right("none")
    }

    test("send: a second message under the same key is ignored") {
      val durable = new InMemoryDurable
      durable.send(id, "t", "approved", Some("k"))
      durable.send(id, "t", "declined", Some("k"))
      durable.unreceived(id, "t") ==> Vector("approved")
    }
  }
}
