package grit.core.durable

import grit.core.durable.InMemoryDurable.{Outcome, Step}
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
      val history = Vector(Step("a", Outcome.Output("a")))
      new InMemoryDurable().replay(id, history)(_ => twoSteps(new Counts)) ==> Right("ab")
    }

    test("replay: a renamed step, an unreadable output, or an early end fails") {
      val counts = new Counts
      val renamed = Vector(Step("x", Outcome.Output("x")))
      assert(new InMemoryDurable().replay(id, renamed)(_ => twoSteps(counts)).isLeft)
      def picked(using d: Durable^): String =
        d.step("pick") { () => Picked(Vector("e1")) }(using pickedJournal).ids.mkString
      val garbage = Vector(Step("pick", Outcome.Output("not json")))
      assert(new InMemoryDurable().replay(id, garbage)(_ => picked).isLeft)
      val longer = Vector(
        Step("a", Outcome.Output("a")),
        Step("b", Outcome.Output("b")),
        Step("c", Outcome.Output("c"))
      )
      new InMemoryDurable().replay(id, longer)(_ => twoSteps(counts)) ==>
        Left("ended after 2 of 3 recorded steps; next was 'c'")
    }

    test("a stream write inside a step is kept, and is not a step") {
      val durable = new InMemoryDurable
      def wf(using d: Durable^): String = {
        val out = d.stream("k")
        d.step("a") { () => out.write("p1"); "a" } + d.step("b") { () => "b" }
      }
      durable.run(id)(_ => wf) ==> "ab"
      durable.streamed(id, "k") ==> Vector("p1")
      durable.recordedSteps(id) ==> Vector("a", "b")
    }

    test("a stream write outside a step fails, as DBOS would record it as an operation") {
      val durable = new InMemoryDurable
      def wf(using d: Durable^): String = {
        val out = d.stream("k")
        out.write("p1")
        d.step("a") { () => "a" }
      }
      val e = assertThrows[InMemoryDurable.WriteOutsideStep](durable.run(id)(_ => wf))
      e.key ==> "k"
      durable.streamed(id, "k") ==> Vector.empty
      durable.recordedSteps(id) ==> Vector.empty
    }

    test("replay: a recorded error rethrows without running, with a message or without") {
      @caps.unsafe.untrackedCaptures
      var runs = 0
      def wf(using d: Durable^): String = d.step("boom") { () => runs += 1; "ran" }
      val said = Vector(Step("boom", Outcome.Threw(Some("boom"))))
      new InMemoryDurable().replay(id, said)(_ => wf) ==> Right("threw the recorded error: boom")
      val silent = Vector(Step("boom", Outcome.Threw(None)))
      assert(new InMemoryDurable().replay(id, silent)(_ => wf).isRight)
      runs ==> 0
    }

    test("a step that threw without a message is kept as an error, not as a patch marker") {
      val durable = new InMemoryDurable
      def wf(using d: Durable^): String =
        d.step("boom") { () => throw new IllegalStateException() }
      assertThrows[IllegalStateException](durable.run(id)(_ => wf))
      val steps = durable.history(id)
      steps ==> Vector(Step("boom", Outcome.Threw(None)))
      val kept = History(id = id, workflow = "wf", epoch = "e", source = "recorded", steps = steps)
      val json = History.write(kept)
      json("steps")(0) ==> ujson.Obj("name" -> "boom", "error" -> ujson.Null)
      History.read(json) ==> Right(kept)
    }

    test("a history step with both an output and an error is refused") {
      val both = ujson.Obj(
        "workflow" -> "wf",
        "id" -> "c1:1",
        "epoch" -> "e",
        "source" -> "captured",
        "steps" -> ujson.Arr(ujson.Obj("name" -> "s", "output" -> "o", "error" -> "e"))
      )
      assert(History.read(both).isLeft)
    }

    test("Unit is not a step output") {
      val err = assertCompileError("summon[Journaled[Unit]]")
      assert(err.msg.contains("return a value describing what it did"))
    }
  }
}
