package grit.core.durable

import grit.core.durable.DurableContract.{Picked, pickedJournal}
import grit.core.durable.InMemoryDurable.{Outcome, Step}
import grit.core.id.WorkflowId

import utest.*

/** [[InMemoryDurable]]'s own API: replaying a recorded history, and the [[History]] codec.
  * What it shares with DBOS is [[DurableContract]]'s.
  */
object DurableTests extends TestSuite {

  private val id = WorkflowId("c1:1")

  /** Step bodies that count their own executions. */
  final class Counts {
    @caps.unsafe.untrackedCaptures
    var a = 0
    @caps.unsafe.untrackedCaptures
    var b = 0
  }

  private def twoSteps(counts: Counts)(using d: Durable^): String = {
    val a = d.step("a") { () => counts.a += 1; "a" }
    val b = d.step("b") { () => counts.b += 1; "b" }
    a + b
  }

  val tests = Tests {
    test("replay: a body that follows the history passes, returning what was recorded") {
      val counts = new Counts
      val history = Vector(Step("a", Outcome.Output("A")))
      new InMemoryDurable().replay(id, history)(_ => twoSteps(counts)) ==> Right("Ab")
      (counts.a, counts.b) ==> (0, 1)
    }

    test("replay: a renamed step, an unreadable output, or an early end fails") {
      val counts = new Counts
      val renamed = Vector(Step("x", Outcome.Output("x")))
      new InMemoryDurable().replay(id, renamed)(_ => twoSteps(counts)) ==>
        Left("workflow c1:1 step 0: ran 'a', recorded 'x'")
      def picked(using d: Durable^): String =
        d.step("pick") { () => Picked(Vector("e1")) }(using pickedJournal).ids.mkString
      val garbage = Vector(Step("pick", Outcome.Output("{}")))
      new InMemoryDurable().replay(id, garbage)(_ => picked) ==>
        Left("step 'pick' of workflow c1:1: recorded output unreadable: expected {ids: [...]}")
      val longer = Vector(
        Step("a", Outcome.Output("a")),
        Step("b", Outcome.Output("b")),
        Step("c", Outcome.Output("c"))
      )
      new InMemoryDurable().replay(id, longer)(_ => twoSteps(counts)) ==>
        Left("ended after 2 of 3 recorded steps; next was 'c'")
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

    test("replay: a recorded recv returns its message, and receives nothing more") {
      val durable = new InMemoryDurable
      durable.send(id, "t", "yes")
      def body(using d: Durable^): String =
        d.recv("t", scala.concurrent.duration.FiniteDuration(1, "s")).getOrElse("none")
      durable.run(id)(_ => body) ==> "yes"
      val history = durable.history(id)
      durable.send(id, "t", "later")
      durable.replay(id, history)(_ => body) ==> Right("yes")
      durable.unreceived(id, "t") ==> Vector("later")
      durable.replay(id, history.map(s => s.copy(outcome = InMemoryDurable.Outcome.Marker)))(_ =>
        body
      ) ==> Right("none")
    }
  }
}
