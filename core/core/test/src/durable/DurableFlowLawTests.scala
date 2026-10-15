package grit.core.durable

import scala.concurrent.duration.FiniteDuration

import grit.core.id.{ConversationId, WorkflowId}
import grit.core.store.Tx
import grit.core.visibility.{Clearance, Label, Level, Subject}

import utest.*
import FlowFixtures.*

/** What each of [[Flow]]'s combinators records, and what it passes, run by [[Flows.run]] over
  * [[InMemoryDurable]].
  */
object DurableFlowLawTests extends TestSuite {

  /** `flow` run once from `in` over `env` on `durable`: what it came to, and the names it
    * recorded.
    */
  private def ran[R, I, E, O](
      flow: Flow[R, I, E, O],
      env: R^,
      in: I,
      durable: InMemoryDurable = new InMemoryDurable()
  ): (String, Vector[String]) = {
    val (out, history) = FlowFixtures.ran(flow, env, in, durable)
    (out, history.map(_.name))
  }

  private val env = new Env("t")

  val tests = Tests {
    test("a step's body is handed the run's environment, its output recorded under its name") {
      ran(Flow.step(a), env, "x") ==> ("Right(ta(x))", Vector("a"))
    }

    test("a keyed step is recorded as its family and the key its input gives") {
      val sized =
        Step[Env, String, String](StepName.keyed[String]("tool")(_.length.toString))(r =>
          in => r.tag + in
        )
      ran(Flow.step(sized), env, "abc") ==> ("Right(tabc)", Vector("tool:3"))
    }

    test(
      "a transaction step runs in a transaction opened for its subject, recorded under its name"
    ) {
      val internal = Label.at(Level.Internal)
      val durable = new InMemoryDurable(resolve = {
        case Subject.Public => Clearance.of(Label.Public)
        case _ => Clearance.of(internal)
      })
      val cleared = Step.transact[Env, String, String](
        StepName("cleared"),
        in => Subject.Conversation(ConversationId(in))
      )(r => _ => r.tag + Label.written(Tx.cleared(summon[Tx])))
      ran(Flow.step(cleared), env, "c1", durable) ==>
        (s"Right(t${Label.written(internal)})", Vector("cleared"))
    }

    test("andThen runs the next flow over what the first passed, recording both in order") {
      ran(Flow.step(a).andThen(Flow.step(b)), env, "x") ==> ("Right(tb(ta(x)))", Vector("a", "b"))
    }

    test("decide ends with a Left, recording nothing after it, and goes on with a Right") {
      val gated = stopping.andThen(Flow.step(b))
      (ran(gated, env, "stop"), ran(gated, env, "go")) ==>
        (
          ("Left(stopped at t(stop))", Vector("item:stop")),
          ("Right(tb(t(go)))", Vector("item:go", "b"))
        )
    }

    test("map changes what passes on, recording nothing of its own") {
      ran(Flow.step(a).map(_.length), env, "x") ==> ("Right(5)", Vector("a"))
    }

    test("keeping passes the input beside the output") {
      ran(Flow.step(a).keeping, env, "x") ==> ("Right((x,ta(x)))", Vector("a"))
    }

    test("either runs only the arm its input names") {
      val counted = Step[Env, Int, String](StepName("n"))(r => n => r.tag * n)
      val split = Flow.either(Flow.step(a), Flow.step(counted))
      (ran(split, env, Left("x")), ran(split, env, Right(2))) ==>
        (("Right(ta(x))", Vector("a")), ("Right(tt)", Vector("n")))
    }

    test("attempt passes an end as a Left, and the flow goes on") {
      val kept = Flow.attempt(stopping).map(_.toString).andThen(Flow.step(b))
      ran(kept, env, "stop") ==> ("Right(tb(Left(stopped at t(stop))))", Vector("item:stop", "b"))
    }

    test("loop runs its round from each Left it passes until a Right, each round recorded") {
      ran(growing, env, "") ==> ("Right(ttt)", Vector("round:0", "round:1", "round:2"))
    }

    test("each runs its flow over each item in order, and ends with the first that ends") {
      val all = Flow.each[Env, String, String, String, String](_.split(",").toVector)(stopping)
      (ran(all, env, "p,q"), ran(all, env, "p,stop,q")) ==>
        (
          ("Right(Vector(t(p), t(q)))", Vector("item:p", "item:q")),
          ("Left(stopped at t(stop))", Vector("item:p", "item:stop"))
        )
    }

    test(
      "patch: a fresh run takes the new branch, recording the marker; one that passed it keeps the old"
    ) {
      val patched = Flow.patch("p")(Flow.step(a), Flow.step(b))
      (
        ran(patched, env, "x"),
        ran(patched, env, "x", new InMemoryDurable(unpatched = Set("p")))
      ) ==>
        (
          ("Right(tb(x))", Vector(StepRecord.marker("p"), "b")),
          ("Right(ta(x))", Vector("a"))
        )
    }

    test("recv waits for a message on its topic, recorded as the runtime's two steps") {
      val heard = Step[Env, Option[String], String](StepName("heard"))(r => got => r.tag + got)
      val waited = Flow
        .recv[String](StepName("wait"), topic => topic, _ => FiniteDuration(10, "ms"))
        .andThen(Flow.step(heard))
      val durable = new InMemoryDurable()
      val id = WorkflowId("law")
      // Started, and so known to the runtime, before anything is sent to it.
      try durable.run(id)(_ => throw new InMemoryDurable.Crash)
      catch { case _: InMemoryDurable.Crash => () }
      durable.send(id, "t", "hello")
      durable.run(id)(_ => Flows.run(waited, env, "t").toString) ==> "Right(tSome(hello))"
      durable.recordedSteps(id) ==> Vector(StepRecord.Received, StepRecord.Slept, "heard")
    }

    test("unrecorded is computed over the run's environment, recording nothing of its own") {
      val unkept = Flow.unrecorded[Env, String, String](r => in => s"${r.tag}u($in)")
      ran(unkept.andThen(Flow.step(b)), env, "x") ==> ("Right(tb(tu(x)))", Vector("b"))
    }

    test("a program makes its steps one at a time, each decided from the last's result") {
      ran(planner, env, "") ==> ("Right(ttt)", Vector("grow:0", "grow:1", "grow:2"))
    }

    test("over hands each body what get finds in the larger environment, recording the same") {
      val flow = Flow.step(a).andThen(Flow.step(b))
      val wide = new Wide(new Env("in"), new Env("out"))
      (ran(flow.over[Wide](w => w.inner), wide, "x"), ran(flow, new Env("in"), "x")) ==>
        (
          ("Right(inb(ina(x)))", Vector("a", "b")),
          ("Right(inb(ina(x)))", Vector("a", "b"))
        )
    }
  }
}
