package grit.core.durable

import scala.concurrent.duration.FiniteDuration

import grit.core.id.WorkflowId

import utest.*
import FlowFixtures.*
import Flows.{Read, Reading, Unreadable}

/** What [[Flows.read]] makes of a history: at every prefix of what [[Flows.run]] recorded, the
  * step the run made next, and over the whole of it, how the run ended.
  */
object DurableFlowReadTests extends TestSuite {

  /** A fact the suite's steps show a reader. */
  final case class Saw(what: String) extends Observed

  /** One reading, as `at:step` for the next step, or how and where the flow ended. */
  private def shown[E, O](read: Either[Unreadable, Reading[E, O]]): String = read match {
    case Right(Reading(Read.Next(step, at), _)) => s"$at:$step"
    case Right(Reading(Read.Ended(result, steps), _)) => s"ended $steps: $result"
    case Left(why) => why.toString
  }

  /** What `read` says at each prefix of `history`, shortest first, the whole of it last. */
  private def prefixes[R, I, E, O](
      flow: Flow[R, I, E, O],
      in: I,
      history: Vector[StepRecord]
  ): Vector[String] =
    (0 to history.size).toVector.map(k => shown(Flows.read(flow, in, history.take(k))))

  /** [[prefixes]] of what `flow` recorded, run once from `in`, with what the run returned. */
  private def readings[R, I, E, O](
      flow: Flow[R, I, E, O],
      env: R^,
      in: I,
      durable: InMemoryDurable = new InMemoryDurable()
  ): (String, Vector[String]) = {
    val (out, history) = ran(flow, env, in, durable)
    (out, prefixes(flow, in, history))
  }

  private val env = new Env("t")

  val tests = Tests {
    test("read names the step a run makes next at every prefix, and how it ended at the last") {
      readings(stopping.andThen(Flow.step(b)), env, "go") ==>
        ("Right(tb(t(go)))", Vector("0:item:go", "1:b", "ended 2: Right(tb(t(go)))"))
    }

    test("an early end is read as the flow's end") {
      readings(stopping.andThen(Flow.step(b)), env, "stop") ==>
        ("Left(stopped at t(stop))", Vector("0:item:stop", "ended 1: Left(stopped at t(stop))"))
    }

    test("a loop and an each are read round by round, item by item") {
      val all = Flow.each[Env, String, String, String, String](_.split(",").toVector)(stopping)
      (readings(growing, env, "")._2, readings(all, env, "p,q")._2) ==>
        (
          Vector("0:round:0", "1:round:1", "2:round:2", "ended 3: Right(ttt)"),
          Vector("0:item:p", "1:item:q", "ended 2: Right(Vector(t(p), t(q)))")
        )
    }

    test("either is read down the arm its input names") {
      val counted = Step[Env, Int, String](StepName("n"))(r => n => r.tag * n)
      val split = Flow.either(Flow.step(a), Flow.step(counted))
      (readings(split, env, Left("x"))._2, readings(split, env, Right(2))._2) ==>
        (Vector("0:a", "ended 1: Right(ta(x))"), Vector("0:n", "ended 1: Right(tt)"))
    }

    test("a patch is read down the branch the history took; a history ending there takes it") {
      val patched = Flow.patch("p")(Flow.step(a), Flow.step(b))
      (
        readings(patched, env, "x")._2,
        readings(patched, env, "x", new InMemoryDurable(unpatched = Set("p")))._2
      ) ==>
        (
          Vector("0:DBOS.patch-p", "1:b", "ended 2: Right(tb(x))"),
          Vector("0:DBOS.patch-p", "ended 1: Right(ta(x))")
        )
    }

    test("a wait is named by its label at both of the runtime's records") {
      val heard = Step[Env, Option[String], String](StepName("heard"))(r => got => r.tag + got)
      val waited = Flow
        .recv[String](StepName("wait"), topic => topic, _ => FiniteDuration(10, "ms"))
        .andThen(Flow.step(heard))
      val durable = new InMemoryDurable()
      val id = WorkflowId("flow")
      // Started, and so known to the runtime, before anything is sent to it.
      try durable.run(id)(_ => throw new InMemoryDurable.Crash)
      catch { case _: InMemoryDurable.Crash => () }
      durable.send(id, "t", "hello")
      readings(waited, env, "t", durable)._2 ==>
        Vector("0:wait", "1:wait", "2:heard", "ended 3: Right(tSome(hello))")
    }

    test("a program is read by running its code again over the results the history holds") {
      readings(planner, env, "") ==>
        ("Right(ttt)", Vector("0:grow:0", "1:grow:1", "2:grow:2", "ended 3: Right(ttt)"))
    }

    test("a history ending inside a program is read from that step, whatever the program does") {
      // The program catches what stopped it, and returns as if it had not been stopped.
      val catching = Flow.program[Env, String, String]("catching") { steps => start =>
        try steps.run(Flow.step(a), start).merge
        catch { case _: RuntimeException => "caught" }
      }
      prefixes(catching, "x", Vector.empty) ==> Vector("0:a")
    }

    test("a read stops at what no step records, opaque from there on") {
      val unkept = Flow.unrecorded[Env, String, String](r => in => s"${r.tag}u($in)")
      readings(Flow.step(a).andThen(unkept).andThen(Flow.step(b)), env, "x") ==>
        ("Right(tb(tu(ta(x))))", Vector("0:a", "Opaque(1)", "Opaque(1)"))
    }

    test("a flow over a larger environment is read as the flow itself") {
      val flow = Flow.step(a).andThen(Flow.step(b))
      val (_, history) = ran(flow, env, "x")
      (prefixes(flow.over[Wide](w => w.inner), "x", history), prefixes(flow, "x", history)) ==>
        (
          Vector("0:a", "1:b", "ended 2: Right(tb(ta(x)))"),
          Vector("0:a", "1:b", "ended 2: Right(tb(ta(x)))")
        )
    }

    test(
      "what each recorded step's input showed is read in order, a step not yet made shows nothing"
    ) {
      val seen =
        Step[Env, String, String](StepName("seen"), in => Some(Saw(in)))(r => in => r.tag + in)
      val again = Step[Env, String, String](StepName("again"), in => Some(Saw(in)))(_ => in => in)
      val flow = Flow.step(seen).andThen(Flow.step(again))
      val (_, history) = ran(flow, env, "x")
      (0 to history.size).toVector.map(k =>
        Flows.read(flow, "x", history.take(k)).map(_.observed)
      ) ==>
        Vector(Right(Vector()), Right(Vector(Saw("x"))), Right(Vector(Saw("x"), Saw("tx"))))
    }

    test(
      "a history that holds another step, an unreadable output or none is unreadable, saying where"
    ) {
      val counted = Step[Env, String, Int](StepName("n"))(_ => in => in.length)(using
        Journaled.json[Int](ujson.Num(_), v => v.numOpt.map(_.toInt).toRight("not a number"))
      )
      val flow = Flow.step(a).andThen(Flow.step(b))
      val gated = Flow.step(a).andThen(Flow.step(counted))
      (
        Flows.read(
          flow,
          "x",
          Vector(StepRecord("a", Some("ta(x)"), None), StepRecord("c", Some(""), None))
        ),
        Flows.read(
          gated,
          "x",
          Vector(StepRecord("a", Some("ta(x)"), None), StepRecord("n", Some("\"x\""), None))
        ),
        Flows.read(flow, "x", Vector(StepRecord("a", None, None))),
        Flows.read(
          planner,
          "",
          Vector(StepRecord("grow:0", Some("t"), None), StepRecord("grow:2", Some("tt"), None))
        )
      ) ==>
        (
          Left(Unreadable.Diverged(1, "b", "c")),
          Left(Unreadable.Undecodable(1, "n", "not a number")),
          Left(Unreadable.Unrecorded(0, "a")),
          Left(Unreadable.Diverged(1, "grow:1", "grow:2"))
        )
    }
  }
}
