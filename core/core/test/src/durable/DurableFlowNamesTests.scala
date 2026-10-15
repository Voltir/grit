package grit.core.durable

import scala.concurrent.duration.FiniteDuration

import utest.*
import FlowFixtures.*
import Flows.Family

/** The families [[Flows.names]] lists of a flow's description, and which every run records. */
object DurableFlowNamesTests extends TestSuite {

  private val counted = Step[Env, Int, String](StepName("n"))(r => n => r.tag * n)

  val tests = Tests {
    test("a sequence's families are listed in order, each recorded by every run") {
      Flows.names(Flow.step(a).andThen(Flow.step(item)).andThen(Flow.step(b))) ==>
        Vector(Family("a", false, true), Family("item", true, true), Family("b", false, true))
    }

    test("what follows a flow that may end early is not every run's, unless an attempt keeps it") {
      (
        Flows.names(stopping.andThen(Flow.step(b))),
        Flows.names(Flow.attempt(stopping).map(_.toString).andThen(Flow.step(b)))
      ) ==>
        (
          Vector(Family("item", true, true), Family("b", false, false)),
          Vector(Family("item", true, true), Family("b", false, true))
        )
    }

    test("an arm of either, and a patch's marker and branches, are not every run's") {
      (
        Flows.names(Flow.either(Flow.step(a), Flow.step(counted))),
        Flows.names(Flow.patch("p")(Flow.step(a), Flow.step(b)).andThen(Flow.step(item)))
      ) ==>
        (
          Vector(Family("a", false, false), Family("n", false, false)),
          Vector(
            Family(StepRecord.marker("p"), false, false),
            Family("a", false, false),
            Family("b", false, false),
            Family("item", true, true)
          )
        )
    }

    test("a loop's round is every run's; an each's flow is not, since its items may be none") {
      val all = Flow.each[Env, String, String, String, String](_.split(",").toVector)(stopping)
      (Flows.names(growing), Flows.names(all.map(_.mkString).andThen(Flow.step(b)))) ==>
        (
          Vector(Family("round", true, true)),
          Vector(Family("item", true, false), Family("b", false, false))
        )
    }

    test("a wait is listed by its label, and a program by its family alone") {
      val waited =
        Flow.recv[String](
          StepName.keyed[String]("wait")(t => t),
          t => t,
          _ => FiniteDuration(10, "ms")
        )
      Flows.names(waited.map(_.mkString).andThen(planner)) ==>
        Vector(Family("wait", true, true), Family("plan", true, false))
    }

    test("a family reached twice is listed once, where first reached, every run's if either is") {
      Flows.names(Flow.either(Flow.step(a), Flow.step(b)).andThen(Flow.step(a))) ==>
        Vector(Family("a", false, true), Family("b", false, false))
    }

    test("a flow over a larger environment lists what the flow lists") {
      Flows.names(stopping.andThen(Flow.step(b)).over[Wide](w => w.inner)) ==>
        Vector(Family("item", true, true), Family("b", false, false))
    }
  }
}
