package grit.core.allot

import grit.core.model.Effort

import utest.*

object AllotmentTests extends TestSuite {

  import Allotment.{Calls, Reasoning}

  val tests = Tests {
    test("reasoning at an effort applies only to a pin that asks one") {
      Reasoning.At(Effort.High).over(Some(Effort.Low)) ==> Some(Effort.High)
      Reasoning.At(Effort.Low).over(Some(Effort.High)) ==> Some(Effort.Low)
      Reasoning.At(Effort.High).over(None) ==> None
    }

    test("reasoning at most an effort lowers the pin's own and never raises it") {
      Reasoning.AtMost(Effort.Medium).over(Some(Effort.High)) ==> Some(Effort.Medium)
      Reasoning.AtMost(Effort.Medium).over(Some(Effort.Low)) ==> Some(Effort.Low)
      Reasoning.AtMost(Effort.Medium).over(None) ==> None
    }

    test("reasoning of the pin's own keeps it") {
      Reasoning.Own.over(Some(Effort.XHigh)) ==> Some(Effort.XHigh)
      Reasoning.Own.over(None) ==> None
    }

    test("scaled calls are at least once the deployment's, and none under it is built") {
      Calls.Scaled.of(3).map(_.times) ==> Some(3)
      Calls.Scaled.of(1).map(_.times) ==> Some(1)
      Calls.Scaled.of(0) ==> None
      Calls.Scaled.of(-2) ==> None
    }
  }
}
