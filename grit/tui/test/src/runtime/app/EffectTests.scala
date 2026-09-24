package grit.tui.runtime.app

import utest.*

object EffectTests extends TestSuite {

  private enum Msg {
    case Tick
  }

  private val autoscroll = TimerId.of("autoscroll")
  private val stream = TimerId.of("stream")

  val tests = Tests {

    test("batch drops NoOp and collapses to the single effect that survived") {
      val one = Effect.batch(Effect.NoOp, Effect.After(autoscroll, 40L, Msg.Tick), Effect.NoOp)
      assert(one == Effect.After(autoscroll, 40L, Msg.Tick))
      assert(Effect.batch[Msg]() == Effect.NoOp)
      assert(Effect.batch(Effect.NoOp, Effect.NoOp) == Effect.NoOp)
    }

    test("batch flattens, so a nested batch never changes what an assertion sees") {
      val nested = Effect.batch(
        Effect.batch(Effect.After(autoscroll, 40L, Msg.Tick), Effect.NoOp),
        Effect.batch(Effect.Cancel(stream), Effect.batch(Effect.Quit))
      )
      assert(
        nested == Effect.Batch(
          Vector(Effect.After(autoscroll, 40L, Msg.Tick), Effect.Cancel(stream), Effect.Quit)
        )
      )
    }

  }
}
