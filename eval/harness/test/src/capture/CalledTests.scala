package grit.eval.harness.capture

import grit.core.tool.ToolName
import grit.turn.TurnVerdict

import utest.*

/** What a tool call named, as a turn's capture reads the name the model sent. */
object CalledTests extends TestSuite {

  private val offered = Set(ToolName("read"), ToolName("search"))

  val tests = Tests {
    test("an offered tool by name, the turn's topic tool apart, and any other name unnamed") {
      Vector("read", ToolName.value(TurnVerdict.Name), "made_up", "").map(
        Called.of(_, offered)
      ) ==> Vector(Called.Tool(ToolName("read")), Called.Topic, Called.Unnamed, Called.Unnamed)
    }

    test("a set that offers a tool named topic counts a call to it as that tool") {
      Called.of("topic", offered + ToolName("topic")) ==> Called.Tool(ToolName("topic"))
    }
  }
}
