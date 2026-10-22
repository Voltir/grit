package grit.slack.event

import grit.core.id.{ToolCallId, WorkflowId}

import utest.*

/** [[Events.clicked]] over `block_actions` payloads shaped as Slack's docs give them. */
object ClickedTests extends TestSuite {
  import Payloads.*

  private val Workflow = WorkflowId("c-1:0")
  private val Call = ToolCallId("toolu_01")
  private val Value = Clicked.value(Workflow, Call)

  val tests = Tests {
    test(
      "a card's click reads as user, workflow, call, button and response url; a payload of anything else reads as none"
    ) {
      val shortcut = ujson.read(click("grit_apply", Value))
      shortcut("type") = "shortcut"
      val two = ujson.read(click("grit_apply", Value))
      two("actions") = ujson.Arr(two("actions")(0), two("actions")(0))
      (
        Events.clicked(click("grit_apply", Value)),
        Events.clicked(click("grit_cancel", Value, user = "U0BEN0001")).map(_.button),
        Vector(
          Events.clicked(shortcut.render()),
          Events.clicked(two.render()),
          Events.clicked(click("someone_elses", Value)),
          Events.clicked(click("grit_apply", "c-1:0/toolu_01")),
          Events.clicked(click("grit_apply", """{"workflow":"c-1:0"}""")),
          Events.clicked("payload=1")
        ).map(_.isLeft)
      ) ==> (
        Right(
          Clicked(
            TeamId(Team),
            UserId(Ana),
            Workflow,
            Call,
            Clicked.Button.Apply,
            ResponseUrl(ActionHook)
          )
        ),
        Right(Clicked.Button.Cancel),
        Vector.fill(6)(true)
      )
    }
  }
}
