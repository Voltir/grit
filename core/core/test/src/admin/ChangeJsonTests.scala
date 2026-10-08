package grit.core.admin

import grit.core.identity.TestAccounts
import grit.core.visibility.TestLabels.{compartment, place}
import grit.core.visibility.{Label, Level}

import utest.*

object ChangeJsonTests extends TestSuite {

  private val ops = place("slack:acme/C1")
  private val bo = TestAccounts.account("slack:T1/U-bo")
  private val trial = compartment("trial")
  private val confidential = Label.at(Level.Confidential, trial)

  private val changes: Vector[Change] = Vector(
    Change.Relabel(ops, Label.Public, Change.To.Set(confidential)),
    Change.Relabel(ops, confidential, Change.To.Default(Label.at(Level.Internal))),
    Change.Quiet(ops, true),
    Change.Quiet(ops, false),
    Change.Clear(bo, trial),
    Change.Remove(bo, trial)
  )

  val tests = Tests {
    test("each change is stored under its kind, in the form the audit row keeps") {
      // The audit table keeps these: a renamed key or kind would misread every row before it.
      changes.map(c => (c.kind, ujson.write(ChangeJson.write(c)))) ==> Vector(
        "relabel" ->
          """{"kind":"relabel","room":"slack:acme/C1","from":"public","to":"confidential+trial","default":false}""",
        "relabel" ->
          """{"kind":"relabel","room":"slack:acme/C1","from":"confidential+trial","to":"internal","default":true}""",
        "quiet" -> """{"kind":"quiet","room":"slack:acme/C1","on":true}""",
        "quiet" -> """{"kind":"quiet","room":"slack:acme/C1","on":false}""",
        "clear" -> """{"kind":"clear","person":"slack:T1/U-bo","compartment":"trial"}""",
        "remove" -> """{"kind":"remove","person":"slack:T1/U-bo","compartment":"trial"}"""
      )
    }

    test("each change reads back as itself") {
      changes.map(c => ChangeJson.read(ChangeJson.write(c))) ==> changes.map(Right(_))
    }

    test("a stored form that names no change says why") {
      Vector(
        ChangeJson.read(ujson.Arr()),
        ChangeJson.read(ujson.Obj("kind" -> "release")),
        ChangeJson.read(ujson.Obj("kind" -> "quiet", "room" -> "slack:acme/C1")),
        ChangeJson.read(
          ujson.Obj("kind" -> "clear", "person" -> "slack:T1/U-bo", "compartment" -> "Trial")
        )
      ) ==> Vector(
        Left("a change is not an object"),
        Left("unknown change: release"),
        Left("quiet: no on"),
        Left(
          "clear: Trial is not a compartment's name: lowercase letters, digits and -, 1 to 32 of them"
        )
      )
    }
  }
}
