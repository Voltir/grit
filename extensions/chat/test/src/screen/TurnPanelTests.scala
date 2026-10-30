package grit.chat.screen

import grit.chat.look.{Look, Theme}
import grit.chat.view.TurnView
import grit.core.id.TurnSeq
import grit.core.message.Tokens

import utest.*

/** The turn panel's step rows, read off the rows it paints: a step the panel shows is scaled
  * against the other shown steps, and one it hides does not set the scale.
  */
object TurnPanelTests extends TestSuite {

  private val panel = TurnPanel(Look(Theme.Default), Tokens(16000))

  private def view(steps: Vector[TurnView.Step]): TurnView =
    TurnView(TurnSeq(0), "hello", None, steps, None, None, None, None)

  /** The bar cells the row naming `name` paints, or none when no row names it. */
  private def cells(rows: Vector[String], name: String): Int =
    rows.find(_.contains(name)).fold(0)(_.count(_ == '█'))

  val tests = Tests {
    test("a step the panel hides does not set its bars' scale") {
      val v = view(
        Vector(
          TurnView.Step("ask:0", Some(1700)),
          TurnView.Step("DBOS.sleep", Some(10001))
        )
      )
      val rows = panel.blocks(Some(v), runningMs = 0L).map(_.text)
      cells(rows, "ask:0") ==> 8
    }
  }
}
