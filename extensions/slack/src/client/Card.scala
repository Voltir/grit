package grit.slack.client

import grit.core.id.{ToolCallId, WorkflowId}
import grit.prose.form.{Block, Doc, Text}
import grit.slack.event.Clicked
import grit.slack.text.RichText

/** One ask as Slack shows it to its asker ([[Slack.ask]]): `shown`, the change the gated call
  * `call` of `workflow`'s turn would make, read as written (nothing in it becomes a mention, a
  * link or markup); `until`, when it expires, as the asker reads a time; and an Apply and a
  * Cancel button whose value names `workflow` and `call` alone ([[Clicked.value]]), never the
  * change.
  */
final case class Card(workflow: WorkflowId, call: ToolCallId, shown: String, until: String)

object Card {

  /** `card`'s Block Kit blocks: its words, as [[RichText]] renders a paragraph of each, then its
    * two buttons, Apply styled primary, under the action ids [[Clicked.Button]] names.
    */
  def blocks(card: Card): ujson.Arr = {
    val words = RichText
      .render(
        Doc(
          Vector(
            Block.Paragraph(Text.plain(card.shown)),
            Block.Paragraph(Text.plain(s"Expires ${card.until}."))
          )
        )
      )
      .flatMap(_.blocks.value)
    val value = Clicked.value(card.workflow, card.call)
    def button(b: Clicked.Button, label: String): ujson.Obj = {
      val o = ujson.Obj(
        "type" -> "button",
        "action_id" -> b.actionId,
        "text" -> ujson.Obj("type" -> "plain_text", "text" -> label),
        "value" -> value
      )
      if (b == Clicked.Button.Apply) o("style") = "primary"
      o
    }
    ujson.Arr.from(
      words :+ ujson.Obj(
        "type" -> "actions",
        "block_id" -> "grit_ask",
        "elements" -> ujson.Arr(
          button(Clicked.Button.Apply, "Apply"),
          button(Clicked.Button.Cancel, "Cancel")
        )
      )
    )
  }

  /** `card`'s words as a notification shows them, escaped as [[RichText]] escapes plain text. */
  def fallback(card: Card): String = RichText.escaped(s"${card.shown} (expires ${card.until})")
}
