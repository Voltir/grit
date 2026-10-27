package grit.core.act

import grit.core.flow.{Bounded, Downgrade, Extracted, Raise, Shape, Trusted}
import grit.core.message.Message
import grit.core.prompt.{Fenced, Tag}
import grit.core.schema.Typed
import grit.core.tool.{Args, Field}
import grit.core.visibility.Integrity

/** The quarantined ask an extraction makes, the turn's `extract` tool's and a planner's
  * ([[Moves.extract]]) alike: one question of one text, answered in a shape.
  */
object Extraction {

  /** What an extraction's answer, or that there is none, is trusted at: the turn's or run's
    * level met with `Member` ([[Downgrade.extraction]]).
    */
  val Node: Downgrade[(answer: Option[Extracted], level: Integrity), Trusted[Option[Extracted]]] =
    Downgrade.extraction[Extracted, Bounded.K]

  /** [[Node]] where it was handed. */
  type Raising = Raise[(answer: Option[Extracted], level: Integrity), Trusted[Option[Extracted]]]

  /** What the quarantined ask is told: that it answers one question of one item, through its
    * one tool; ending with [[Fenced.Rule]], which says the item is material.
    */
  val System: String = Fenced.told(
    "You answer one question about one item of text, by calling reply. The item is in the " +
      "<item> tags. Answer from what the item says alone; when it does not say, answer null."
  )

  /** What it is sent after [[System]]: one message holding the item's `text` fenced in
    * `<item>` ([[grit.core.prompt.Fenced]]), then the `question` and the shape's answer
    * described.
    */
  def messages(text: String, question: String, shape: Shape): Vector[Message] =
    Vector(
      Message.User(
        Fenced.in(Tag.Item, text).rendered + s"\n\nQuestion: $question\n\n" +
          s"Answer with ${shape.accepts}, or null when the item does not say."
      )
    )

  /** The quarantined ask's reply, `{"answer": …}`: the answer read in `shape`
    * ([[Field.shaped]]), `None` when it is `null`; refused, saying what it takes in words the
    * model can act on, when it does not read. Why not, when `shape` writes no schema (a
    * reversed range, or no options).
    */
  def reply(shape: Shape): Either[String, Typed[Option[Extracted]]] =
    Field
      .shaped(s"${shape.accepts.capitalize}, or null when the item does not say.", shape)
      .flatMap(answer =>
        Args.of((answer = answer.optional)).map(_.answer).typed.left.map(_.toString)
      )

}
