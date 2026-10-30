package grit.turn

import grit.core.message.AssistantBlock
import grit.core.store.{Entry, Payload}
import grit.core.tool.{
  Args,
  ArgsError,
  Believed,
  Field,
  Gate,
  Hosted,
  Outcome,
  Tool,
  ToolName,
  ToolSpec
}

/** `read_hidden`: how the model reads an item a round's request shows only as its stub
  * ([[TurnGate.stub]]). Reading it is the call: its answer is what the item says, and the next
  * round's gate lowers the turn for that answer, whatever it withdraws ([[TurnGate.decide]]);
  * the stub stays in its place.
  */
object ReadHidden {

  /** Its tool's name, `read_hidden`. */
  val Name: ToolName = ToolName("read_hidden")

  /** What a hidden item says in full, as a request over its window would show it; or why it
    * could not be read.
    */
  type Text = Item -> Either[String, String]

  /** A [[Text]] that reads nothing: for the tool's name and schema alone. */
  val Unread: Text = _ => Left("not read")

  private val spec: ToolSpec[HiddenName] =
    ToolSpec(
      Name,
      "Shows in full something your request shows only as a note (\"Hidden here as h1: " +
        "…\"): what it says comes back as this call's answer. After you read it, you can't do " +
        "what that note says you'd lose until you've answered. Read it when your answer needs " +
        "what it says.",
      Args
        .of((name = Field.text("The name its note gives, such as h1.")))
        .refine(a =>
          HiddenName
            .read(a.name)
            .left
            .map(_ => ArgsError.Invalid("name", "a name such as h1", a.name))
        )
    )

  /** The tool as a round whose gate was `gated` offers it (none for a round ungated): it
    * requires nothing and asks no one. A call naming an item `gated` hides is answered with
    * what `text` reads of it, believed at the item's integrity, or, when `text` reads nothing,
    * fails, quoting nothing of why; any other, that nothing in the request is hidden under that
    * name, as grit's own words ([[Believed.Own]]).
    */
  def tool(gated: Option[Gated], text: Text^): Tool[HiddenName]^{text} = {
    val hidden: Map[HiddenName, Item] =
      gated.fold(Vector.empty)(_.hidden).map((item, h) => h.name -> item).toMap
    new Hosted(spec, Gate.Free, HiddenName.value, Believed.Own).over { name =>
      val n = HiddenName.value(name)
      hidden.get(name) match {
        case Some(item) =>
          text(item) match {
            case Right(said) => Outcome.Done(said, Some(item.integrity))
            case Left(_) => Outcome.Failed(s"$n could not be read.")
          }
        case None => Outcome.Done(s"Nothing in the request you answered is hidden as $n.")
      }
    }
  }

  /** Each item that answers a call of `read_hidden` among `entries`' replies, by the call: what
    * [[TurnGate.decide]] shows whatever reading it withdraws.
    */
  def answers(entries: Vector[Entry]): Set[ItemRef] =
    entries
      .flatMap(_.payload match {
        case Payload.Exchange(reply) =>
          reply.blocks.collect {
            case AssistantBlock.ToolCall(id, name, _) if name == ToolName.value(Name) =>
              ItemRef.Result(id)
          }
        case _ => Vector.empty
      })
      .toSet
}
