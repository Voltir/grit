package grit.turn

import grit.core.message.AssistantBlock
import grit.core.store.{Entry, Payload}
import grit.core.tool.{Args, ArgsError, Field, Gate, Hosted, Outcome, Tool, ToolName, ToolSpec}

/** `read_hidden`: how the model reads an item a round's request shows only as its stub
  * ([[TurnGate.stub]]). Reading it is the call: the next round's gate shows the item in full,
  * in its place, and lowers the turn for it, whatever it withdraws ([[TurnGate.decide]]).
  */
object ReadHidden {

  /** Its tool's name, `read_hidden`. */
  val Name: ToolName = ToolName("read_hidden")

  private val spec: ToolSpec[HiddenName] =
    ToolSpec(
      Name,
      "Shows in full something your request shows only as a note (\"Hidden here as h1: " +
        "…\"), in its place, from your next request on. After you read it, you can't do what " +
        "that note says you'd lose until you've answered. Read it when your answer needs what " +
        "it says.",
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
    * requires nothing and asks no one. A call naming an item `gated` hides is answered that the
    * item is shown in full from the next request on, believed at the item's integrity; any
    * other, that nothing in the request is hidden under that name, as grit's own words
    * ([[Believed.Own]]).
    */
  def tool(gated: Option[Gated]): Tool[HiddenName] = {
    val hidden: Map[HiddenName, Item] =
      gated.fold(Vector.empty)(_.hidden).map((item, h) => h.name -> item).toMap
    new Hosted(spec, Gate.Free, HiddenName.value).own.over { name =>
      val n = HiddenName.value(name)
      hidden.get(name) match {
        case Some(item) =>
          Outcome.Done(
            s"$n is shown in full in its place from your next request on.",
            Some(item.integrity)
          )
        case None => Outcome.Done(s"Nothing in the request you answered is hidden as $n.")
      }
    }
  }

  /** The names `entries`' replies called `read_hidden` with, arguments that do not read as a
    * name skipped.
    */
  def read(entries: Vector[Entry]): Set[HiddenName] =
    entries
      .flatMap(_.payload match {
        case Payload.Exchange(reply) =>
          reply.blocks.collect {
            case AssistantBlock.ToolCall(_, name, args) if name == ToolName.value(Name) => args
          }
        case _ => Vector.empty
      })
      .flatMap(spec.args.read(_).toOption)
      .toSet
}
