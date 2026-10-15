package grit.turn

import grit.act.phase.Shaping
import grit.core.act.{Extraction, Posed}
import grit.core.flow.{Extracted, Shape, Trusted}
import grit.core.id.{CallSlot, EntryId, WorkflowId}
import grit.core.model.StrictSchemas
import grit.core.provider.ModelRequest
import grit.core.tool.{Args, ArgsError, Field, Gate, Hosted, Outcome, Tool, ToolName, ToolSpec}
import grit.core.visibility.Integrity

/** `extract`: how the model asks a question of an item a round's request shows only as its
  * stub ([[TurnGate.stub]]), without reading it.
  */
object Extract {

  /** Its tool's name, `extract`. */
  val Name: ToolName = ToolName("extract")

  /** A call: the stub's `name`, the `question` asked of its item, and the `shape` its answer
    * is read in.
    */
  type Asking = (name: HiddenName, question: String, shape: Shape)

  /** How a question is put to a hidden item, told the call that puts it: its answer in the
    * shape, `None` when the item does not say, trusted as an extraction is
    * ([[grit.core.act.Extraction.Node]]); or why no answer read.
    */
  type Answer = (Item, String, Shape, CallSlot) -> Either[String, Trusted[Option[Extracted]]]

  /** An answer for a tool that is described or bound but never called: it answers nothing. */
  val Unasked: Answer = (_, _, _, _) => Left("not asked")

  /* What a refused shape's reason ends with: for a short code, where longer text is had. */
  private def longer(kind: String): String =
    if (kind == "token") " For longer text, read the item." else ""

  private val spec: ToolSpec[Asking] =
    ToolSpec(
      Name,
      "It can only answer yes or no, a whole number, a date, one of options you list, or a " +
        s"short code of up to ${Shape.MaxToken} letters, digits, _ or -. It can't give you any " +
        "other text, such as a title, a name or a sentence: for those, read the item. It asks " +
        "a question of something your request shows only as a note (\"Hidden here as h1: " +
        "…\"), without reading it: a separate model, shown only that and your question, " +
        "answers in the shape you choose, and you are told only that answer. Using it never " +
        "stops you doing anything you can do now. Shapes: yes_no; number, a whole number from " +
        "min to max; day, a date; one_of, one of the options you list (at most " +
        s"${Shape.MaxOptions}, each at most ${Shape.MaxOption} characters); token, a short code " +
        s"of at most max (up to ${Shape.MaxToken}) characters of A-Z, a-z, 0-9, _ and -. The " +
        "answer may be that the item does not say.",
      Args
        .of(
          (
            name = Field.text("The name its note gives, such as h1."),
            question = Field.text("What to ask of the item, in one sentence."),
            shape = Field.oneOf(
              "The shape the answer is read in.",
              "yes_no",
              "number",
              "day",
              "one_of",
              "token"
            ),
            min =
              Field.count("For number: the least it may be.", Int.MinValue, Int.MaxValue).optional,
            max = Field
              .count(
                "For number: the most it may be. For token: the most characters it may have.",
                Int.MinValue,
                Int.MaxValue
              )
              .optional,
            options =
              Field.texts("For one_of: the options, in order.", 1, Shape.MaxOptions).optional
          )
        )
        .refine(a =>
          for {
            name <- HiddenName
              .read(a.name)
              .left
              .map(_ => ArgsError.Invalid("name", "a name such as h1", a.name))
            shape <- Shape
              .of(
                ujson.Obj.from(
                  Vector[(String, ujson.Value)]("kind" -> a.shape) ++
                    a.min.map(n => "min" -> ujson.Num(n)) ++
                    a.max.map(n => "max" -> ujson.Num(n)) ++
                    a.options.map(o => "options" -> ujson.Arr.from(o.map(ujson.Str(_))))
                )
              )
              .left
              .map(why => ArgsError.Refused("shape", s"$why.${longer(a.shape)}"))
          } yield (name = name, question = a.question, shape = shape)
        )
    )

  /** The tool as a round whose gate was `gated` offers it (none for a round ungated): it
    * requires nothing, asks no one, and puts each question to `answer`. A call naming an item
    * `gated` hides is answered with the answer, or that the item does not say, believed as
    * `answer` trusts it. When `answer` gives no answer the call fails, quoting nothing of why;
    * a name `gated` hides nothing under is answered so, as grit's own words
    * ([[grit.core.tool.Believed.Own]]), and nothing is asked.
    */
  def tool(gated: Option[Gated], answer: Answer^): Tool[Asking]^{answer} = {
    val hidden: Map[HiddenName, Item] =
      gated.fold(Vector.empty)(_.hidden).map((item, h) => h.name -> item).toMap
    new Hosted(spec, Gate.Free, a => s"${HiddenName.value(a.name)}: ${a.question}").own.calling {
      (a, slot) =>
        val n = HiddenName.value(a.name)
        hidden.get(a.name) match {
          case None => Outcome.Done(s"Nothing in the request you answered is hidden as $n.")
          case Some(item) =>
            answer(item, a.question, a.shape, slot) match {
              case Left(_) => Outcome.Failed(s"$n gave no answer that reads as ${named(a.shape)}.")
              case Right(said) =>
                said.extract match {
                  case None => Outcome.Done(s"$n does not say.", Some(said.integrity))
                  case Some(x) => Outcome.Done(s"$n answers: ${written(x)}.", Some(said.integrity))
                }
            }
        }
    }
  }

  /** The level an answer to a question asked under `gated` is extracted at: the gate's,
    * `Untrusted` with none. [[grit.core.act.Extraction.Node]] meets it with `Member`, so a turn
    * at `Member` or below does not fall for extracting, and an `Authority` turn that extracts
    * falls to `Member`.
    */
  def level(gated: Option[Gated]): Integrity = gated.fold(Integrity.Untrusted)(_.level)

  /** The quarantined ask's request: [[Extraction.System]], then [[Extraction.messages]],
    * offered only the tool `reply`, which it must call ([[grit.act.phase.Shaping.request]],
    * `strict` as the pin's settings say); why not, when `shape` writes no schema (a reversed
    * range).
    */
  def request(
      text: String,
      question: String,
      shape: Shape,
      strict: StrictSchemas
  ): Either[String, ModelRequest] =
    Extraction
      .reply(shape)
      .map(typed =>
        Shaping.request(
          Extraction.System,
          Extraction.messages(text, question, shape),
          Posed.ReplyTool,
          typed.schema,
          strict
        )
      )

  /** The id the `call`th model call (from 0) of the extraction `slot` makes is kept under in
    * the usage ledger: `extract:{workflow}:{round}:{index}`, then `:{call}` after the first.
    */
  def costId(slot: CallSlot, call: Int): EntryId = {
    val base = s"$Prefix${WorkflowId.value(slot.turn.workflowId)}:${slot.round}:${slot.index}"
    EntryId(if (call == 0) base else s"$base:$call")
  }

  /** What every [[costId]] starts with. */
  private[turn] val Prefix = "extract:"

  /** What an answer in `shape` is, briefly. */
  private def named(shape: Shape): String = shape match {
    case Shape.YesNo => "true or false"
    case Shape.Number(min, max) => s"a whole number from $min to $max"
    case Shape.Day => "a date"
    case Shape.OneOf(_) => "one of your options"
    case Shape.Token(max) => s"a token of at most $max characters"
  }

  /** `x` as the model is told it. */
  private def written(x: Extracted): String = x match {
    case Extracted.Yes(b) => if (b) "yes" else "no"
    case Extracted.Number(n) => n.toString
    case Extracted.Day(d) => d.toString
    case Extracted.Chosen(text) => text
    case Extracted.Token(text) => text
  }
}
