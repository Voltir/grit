package grit.turn

import grit.core.id.{EntryId, ToolCallId, TurnRef, WorkflowId}
import grit.core.message.{AssistantBlock, Message}
import grit.core.provider.ModelRequest
import grit.core.store.{Entry, Payload}
import grit.core.topic.Topic

/** A turn's short summary: what the summary model is asked, and what of its reply is kept.
  * The summary is written from the turn's own messages alone, so it reads on its own
  * wherever it is retrieved.
  */
object TurnSummary {

  val System: String =
    "You write the memory of a conversation. Summarise the exchange below in one or two " +
      "sentences: what was asked, and what was answered or decided. Keep names, numbers, " +
      "file names and identifiers exactly as written. Reply with the summary only."

  /** The system prompt when the summary also names and describes the turn's topic. */
  val TopicalSystem: String =
    "You write the memory of a conversation. Read the exchange below and reply with exactly " +
      "three lines and nothing else:\n" +
      "Summary: one or two sentences: what was asked, and what was answered or decided. Keep " +
      "names, numbers, file names and identifiers exactly as written.\n" +
      "Topic: the name of the topic this exchange belongs to, at most four words. If the " +
      "topic already has a name, repeat it.\n" +
      "About: one line saying what the topic covers so far, this exchange included."

  /** What the request says of a topic with no name yet. Worded so it has nothing a model
    * could repeat as a name: "Topic so far: new, not yet named." came back live as the
    * name "new, not yet named".
    */
  val TopicUnnamed: String = "This exchange starts a topic that has no name yet: give it one."

  /** The most words a topic's name keeps. */
  val NameWords = 4

  /** How much of a tool result the summary model sees. The store keeps all of it. */
  val ToolResultChars = 2000

  /** The id of `turn`'s summary entry. */
  def id(turn: TurnRef): EntryId =
    EntryId(s"summary:${WorkflowId.value(turn.workflowId)}")

  /** The request that summarises the turn whose entries are `own`, as one transcript. With
    * `topic`, the topic its message is in, as it stands, it also asks for the topic's name
    * and what it covers ([[read]]).
    */
  def request(own: Vector[Entry], topic: Option[Topic] = None): ModelRequest = {
    val transcript = own.flatMap(e => line(e.payload)).mkString("\n\n")
    topic match {
      case None => ModelRequest(System, Vector(Message.User(transcript)))
      case Some(t) =>
        val so = t.name.fold(TopicUnnamed)(n =>
          s"Topic so far: $n${t.summary.fold("")(a => s" (about: $a)")}."
        )
        ModelRequest(TopicalSystem, Vector(Message.User(s"$so\n\n$transcript")))
    }
  }

  /** A summary as [[read]] reads it: the turn's `summary`, and its topic's `name` and what
    * it is `about`, when the reply gave both.
    */
  final case class Read(summary: String, topic: Option[(String, String)])

  /** The summary in `reply`, and the topic lines when it has them: `Summary:`, `Topic:` and
    * `About:` (any case, markdown emphasis ignored), a label's text running to the next
    * label, text before the first label standing for a missing `Summary:`. Three unlabelled
    * lines are read as those three lines in order. Otherwise, without a summary, the whole
    * text is the summary. `None` when it has no text. A name is cut to [[NameWords]] words.
    */
  def read(reply: Message.Assistant): Option[Read] =
    text(reply).map { whole =>
      val Label = """(?i)^[*_#\s]*(summary|topic|about)[*_\s]*:[*_\s]*(.*)$""".r
      val labelled = whole.linesIterator.toVector.foldLeft(Vector.empty[(String, String)]) {
        (acc, line) =>
          line match {
            case Label(label, rest) => acc :+ (label.toLowerCase -> rest.trim)
            case other =>
              acc.lastOption match {
                case Some((label, text)) => acc.dropRight(1) :+ (label -> s"$text\n$other".trim)
                case None => acc :+ ("" -> other.trim)
              }
          }
      }
      def field(label: String): Option[String] =
        labelled.collectFirst { case (l, t) if l == label => t.trim }.filter(_.nonEmpty)
      val lines = whole.linesIterator.map(_.trim).filter(_.nonEmpty).toVector
      if (
        labelled.forall(_._1.isEmpty) && lines.size == 3 && lines.lift(1).exists(!_.contains(':'))
      ) {
        // Three bare lines: the asked-for lines without their labels (seen live with
        // gemini-2.5-flash-lite). A name has no colon; a transcript's "User: ..." line does.
        val name = lines.lift(1).map(cleanName).filter(_.nonEmpty)
        Read(lines.headOption.getOrElse(whole), name.zip(lines.lift(2)))
      } else
        // Text before the first label is the summary when `Summary:` is missing (also
        // seen live): the model wrote the summary and labelled only the other lines.
        field("summary").orElse(field("").filter(_ => labelled.exists(_._1.nonEmpty))) match {
          case None => Read(whole, None)
          case Some(summary) =>
            val name = field("topic").map(cleanName).filter(_.nonEmpty)
            Read(summary, name.zip(field("about").map(_.linesIterator.mkString(" "))))
        }
    }

  private def cleanName(raw: String): String =
    raw
      .replaceAll("""[*_"“”`]""", "")
      .split("\\s+")
      .toVector
      .filter(_.nonEmpty)
      .take(NameWords)
      .mkString(" ")
      .stripSuffix(".")

  /** The summary in `reply`: its text, trimmed; `None` when it has none. */
  def text(reply: Message.Assistant): Option[String] =
    Some(reply.blocks.collect { case AssistantBlock.Text(t) => t }.mkString.trim)
      .filter(_.nonEmpty)

  private def line(payload: Payload): Option[String] = payload match {
    case Payload.Exchange(reply) => line(Payload.Message(reply))
    case Payload.Result(result, _) => line(Payload.Message(result))
    case Payload.Message(Message.User(text)) => Some(s"User: $text")
    case Payload.Message(Message.Assistant(blocks, _, _, _, _)) =>
      val said = blocks.collect {
        case AssistantBlock.Text(t) => t
        case AssistantBlock.ToolCall(id, name, arguments) =>
          s"[called $name (${ToolCallId.value(id)}) with ${arguments.render()}]"
      }
      Option.when(said.nonEmpty)(s"Assistant: ${said.mkString("\n")}")
    case Payload.Message(Message.ToolResult(id, content, isError)) =>
      val kind = if (isError) "Tool error" else "Tool result"
      Some(s"$kind (${ToolCallId.value(id)}): ${content.take(ToolResultChars)}")
    case Payload.Summary(_) | Payload.Query(_) | Payload.Window(_, _) | Payload.Topic(_) |
        Payload.Attempt(_) | Payload.Ask(_, _) | Payload.Closed(_, _, _) =>
      None
  }
}
