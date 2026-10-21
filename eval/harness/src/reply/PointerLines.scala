package grit.eval.harness.reply

import grit.core.context.{Retrieved, SectionTag}
import grit.core.document.{DocText, Document}
import grit.core.id.DocumentVersionId
import grit.core.message.Message
import grit.core.provider.ModelRequest
import grit.core.tool.ToolName
import grit.turn.{TurnRecord, TurnRelevance}

import TurnRelevance.Verdict

/** The pointer-lines window variant, an eval recipe that grit never ships: a turn's request
  * as sent, where a document its judge rejected is shown nothing, and again with one line for
  * each such document, its first line and the call that reads it, never its text; so a reply
  * that would have read a rejected document can be told from one that would not.
  */
object PointerLines {

  /** How many characters of a document's first line its pointer line shows. */
  val Title = 80

  /** What opens the message of pointer lines ([[variant]]). */
  val Lead: String =
    s"${SectionTag.Document.tag} documents found for this turn but judged not relevant, " +
      "so not shown; any of them can still be read:"

  /** The documents `retrieval`'s verdicts rejected, in its results' order. */
  def rejected(retrieval: TurnRecord.Retrieval): Vector[DocumentVersionId] = {
    val no = retrieval.verdicts.collect { case (r, Verdict.Rejected(_)) => r }.toSet
    retrieval.results.ranked.collect { case (r @ Retrieved.Doc(v), _) if no(r) => v }
  }

  /** `request` with one user message after the sections and documents it opens with: [[Lead]],
    * then for each of `documents` whose pointer names a tool `request` offers, a line "- {its
    * first line, cut to [[Title]] characters}: {its pointer's line}"
    * ([[grit.core.document.Pointer.shown]]); `request` unchanged when none has such a pointer.
    */
  def variant(request: ModelRequest, documents: Vector[Document]): ModelRequest = {
    val offered = request.tools.map(_.name).toSet
    val lines = documents.flatMap(d =>
      d.pointer
        .filter(p => offered(ToolName.value(p.tool)))
        .map(p => s"- ${title(d)}: ${p.shown}")
    )
    if (lines.isEmpty) request
    else {
      val (window, own) = request.messages.span(opens)
      request.copy(messages = (window :+ Message.User((Lead +: lines).mkString("\n"))) ++ own)
    }
  }

  private def title(d: Document): String =
    DocText.value(d.text).linesIterator.nextOption().getOrElse("").take(Title)

  // What the request's window opens with, as the turn builds it: its sections, then its documents.
  private def opens(m: Message): Boolean = m match {
    case Message.User(text) =>
      Vector(SectionTag.Afar, SectionTag.Strand, SectionTag.Document).exists(t =>
        text.startsWith(t.tag)
      )
    case _ => false
  }
}
