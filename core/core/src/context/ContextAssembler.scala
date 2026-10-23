package grit.core.context

import grit.core.id.{ConversationId, DocumentVersionId, EntrySeq, PieceId, TurnRef, TurnSeq}
import grit.core.message.{AssistantBlock, Message, Tokens, Usage}
import grit.core.provider.ModelRequest
import grit.core.store.{Db, Nearby, StoreError}

/** Chooses what the model sees on a turn: a fresh window over the store, computed each turn,
  * never carried from the last one, in four reads a turn records apart ([[prepare]],
  * [[search]], [[briefs]], [[assemble]]), so a turn resumed after a crash repeats none of the
  * calls it makes between them. It holds no write capability and no model; each read is made
  * for its turn ([[grit.core.visibility.Subject.Turn]]) over entries recorded before it.
  * `AssemblyError.Store` when a store fails or the conversation is gone.
  */
trait ContextAssembler extends caps.Pure {

  /** Whether `request.turn`'s window needs a search: [[Prepared.Drawn]] when nothing could be
    * found by searching, else the request its search query is written from.
    */
  def prepare(request: AssemblyRequest)(using Db^): Either[AssemblyError, Prepared]

  /** What `written`'s query finds for `request.turn`, best first, at most the request's
    * width's hits of each kind; empty for an assembler that never asks for a query.
    */
  def search(request: AssemblyRequest, written: Querying.Written)(using
      Db^
  ): Either[AssemblyError, Results]

  /** The turn's own messages, and each of `retrieved` still there and still a candidate, as a
    * judge is shown them, each cut to `chars` characters; one gone since it was found is left
    * out.
    */
  def briefs(
      request: AssemblyRequest,
      retrieved: Vector[Retrieved],
      chars: Int
  )(using Db^): Either[AssemblyError, Briefs]

  /** `request.turn`'s window. Drawn: the window with no search. Searched: that window's recent
    * part, then each admitted result still there and still a candidate, in its order, each one
    * that fits what is left; one not admitted is never shown. Its notes are the query's, then which
    * earlier turns of its own it recalled.
    */
  def assemble(request: AssemblyRequest, packing: Packing)(using
      Db^
  ): Either[AssemblyError, Window]
}

object ContextAssembler {

  /** `assembler`'s window for `request`, its reads run in order and recorded nowhere, as a
    * turn runs them: its query written by `write` (`Left`, why none was), then the results
    * `admit` keeps.
    */
  def draw(assembler: ContextAssembler, request: AssemblyRequest)(
      write: ModelRequest => Either[String, Message.Assistant],
      admit: Results -> Results
  )(using Db^): Either[AssemblyError, Window] =
    assembler.prepare(request).flatMap {
      case Prepared.Drawn(drawn) => assembler.assemble(request, Packing.Drawn(drawn))
      case Prepared.Asking(asked, written) =>
        written(write(asked)) match {
          case drawn: Querying.Drawn => assembler.assemble(request, Packing.Drawn(drawn))
          case query: Querying.Written =>
            assembler
              .search(request, query)
              .flatMap(found => assembler.assemble(request, Packing.Searched(query, admit(found))))
        }
    }
}

/** What an assembler is asked to build a window for, and how wide. */
final case class AssemblyRequest(turn: TurnRef, width: Width = Width.Deployed)

/** Where a window starts: drawn with no search, or a query to write first. */
enum Prepared {
  case Drawn(drawn: Querying.Drawn)

  /** `request` is sent to the query writer; `written` makes the result of its reply, or of why
    * there is none (`Left`): a blank query, or none, is [[Querying.Drawn]] with a note saying
    * why.
    */
  case Asking(request: ModelRequest, written: Either[String, Message.Assistant] -> Querying)
}

object Prepared {

  /** Why no query is written for a turn that holds no pin to write one under. */
  val NoPin: String = "none was asked for"
}

/** What a turn's query step came to. */
enum Querying {

  /** No search: the window is drawn without one, these notes on it (none when nothing could be
    * found).
    */
  case Drawn(notes: Vector[AssemblyNote])

  /** The query `text`, never blank, as `queried` notes it. */
  case Written(text: String, queried: AssemblyNote.Queried)
}

/** What a search can find: an earlier turn of the period, an open turn elsewhere, a
  * conversation elsewhere shown by its newest kept closing, a document version, or a piece of
  * a kept file.
  */
enum Retrieved {
  case Own(turn: TurnSeq)
  case Near(conversation: ConversationId, turn: TurnSeq)
  case Closed(conversation: ConversationId)
  case Doc(version: DocumentVersionId)

  /** Piece `id`, and how many more of its file's pieces its search matched past
    * [[grit.core.files.PieceSearch.PerFile]], which the window says.
    */
  case Piece(id: PieceId, more: Int)
}

/** What a search found, best first, each with its weighted score; `beyond`, how many more
  * ranked after them.
  */
final case class Results(ranked: Vector[(Retrieved, Double)], beyond: Int = 0)

object Results {
  val empty: Results = Results(Vector.empty)
}

/** What a window packs beyond the one drawn with no search. */
enum Packing {

  /** Nothing: no search was made, and `drawn`'s notes are on the window. */
  case Drawn(drawn: Querying.Drawn)

  /** Of what `written`'s query found, the results the turn admits, in their order. */
  case Searched(written: Querying.Written, admitted: Results)
}

/** The turn's own messages as one text, and what was found, each as text, in the order asked. */
final case class Briefs(message: String, found: Vector[(Retrieved, String)])

object Briefs {

  /** `messages` as a judge reads them: each one's text, a reply's text blocks and a tool
    * result's content among them, one after another on lines of their own, cut to `chars`
    * characters.
    */
  def text(messages: Vector[Message], chars: Int): String =
    messages
      .map {
        case Message.User(text) => text
        case Message.ToolResult(_, content, _) => content
        case a: Message.Assistant =>
          a.blocks.collect { case AssistantBlock.Text(text) => text }.mkString("\n")
      }
      .filter(_.nonEmpty)
      .mkString("\n")
      .take(math.max(chars, 0))
}

/** How wide a window is drawn. */
enum Width {

  /** As wide as the assembler was built to draw it. */
  case Deployed

  /** Within `budget` estimated tokens, wider or narrower than the assembler was built for, each
    * search ranking at most `hits` entries (none when it is not positive); an assembler that
    * does not search reads `budget` alone.
    */
  case Within(budget: Tokens, hits: Int)
}

/** What the model sees before the turn's own messages: each of `nearby`'s sections, then each
  * of `documents` under its plugin's label, then each of `pieces` under its plugin's label
  * ([[Shown.piece]]), then the entries of its own conversation at `entries`, in that order; and
  * notes on how they were chosen. A nearby entry, a document or a piece gone by the time the
  * model is called is left out, as is a document or a piece whose plugin is no longer enabled,
  * and a section left with no message is dropped; an own entry gone fails the turn.
  */
final case class Window(
    entries: Vector[EntrySeq],
    notes: Vector[AssemblyNote] = Vector.empty,
    nearby: Vector[Nearby] = Vector.empty,
    documents: Vector[DocumentVersionId] = Vector.empty,
    pieces: Vector[Retrieved.Piece] = Vector.empty
)

/** Something an assembler did while choosing a window, kept for the record and never shown
  * to the model.
  */
enum AssemblyNote {

  /** It asked `model` for a search query and got `query` back, at `usage`, served by
    * `upstream` as its provider reported (`None`: not reported); `estimate` is the estimated
    * input of that request.
    */
  case Queried(
      query: String,
      model: String,
      usage: Usage,
      estimate: Tokens,
      upstream: Option[String] = None
  )

  /** It added these earlier `turns`, found by search, to the recent ones; none when
    * nothing it found fitted.
    */
  case Recalled(turns: Vector[TurnSeq])

  /** It built a simpler window than it set out to, for `reason`. */
  case FellBack(reason: String)
}

/** An assembly that produced no window. */
enum AssemblyError {
  case Store(error: StoreError)
}
