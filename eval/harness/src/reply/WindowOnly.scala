package grit.eval.harness.reply

import java.time.Instant

import grit.core.context.{Width, Window}
import grit.core.id.{TurnRef, WorkflowId}
import grit.core.message.{AssistantBlock, Message, StopReason}
import grit.core.period.Probability
import grit.core.provider.{ModelRequest, Provider, ProviderError}
import grit.core.triage.{Bound, Gate, Reading, Tags}
import grit.dbos.engine.Reader
import grit.eval.harness.capture.Capture

/** A heard message live triage read as asking (its `gap` question's most weighted key `asks`,
  * as triage's v2 asks it) that no turn answered: the turn it is the first message of, and
  * when triage answered, the moment a turn would have assembled its window.
  */
final case class WindowOnly(turn: TurnRef, at: Instant)

object WindowOnly {

  /** `gap`'s most weighted key is `asks`. */
  private val Asks: Gate =
    Gate.bounds(Bound.AtLeast(Reading.Chosen(Tags.V2.gap, "asks"), Probability.clamped(1)))

  /** Every such message `reader`'s database tagged before `until`, oldest tagged first. `Left`
    * naming what could not be read.
    */
  def all(reader: Reader^, until: Instant): Either[String, Vector[WindowOnly]] =
    for {
      tagged <- reader.all
        .read(reader.triage.tagged(Instant.EPOCH, until))
        .left
        .map(e => s"triage unread: ${Capture.kind(e)}")
      turns <- reader.turns(until).left.map(e => s"turn workflows unread: ${Capture.kind(e)}")
    } yield {
      val answered = turns.map(_._1).toSet
      tagged.flatMap { t =>
        val turn = TurnRef(t.triage.period.conversationId, t.triage.turn)
        val asks = t.tags match {
          case Tags.Weighed(answers, _, _) => Asks.drafts(answers).contains(true)
          case Tags.Unanswered(_) => false
        }
        Option.when(asks && !answered(turn.workflowId))(WindowOnly(turn, t.at))
      }
    }

  /** The request the shipped assembler would send `w`'s query writer, `width` wide, read by
    * rebuilding its window ([[Rebuild.window]]) over `reader`'s database as of `w.at` with a
    * writer that answers nothing; `None` when it would ask none. `Left` naming what failed.
    */
  def asked(
      reader: Reader^,
      w: WindowOnly,
      assembled: Assembled,
      width: Width
  ): Either[String, Option[ModelRequest]] = {
    val listener = new Listener
    Rebuild
      .window(AsOf(reader, w.at), w.turn, assembled, listener, width)(using reader.db)
      .map(_ => listener.request)
  }

  /** `w`'s window, `width` wide, rebuilt over `reader`'s database as of `w.at`, its query the
    * answer `answers` holds for its request; with none there, the assembler falls back as
    * when its writer fails. `Left` naming what failed.
    */
  def window(
      reader: Reader^,
      w: WindowOnly,
      assembled: Assembled,
      answers: Map[ModelRequest, Message.Assistant],
      width: Width
  ): Either[String, Window] =
    Rebuild.window(AsOf(reader, w.at), w.turn, assembled, new From(answers), width)(using
      reader.db
    )

  /** The id a window-only case is printed by: its turn's workflow id, never its text. */
  def id(w: WindowOnly): String = WorkflowId.value(w.turn.workflowId)

  /** A writer that answers nothing, keeping the last request it was sent. */
  private final class Listener extends Provider {
    // Untracked: a Listener is made by `asked` alone, handed to the one rebuild it runs, and
    // read once that returns; no other code holds it, so no write to it is seen elsewhere.
    @caps.unsafe.untrackedCaptures
    var request: Option[ModelRequest] = None

    def complete(r: ModelRequest): Either[ProviderError, Message.Assistant] = {
      request = Some(r)
      Left(ProviderError.Unavailable("only listening"))
    }
  }

  /** A writer answering from `answers`, failing a request they do not hold. */
  private final class From(answers: Map[ModelRequest, Message.Assistant]) extends Provider {
    def complete(r: ModelRequest): Either[ProviderError, Message.Assistant] =
      answers.get(r).toRight(ProviderError.Unavailable("no answer kept for the request"))
  }

  /** A reply of `text` alone, as the writer's kept answer is replayed. */
  private[reply] def reply(
      text: String,
      usage: grit.core.message.Usage,
      model: String
  ): Message.Assistant =
    Message.Assistant(Vector(AssistantBlock.Text(text)), StopReason.EndTurn, usage, model)
}
