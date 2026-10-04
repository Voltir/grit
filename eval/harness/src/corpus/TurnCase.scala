package grit.eval.harness.corpus

import java.time.Instant

import scala.collection.immutable.VectorMap

import grit.core.id.{ConversationId, EntryId, EntrySeq, TurnSeq, WorkflowId}
import grit.core.message.{Tokens, Usage}
import grit.core.period.Probability
import grit.core.place.Place
import grit.core.prompt.Layer
import grit.core.store.Focus
import grit.core.tool.{ToolName, ToolSetId}
import grit.dbos.engine.Build
import grit.turn.{TurnOffer, TurnRecord, TurnShape, TurnVerdict}

/** One turn of a corpus, text-free: its workflow, conversation and turn; the message it
  * answers (`said`), whether that message was said to grit or heard (`root`) and where
  * (`focus`); when it `started` and the `build` that ran it (as [[Triaged.buildAt]] finds it);
  * what live triage made of a heard root; its tool loop's `rounds`; how it `ended`; and what
  * each of its model calls cost (`spend`, in the order recorded).
  *
  * @param offered
  *   what it was offered; `None` when it recorded no offer
  * @param weighed
  *   what its `weigh` step recorded its root was weighed with
  * @param window
  *   its window by part; `None` when it recorded none
  * @param speech
  *   what became of a heard root's draft; `None` for an addressed turn, or a heard one that
  *   recorded no outcome
  */
final case class TurnCase(
    workflow: WorkflowId,
    conversation: ConversationId,
    turn: TurnSeq,
    said: Said,
    root: TurnOffer.Root,
    focus: Focus,
    started: Instant,
    build: Build,
    triage: Option[Live],
    offered: Option[Offered],
    weighed: TurnRecord.Weigh,
    window: Option[Parts],
    rounds: Vector[Round],
    ended: Ended,
    spend: Vector[Spent],
    speech: Option[Drafted]
)

/** The turns of a corpus, and how many turn workflows were `skipped`: their conversation or
  * first message gone, or a Slack message with no case id.
  */
final case class Turns(cases: Vector[TurnCase], skipped: Int)

/** The message a turn answers, by id: a Slack message by its [[CaseId]], which a reset or a
  * re-backfill keeps; a TUI session's or a task's by its entry's id in the database captured.
  */
enum Said {
  case Slack(at: CaseId)
  case Tui(entry: EntryId)
  case Task(entry: EntryId)
}

/** What a turn was offered: its tools by name, in the order offered, the set they are (`set`,
  * by id), and what their definitions cost as a request is costed (`schema`); its system
  * prompt's tokens by layer, in the order its fragments came; the workspace its hosted calls
  * went to; the places of the services it reached besides, in name order; and `shape`, what
  * its offer decided under its deployment's recipe, `None` for an offer recorded before
  * shapes.
  */
final case class Offered(
    tools: Vector[ToolName],
    set: ToolSetId,
    schema: Tokens,
    prompt: VectorMap[Layer, Tokens],
    workspace: Option[Place],
    reached: Vector[Place],
    shape: Option[TurnShape]
) {

  /** The set a variant draws its tools from: the shape's whole set, else the set offered. */
  def drawn: ToolSetId = shape.fold(set)(_.whole)
}

/** A turn's window as the model was shown it, part by part in the order shown: its nearby
  * sections, then its own conversation's record and turns; the tokens of the gap lines among
  * them, and of the turn's own messages before its first call.
  */
final case class Parts(parts: Vector[Part], gaps: Tokens, own: Tokens)

/** One part of a window: what kind it is, the conversation and the entries (by seq) it shows,
  * its tokens as [[grit.core.context.Shown]] shows it today (0 when none of it is left to
  * show), and how much of the turn's reply it carries.
  *
  * @param support
  *   the reply's support in it; `None` when the turn has no reply, or its reply passed
  */
final case class Part(
    kind: Part.Kind,
    conversation: ConversationId,
    seqs: Vector[EntrySeq],
    tokens: Tokens,
    support: Option[Support]
)

object Part {

  /** A nearby section's kind ([[grit.core.store.Nearby]]), or one of the window's own. */
  enum Kind {
    case Open, Closed, Along, Asked

    /** Its own conversation's record: a closing entry. */
    case Record

    /** One of its own conversation's turns, chosen by recency. */
    case Recent

    /** One of its own conversation's turns, chosen by search. */
    case Recalled
  }

  object Kind {

    /** `k`'s written name, lower-case. */
    def written(k: Kind): String = k.toString.toLowerCase

    /** The kind written `name`; `None` for no kind's. */
    def read(name: String): Option[Kind] = values.find(written(_) == name)
  }
}

/** One reply of the tool loop that called tools, its calls in the order made. */
final case class Round(calls: Vector[Call])

/** One tool call: the tool it named, and how it settled. */
final case class Call(tool: Called, settled: Settled)

/** The tool a call named, never its arguments. */
enum Called {

  /** A tool of the turn's offered set. */
  case Tool(name: ToolName)

  /** The turn's own `topic` tool ([[grit.turn.TurnVerdict.Name]]), which it offers beside its
    * set when its message's topic was unsure.
    */
  case Topic

  /** A name that is neither. */
  case Unnamed
}

object Called {

  /** What the name `sent` names in a turn offered `offered`: one of `offered` before the
    * turn's `topic` tool.
    */
  def of(sent: String, offered: Set[ToolName]): Called =
    ToolName.of(sent).toOption match {
      case Some(n) if offered.contains(n) => Tool(n)
      case Some(n) if n == TurnVerdict.Name => Topic
      case _ => Unnamed
    }
}

/** How a tool call settled, never what its result said. */
enum Settled {

  /** It ran, and its result is `length` characters. */
  case Ok(length: Int)

  /** It failed, was refused or denied, its result `length` characters. */
  case Failed(length: Int)

  /** No edge claimed it in time. */
  case Expired

  /** Its edge claimed it and did not answer in time. */
  case Abandoned

  /** No result was recorded. */
  case Unsettled
}

/** How a turn ended. */
enum Ended {

  /** It replied (or drafted, on a heard root), `length` characters; `passed` when the reply
    * says nothing ([[grit.turn.TurnJudge.said]]).
    */
  case Replied(length: Int, passed: Boolean)

  /** It failed before a reply, at the step of family `step`, of this kind. */
  case Failed(step: String, why: Ended.Why)

  /** Neither: DBOS's `status` for it, not finished when the database was dumped, or ended
    * otherwise.
    */
  case Unfinished(status: String)
}

object Ended {

  /** A [[grit.turn.TurnFailure]]'s kind, never its reason. */
  enum Why {
    case Assembly, Model, Store
  }
}

/** One model call of a turn: what it was for (`None` when its ledger row names no call of the
  * turn), the model that answered, what it consumed, and grit's estimate of its input.
  */
final case class Spent(
    role: Option[TurnRecord.Role],
    model: String,
    usage: Usage,
    estimated: Tokens
)

/** What became of a heard root's draft: its outcome's kind, the unprompted judge's scores
  * when it was judged by them (a named draft's one score is not kept here), and the bar it
  * was held to when the outcome records one (a draft below it).
  */
final case class Drafted(
    outcome: Drafted.Kind,
    grounded: Option[Probability],
    worth: Option[Probability],
    postAt: Option[Probability]
)

object Drafted {

  /** [[grit.core.speech.Outcome]]'s case, never its words. */
  enum Kind {
    case Passed, NothingRecalled, Spoken, Withdrawn, Unjudged, Below, Shadowed, Posted, Failed
  }
}
