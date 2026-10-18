package grit.turn

import grit.act.moves.Judging
import grit.core.context.{Querying, Results, Retrieved}
import grit.core.document.DocumentSearch
import grit.core.durable.StepRecord
import grit.core.edge.RequestState
import grit.core.either.Traversal.sequence
import grit.core.id.{DocumentVersion, EntryId, TurnRef, WorkflowId}
import grit.core.provider.ModelRequest
import grit.core.speech.Outcome
import grit.core.store.{EntryStore, ModelProfileStore, Principals, PromptStore, Tx}
import grit.core.tool.ToolSets
import grit.core.visibility.Label

import TurnJournal.given

/** A turn after the fact, read from the steps it recorded ([[Turn.Step]]) and the ids its
  * entries and ledger rows are kept under, by the turn's own codecs: for a tool reading a turn
  * it did not run, never for the turn itself.
  */
object TurnRecord {

  /** The offer `steps` recorded; `None` when no `offer` step is recorded, or it recorded a
    * failure ([[failure]] says which). `Left` naming the step when its output does not read.
    */
  def offer(steps: Vector[StepRecord]): Either[String, Option[TurnOffer.Recorded]] = {
    read[Either[TurnFailure, TurnOffer.Recorded]](steps, Turn.Step.Offer).map(_.flatMap(_.toOption))
  }

  /** The label the `route` step `steps` recorded the turn's reads at, what its calls are routed
    * for; `None` when no `route` step is recorded, or it recorded a failure. `Left` naming the
    * step when its output does not read.
    */
  def routedAt(steps: Vector[StepRecord]): Either[String, Option[Label]] =
    read[Either[TurnFailure, Route.Recorded]](steps, Turn.Step.Route)
      .map(_.flatMap(_.toOption).map(_.classification))

  /** What the `weigh` step `steps` recorded. `Left` naming the step when its output does not
    * read.
    */
  def weighed(steps: Vector[StepRecord]): Either[String, Weigh] = {
    read[Option[TurnWeighing.Weighed]](steps, Turn.Step.Weigh)
      .map(_.fold(Weigh.Unrecorded)(Weigh.Recorded(_)))
  }

  /** What the `judge` step recorded of the draft of a turn whose root is `root`; `None`
    * unless `root` is [[TurnOffer.Root.Heard]] and the step recorded a judgement. `Left`
    * naming the step when its output does not read.
    */
  def judged(
      steps: Vector[StepRecord],
      root: TurnOffer.Root
  ): Either[String, Option[TurnJudge.Judgement]] = {
    root match {
      case TurnOffer.Root.Heard => read[TurnJudge.Judgement](steps, Turn.Step.Judge)
      case TurnOffer.Root.Addressed | TurnOffer.Root.Named | TurnOffer.Root.ByName => Right(None)
    }
  }

  /** The stores a turn's requests are rebuilt from, as a reader sees them. */
  final case class Reads(
      entries: EntryStore,
      principals: Principals,
      documents: DocumentSearch,
      prompts: PromptStore,
      toolSets: ToolSets,
      profiles: ModelProfileStore
  )

  /** The model calls `turn`'s reply made, each with the request it was sent, rebuilt from
    * `steps` and `reads` through this build's code: the system prompt and tool set its
    * `offer` step recorded, the profile `pin-models` pinned, the window `assemble` drew, the
    * classification `classify` made, and, for each call, the turn's own entries kept before
    * that call's reply. The texts this build adds (labels, gap lines, the topic tag, the
    * last-call note, "asks first") are this build's, which may differ from those of the build
    * that ran the turn. `Calls(Vector.empty, None, _)` for a turn whose first call's reply is
    * not kept yet.
    *
    * `Left` naming what does not read: steps that do not read as this build's turn (one of
    * another epoch, [[grit.core.durable.Flows.read]]), a step whose output does not read, a prompt, tool set
    * or profile not kept, a call's reply entry no longer kept, or a window naming entries
    * since deleted, with their seqs. A nearby entry or document since deleted, a plugin since disabled or a person since
    * renamed rebuilds as it is now, not as sent: each call's ledger row keeps the estimate of
    * what was sent to compare with.
    */
  def requests(turn: TurnRef, steps: Vector[StepRecord], reads: Reads)(using
      Tx^
  ): Either[String, Calls] =
    Turn.rebuilt(turn, steps, reads)

  /** A turn's model calls: each whose reply called tools, oldest first (`looped`), then the
    * one that answered, once its reply or draft was kept (`answered`); `schemas` says whether
    * their tool definitions are those sent.
    */
  final case class Calls(looped: Vector[Call], answered: Option[Answered], schemas: Schemas)

  /** Call `round` (from 0), whose reply is kept as `reply`, sent `request`. */
  final case class Call(round: Int, reply: EntryId, request: ModelRequest)

  /** The call that answered, round `round`, its reply or draft kept as `reply`: sent `on`,
    * with the tools on, unless it was the turn's budget's last call, when it was sent `off`
    * ([[TurnLoop.use]], [[TurnLoop.LastCall]]). Which one is not recorded; its ledger row
    * keeps the estimate of the request it was sent.
    */
  final case class Answered(round: Int, reply: EntryId, on: ModelRequest, off: ModelRequest)

  /** Whether a rebuilt request's tool definitions are those sent. */
  enum Schemas {

    /** As sent: the turn's pair did not enforce strict schemas. */
    case Sent

    /** As recorded: the pair enforced strict schemas, and a tool set records each tool's
      * non-strict parameters, so a tool the build typed was sent a form not rebuilt here, and
      * no rebuilt request's estimate is that of what was sent. Its name and description, and
      * every message and the system prompt, are rebuilt as for [[Sent]].
      */
    case Recorded
  }

  /** What a turn's `weigh` step recorded. */
  enum Weigh {

    /** No `weigh` step is recorded: the turn passed that point before weighing shipped, or has
      * not reached it.
      */
    case Unrecorded

    /** The step recorded `weighed`; `None` when it weighed nothing: no tags kept for its heard
      * root, or a root said to grit that its recipe did not read.
      */
    case Recorded(weighed: Option[TurnWeighing.Weighed])
  }

  /** Each round's gate `steps` recorded (`gate:n`), in round order; a round whose gate recorded
    * a failure, or no output, is left out. `Left` naming the step when its output does not read.
    */
  def gates(steps: Vector[StepRecord]): Either[String, Vector[Gated]] = {
    val journal = summon[grit.core.durable.Journaled[Either[TurnFailure, Gated]]]
    val rounds = steps
      .flatMap(s =>
        (Loop.of(s.name), s.output) match {
          case (Some(Loop.Gate(n)), Some(output)) => Some((n, s.name, output))
          case _ => None
        }
      )
      .sortBy(_._1)
    rounds.foldLeft[Either[String, Vector[Gated]]](Right(Vector.empty)) {
      case (acc, (_, name, output)) =>
        acc.flatMap(done =>
          journal.decode(output).left.map(why => s"$name: $why").map(_.fold(_ => done, done :+ _))
        )
    }
  }

  /** Whether `item` is an entry's closing record, as a gate records it. */
  def closing(item: Item): Boolean = item.ref match {
    case ItemRef.Own(_) | ItemRef.Nearby(_, _) => item.source == TurnGate.ClosingRecord
    case ItemRef.Document(_) | ItemRef.Result(_) => false
  }

  /** The first of `steps` whose output is a [[TurnFailure]]: its step's family
    * ([[Turn.Step.family]]; the name as recorded when it has none) and the failure. A failure
    * ends the turn only when no reply follows it: a failed summary leaves the reply standing.
    */
  def failure(steps: Vector[StepRecord]): Option[(String, TurnFailure)] =
    steps.iterator
      .flatMap(s =>
        s.output.flatMap(TurnJournal.failure).map(Turn.Step.family(s.name).getOrElse(s.name) -> _)
      )
      .nextOption()

  /** What the `record-speech` step recorded became of a heard-rooted turn's draft; `None` when
    * the step is not recorded, or recorded a failure. `Left` when its output does not read.
    */
  def speech(steps: Vector[StepRecord]): Either[String, Option[Outcome]] = {
    read[Either[TurnFailure, Outcome]](steps, Turn.Step.RecordSpeech).map(_.flatMap(_.toOption))
  }

  /** The tool loop's calls `steps` show given up on, by round and index, each from 0 (the
    * `n` and `j` of `call:n:j`). A call whose edge answered in the end is not among them.
    */
  def givenUp(steps: Vector[StepRecord]): Map[(Int, Int), GivenUp] = {
    val journal = summon[grit.core.durable.Journaled[Either[TurnFailure, RequestState]]]
    def state(s: StepRecord): Option[RequestState] =
      s.output.flatMap(journal.decode(_).toOption).flatMap(_.toOption)
    steps.flatMap { s =>
      Loop.of(s.name) match {
        case Some(Loop.Expire(n, j)) =>
          state(s).collect { case RequestState.Expired => (n, j) -> GivenUp.Expired }
        case Some(Loop.Abandon(n, j)) =>
          state(s).collect { case RequestState.Expired | RequestState.Claimed =>
            (n, j) -> GivenUp.Abandoned
          }
        case _ => None
      }
    }.toMap
  }

  /** A tool call the turn stopped waiting for. */
  enum GivenUp {

    /** No edge claimed it in time. */
    case Expired

    /** Its edge claimed it and did not answer in time. */
    case Abandoned
  }

  /** What a ledger row of `turn` paid for, by the id of the entry it is kept under; `None`
    * for an id none of `turn`'s calls is kept under.
    */
  def role(turn: TurnRef, entry: EntryId): Option[Role] = {
    val id = EntryId.value(entry)
    // The last part of an id, as a number: a candidate index, confirmed only by the id it makes.
    val last = id.lastIndexOf(':') match {
      case -1 => None
      case k => id.drop(k + 1).toIntOption.filter(_ >= 0)
    }
    // Where a move's answered call is kept, as its record step keeps it: `move:{wf}:{name}`.
    val moves = s"move:${WorkflowId.value(turn.workflowId)}:"
    if (entry == turn.replyId || entry == turn.draftId) Some(Role.Reply)
    else if (entry == TurnJudge.id(turn)) Some(Role.Judge)
    else if (entry == TurnSummary.id(turn)) Some(Role.Summary)
    else if (entry == TurnWeighing.id(turn)) Some(Role.Weigh)
    else if (id.startsWith(s"${Extract.Prefix}${WorkflowId.value(turn.workflowId)}:"))
      Some(Role.Extract)
    else if (
      Vector(TurnTopics.placedId(turn), TurnTopics.describedId(turn), TurnVerdict.verdictId(turn))
        .contains(entry)
    ) Some(Role.Topic)
    else if ((0 +: last.toVector).exists(i => Turn.queryId(turn, i) == entry)) Some(Role.Query)
    else if (id == s"${moves}needs") Some(Role.Needs)
    else if (id == s"${moves}query") Some(Role.Query)
    else if (id.startsWith(s"${moves}relevance-"))
      id.stripPrefix(s"${moves}relevance-").toIntOption.filter(_ >= 0).map(Role.Relevance(_))
    else last.filter(n => TurnTools.callId(turn, TurnLoop.Round.at(n)) == entry).map(Role.Round(_))
  }

  /** What a model call of a turn was for. */
  enum Role {

    /** The classifier call judging whether the turn's message needs a search ([[TurnNeeds]]). */
    case Needs

    /** Assembly's search query. */
    case Query

    /** A classifier call judging batch `batch` (from 0) of what the turn's search found. */
    case Relevance(batch: Int)

    /** Placing the turn's message among its conversation's topics, describing a topic, or
      * the reply's verdict on one.
      */
    case Topic

    /** The reply to round `index` (from 0) of the tool loop, which called tools. */
    case Round(index: Int)

    /** The reply, or a heard-rooted turn's draft. */
    case Reply

    /** The judge of a heard-rooted turn's draft. */
    case Judge

    /** The turn's summary. */
    case Summary

    /** Live triage's set asked of the message said to grit the turn answers, before its offer
      * ([[TurnWeighing.Weighed.Asked]]).
      */
    case Weigh

    /** A question `extract` put to a hidden item ([[Extract.costId]]). */
    case Extract
  }

  private val Loop = TurnRound.Names.Step

  /** The output of the step named `name` among `steps`, read by `A`'s journal; `None` when no
    * such step recorded one.
    */
  /** What a turn's search was: the query it wrote, the results its `search` step recorded, and
    * each judged one's verdict, in its batches' order.
    */
  final case class Retrieval(
      written: Querying.Written,
      results: Results,
      verdicts: Vector[(Retrieved, TurnRelevance.Verdict)]
  )

  /** What `steps` recorded of the turn's search; `None` when the turn wrote no query, its
    * search failed, or it ran before its window was drawn in steps. `Left` when an output does
    * not read.
    */
  def retrieval(steps: Vector[StepRecord]): Either[String, Option[Retrieval]] = {
    for {
      queried <- steps.find(_.name == Turn.Step.Query) match {
        case None => Right(None)
        case Some(s) => made[Either[TurnFailure, Querying]](s).map(Some(_))
      }
      found <- read[Either[TurnFailure, Results]](steps, Turn.Step.Search)
      verdicts <- sequence(
        steps
          .filter(s => Turn.Step.family(s.name).contains(Turn.Step.Relevance))
          .map(made[Vector[(Retrieved, TurnRelevance.Verdict)]](_))
      )
    } yield (queried.flatMap(_.toOption), found.flatMap(_.toOption)) match {
      case (Some(written: Querying.Written), Some(results)) =>
        Some(Retrieval(written, results, verdicts.flatten))
      case _ => None
    }
  }

  /** What the `ask:needs` step `steps` recorded: the needs gate's verdict, unjudged when it
    * asked nothing; `None` when the turn ended before it. `Left` when its output does not read.
    */
  def needs(steps: Vector[StepRecord]): Either[String, Option[TurnRelevance.Verdict]] =
    steps.find(_.name == Turn.Step.Needs) match {
      case None => Right(None)
      case Some(s) => made[TurnRelevance.Verdict](s)(using TurnJournal.verdict).map(Some(_))
    }

  /** The document versions the turn's reply or draft counted as followed; `None` when `append`
    * is not recorded, or recorded a failure. `Left` when its output does not read.
    */
  def followed(steps: Vector[StepRecord]): Either[String, Option[Vector[DocumentVersion]]] =
    read[Either[TurnFailure, TurnSteps.Appended]](steps, Turn.Step.Append)
      .map(_.flatMap(_.toOption).map(_.followed))

  /** What a judgment's recorded `ask:{name}` step `s` made; `Left`, naming it, when it does not
    * read.
    */
  private def made[A](s: StepRecord)(using grit.core.durable.Journaled[A]): Either[String, A] =
    s.output
      .toRight("no output")
      .flatMap(Judging.made[A](_))
      .left
      .map(why => s"${s.name}: $why")

  private def read[A](steps: Vector[StepRecord], name: String)(using
      journal: grit.core.durable.Journaled[A]
  ): Either[String, Option[A]] =
    steps.find(_.name == name).flatMap(_.output) match {
      case None => Right(None)
      case Some(output) => journal.decode(output).map(Some(_)).left.map(why => s"$name: $why")
    }
}
