package grit.eval.harness.sent

import grit.assembly.estimate.CharEstimate
import grit.core.context.{
  AssemblyRequest,
  CarriedWindow,
  ContextAssembler,
  Packing,
  Projection,
  Querying,
  Shown
}
import grit.core.id.{EntrySeq, TurnSeq}
import grit.core.message.{AssistantBlock, Message, Tokens}
import grit.core.provider.{ModelRequest, TokenEstimator}
import grit.core.store.{Entry, Payload, Tx}
import grit.core.visibility.Subject
import grit.dbos.internal.Reader
import grit.turn.{Turn, TurnRecord}

/** A boundary's first call by part, in tokens: its system prompt and tools; the task it goes on
  * with (`task`, none for a turn that goes on with no task); what its window showed of the
  * conversation's earlier turns, gap lines included (`earlier`); the session's digest
  * (`digest`); and what follows the window, the turn's own dated line and note or message
  * (`own`).
  */
final case class FirstCall(
    system: Tokens,
    tools: Tokens,
    task: Tokens,
    earlier: Tokens,
    digest: Tokens,
    own: Tokens
) extends caps.Pure {

  /** Every part summed. */
  def total: Tokens = system + tools + task + earlier + digest + own

  /** Each part times `by`, rounded half up. */
  def scaled(by: BigDecimal): FirstCall = {
    def times(t: Tokens) =
      Tokens((BigDecimal(Tokens.value(t)) * by).setScale(0, BigDecimal.RoundingMode.HALF_UP).toLong)
    FirstCall(times(system), times(tools), times(task), times(earlier), times(digest), times(own))
  }
}

object FirstCall {

  /** `request`'s parts by `estimator`, its first `window` messages those of its window, drawn as
    * `record` names: the first of them the task when `record` holds one, each equal to
    * `record`'s digest as shown ([[Shown.sessionLog]]) the digest, the rest earlier turns.
    */
  def of(
      request: ModelRequest,
      window: Int,
      record: Payload.Window,
      estimator: TokenEstimator
  ): FirstCall = {
    val digest = record.sessionLog.map(Shown.sessionLog)
    val (shown, own) = request.messages.splitAt(window)
    val (task, rest) =
      if (record.task.isDefined) shown.splitAt(1) else (Vector.empty, shown)
    val (digests, earlier) = rest.partition(m => digest.contains(m))
    def cost(ms: Vector[Message]) = ms.map(estimator.message).foldLeft(Tokens.Zero)(_ + _)
    FirstCall(
      estimator.system(request.system),
      estimator.tools(request.tools),
      cost(task),
      cost(earlier),
      cost(digests),
      cost(own)
    )
  }
}

/** A turn its window was drawn for at a boundary, its first call as recorded (`was`) and as the
  * same turn's would have been with its window drawn afresh by an assembler of this build
  * (`now`): `was` by grit's estimate scaled to the input its provider counted, `now` by the same
  * scale; the earlier turns each window showed, oldest first (`shown` as recorded, `drawn`
  * afresh); its last call's input (`last`) and its input limit (`limit`), each as recorded,
  * `None` when not; what the summaries of the conversation's turns before it would cost, each
  * appended as one message, by grit's estimate (`summaries`); and of its tool calls (`calls`),
  * how many name a file its digest lists as read or edited (`reread`). `request` is the first
  * call's request as redrawn.
  */
final case class Redrawn(
    turn: TurnSeq,
    shown: Vector[TurnSeq],
    drawn: Vector[TurnSeq],
    was: FirstCall,
    now: FirstCall,
    last: Option[Tokens],
    limit: Option[Tokens],
    summaries: Tokens,
    reread: Int,
    calls: Int,
    request: ModelRequest
) extends caps.Pure

object Redrawn {

  /** `redrawn` as a markdown table, a row a turn, each figure as recorded, then an arrow, then
    * as redrawn: the earlier turns shown; the first call and its task, earlier turns and digest;
    * its room, its last call's input less its first's (the last as recorded both ways); its room
    * at the limit, its input limit less its first call's; then the summaries appended, and the
    * calls re-reading a file. A figure not recorded is said as `?`.
    */
  def table(redrawn: Vector[Redrawn]): Vector[String] = {
    val head = Vector(
      "turn",
      "earlier turns shown",
      "first call",
      "task",
      "earlier turns",
      "digest",
      "room",
      "room at limit",
      "summaries appended",
      "calls re-reading a file the digest lists"
    )
    def row(r: Redrawn): Vector[String] = {
      def less(of: Option[Tokens], call: FirstCall) =
        of.fold("?")(o => k(Tokens(Tokens.value(o) - Tokens.value(call.total))))
      def both(a: String, b: String) = s"$a → $b"
      Vector(
        TurnSeq.value(r.turn).toString,
        both(turns(r.shown), turns(r.drawn)),
        both(k(r.was.total), k(r.now.total)),
        both(k(r.was.task), k(r.now.task)),
        both(k(r.was.earlier), k(r.now.earlier)),
        both(k(r.was.digest), k(r.now.digest)),
        both(less(r.last, r.was), less(r.last, r.now)),
        both(less(r.limit, r.was), less(r.limit, r.now)),
        k(r.summaries),
        s"${r.reread} of ${r.calls}"
      )
    }
    def line(cells: Vector[String]) = cells.mkString("| ", " | ", " |")
    Vector(line(head), head.map(_ => "---").mkString("|", "|", "|")) ++ redrawn.map(r =>
      line(row(r))
    )
  }

  /** `turn`'s redrawn first call as markdown for a person to read: a heading, its parts as
    * [[table]] gives them, then each message of its request numbered, with its estimate in grit's
    * tokens, those after the task with their text whole.
    */
  def markdown(r: Redrawn): Vector[String] = {
    val estimate = (m: Message) => Tokens.value(CharEstimate.message(m))
    val first = if (Tokens.value(r.now.task) > 0) 1 else 0
    Vector("", s"### Turn ${TurnSeq.value(r.turn)}, its window redrawn", "") ++ table(
      Vector(r)
    ) ++ r.request.messages.zipWithIndex.flatMap { (m, i) =>
      if (i < first) Vector("", s"**$i · the task**: ${estimate(m)} tokens by grit's estimate")
      else SentMarkdown.message(i, m, Cut.Whole) :+ s"(${estimate(m)} tokens by grit's estimate)"
    }
  }

  /** Each turn of `conversation` whose window was drawn at a boundary, with a first call that a
    * ledger row counted, read through `reader` and redrawn by `assembler`, as the turn asked for
    * it but with nothing carried and reserved what its window record shows before the drawn
    * part: the task and the digest as recorded, costed by grit's estimate (the turn reserved
    * the digest without its last said line, so a redraw may reserve less). Its window record
    * is kept but for the entries drawn; its requests rebuilt under it
    * ([[TurnRecord.redrawn]]). `Left` naming the turn when its steps or stores do not read, its
    * window held a brief (not costed here), or `assembler` appends rather than draws.
    */
  def read(
      reader: Reader^,
      conversation: ConversationSent,
      assembler: ContextAssembler
  ): Either[String, Vector[Redrawn]] = {
    val estimator = CharEstimate
    val turnOf = conversation.entries.map(e => e.seq -> e.turnSeq).toMap
    def turnsOf(seqs: Vector[EntrySeq], task: Option[EntrySeq]) =
      seqs.filterNot(task.contains).flatMap(turnOf.get).distinct.sortBy(TurnSeq.value)
    val boundaries = conversation.turns.flatMap(t =>
      for {
        record <- conversation.entries.collectFirst {
          case Entry(id, _, _, _, _, w: Payload.Window, _, _)
              if id == Turn.windowId(t.turn) && w.carry.forall(_.after.isEmpty) =>
            w
        }
        first <- t.measured.headOption
        row <- first.row
      } yield (t, record, first, row)
    )
    boundaries.foldLeft[Either[String, Vector[Redrawn]]](Right(Vector.empty)) {
      case (acc, (t, record, first, row)) =>
        val named = s"turn ${TurnSeq.value(t.turn.turnSeq)}"
        def kept[A](what: String)(read: Either[grit.core.store.StoreError, A]) =
          read.left.map(e => s"$named: $what unread: ${e.kind}")
        acc.flatMap { done =>
          for {
            _ <- Either.cond(record.brief.isEmpty, (), s"$named: its window held a brief")
            steps <- kept("steps")(reader.steps(t.workflow))
            offer <- TurnRecord
              .offer(steps)
              .flatMap(_.toRight("no offer"))
              .left
              .map(w => s"$named: $w")
            limits <- TurnRecord.limits(steps).left.map(w => s"$named: $w")
            held <- kept("task")(reader.db.read(Subject.Turn(t.turn)) {
              for {
                at <- reader.entries.at(t.turn.conversationId, record.task.toVector)
                speakers <- reader.principals.speakers(at.map(_.id))
              } yield at.flatMap(Shown.of(_, speakers))
            })
            reserved = (held ++ record.sessionLog.map(Shown.sessionLog))
              .map(estimator.message)
              .foldLeft(Tokens.Zero)(_ + _)
            asked = AssemblyRequest(t.turn, offer.width, CarriedWindow.Empty, reserved, record.task)
            projection <- assembler
              .assemble(asked, Packing.Drawn(Querying.Drawn(Vector.empty)))(using reader.db)
              .left
              .map(e => s"$named: not drawn: $e")
            window <- (projection match {
              case Projection.Boundary(w, _) => Right(record.copy(entries = w.entries))
              case _: Projection.Appended => Left(s"$named: appended, not drawn afresh")
            }): Either[String, Payload.Window]
            calls <- kept("requests")(
              reader.all.read((tx: Tx^) ?=>
                Right(TurnRecord.redrawn(t.turn, steps, TurnSent.reads(reader), window))
              )
            ).flatMap(_.left.map(w => s"$named: $w"))
            request <- calls.looped.headOption
              .map(_.request)
              .orElse(calls.answered.map(_.on))
              .toRight(s"$named: no call redrawn")
          } yield {
            val was = FirstCall.of(first.request, t.calls.window.size, record, estimator)
            val scale =
              if (Tokens.value(was.total) == 0) BigDecimal(1)
              else BigDecimal(Tokens.value(row.usage.input)) / Tokens.value(was.total)
            val own = conversation.entries.filter(_.turnSeq == t.turn.turnSeq)
            val calling = own.flatMap(_.payload match {
              case Payload.Exchange(m) => m.blocks.collect { case c: AssistantBlock.ToolCall => c }
              case _ => Vector.empty
            })
            val listed = record.sessionLog.fold(Set.empty[String])(listedIn)
            done :+ Redrawn(
              t.turn.turnSeq,
              turnsOf(record.entries, record.task),
              turnsOf(window.entries, record.task),
              was.scaled(scale),
              FirstCall.of(request, calls.window.size, window, estimator).scaled(scale),
              t.requests.lastOption.flatMap((_, reply, _) => t.row(reply)).map(_.usage.input),
              limits.flatMap(_.budget.limit),
              summariesBefore(conversation.entries, t.turn.turnSeq, estimator),
              calling.count(c => strings(c.arguments).exists(listed.contains)),
              calling.size,
              request
            )
          }
        }
    }
  }

  /** What the summaries of `entries`' turns before `turn` cost, each as one message. */
  private def summariesBefore(
      entries: Vector[Entry],
      turn: TurnSeq,
      estimator: TokenEstimator
  ): Tokens =
    entries
      .filter(e => TurnSeq.value(e.turnSeq) < TurnSeq.value(turn))
      .collect { case Entry(_, _, _, _, _, Payload.Summary(text), _, _) =>
        estimator.message(Message.User(text))
      }
      .foldLeft(Tokens.Zero)(_ + _)

  /** The items a digest lists, a line each after "- ". */
  private def listedIn(digest: String): Set[String] =
    digest.linesIterator.collect { case l if l.startsWith("- ") => l.drop(2).trim }.toSet

  /** Every string in `value`, however deep. */
  private def strings(value: ujson.Value): Vector[String] = value match {
    case ujson.Str(s) => Vector(s)
    case ujson.Arr(items) => items.toVector.flatMap(strings)
    case ujson.Obj(fields) => fields.values.toVector.flatMap(strings)
    case _ => Vector.empty
  }

  /** Turns as a person reads them: runs of consecutive turns as `a–b`, comma-separated. */
  private def turns(ts: Vector[TurnSeq]): String =
    if (ts.isEmpty) "none"
    else
      ts.map(TurnSeq.value)
        .foldLeft(Vector.empty[(Long, Long)]) {
          case (runs :+ ((a, b)), n) if n == b + 1 => runs :+ (a -> n)
          case (runs, n) => runs :+ (n -> n)
        }
        .map((a, b) => if (a == b) s"$a" else s"$a–$b")
        .mkString(", ")

  /** `t` in thousands, to one place, rounded half up. */
  private def k(t: Tokens): String =
    (BigDecimal(Tokens.value(t)) / 1000).setScale(1, BigDecimal.RoundingMode.HALF_UP).toString +
      "k"
}
