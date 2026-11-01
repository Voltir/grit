package grit.eval.harness.capture

import scala.collection.immutable.VectorMap

import grit.assembly.estimate.CharEstimate
import grit.core.context.{AssemblyNote, CarriedWindow, Shown, Window}
import grit.core.document.{DocLabel, Document, Pointer}
import grit.core.durable.StepRecord
import grit.core.either.Traversal
import grit.core.id.{DocumentVersionId, EntrySeq, TurnRef, WorkflowId}
import grit.core.message.{AssistantBlock, Message, Tokens}
import grit.core.period.Probability
import grit.core.provider.{ModelRequest, ToolSchema}
import grit.core.speech.Outcome
import grit.core.store.{Entry, Nearby, Origin, Payload, Position, Speakers, StoreError, Tx}
import grit.core.tool.{ToolName, ToolSet}
import grit.dbos.engine.Build
import grit.dbos.internal.Reader
import grit.turn.{Turn, TurnFailure, TurnJudge, TurnOffer, TurnRecord}

/** The turns a restored database recorded, read through `grit.dbos`'s `Reader` and the turn's
  * own reading of its records ([[TurnRecord]]), never through SQL of its own.
  */
object TurnCapture {

  /** Every turn workflow `reader`'s database recorded before `dump.at`, as a case, ordered by
    * when it started, then by workflow id; and how many were skipped. Each window part's
    * tokens are what [[CharEstimate]] costs it as [[Shown]] shows it now, and its support, and
    * each call's, is computed from the reply's text, which is never kept; each call's
    * [[Reading]] is classified by the documents current in the database, whichever plugin kept
    * them. Equal databases capture equal turns. `Left` naming what could not be read; never a
    * message's text.
    */
  def apply(reader: Reader^, dump: Dump): Either[String, Turns] = {
    def read[A](what: String)(body: (Tx^) ?=> Either[StoreError, A]) =
      reader.all.read(body).left.map(e => s"$what unread: ${e.kind}")
    for {
      workflows <- reader.turns(dump.at).left.map(e => s"turn workflows unread: ${e.kind}")
      starts <- reader.starts().left.map(e => s"engine starts unread: ${e.kind}")
      current <- read("current documents")(reader.current())
      pointers = current.flatMap(_.pointer)
      cases <- Traversal.traverse(workflows) { (workflow, recorded) =>
        TurnRef.fromWorkflowId(workflow) match {
          case None => Right(None)
          case Some(turn) =>
            for {
              conversation <- read("conversation")(reader.conversations.get(turn.conversationId))
              all <- read("entries")(reader.entries.list(turn.conversationId))
              built <- conversation.fold[Either[String, Option[TurnCase]]](Right(None))(c =>
                one(
                  reader,
                  turn,
                  c.origin,
                  all,
                  recorded,
                  Triaged.buildAt(starts, recorded.created),
                  pointers
                )
              )
            } yield built
        }
      }
    } yield Turns(cases.flatten, workflows.size - cases.count(_.isDefined))
  }

  /** `turn`'s case, from `all` of its conversation's entries; `None` when its first message
    * is gone, is a Slack message with no case id, or is a direct message's.
    */
  private def one(
      reader: Reader^,
      turn: TurnRef,
      origin: Origin,
      all: Vector[Entry],
      recorded: Reader.Recorded,
      build: Build,
      current: Vector[Pointer]
  ): Either[String, Option[TurnCase]] = {
    def read[A](what: String)(body: (Tx^) ?=> Either[StoreError, A]) =
      reader.all
        .read(body)
        .left
        .map(e => s"$what of ${WorkflowId.value(turn.workflowId)} unread: ${e.kind}")
    val mine = all.filter(_.turnSeq == turn.turnSeq).sortBy(e => EntrySeq.value(e.seq))
    val said: Option[(Entry, Said)] = mine.headOption.flatMap { first =>
      origin match {
        case Origin.Slack(_, _, _) => CaseId.of(origin, first.id).map(id => first -> Said.Slack(id))
        case Origin.Tui(_, _) => Some(first -> Said.Tui(first.id))
        case Origin.Task(_, _) => Some(first -> Said.Task(first.id))
        // A direct message is its one person's: no case is made of it.
        case Origin.Direct(_, _) => None
      }
    }
    said match {
      case None => Right(None)
      case Some((first, saidAs)) =>
        // Whether its message was heard: what its tags are read by. Its answer is its draft,
        // when it drafted one, else its reply: a heard message answered as said to grit
        // replies as an addressed one. The case keeps the root its offer recorded.
        val heard = first.payload match {
          case Payload.Heard(_) => true
          case _ => false
        }
        val opening = all.minByOption(e => EntrySeq.value(e.seq)).exists(_.id == first.id)
        val focus = origin.focus(if (opening) Position.Opening else Position.Reply)
        val reply = mine
          .find(_.id == turn.draftId)
          .orElse(mine.find(_.id == turn.replyId))
          .flatMap(e =>
            e.payload match {
              case Payload.Message(m: Message.Assistant) => Some(m)
              case Payload.Draft(m) => Some(m)
              case _ => None
            }
          )
        // The reply's words, when it says something: what support is measured against.
        val replied = reply.flatMap(TurnJudge.said)
        val asked = first.payload.said.getOrElse("")
        for {
          steps <- reader
            .steps(turn.workflowId)
            .left
            .map(e => s"steps of ${WorkflowId.value(turn.workflowId)} unread: ${e.kind}")
          tags <-
            if (heard) read("tags")(reader.triage.of(Vector(first.id))).map(_.get(first.id))
            else Right(None)
          offer <- TurnRecord
            .offer(steps)
            .left
            .map(why => s"offer of ${WorkflowId.value(turn.workflowId)} unread: $why")
          offered <- offer.fold[Either[String, Option[Offered]]](Right(None))(o =>
            read("offer")(for {
              set <- reader.toolSets.get(o.tools)
              prompt <- reader.prompts.prompt(o.prompt)
            } yield {
              val byLayer = prompt.fragments.groupBy(_.layer)
              Some(
                Offered(
                  set.tools.map(_.name),
                  o.tools,
                  schema(set.tools),
                  VectorMap.from(
                    prompt.fragments
                      .map(_.layer)
                      .distinct
                      .map(l =>
                        l -> minus(
                          CharEstimate.system(
                            byLayer.getOrElse(l, Vector.empty).map(_.text).mkString("\n\n")
                          ),
                          CharEstimate.PerMessage
                        )
                      )
                  ),
                  o.workspace,
                  o.reached.values.toVector.distinct.sortBy(_.written),
                  o.shaped
                )
              )
            })
          )
          weighed <- TurnRecord
            .weighed(steps)
            .left
            .map(why => s"weighing of ${WorkflowId.value(turn.workflowId)} unread: $why")
          // The window as the turn was shown it: a carried one with every record it follows.
          window <- CarriedWindow.whole(Turn.windowId(turn), all) match {
            case Some(Payload.Window(seqs, recalled, nearby, versions, _, _, _, _)) =>
              read("window")(
                for {
                  at <- reader.entries.at(turn.conversationId, seqs)
                  near <- Nearby.read(nearby, reader.entries)
                  named <- reader.principals.speakers((at ++ near ++ mine).map(_.id).distinct)
                  documents <- held(reader, versions)
                } yield Some(
                  documents.flatMap(_._2).flatMap(_._1.pointer) -> parts(
                    turn,
                    at,
                    recalled.toSet,
                    nearby,
                    near,
                    documents,
                    offered.fold(Set.empty[ToolName])(_.tools.toSet).contains,
                    mine,
                    named,
                    replied,
                    asked
                  )
                )
              )
            case _ => Right(None)
          }
          held = window.fold(Vector.empty[Pointer])(_._1)
          ledger <- read("ledger")(reader.ledger.of(turn.workflowId))
          speech <- TurnRecord
            .speech(steps)
            .left
            .map(why => s"speech of ${WorkflowId.value(turn.workflowId)} unread: $why")
        } yield {
          val tools = offered.fold(Vector.empty[ToolName])(_.tools).toSet
          Some(
            TurnCase(
              turn.workflowId,
              turn.conversationId,
              turn.turnSeq,
              saidAs,
              // Its offer's root when it recorded one: a named turn is named. A turn ended
              // before its offer has its message's.
              offer.fold(if (heard) TurnOffer.Root.Heard else TurnOffer.Root.Addressed)(_.root),
              focus,
              recorded.created,
              build,
              tags.map(Live.of),
              offered,
              weighed,
              window.map(_._2),
              rounds(turn, mine, steps, tools, current, held, replied, asked),
              ended(reply, steps, recorded.status),
              ledger.map(r =>
                Spent(TurnRecord.role(turn, r.entry), r.model, r.usage, r.estimatedInput)
              ),
              speech.map(drafted)
            )
          )
        }
    }
  }

  /** What the definitions of `tools` cost as a request is costed ([[CharEstimate]]): nothing
    * for none.
    */
  def schema(tools: Vector[ToolSet.Entry]): Tokens =
    if (tools.isEmpty) Tokens.Zero
    else
      minus(
        CharEstimate.request(
          ModelRequest(
            "",
            Vector.empty,
            tools.map(e => ToolSchema(ToolName.value(e.name), e.does, e.parameters))
          )
        ),
        CharEstimate.system("")
      )

  /** `window`, drawn for `turn`, by part as [[apply]] captures a recorded one, each costed by
    * [[CharEstimate]] as [[Shown]] shows it now, none with a support, and each document with its
    * pointer's line, whatever tools the turn would be offered. `Left` naming what could not be
    * read; never a message's text.
    */
  def costed(reader: Reader^, turn: TurnRef, window: Window): Either[String, Parts] =
    reader.all
      .read(
        for {
          all <- reader.entries.list(turn.conversationId)
          at <- reader.entries.at(turn.conversationId, window.entries)
          near <- Nearby.read(window.nearby, reader.entries)
          mine = all.filter(_.turnSeq == turn.turnSeq).sortBy(e => EntrySeq.value(e.seq))
          named <- reader.principals.speakers((at ++ near ++ mine).map(_.id).distinct)
          documents <- held(reader, window.documents)
        } yield parts(
          turn,
          at,
          window.notes.flatMap {
            case AssemblyNote.Recalled(turns) => turns
            case _ => Vector.empty
          }.toSet,
          window.nearby,
          near,
          documents,
          _ => true,
          mine,
          named,
          None,
          ""
        )
      )
      .left
      .map(e => s"the window of ${WorkflowId.value(turn.workflowId)} unread: ${e.kind}")

  /** Each of `versions` as a window holding it shows it today: its document, if it is still
    * kept, and its plugin's label, if its plugin is enabled.
    */
  private def held(reader: Reader^, versions: Vector[DocumentVersionId])(using
      Tx^
  ): Either[StoreError, Vector[(DocumentVersionId, Option[(Document, Option[DocLabel])])]] =
    for {
      kept <- reader.documents.read(versions)
      labelled <- reader.documents.labelled(versions)
    } yield versions.map(v =>
      v -> kept
        .find(_.version == v)
        .map(d => d -> labelled.collectFirst { case (l, label) if l.version == v => label })
    )

  /** The window's parts in the order shown: each of `nearby`'s sections over `near`, then
    * `documents`, then `at`, its own entries, by turn (a closing alone, as the record).
    */
  private def parts(
      turn: TurnRef,
      at: Vector[Entry],
      recalled: Set[grit.core.id.TurnSeq],
      nearby: Vector[Nearby],
      near: Vector[Entry],
      documents: Vector[(DocumentVersionId, Option[(Document, Option[DocLabel])])],
      offered: ToolName => Boolean,
      mine: Vector[Entry],
      named: Speakers,
      replied: Option[String],
      asked: String
  ): Parts = {
    def support(shown: Vector[Message]) =
      replied.map(r => Support.of(r, asked, shown.map(text).mkString("\n")))
    val sections = nearby.map { n =>
      val shown = Shown.section(n, near, named).toVector
      val kind = n match {
        case Nearby.Open(_, _, _) => Part.Kind.Open
        case Nearby.Closed(_, _, _) => Part.Kind.Closed
        case Nearby.Along(_, _, _) => Part.Kind.Along
        case Nearby.Asked(_, _, _) => Part.Kind.Asked
      }
      Part(kind, n.conversation, n.names, cost(shown), support(shown))
    }
    def kindOf(e: Entry): Part.Kind = e.payload match {
      case Payload.Closed(_, _, _) => Part.Kind.Record
      case _ if recalled.contains(e.turnSeq) => Part.Kind.Recalled
      case _ => Part.Kind.Recent
    }
    // Runs of entries of one turn and kind: each a part.
    val keys = at.map(e => (e.turnSeq, kindOf(e)))
    val starts = keys.indices.toVector.filter(i => i == 0 || keys.lift(i) != keys.lift(i - 1))
    val groups = starts.zip(starts.drop(1) :+ at.size).map((from, until) => at.slice(from, until))
    val own = groups.flatMap(g =>
      g.headOption.map { h =>
        val shown = g.flatMap(Shown.of(_, named))
        Part(kindOf(h), turn.conversationId, g.map(_.seq), cost(shown), support(shown))
      }
    )
    val before = mine.takeWhile(e =>
      e.payload match {
        case Payload.Exchange(_) | Payload.Result(_, _) | Payload.Draft(_) => false
        case Payload.Message(_: Message.Assistant) => false
        case _ => true
      }
    )
    val shelf = documents.map {
      case (v, None) => Held.Gone(v)
      case (v, Some((d, label))) =>
        // A document whose plugin is no longer enabled is shown by no window: nothing of it. Its
        // pointer's line is shown only when the turn was offered its tool, as the turn shows it.
        val shown =
          label
            .map(Shown.document(d.copy(pointer = d.pointer.filter(p => offered(p.tool))), _))
            .toVector
        Held.Kept(v, d.plugin, d.pointer.map(_.tool), cost(shown), support(shown))
    }
    Parts(
      sections ++ own,
      shelf,
      minus(
        cost(Shown.own(at, None, turn.turnSeq, named)),
        own.foldLeft(Tokens.Zero)(_ + _.tokens)
      ),
      cost(Shown.turn(before, named))
    )
  }

  /** Each reply of the loop that called tools, by its round, how each call settled (given up
    * on as `steps` show, else by the result recorded after it), and what it read: by `current`'s
    * pointers and those of the documents its window `held`, its support `replied`'s in its
    * result.
    */
  private def rounds(
      turn: TurnRef,
      mine: Vector[Entry],
      steps: Vector[StepRecord],
      tools: Set[ToolName],
      current: Vector[Pointer],
      held: Vector[Pointer],
      replied: Option[String],
      asked: String
  ): Vector[Round] = {
    val givenUp = TurnRecord.givenUp(steps)
    val calls = mine
      .flatMap(e =>
        (TurnRecord.role(turn, e.id), e.payload) match {
          case (Some(TurnRecord.Role.Round(n)), Payload.Exchange(reply)) => Some((n, e.seq, reply))
          case _ => None
        }
      )
      .sortBy(_._1)
    calls.zipWithIndex.map { case ((n, seq, reply), i) =>
      val until = calls.lift(i + 1).map(c => EntrySeq.value(c._2))
      val results = mine
        .filter(e =>
          EntrySeq.value(e.seq) > EntrySeq.value(seq) && until.forall(EntrySeq.value(e.seq) < _)
        )
        .collect { case Entry(_, _, _, _, _, Payload.Result(r, _), _, _) => r }
      val made = reply.blocks.collect { case c: AssistantBlock.ToolCall => c }
      Round(made.zipWithIndex.map { (c, j) =>
        val tool = Called.of(c.name, tools)
        // Calls of one reply may share an id: the j-th result with it answers the j-th call.
        val result = givenUp.get((n, j)) match {
          case Some(_) => None
          case None => results.filter(_.callId == c.id).lift(made.take(j).count(_.id == c.id))
        }
        val settled = (givenUp.get((n, j)), result) match {
          case (Some(TurnRecord.GivenUp.Expired), _) => Settled.Expired
          case (Some(TurnRecord.GivenUp.Abandoned), _) => Settled.Abandoned
          case (None, Some(r)) if r.isError => Settled.Failed(r.content.length)
          case (None, Some(r)) => Settled.Ok(r.content.length)
          case (None, None) => Settled.Unsettled
        }
        val support = for {
          said <- replied
          r <- result.filterNot(_.isError)
        } yield Support.of(said, asked, r.content)
        Call(tool, settled, Reading.of(c.name, c.arguments, current, held, support))
      })
    }
  }

  private def ended(
      reply: Option[Message.Assistant],
      steps: Vector[StepRecord],
      status: String
  ): Ended =
    reply match {
      case Some(m) => Ended.Replied(text(m).length, TurnJudge.said(m).isEmpty)
      case None =>
        TurnRecord.failure(steps) match {
          case Some((step, f)) =>
            Ended.Failed(
              step,
              f match {
                case TurnFailure.Assembly(_) => Ended.Kind.Assembly
                case TurnFailure.Model(_) => Ended.Kind.Model
                case TurnFailure.Store(_) => Ended.Kind.Store
                case TurnFailure.Uncleared(_) => Ended.Kind.Uncleared
                case TurnFailure.ToolsChanged(_) => Ended.Kind.ToolsChanged
              }
            )
          case None => Ended.Unfinished(status)
        }
    }

  /** What became of a draft, `o`, as a case keeps it: its kind and the scores it was judged by. */
  private[capture] def drafted(o: Outcome): Drafted = {
    def scored(kind: Drafted.Kind, j: grit.core.speech.Judged, postAt: Option[Probability]) =
      Drafted(kind, Some(j.grounded), Some(j.worth), postAt)
    def cleared(kind: Drafted.Kind, c: grit.core.speech.Cleared) = c match {
      case grit.core.speech.Cleared.Scored(j) => scored(kind, j, None)
      // A named draft is not judged.
      case grit.core.speech.Cleared.Named => Drafted(kind, None, None, None)
    }
    o match {
      case Outcome.Passed => Drafted(Drafted.Kind.Passed, None, None, None)
      case Outcome.NothingRecalled => Drafted(Drafted.Kind.NothingRecalled, None, None, None)
      case Outcome.Spoken(_) => Drafted(Drafted.Kind.Spoken, None, None, None)
      case Outcome.Withdrawn => Drafted(Drafted.Kind.Withdrawn, None, None, None)
      case Outcome.Unjudged(_) => Drafted(Drafted.Kind.Unjudged, None, None, None)
      case Outcome.Below(j, postAt) => scored(Drafted.Kind.Below, j, Some(postAt))
      case Outcome.Shadowed(c) => cleared(Drafted.Kind.Shadowed, c)
      case Outcome.Posted(c) => cleared(Drafted.Kind.Posted, c)
      case Outcome.Replied => Drafted(Drafted.Kind.Replied, None, None, None)
      case Outcome.Failed(_) => Drafted(Drafted.Kind.Failed, None, None, None)
    }
  }

  /** What `shown` costs, message by message. */
  private def cost(shown: Vector[Message]): Tokens =
    shown.map(CharEstimate.message).foldLeft(Tokens.Zero)(_ + _)

  private def minus(a: Tokens, b: Tokens): Tokens =
    Tokens(math.max(0L, Tokens.value(a) - Tokens.value(b)))

  /** A message's words as the model reads them: a person's text, a reply's text blocks, a
    * tool result's content.
    */
  private def text(m: Message): String = m match {
    case Message.User(t) => t
    case Message.Assistant(blocks, _, _, _, _) =>
      blocks.collect { case AssistantBlock.Text(t) => t }.mkString
    case Message.ToolResult(_, content, _) => content
  }
}
