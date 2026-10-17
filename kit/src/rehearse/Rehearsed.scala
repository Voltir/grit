package grit.kit.rehearse

import grit.act.moves.MovesJournal
import grit.assembly.estimate.CharEstimate
import grit.core.classify.{ClassifierError, Question}
import grit.core.durable.StepRecord
import grit.core.id.WorkflowId
import grit.core.message.{AssistantBlock, Message, Tokens}
import grit.core.model.Pinned
import grit.core.provider.{ModelRequest, ToolSchema}
import grit.core.visibility.Label
import grit.turn.TurnRecord

/** What a dry rehearsal showed, in the order it happened: each workflow it ran, with how its
  * body ended, the steps it ran (as a recorded workflow's are read) and each call made in it.
  */
final case class Rehearsed(workflows: Vector[Rehearsed.Ran]) {

  /** Every call, in the order made. */
  def calls: Vector[Rehearsed.Call] = workflows.flatMap(_.calls)

  /** All of it as text, every part's text whole: message text, for a file. It opens with
    * [[Rehearsed.Header]].
    */
  def written: String = shown(whole = true)

  /** Its shape as text: each workflow, and each call's step, model or service, label and
    * answer by kind, its parts by name and size, and no part's or answer's text.
    */
  def counted: String = shown(whole = false)

  private def shown(whole: Boolean): String = {
    val lines = Vector.newBuilder[String]
    lines += Rehearsed.Header
    workflows.foreach { w =>
      lines += ""
      lines += s"workflow ${WorkflowId.value(w.workflow)}: ${Rehearsed.ended(w.came)}"
      w.calls.foreach { c =>
        lines += s"  call in ${c.step.getOrElse("no step")} to ${c.to}, ${Rehearsed.labelled(c.label)}"
        c.parts.foreach { p =>
          lines += s"    ${p.name}: ${p.chars} chars, ~${Tokens.value(p.tokens)} tokens"
          if (whole) lines ++= p.text.linesIterator.map("      " + _)
        }
        lines += s"    ${Rehearsed.answeredBy(c.answered)}"
        c.answered match {
          case Rehearsed.Answered.StandIn(_, reply) if whole =>
            lines ++= reply.linesIterator.map("      " + _)
          case _ => ()
        }
      }
    }
    lines.result().mkString("", "\n", "\n")
  }
}

object Rehearsed {

  /** What a dry rehearsal's text opens with: that no call was sent, and what the stub judge
    * does to the chain after it.
    */
  val Header: String =
    "A dry rehearsal: every call is answered by a stand-in, and none is sent. The stub " +
      "classifier answers every yes/no question yes at 0.9, so Jev always speaks and every " +
      "candidate a search finds is admitted; supply a step's answer (ask:relevance-0=…) to see " +
      "one dropped."

  /** The workflow `workflow`, whose body ended as `came`, having run `steps`, in which the calls
    * `calls` were made.
    */
  final case class Ran(
      workflow: WorkflowId,
      came: RehearsalRunner.Came,
      steps: Vector[StepRecord],
      calls: Vector[Call]
  )

  /** A call made in the step `step` (`None`: between operations) to `to` (a model, with its
    * output budget, effort and tool use, or a classifier's service), sent `input`, routed for
    * `label` (`None`: no record the rehearsal ran names it), and how it was answered.
    */
  final case class Call(
      step: Option[String],
      to: String,
      label: Option[Label],
      input: Input,
      answered: Answered
  ) {

    /** Its input by part: a request's system prompt, each tool and each message; a
      * classifier's state and each question.
      */
    def parts: Vector[Part] = input match {
      case Input.Model(request) => Rehearsed.parts(request)
      case Input.Classifier(state, questions) => judged(state, questions)
    }
  }

  /** What a call was sent. */
  enum Input {

    /** A model's request. */
    case Model(request: ModelRequest)

    /** A classifier's questions about a state. */
    case Classifier(state: ujson.Value, questions: Vector[Question])
  }

  /** One part of a call's input, `name`d, its `text` as sent, `chars` long, estimated at
    * `tokens` by [[CharEstimate]].
    */
  final case class Part(name: String, text: String, chars: Int, tokens: Tokens)

  /** How a call was answered. */
  enum Answered {

    /** By the stand-in `by`, which replied `reply`. */
    case StandIn(by: Answers.StandIn, reply: String)

    /** Not at all: the turn's own call, shown and never sent ([[Stop]]). */
    case Unsent
  }

  /** What `entries` kept of the workflows `ran`, in that order. */
  private[rehearse] def of(
      entries: Vector[Tape.Entry],
      ran: Vector[RehearsalRunner.Ran]
  ): Rehearsed = {
    // Each workflow's entries: those after the runner told it running, up to the next.
    val kept: Map[WorkflowId, Vector[Tape.Entry]] =
      entries
        .foldLeft(Vector.empty[(WorkflowId, Vector[Tape.Entry])]) {
          case (groups, Tape.Entry.Told(Rehearsal.Event.Running(id))) => groups :+ (id -> Vector())
          case (groups, entry) =>
            groups.lastOption.fold(groups)((id, es) =>
              groups.updated(groups.size - 1, id -> (es :+ entry))
            )
        }
        .toMap
    Rehearsed(ran.map(r => workflow(r, kept.getOrElse(r.workflow, Vector.empty))))
  }

  /** The workflow `r` ran, from what was kept while it ran: each call labelled by the record of
    * the ask it was made in ([[MovesJournal.askedAt]]), else by the turn's route record.
    */
  private def workflow(r: RehearsalRunner.Ran, entries: Vector[Tape.Entry]): Ran = {
    val steps = entries.collect { case Tape.Entry.Told(Rehearsal.Event.Ended(record)) => record }
    val routed = TurnRecord.routedAt(steps).toOption.flatten
    def asked(step: Option[String], after: Int): Option[Label] =
      entries
        .drop(after)
        .collectFirst {
          case Tape.Entry.Told(Rehearsal.Event.Ended(record)) if step.contains(record.name) =>
            record.output
        }
        .flatten
        .flatMap(MovesJournal.askedAt)
    val calls = entries.zipWithIndex.collect { case (Tape.Entry.Called(call), at) =>
      call match {
        case Tape.Call.Model(step, pinned, request, answered) =>
          Call(
            step,
            model(pinned, request),
            asked(step, at).orElse(routed),
            Input.Model(request),
            answered.fold(Answered.Unsent)(a => Answered.StandIn(a.by, said(a.answer)))
          )
        case Tape.Call.Classifier(step, service, state, questions, answered) =>
          Call(
            step,
            service.place.written,
            asked(step, at).orElse(routed),
            Input.Classifier(state, questions),
            Answered.StandIn(answered.by, judgment(answered.answer))
          )
      }
    }
    Ran(r.workflow, r.came, steps, calls)
  }

  private def model(pinned: Pinned, request: ModelRequest): String = {
    val a = pinned.assignment
    s"${a.ref}, at most ${a.maxTokens} tokens out" +
      a.effort.fold("")(e => s", effort $e") +
      (if (request.tools.isEmpty) "" else s", tools ${request.use}")
  }

  private def parts(request: ModelRequest): Vector[Part] =
    Part("system", request.system, request.system.length, CharEstimate.system(request.system)) +:
      (request.tools.map(tool) ++
        request.messages.zipWithIndex.map { (m, i) =>
          val (role, text) = m match {
            case Message.User(text) => ("user", text)
            case a: Message.Assistant => ("assistant", said(a))
            case Message.ToolResult(_, content, _) => ("tool result", content)
          }
          Part(s"message $i, $role", text, text.length, CharEstimate.message(m))
        })

  private def tool(t: ToolSchema): Part = {
    val text = ujson.write(
      ujson.Obj("description" -> t.description, "parameters" -> t.parameters),
      indent = 2
    )
    Part(s"tool ${t.name}", text, text.length, estimated(text))
  }

  private def judged(state: ujson.Value, questions: Vector[Question]): Vector[Part] = {
    val written = ujson.write(state, indent = 2)
    Part("state", written, written.length, estimated(written)) +:
      questions.zipWithIndex.map { (q, i) =>
        val text = q.toString
        Part(s"question $i", text, text.length, estimated(text))
      }
  }

  /** A token per [[CharEstimate.CharsPerToken]] characters, rounded up: as the estimator counts
    * what it has no framing for.
    */
  private def estimated(text: String): Tokens =
    Tokens((text.length.toLong + CharEstimate.CharsPerToken - 1) / CharEstimate.CharsPerToken)

  private def said(m: Message.Assistant): String =
    m.blocks
      .map {
        case AssistantBlock.Text(text) => text
        case AssistantBlock.Reasoning(text, _) => s"(reasoning) $text"
        case AssistantBlock.ToolCall(_, name, arguments) => s"$name ${ujson.write(arguments)}"
      }
      .mkString("\n")

  private def judgment(
      answer: Either[ClassifierError, grit.core.classify.Answers]
  ): String = answer match {
    case Left(error) => s"refused: $error"
    case Right(answers) =>
      answers.answers
        .map {
          case grit.core.classify.Answer.YesNo(yes) => s"yes/no: $yes"
          case other => other.toString
        }
        .mkString("\n")
  }

  private def ended(came: RehearsalRunner.Came): String = came match {
    case RehearsalRunner.Came.Returned(_) => "returned"
    case RehearsalRunner.Came.Threw(why) => s"threw: $why"
    case RehearsalRunner.Came.Stopped(step) => s"stopped at the turn's own call, in $step"
  }

  private def labelled(label: Option[Label]): String =
    label.fold("label unrecorded")(l => s"routed for ${Label.shown(l)}")

  private def answeredBy(answered: Answered): String = answered match {
    case Answered.StandIn(Answers.StandIn.Stub, _) => "answered: STAND-IN (stub)"
    case Answered.StandIn(Answers.StandIn.Supplied, _) => "answered: STAND-IN (supplied)"
    case Answered.Unsent => "not sent: the turn's own call"
  }
}
