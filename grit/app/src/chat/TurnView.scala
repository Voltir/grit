package grit.app.chat

import java.time.Duration

import grit.core.context.Shown
import grit.core.id.{EntryId, TurnRef, TurnSeq}
import grit.core.message.{Cost, Message, Tokens}
import grit.core.model.{ModelRef, TurnProfile}
import grit.core.place.Place
import grit.core.prompt.{Layer, SystemPrompt}
import grit.core.provider.TokenEstimator
import grit.core.store.{Entry, Payload, Speakers, UsageLedger}
import grit.dbos.engine.RecordedStep
import grit.turn.Turn

/** One turn as the turn panel shows it: where it is, what each step took, what it searched
  * for, what its window held and what it cost. Pure data, built by [[TurnView.of]] from
  * what the store, DBOS and the ledger recorded.
  *
  * @param turn the turn's position in the conversation, from 0
  * @param asked the user's message that started it
  * @param running the step it is in, while it runs
  * @param steps the steps it recorded, in order, named as [[Turn.Step.named]] shows them
  * @param query what assembly searched for, when it searched
  * @param window what the model saw, once the reply is recorded
  * @param spent what its model calls cost, once one was made
  * @param billed the input tokens the provider counted for the reply
  * @param models what its model calls were made under, once it pinned them
  */
final case class TurnView(
    turn: TurnSeq,
    asked: String,
    running: Option[String],
    steps: Vector[TurnView.Step],
    query: Option[String],
    window: Option[TurnView.Window],
    spent: Option[Cost],
    billed: Option[Tokens],
    models: Option[TurnView.Models] = None,
    prompt: Vector[TurnView.Part] = Vector.empty
) {

  /** Whether nothing more will change: finished, its last step recorded. A turn that
    * failed short of it is read again, which costs a poll's queries and nothing more.
    */
  def settled: Boolean = running.isEmpty && steps.exists(_.name == Turn.Step.AppendSummary)
}

object TurnView {

  /** One fragment of the system prompt a turn was sent: its `label` (its layer, or for an
    * instruction file the file's name) and its estimated `tokens`.
    */
  final case class Part(label: String, tokens: Tokens)

  /** `prompt`'s fragments, each labelled and estimated with `estimator`, in its order. */
  def parts(prompt: SystemPrompt, estimator: TokenEstimator): Vector[Part] =
    prompt.fragments.map { f =>
      val label =
        if (f.layer == Layer.Place)
          f.source.split('/').lastOption.filter(_.nonEmpty).getOrElse(f.source)
        else f.layer.key
      Part(label, estimator.system(f.text))
    }

  /** What a turn's calls were made under: each role's pair, the turn's first and then any
    * role whose pair differs from it; whether grit had a profile for the turn's pair; and
    * the upstream the reply says served it.
    */
  final case class Models(
      roles: Vector[(String, ModelRef)],
      profiled: Boolean,
      served: Option[String]
  )

  object Models {

    /** From the turn's pinned `profile`, its reply's `served` upstream. */
    def of(profile: TurnProfile, served: Option[String]): Models = {
      val turn = profile.turn.assignment.ref
      val others = Vector("summary" -> profile.summary, "query" -> profile.query).collect {
        case (role, p) if p.assignment.ref != turn => role -> p.assignment.ref
      }
      Models(("model" -> turn) +: others, profile.turn.settings.profiled, served)
    }
  }

  /** A recorded step and how long it took, when DBOS kept both ends. */
  final case class Step(name: String, ms: Option[Long])

  /** What the reply's request held, estimated: the system prompt, the closing entry of the
    * period before, the recent turns with the gap lines between the window's turns, the
    * turns search recalled (`recalledTurns`), the
    * turn's own messages, and the sections from other conversations (`nearby`, their turns
    * `nearbyTurns`).
    */
  final case class Window(
      system: Tokens,
      closing: Tokens,
      recent: Tokens,
      recalled: Tokens,
      message: Tokens,
      recalledTurns: Vector[TurnSeq],
      nearby: Tokens = Tokens.Zero,
      nearbyTurns: Vector[Near] = Vector.empty
  ) {
    def total: Tokens = system + closing + recent + recalled + message + nearby
  }

  /** A nearby section's place, and the turns of its conversation it showed. */
  final case class Near(place: Place, turns: Vector[TurnSeq])

  object Near {

    /** `near` in one line, as the panel lists it: each turn under its place's last segment,
      * `api turn 3 · web turn 5`.
      */
    def shown(near: Vector[Near]): String =
      near
        .flatMap(n =>
          n.turns.map(t =>
            s"${n.place.segments.lastOption.getOrElse(n.place.written)} turn ${TurnSeq.value(t)}"
          )
        )
        .mkString(" · ")
  }

  /** The latest turn in `entries`: the one its last user message started. */
  def latest(entries: Vector[Entry]): Option[TurnRef] =
    entries.reverseIterator
      .collectFirst { case e @ Entry(_, _, _, _, _, Payload.Message(Message.User(_)), _) => e }
      .map(e => TurnRef(e.conversationId, e.turnSeq))

  /** `turn`, from every entry of its conversation, whose person's messages `speakers` names,
    * and the `nearby` entries of other conversations its window showed, the `steps` its
    * workflow recorded,
    * whether it is `running`, its ledger rows `costs`, the `profile` it pinned, and the
    * system `prompt` it was sent (none for a turn that recorded none). The window is
    * estimated with `estimator`, as assembly estimated it, the prompt with it too.
    */
  def of(
      turn: TurnRef,
      entries: Vector[Entry],
      speakers: Speakers,
      steps: Vector[RecordedStep],
      running: Boolean,
      costs: Vector[UsageLedger.Row],
      prompt: Option[SystemPrompt],
      estimator: TokenEstimator,
      profile: Option[TurnProfile] = None,
      nearby: Vector[Entry] = Vector.empty
  ): TurnView = {
    val own = entries.filter(_.turnSeq == turn.turnSeq)
    val byId: Map[EntryId, Entry] = entries.map(e => e.id -> e).toMap
    val window =
      own.collectFirst { case Entry(_, _, _, _, _, w: Payload.Window, _) => w }.map { w =>
        val seen = w.entries.flatMap(byId.get)
        val (closings, turns) = seen.partition(isClosed)
        val (recalled, recent) = turns.partition(e => w.recalled.contains(e.turnSeq))
        val theirs = nearby.map(e => e.id -> e).toMap
        val sections = w.nearby.map(n => n -> n.entries.flatMap(theirs.get))
        Window(
          prompt.fold(Tokens.Zero)(p => estimator.system(p.render)),
          tokens(closings, speakers, estimator),
          tokens(recent, speakers, estimator) + gaps(seen, turn, estimator),
          tokens(recalled, speakers, estimator),
          tokens(own.filter(isUser), speakers, estimator),
          w.recalled,
          sections
            .flatMap((n, es) => Shown.nearby(n.place, es))
            .map(estimator.message)
            .foldLeft(Tokens.Zero)(_ + _),
          sections.map((n, es) => Near(n.place, es.map(_.turnSeq).distinct))
        )
      }
    val reply = own.collectFirst {
      case Entry(_, _, _, _, _, Payload.Message(m: Message.Assistant), _) => m
    }
    val spent = Option.when(costs.nonEmpty)(Cost.total(costs.map(_.usage)))
    TurnView(
      turn.turnSeq,
      own
        .collectFirst { case Entry(_, _, _, _, _, Payload.Message(Message.User(t)), _) => t }
        .getOrElse(""),
      Option.when(running)(Turn.running(steps.map(_.name))),
      timed(steps),
      own.collectFirst { case Entry(_, _, _, _, _, Payload.Query(q), _) => q },
      window,
      spent,
      reply.map(_.usage.input),
      profile.map(Models.of(_, reply.flatMap(_.upstream))),
      prompt.fold(Vector.empty[Part])(parts(_, estimator))
    )
  }

  /** `recorded` as the panel shows them ([[Turn.Step.named]]), each timed from its start to
    * its end; a person's wait from the end of the ask before it, since DBOS keeps no start
    * for it that the panel can trust.
    */
  private def timed(recorded: Vector[RecordedStep]): Vector[Step] = {
    val names = Turn.Step.named(recorded.map(_.name))
    recorded.zip(names).zipWithIndex.map { case ((step, name), i) =>
      val from =
        if (Turn.Step.family(name).contains(Turn.Step.Wait))
          recorded
            .take(i)
            .zip(names)
            .findLast((_, n) => Turn.Step.family(n).contains(Turn.Step.Ask))
            .flatMap(_._1.completed)
        else step.started
      Step(name, from.zip(step.completed).map(Duration.between(_, _).toMillis))
    }
  }

  private def isUser(e: Entry): Boolean = e.payload match {
    case Payload.Message(Message.User(_)) => true
    case _ => false
  }

  private def isClosed(e: Entry): Boolean = e.payload match {
    case Payload.Closed(_, _, _) => true
    case _ => false
  }

  /** What the gap lines `seen`, a window's own entries, were shown with cost ([[Shown.own]]):
    * counted with the recent turns. Names add no lines, only length, so none are needed to
    * count them.
    */
  private def gaps(seen: Vector[Entry], turn: TurnRef, estimator: TokenEstimator): Tokens = {
    val lines =
      Shown.own(seen, turn.turnSeq, Speakers.none).size -
        seen.flatMap(e => Shown.of(e, Speakers.none)).size
    Tokens(lines * Tokens.value(estimator.message(Shown.Gap)))
  }

  /** What `entries` cost as the model is shown them with `speakers`' names ([[Shown.of]]), as
    * assembly costs them.
    */
  private def tokens(
      entries: Vector[Entry],
      speakers: Speakers,
      estimator: TokenEstimator
  ): Tokens =
    entries.flatMap(e => Shown.of(e, speakers)).map(estimator.message).foldLeft(Tokens.Zero)(_ + _)
}
