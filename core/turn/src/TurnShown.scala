package grit.turn

import java.time.Instant

import grit.core.clock.Utc
import grit.core.context.{Shown, Window}
import grit.core.document.{DocLabel, DocText, Document, DocumentSearch}
import grit.core.files.{KeptPiece, PieceSearch}
import grit.core.id.{EntrySeq, ToolCallId, TurnRef, WorkflowId}
import grit.core.message.{AssistantBlock, Message}
import grit.core.model.{AfterToolResult, ToolGuidance}
import grit.core.period.{Balance, Closing, Flows}
import grit.core.provider.{ModelRequest, ToolSchema, ToolUse}
import grit.core.store.{
  ConversationStore,
  Entry,
  EntryStore,
  Nearby,
  Payload,
  Principals,
  Speakers,
  StoreError,
  Tx
}
import grit.core.tool.{DuplicateName, Tool, ToolName, ToolSet, Toolbox}
import grit.core.visibility.{Integrity, Subject}

import TurnLoop.Round

/** What a turn's model is shown: the tools each call offers and the lines its gate adds, the
  * request built from its window and its own entries, each item its gate hid shown as its
  * stub, and the items a request shows, which its gate decides over. Pure but for the reads
  * that take a transaction or a `TurnEnv`, so a turn read back rebuilds what it sent
  * ([[Turn.rebuilt]]).
  */
private[turn] object TurnShown {

  /** `c`, when the classifier was unsure: the classification the model is asked about. */
  def asked(c: TurnAgenda.Classification): Option[TurnAgenda.Classification] =
    Option.when(c.uncertain)(c)

  /** The tools each call of the loop offers: `own`, after `topic` for the classification
    * `asked` holds. `topic` stays on after the first call, and every call carries the same tag
    * asking for it ([[loopShape]]): a request whose messages hold a call names the tool it
    * called, and each call's request repeats the one before it, which a provider's prompt
    * cache relies on.
    * read_hidden and extract are not among them: a round binds and offers them apart
    * ([[gatedOffer]]). `TurnFailure.Model` when `own` already names one `topic`, read_hidden or
    * extract.
    */
  def offered[C^](
      own: Toolbox[C],
      asked: Option[TurnAgenda.Classification]
  ): Either[TurnFailure, Toolbox[C]] = {
    val tools = asked match {
      case None => Right(own)
      case Some(c) => own.including(TurnVerdict.tool(c))
    }
    tools
      .flatMap(t => t.preceded(Vector(ReadHidden.tool(None), unasked)).map(_ => t))
      .left
      .map { case DuplicateName(name) =>
        TurnFailure.Model(s"two tools are named ${ToolName.value(name)}")
      }
  }

  /** What every request of a turn offers of `schemas`, the schemas of `offered`, and the lines
    * its system prompt ends with, as the turn's first gate, `first`, decided (`None`: no gate):
    * when [[mayGate]], read_hidden and extract first ([[ReadHidden]], [[Extract]], `strict` as
    * `schemas` are), and [[TurnGate.Noted]]; then `schemas` less those `first` withholds. A tool
    * a later round's gate withholds stays offered, and a call of it is refused
    * ([[grit.core.tool.CallError.Withheld]]), as that round's note says ([[gateNotes]]).
    */
  def gatedOffer(
      first: Option[Gated],
      offered: ToolSet,
      schemas: Vector[ToolSchema],
      strict: Boolean
  ): (Vector[ToolSchema], Vector[String]) = {
    val gating = mayGate(first, offered)
    val withheld = first.fold(Vector.empty[Withheld])(_.withheld)
    (
      Vector(ReadHidden.tool(None).schema(strict), unasked.schema(strict)).filter(_ => gating) ++
        schemas.filterNot(s => withheld.exists(w => ToolName.value(w.tool) == s.name)),
      Vector(TurnGate.Noted).filter(_ => gating)
    )
  }

  /** Whether a gate of a turn whose first gate was `first`, offering `offered`, could hide an
    * item or withhold a tool on any round: `first` hid one, or a tool of `offered` requires
    * what is not the least ([[grit.core.visibility.Integrity.Untrusted]]) and `first`'s level
    * dominates, so a result read later could withdraw it. False with no gate.
    */
  def mayGate(first: Option[Gated], offered: ToolSet): Boolean =
    first.exists(g =>
      g.hidden.nonEmpty || offered.tools.exists(t =>
        !Integrity.Untrusted.dominates(t.requires) && g.level.dominates(t.requires)
      )
    )

  /** The note each round up to `round` adds, oldest first, as `gates`, each round's gate by
    * its index, made them: [[TurnGate.Guidance]] on the first round whose gate hid an item,
    * then the withdrawal the round's gate makes against the round before's
    * ([[TurnGate.withdrawn]]), its tools as `offered` says them; each note as
    * [[TurnGate.noted]] fences it. A round adding neither has none.
    */
  def gateNotes(gates: Map[Int, Gated], round: Round, offered: ToolSet): Vector[(Round, String)] = {
    val firstHid = gates.collect { case (index, g) if g.hidden.nonEmpty => index }.minOption
    (Round.First.index to round.index).toVector.flatMap { index =>
      gates.get(index).flatMap { now =>
        val lines = Vector(TurnGate.Guidance).filter(_ => firstHid.contains(index)) ++
          gates.get(index - 1).flatMap(TurnGate.withdrawn(_, now, offered))
        Option.when(lines.nonEmpty)(Round.at(index) -> TurnGate.noted(lines))
      }
    }
  }

  /** extract as a request offers it, asking nothing: for its name and schema alone. */
  private val unasked: Tool.Offered = Extract.tool(None, Extract.Unasked)

  /** How each call of the loop is built from the plain request: tagged for `asked`
    * ([[TurnVerdict.tagged]]); then each of `notes` as a user message, as written: a round's
    * right before the turn's reply of that round (the round's index counting the replies after
    * the asker's message, [[TurnVerdict.asker]], or from the first message when there is
    * none), one whose reply is not there at the end; offering `tools` as `use` says, guided as
    * [[TurnLoop.guided]] says under `guidance`, told as [[TurnLoop.told]] says under `after`,
    * and its system prompt ending with each of `lines`, in order, each after a blank line. Fed
    * by [[gatedOffer]] and [[gateNotes]], each call's request repeats every message of the call
    * before it, with the same system prompt and tools. It marks stable
    * ([[ModelRequest.stable]]) every message the call shares with the same request told with
    * tools on: all of them but the last call's note, or the last result that note ends.
    */
  def loopShape(
      asked: Option[TurnAgenda.Classification],
      use: ToolUse,
      tools: Vector[ToolSchema],
      after: AfterToolResult,
      guidance: ToolGuidance,
      lines: Vector[String],
      notes: Vector[(Round, String)]
  ): ModelRequest -> ModelRequest =
    base => {
      val tagged = asked.fold(base)(TurnVerdict.tagged(base, _))
      val from = TurnVerdict.asker(base.messages).getOrElse(-1)
      val replies = tagged.messages.zipWithIndex.collect {
        case (_: Message.Assistant, i) if i > from => i
      }
      val before = notes
        .flatMap((round, note) => replies.lift(round.index).map(_ -> note))
        .groupMap(_._1)(_._2)
      val late = notes.collect { case (round, note) if replies.lift(round.index).isEmpty => note }
      val noted = tagged.copy(messages =
        tagged.messages.zipWithIndex.flatMap((m, i) =>
          before.getOrElse(i, Vector.empty).map(Message.User(_)) :+ m
        ) ++ late.map(Message.User(_))
      )
      val guided = TurnLoop.guided(noted.copy(tools = tools, use = use), guidance)
      val shaped = TurnLoop.told(use, guided, after)
      // A later call is sent this one's messages as a call with tools is told them, then more.
      val repeated = TurnLoop.told(ToolUse.Auto, guided, after).messages
      val stable = shaped.messages.zip(repeated).takeWhile(_ == _).size
      shaped.copy(system = (shaped.system +: lines).mkString("\n\n"), stable = stable)
    }

  /** The stubs `round`'s request shows, by the ref of the item each stands for, as its gate
    * in `gates` hid them, their tools as `offered` says them ([[TurnGate.stub]]); none for a
    * round with no gate.
    */
  def stubsAt(
      gates: Map[Int, Gated],
      round: Round,
      offered: ToolSet
  ): Map[ItemRef, String] =
    gates
      .get(round.index)
      .fold(Map.empty)(
        _.hidden.map((item, h) => item.ref -> TurnGate.stub(item, h, offered)).toMap
      )

  /** `turn`'s own entries, from `all` of its conversation's. */
  def own(all: Vector[Entry], turn: TurnRef): Vector[Entry] =
    all.filter(_.turnSeq == turn.turnSeq)

  /** The window's entries, then the turn's own, as one model request under `system`, its
    * message dated in `zoned` ([[requestOf]]), each item `stubs` names shown as its stub, to be
    * shaped, read through `env`'s `db`.
    */
  def request(
      env: TurnEnv^,
      system: String,
      turn: TurnRef,
      window: Window,
      stubs: Map[ItemRef, String],
      zoned: Option[TurnOffer.Zoned]
  )(using pins: Turn.Bounded): Either[TurnFailure, Unshaped] =
    pins
      .reads(env.db)
      .read(Subject.Turn(turn))(
        shown(env.records, turn, window).flatMap(s =>
          direct(env.hosting.conversations, turn).map(s -> _)
        )
      )
      .left
      .map(TurnFailure.ofStore)
      .flatMap((showing, direct) => requestOf(system, showing, turn, window, stubs, zoned, direct))

  /** What a request for `turn` over `window` can show, read from `records`: the entries of
    * its conversation at the window's seqs, the turn's own entries, the nearby sections'
    * entries that still exist, who said each, and the window's documents and pieces as it
    * shows them ([[grit.core.document.DocumentSearch.labelled]]); nothing else of the
    * conversation.
    */
  def shown(records: TurnRecords, turn: TurnRef, window: Window)(using
      Tx^
  ): Either[StoreError, Showing] =
    shown(records.entries, records.principals, records.documents, records.pieces, turn, window)

  /** [[shown]], read from the four stores it reads. */
  def shown(
      entries: EntryStore,
      principals: Principals,
      documents: DocumentSearch,
      pieces: PieceSearch,
      turn: TurnRef,
      window: Window
  )(using Tx^): Either[StoreError, Showing] =
    for {
      at <- entries.at(turn.conversationId, window.entries)
      mine <- entries.ofTurn(turn)
      near <- Nearby.read(window.nearby, entries)
      named <- principals.speakers((at ++ mine ++ near).map(_.id).distinct)
      labelled <- documents.labelled(window.documents)
      kept <-
        if (window.pieces.isEmpty) Right(Vector.empty) else pieces.read(window.pieces.map(_.id))
      shown <- documents.labelOf(kept)(_.plugin)
    } yield {
      val more = window.pieces.map(p => p.id -> p.more).toMap
      val parts = shown.map((k, l) => (k, l, more.getOrElse(k.id, 0)))
      Showing(at, mine, near, named, labelled, parts)
    }

  /** What [[shown]] read: `window`, the entries at the window's seqs that exist; `mine`,
    * the turn's own; `near`, the nearby entries that still exist; `named`, their speakers;
    * `documents`, the window's documents still kept whose plugin is enabled, in its order,
    * each with its plugin's label; `pieces`, its pieces likewise, each with its plugin's label
    * and how many more of its file matched.
    */
  final case class Showing(
      window: Vector[Entry],
      mine: Vector[Entry],
      near: Vector[Entry],
      named: Speakers,
      documents: Vector[(Document, DocLabel)],
      pieces: Vector[(KeptPiece, DocLabel, Int)]
  )

  /** The request [[request]] builds from `showing`, each item `stubs` names shown as its stub
    * there ([[hiding]]), to be shaped
    * ([[Unshaped]]): each nearby section as one user message ([[Shown.section]], a section with
    * none of its entries left dropped), then each document as one ([[Shown.document]]), then
    * each piece as one ([[Shown.piece]]), then
    * what the model is shown of the window's entries ([[Shown.own]]: its messages, a closing
    * entry as one user message, and a gap line wherever turns are left out), then, when the turn
    * has an own entry, [[answering]] of its first, in `zoned` (its offer's,
    * [[TurnOffer.Recorded]]), in a direct message when `direct` ([[direct]]), as one user
    * message, then the turn's own, its tool loop's exchange among them in order. A summary is not
    * shown to the model yet. `system` is sent as it is given. A window seq with no entry is an
    * `Assembly` failure naming every such seq.
    */
  def requestOf(
      system: String,
      showing: Showing,
      turn: TurnRef,
      window: Window,
      stubs: Map[ItemRef, String],
      zoned: Option[TurnOffer.Zoned],
      direct: Boolean
  ): Either[TurnFailure, Unshaped] = {
    val Showing(at, mine, near, named, documents, pieces) = hiding(showing, stubs)
    val bySeq = at.map(e => e.seq -> e).toMap
    val sections = window.nearby.flatMap(Shown.section(_, near, named))
    window.entries.filterNot(bySeq.contains) match {
      case missing if missing.nonEmpty =>
        Left(
          TurnFailure.Assembly(
            s"window names unknown entries at seqs: ${missing.map(EntrySeq.value).mkString(", ")}"
          )
        )
      case _ =>
        val shown = Shown.own(window.entries.flatMap(bySeq.get), turn.turnSeq, named)
        // The dated line is a message, not part of `system`: a provider caches its prompt from
        // the front, and nothing that changes per turn may sit before the messages. It is made
        // here, from the turn's first own entry, and no entry keeps it: a later turn shown this
        // one (Shown.own) sees no such line, so a window carried from turn to turn would differ
        // from this request at it.
        val dated = mine
          .minByOption(e => EntrySeq.value(e.seq))
          .map(root => Message.User(answering(root.createdAt, zoned, direct)))
        val parts = pieces.map((k, label, more) => Shown.piece(k, label, more))
        Right(
          Unshaped(system, sections, documents, parts ++ shown ++ dated ++ Shown.turn(mine, named))
        )
    }
  }

  /** The line [[requestOf]] sends just before a turn's own messages, for a turn whose first
    * entry was made `at`, in a direct message when `direct`. With no `zoned`, in UTC alone,
    * as before zones: "The message you are answering was said Wed 2026-10-21 15:03 UTC." With one, in
    * its zone, saying whose, then in UTC, then how to read times:
    * "The message you are answering was said Wed 2026-10-21 08:03 PDT, the person's time
    * (America/Los_Angeles); 15:03 UTC. People here give times in their own zone, not UTC: read the
    * times the person gives as PDT unless they say otherwise, and answer with times in PDT. Times
    * grit shows you in your view are UTC."
    * For a deployment's zone the first sentence reads "… 08:03 PDT, this deployment's time
    * (America/Los_Angeles); the person's own zone is not known; 15:03 UTC.", with the same tail.
    * Outside a direct message the tail ends "Read times other people here give as PDT, this
    * deployment's time, unless they say otherwise." (`zoned.deployment`'s short name). A direct
    * message's line stays short. The UTC time carries its UTC date when that is another day
    * ("06:30 UTC Thu 2026-10-22", [[Utc.toClock]]), and UTC under any name is said as UTC.
    */
  def answering(at: Instant, zoned: Option[TurnOffer.Zoned], direct: Boolean): String =
    zoned.fold(s"The message you are answering was said ${Utc.toDayMinute(at)}.") { z =>
      val short = Utc.shortName(at, z.dated)
      val whose = z.person.fold(
        s"this deployment's time (${Utc.canonical(z.deployment).getId}); the person's own zone " +
          "is not known"
      )(p => s"the person's time (${Utc.canonical(p).getId})")
      val others =
        if (direct) ""
        else
          s" Read times other people here give as ${Utc.shortName(at, z.deployment)}, this " +
            "deployment's time, unless they say otherwise."
      s"The message you are answering was said ${Utc.toDayMinute(at, z.dated)}, $whose; " +
        s"${Utc.toClock(at, z.dated)}. People here give times in their own zone, not UTC: read the times " +
        s"the person gives as $short unless they say otherwise, and answer with times in $short. " +
        s"Times grit shows you in your view are UTC.$others"
    }

  /** Whether `turn`'s conversation is held in a direct message's room ([[grit.core.place.Place.direct]]), which
    * [[answering]] is told; `Invalid` when the conversation is gone.
    */
  def direct(conversations: ConversationStore, turn: TurnRef)(using
      Tx^
  ): Either[StoreError, Boolean] =
    conversations
      .get(turn.conversationId)
      .flatMap(
        _.toRight(
          StoreError.Invalid(s"conversation of ${WorkflowId.value(turn.workflowId)} is gone")
        )
      )
      .map(_.origin.place.direct)

  /** A request of `system`: `before`, then each of `documents` as one user message, then
    * `after`.
    */
  final case class Unshaped(
      system: String,
      before: Vector[Message],
      documents: Vector[(Document, DocLabel)],
      after: Vector[Message]
  ) {

    /** This request shaped by `shape`, each document's pointer line shown only when the shaped
      * request offers the tool it names: a model offered no tool of that name could not follow
      * it. The tools offered are those `shape` gives the request with no pointer line shown.
      */
    def shaped(shape: ModelRequest -> ModelRequest): ModelRequest = {
      def built(offered: Set[String]): ModelRequest =
        ModelRequest(
          system,
          before ++ documents.map((document, label) =>
            Shown.document(
              document.copy(pointer =
                document.pointer.filter(p => offered.contains(ToolName.value(p.tool)))
              ),
              label
            )
          ) ++ after
        )
      shape(built(shape(built(Set.empty)).tools.map(_.name).toSet))
    }
  }

  /** Each item a request over `window` shows the model of `showing`, in the order it shows
    * them: the entries `window`'s nearby sections show, section by section ([[Shown.section]]),
    * the documents, the window's entries [[Shown.own]] shows, then the turn's own entries
    * [[Shown.turn]] shows; an entry kept as a tool's result by the call it answers, named by
    * its tool, and any other by its position, a person's named as [[Showing.named]] names them
    * ("someone" when it does not), grit's as "an earlier reply" and a closing as "a closing
    * record". An entry the request does not show (an earlier turn's tool exchange or result, a
    * window, a query, a summary, an item, an ask, a message with no text nearby) is none.
    */
  def itemsOf(showing: Showing, window: Window): Vector[Item] = {
    val Showing(shownWindow, mine, near, named, documents, pieces) = showing
    val tools: Map[ToolCallId, String] = (shownWindow ++ mine)
      .flatMap(_.payload match {
        case Payload.Exchange(reply) =>
          reply.blocks.collect { case AssistantBlock.ToolCall(id, name, _) => id -> name }
        case _ => Vector.empty
      })
      .toMap
    def person(e: Entry): String = named.of(e.id).getOrElse("someone")
    def entry(e: Entry, ref: ItemRef): Option[Item] = {
      val source = e.payload match {
        case Payload.Result(r, _) => Some(tools.getOrElse(r.callId, "a tool"))
        case Payload.Message(Message.User(_)) | Payload.Heard(_) => Some(person(e))
        case Payload.Message(_) | Payload.Exchange(_) | Payload.Posted(_) =>
          Some("an earlier reply")
        case Payload.Closed(_, _, _) => Some(TurnGate.ClosingRecord)
        case _ => None
      }
      val at = e.payload match {
        case Payload.Result(r, _) => ItemRef.Result(r.callId)
        case _ => ref
      }
      source.map(Item(at, e.integrity, _))
    }
    // Each entry as its section shows it alone: shown there exactly when it is shown alone.
    val nearby = window.nearby.flatMap { section =>
      section.names.flatMap { seq =>
        near
          .find(e => e.conversationId == section.conversation && e.seq == seq)
          .filter(e => Shown.section(only(section, seq), Vector(e), named).isDefined)
          .flatMap(e => entry(e, ItemRef.Nearby(e.conversationId, e.seq)))
      }
    }
    nearby ++
      documents.map((d, label) =>
        Item(ItemRef.Document(d.version), d.integrity, DocLabel.value(label))
      ) ++
      pieces.map((k, label, _) => Item(ItemRef.Piece(k.id), k.integrity, DocLabel.value(label))) ++
      shownWindow
        .filter(e => Shown.of(e, named).isDefined)
        .flatMap(e => entry(e, ItemRef.Own(e.seq))) ++
      mine
        .filter(e => Shown.turn(Vector(e), named).nonEmpty)
        .flatMap(e => entry(e, ItemRef.Own(e.seq)))
  }

  /** What a request over `window` would show of the item at `ref` among `showing` in full, as
    * its one message's text: a tool's result as [[Shown.result]] shows it, an entry as
    * [[Shown.of]] does, a nearby one as its section alone shows it ([[Shown.section]]), a
    * document as [[Shown.document]] does; `None` when `showing` no longer holds it.
    */
  def textOf(showing: Showing, window: Window, ref: ItemRef): Option[String] = {
    def text(m: Message): String = m match {
      case Message.User(t) => t
      case Message.Assistant(blocks, _, _, _, _) =>
        blocks.collect { case AssistantBlock.Text(t) => t }.mkString
      case Message.ToolResult(_, content, _) => content
    }
    val own = showing.window ++ showing.mine
    ref match {
      case ItemRef.Result(call) =>
        own.map(_.payload).collectFirst {
          case Payload.Result(r, _) if r.callId == call => Shown.result(r).content
        }
      case ItemRef.Own(seq) =>
        own.find(_.seq == seq).flatMap(Shown.of(_, showing.named)).map(text)
      case ItemRef.Nearby(conversation, seq) =>
        for {
          section <- window.nearby.find(s =>
            s.conversation == conversation && s.names.contains(seq)
          )
          e <- showing.near.find(e => e.conversationId == conversation && e.seq == seq)
          m <- Shown.section(only(section, seq), Vector(e), showing.named)
        } yield text(m)
      case ItemRef.Document(version) =>
        showing.documents.collectFirst {
          case (d, label) if d.version == version => text(Shown.document(d, label))
        }
      case ItemRef.Piece(id) =>
        showing.pieces.collectFirst {
          case (k, label, more) if k.id == id => text(Shown.piece(k, label, more))
        }
    }
  }

  /** `section` naming `seq` alone. */
  private def only(section: Nearby, seq: EntrySeq): Nearby = section match {
    case Nearby.Open(c, place, _) => Nearby.Open(c, place, Vector(seq))
    case Nearby.Along(c, place, _) => Nearby.Along(c, place, Vector(seq))
    case Nearby.Asked(c, place, _) => Nearby.Asked(c, place, Vector(seq))
    case Nearby.Closed(c, place, _) => Nearby.Closed(c, place, seq)
  }

  /** `showing` with each item `stubs` names, by the ref [[itemsOf]] gives it, in its kind's
    * place but holding its stub in place of what it says: a tool's result as a result that is
    * no error, grit's reply as its text, a heard message or grit's post as one, a closing as
    * its record's prose, a document as its text (dropped if the stub cannot be one), a piece as
    * its text under its heading path ([[grit.core.files.Piece.holding]]), and any
    * other entry as a person's message.
    */
  private def hiding(showing: Showing, stubs: Map[ItemRef, String]): Showing =
    if (stubs.isEmpty) showing
    else {
      def entry(ref: Entry => ItemRef)(e: Entry): Entry = {
        val at = e.payload match {
          case Payload.Result(r, _) => ItemRef.Result(r.callId)
          case _ => ref(e)
        }
        stubs.get(at).fold(e)(stub => e.copy(payload = stubbed(e.payload, stub)))
      }
      val own = entry(e => ItemRef.Own(e.seq))
      showing.copy(
        window = showing.window.map(own),
        mine = showing.mine.map(own),
        near = showing.near.map(entry(e => ItemRef.Nearby(e.conversationId, e.seq))),
        documents = showing.documents.flatMap((document, label) =>
          stubs.get(ItemRef.Document(document.version)) match {
            case None => Some((document, label))
            case Some(stub) => DocText.of(stub).toOption.map(t => (document.copy(text = t), label))
          }
        ),
        pieces = showing.pieces.map((k, label, more) =>
          stubs.get(ItemRef.Piece(k.id)) match {
            case None => (k, label, more)
            case Some(stub) => (k.copy(piece = k.piece.holding(stub)), label, more)
          }
        )
      )
    }

  /** `payload` holding `stub` in place of what it says, as [[hiding]] says. */
  private def stubbed(payload: Payload, stub: String): Payload = payload match {
    case Payload.Result(r, shown) => Payload.Result(r.copy(content = stub, isError = false), shown)
    case Payload.Message(reply: Message.Assistant) =>
      Payload.Message(reply.copy(blocks = Vector(AssistantBlock.Text(stub))))
    // Its calls are kept so their results still pair, and their arguments, which may copy what
    // that turn read, never carry a hidden item here: no exchange is ever hidden. An earlier
    // turn's is not shown, so is no item (itemsOf); this turn's own is believed at the level
    // the turn had when it was kept (ask:n:record), which only falls after, so it dominates
    // every later gate's level and is never listed.
    case Payload.Exchange(reply) =>
      Payload.Exchange(reply.copy(blocks = AssistantBlock.Text(stub) +: reply.blocks.collect {
        case call: AssistantBlock.ToolCall => call
      }))
    case Payload.Heard(_) => Payload.Heard(stub)
    case Payload.Posted(_) => Payload.Posted(stub)
    case Payload.Closed(period, reason, _) =>
      Flows
        .of(stub, None, Vector.empty)
        .fold(Payload.Message(Message.User(stub)))(f =>
          Payload.Closed(period, reason, Closing(f, Balance.empty))
        )
    case _ => Payload.Message(Message.User(stub))
  }
}
