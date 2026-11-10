package grit.turn

import java.time.Instant

import grit.core.clock.Utc
import grit.core.context.{CarriedWindow, Shown, Window}
import grit.core.document.{DocLabel, DocText, Document, DocumentSearch}
import grit.core.files.{KeptPiece, PieceSearch}
import grit.core.id.{EntryId, EntrySeq, Sha256, ToolCallId, TurnRef, TurnSeq, WorkflowId}
import grit.core.message.{AssistantBlock, Message}
import grit.core.model.{AfterToolResult, ToolGuidance}
import grit.core.period.{Balance, Closing, Flows}
import grit.core.prompt.Fenced
import grit.core.provider.{ModelRequest, Repeats, ToolSchema}
import grit.core.store.{
  ConversationStore,
  Entry,
  EntryStore,
  Nearby,
  Payload,
  PayloadJson,
  Principals,
  Speakers,
  StoreError,
  Tx
}
import grit.core.tool.{Tool, ToolName, ToolSet, Toolbox}
import grit.core.visibility.Subject
import grit.prose.either.Traversal.traverse

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

  /** The tools every call of every turn offers: `own`, after `topic` ([[TurnVerdict.tool]]),
    * whose schema is the same whatever `asked` holds: `asked` decides only what a call is
    * checked against. So a turn's tools are its tool set's alone, and a later turn can carry
    * its window (ADR 0052); a turn that asks also tags its message ([[loopShape]]). read_hidden
    * and extract are not among them: a round binds and offers them apart ([[gatedOffer]]).
    * `TurnFailure.Model` when `own` already names `topic`, read_hidden or extract, which a
    * deployment's own tools are refused for at its start ([[TurnTooling.reserving]]); an edge's advert can
    * still name one.
    */
  def offered[C^](
      own: Toolbox[C],
      asked: Option[TurnAgenda.Classification]
  ): Either[TurnFailure, Toolbox[C]] = {
    val tools = own.including(TurnVerdict.tool(asked))
    tools
      .flatMap(t => t.preceded(Vector(unread, unasked)).map(_ => t))
      .left
      .map(refused => TurnFailure.Model(refused.message))
  }

  /** What every request of every turn offered `offered` offers of `schemas`, whatever its gates
    * decide: when [[ToolSet.mayGate]], read_hidden and extract first ([[ReadHidden]],
    * [[Extract]], `strict` as `schemas` are); then `schemas`. A tool a round's gate withholds,
    * the first's included, stays offered, and a call of it is refused
    * ([[grit.core.tool.CallError.Withheld]]), as that round's note says ([[gateNotes]]).
    */
  def gatedOffer(
      offered: ToolSet,
      schemas: Vector[ToolSchema],
      strict: Boolean
  ): Vector[ToolSchema] =
    Vector(unread.schema(strict), unasked.schema(strict)).filter(_ => offered.mayGate) ++ schemas

  /** The note each round up to `round` adds, oldest first, as `gates`, each round's gate by
    * its index, made them: [[TurnGate.Guidance]] on the first round whose gate hid an item,
    * then the withdrawal the round's gate makes against the round before's
    * ([[TurnGate.withdrawn]]; the first round's against one withholding nothing), its tools as
    * `offered` says them; each note as [[Fenced.noted]] fences it. A round adding neither has
    * none.
    */
  def gateNotes(gates: Map[Int, Gated], round: Round, offered: ToolSet): Vector[(Round, String)] = {
    val firstHid = gates.collect { case (index, g) if g.hidden.nonEmpty => index }.minOption
    (Round.First.index to round.index).toVector.flatMap { index =>
      gates.get(index).flatMap { now =>
        val before =
          if (index == Round.First.index) Some(now.copy(withheld = Vector.empty))
          else gates.get(index - 1)
        val lines = Vector(TurnGate.Guidance).filter(_ => firstHid.contains(index)) ++
          before.flatMap(TurnGate.withdrawn(_, now, offered))
        Option.when(lines.nonEmpty)(Round.at(index) -> Fenced.noted(lines))
      }
    }
  }

  /** extract as a request offers it, asking nothing: for its name and schema alone. */
  private val unasked: Tool.Offered = Extract.tool(None, Extract.Unasked)

  /** read_hidden as a request offers it, reading nothing: for its name and schema alone. */
  private val unread: Tool.Offered = ReadHidden.tool(None, ReadHidden.Unread)

  /** How each call of the loop is built from the plain request: tagged for `asked`
    * ([[TurnVerdict.tagged]]); then each of `notes` as a user message, as written: a round's
    * right before the turn's reply of that round (the round's index counting the replies after
    * the asker's message, [[TurnVerdict.asker]], or from the first message when there is
    * none), one whose reply is not there at the end; offering `tools`, guided as
    * [[TurnLoop.guided]] says under `guidance`, and told as [[TurnLoop.told]] says under
    * `after`. Fed by [[gatedOffer]], [[gateNotes]] and [[TurnLoop.notes]], each call's request
    * repeats every message of the call before it, with the same system prompt, tools and tool
    * use, so it marks every message stable ([[Repeats.stable]]); and it keeps the plain
    * request's task and established messages so named.
    */
  def loopShape(
      asked: Option[TurnAgenda.Classification],
      tools: Vector[ToolSchema],
      after: AfterToolResult,
      guidance: ToolGuidance,
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
      val shaped = TurnLoop.told(TurnLoop.guided(noted.copy(tools = tools), guidance), after)
      // The plain request's task and established counts, each past the notes put before it.
      def past(count: Int): Int = count + before.collect { case (i, n) if i < count => n.size }.sum
      shaped.copy(repeats =
        Repeats.of(
          past(base.repeats.task),
          past(base.repeats.established),
          shaped.messages.size
        )
      )
    }

  /** The stubs `round`'s request shows, by the ref of the item each stands for, as its gate
    * in `gates` hid them, their tools as `offered` says them ([[TurnGate.stub]]); none for a
    * round with no gate. A round's gate keeps every item an earlier one hid as it hid it, and
    * hides none an earlier one showed, whose integrity dominates every later level
    * ([[TurnGate.decide]]): so each round's stubs hold every earlier round's, unchanged.
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

  /** What a later call that repeats `request`'s system prompt, tools and messages, byte for
    * byte, is kept as ([[Payload.Prefix]]). Equal prefixes mean those parts are equal as a
    * gateway is given them.
    */
  def prefixOf(request: ModelRequest): Payload.Prefix = {
    val written = ujson.Obj(
      "system" -> request.system,
      "tools" -> ujson.Arr.from(request.tools.map { t =>
        ujson.Obj(
          "name" -> t.name,
          "description" -> t.description,
          "parameters" -> t.parameters,
          "strict" -> t.strict
        )
      }),
      "messages" -> ujson.Arr.from(request.messages.map(PayloadJson.writeMessage))
    )
    Payload.Prefix(request.messages.size, Sha256.text(written.render()))
  }

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
      seen: Seen,
      stubs: Map[ItemRef, String],
      zoned: Option[TurnOffer.Zoned]
  )(using pins: Turn.Bounded): Either[TurnFailure, Unshaped] =
    pins
      .reads(env.db)
      .read(Subject.Turn(turn))(
        shown(env.records, turn, seen).flatMap(s =>
          direct(env.hosting.conversations, turn).map(s -> _)
        )
      )
      .left
      .map(TurnFailure.ofStore)
      .flatMap((showing, direct) => requestOf(system, showing, turn, stubs, zoned, direct))

  /** The window a turn's request shows before its own entries: `window`, drawn for the turn,
    * appended after the window record `after` and the records it follows
    * ([[CarriedWindow.chain]]); or, with no `after`, the whole window.
    */
  final case class Seen(after: Option[EntryId], window: Window)

  object Seen {

    /** What the window `record` shows before its turn's own entries. */
    def of(record: Payload.Window): Seen = Seen(record.carry.flatMap(_.after), Window.of(record))
  }

  /** What a request for `turn` over `seen` can show, read from `records`: each part of its
    * window, oldest first ([[Part]]), the turn's own entries, and who said each; nothing else
    * of the conversation. `Invalid` when `seen.after` is not read back as a chain of window
    * records.
    */
  def shown(records: TurnRecords, turn: TurnRef, seen: Seen)(using
      Tx^
  ): Either[StoreError, Showing] =
    shown(records.entries, records.principals, records.documents, records.pieces, turn, seen)

  /** [[shown]], read from the four stores it reads. */
  def shown(
      entries: EntryStore,
      principals: Principals,
      documents: DocumentSearch,
      pieces: PieceSearch,
      turn: TurnRef,
      seen: Seen
  )(using Tx^): Either[StoreError, Showing] = {
    def part(window: Window, record: Option[CarriedWindow.Record]): Either[StoreError, Part] =
      for {
        at <- entries.at(turn.conversationId, window.task.toVector ++ window.entries)
        near <- Nearby.read(window.nearby, entries)
        labelled <- documents.labelled(window.documents)
        brief <- documents.labelled(window.brief.toVector)
        kept <-
          if (window.pieces.isEmpty) Right(Vector.empty) else pieces.read(window.pieces.map(_.id))
        shown <- documents.labelOf(kept)(_.plugin)
        own <- record.fold[Either[StoreError, Vector[Entry]]](Right(Vector.empty))(r =>
          entries.ofTurn(TurnRef(turn.conversationId, r.turn))
        )
      } yield {
        val more = window.pieces.map(p => p.id -> p.more).toMap
        val parts = shown.map((k, l) => (k, l, more.getOrElse(k.id, 0)))
        Part(
          window,
          at,
          near,
          labelled,
          parts,
          record.map(r => Carried(r.turn, own, r.carry)),
          brief.headOption
        )
      }
    for {
      chain <- seen.after.fold[Either[StoreError, Vector[CarriedWindow.Record]]](
        Right(Vector.empty)
      )(after =>
        CarriedWindow
          .chain(after)(entries.get)
          .filterOrElse(
            _.nonEmpty,
            StoreError.Invalid(s"the carried window record ${EntryId.value(after)} is not read")
          )
      )
      carried <- traverse(chain)(r => part(r.window, Some(r)))
      own <- part(seen.window, None)
      mine <- entries.ofTurn(turn)
      parts = carried :+ own
      named <- principals.speakers(
        (parts.flatMap(p => p.entries ++ p.near ++ p.carried.fold(Vector.empty)(_.own)) ++ mine)
          .map(_.id)
          .distinct
      )
    } yield Showing(parts, mine, named)
  }

  /** A carried turn as [[shown]] read it: `turn`, its own entries `own`, and the lines its
    * window record keeps (`carry`).
    */
  final case class Carried(turn: TurnSeq, own: Vector[Entry], carry: Payload.Carry)

  /** One part of a window as [[shown]] read it: `window`, what was drawn; `entries`, the
    * entries at its seqs and its task's that exist; `near`, its nearby entries that still exist; `documents`,
    * its documents still kept whose plugin is enabled, in its order, each with its plugin's
    * label; `pieces`, its pieces likewise, each with its plugin's label and how many more of
    * its file matched; for a part carried from an earlier turn, that turn (`carried`); and its
    * brief, when still kept and its plugin enabled, with its plugin's label (`brief`).
    */
  final case class Part(
      window: Window,
      entries: Vector[Entry],
      near: Vector[Entry],
      documents: Vector[(Document, DocLabel)],
      pieces: Vector[(KeptPiece, DocLabel, Int)],
      carried: Option[Carried],
      brief: Option[(Document, DocLabel)] = None
  )

  /** What [[shown]] read: `parts`, the window's, oldest first, the turn's own last; `mine`,
    * the turn's own entries; `named`, the speakers of all of them.
    */
  final case class Showing(parts: Vector[Part], mine: Vector[Entry], named: Speakers) {

    /** Every part's entries, each carried part's followed by its turn's own. */
    def window: Vector[Entry] = parts.flatMap(p => p.entries ++ p.carried.fold(Vector.empty)(_.own))

    /** Every part's nearby sections, in order. */
    def nearby: Vector[Nearby] = parts.flatMap(_.window.nearby)

    /** Every part's nearby entries. */
    def near: Vector[Entry] = parts.flatMap(_.near)

    /** Every part's documents, in order. */
    def documents: Vector[(Document, DocLabel)] = parts.flatMap(_.documents)

    /** Every part's pieces, in order. */
    def pieces: Vector[(KeptPiece, DocLabel, Int)] = parts.flatMap(_.pieces)
  }

  /** The request [[request]] builds from `showing`, each item `stubs` names shown as its stub
    * there ([[hiding]]), to be shaped ([[Unshaped]]): each part in order, then the turn's own.
    * A part shows its brief as one user message ([[Shown.brief]]), its task as [[Shown.of]]
    * shows it, each nearby section as one
    * ([[Shown.section]], a section with none of its entries left dropped), then each document
    * as one ([[Shown.document]]), then each piece as one ([[Shown.piece]]), then what the
    * model is shown of its entries ([[Shown.own]]: its messages, a closing entry as one user
    * message, and a gap line wherever turns are left out, the turn before them the part
    * before's), then its digest as one ([[Shown.sessionLog]]), which changes at every boundary
    * and so follows what earlier turns established; then, for a carried part,
    * its record's dated line as one user message and its turn's own entries as
    * [[Shown.turn]] shows them, its record's note added to its asking message
    * ([[TurnVerdict.taggedWith]]). The turn's own follow: when it has an own entry,
    * [[answering]] of its first, in `zoned` (its offer's, [[TurnOffer.Recorded]]), in a direct
    * message when `direct` ([[direct]]), as one user message, then its own, its tool loop's
    * exchange among them in order. A summary is not shown to the model yet. `system` is sent as
    * it is given. A part's seq with no entry is an `Assembly` failure naming every such seq.
    */
  def requestOf(
      system: String,
      showing: Showing,
      turn: TurnRef,
      stubs: Map[ItemRef, String],
      zoned: Option[TurnOffer.Zoned],
      direct: Boolean
  ): Either[TurnFailure, Unshaped] = {
    val Showing(parts, mine, named) = hiding(showing, stubs)
    val befores = None +: parts.map(_.carried.map(_.turn))
    traverse(parts.zip(befores)) { (part, before) =>
      val bySeq = part.entries.map(e => e.seq -> e).toMap
      (part.window.task.toVector ++ part.window.entries).filterNot(bySeq.contains) match {
        case missing if missing.nonEmpty =>
          Left(
            TurnFailure.Assembly(
              s"window names unknown entries at seqs: ${missing.map(EntrySeq.value).mkString(", ")}"
            )
          )
        case _ =>
          val sections = part.window.nearby.flatMap(Shown.section(_, part.near, named))
          val pieces = part.pieces.map((k, label, more) => Shown.piece(k, label, more))
          val of = part.carried.fold(turn.turnSeq)(_.turn)
          val shown = Shown.own(part.window.entries.flatMap(bySeq.get), before, of, named)
          // A carried turn repeats the lines its record keeps, as its own request sent them.
          val carried = part.carried.fold(Vector.empty[Message])(c =>
            TurnVerdict.taggedWith(
              c.carry.dated.map(Message.User(_)).toVector ++ Shown.turn(c.own, named),
              c.carry.tag
            )
          )
          val brief = part.brief.map((d, _) => Shown.brief(d)).toVector
          val task = part.window.task.flatMap(bySeq.get).flatMap(Shown.of(_, named)).toVector
          val digest = part.window.sessionLog.map(Shown.sessionLog)
          Right(
            Unshaped.Part(
              brief ++ task ++ sections,
              part.documents,
              pieces ++ shown,
              digest,
              carried
            )
          )
      }
    }.map { shownParts =>
      // The dated line is a message, not part of `system`: a provider caches its prompt from
      // the front, and nothing that changes per turn may sit before the messages. It is made
      // here, from the turn's first own entry, and kept in the turn's window record for a
      // later turn of its window to repeat.
      val dated = mine
        .minByOption(e => EntrySeq.value(e.seq))
        .map(root => Message.User(answering(root.createdAt, zoned, direct)))
      Unshaped(system, shownParts, dated.toVector ++ Shown.turn(mine, named))
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

  /** The request the call whose reply is kept as `reply` was sent, rebuilt from `showing`:
    * [[requestOf]] over the turn's own entries kept before that reply alone, each item `stubs`
    * names shown as its stub, shaped by `shape`; with the reply as kept: a round's that called
    * tools, the turn's reply or its draft. `Store` naming the reply when `showing` keeps none
    * of those under it.
    */
  def sentBefore(
      system: String,
      showing: Showing,
      turn: TurnRef,
      reply: EntryId,
      stubs: Map[ItemRef, String],
      zoned: Option[TurnOffer.Zoned],
      direct: Boolean,
      shape: ModelRequest -> ModelRequest
  ): Either[TurnFailure, (ModelRequest, Message.Assistant)] =
    // A call's reply is kept after every own entry the call was shown (recordCall and append
    // read `shown` after lockNext, which serialises the conversation, and before inserting
    // the reply), so the own entries it saw are exactly those below the reply's seq. Own
    // entries `Shown.turn` does not show (a window, a query, an agenda event) change nothing.
    showing.mine
      .find(_.id == reply)
      .flatMap(e =>
        (e.payload match {
          case Payload.Message(m: Message.Assistant) => Some(m)
          case Payload.Exchange(m) => Some(m)
          case Payload.Draft(m) => Some(m)
          case _ => None
        }).map(e -> _)
      )
      .toRight(TurnFailure.Store(s"the reply ${EntryId.value(reply)} is not kept"))
      .flatMap { (at, message) =>
        val before = showing.mine.filter(e => EntrySeq.value(e.seq) < EntrySeq.value(at.seq))
        requestOf(system, showing.copy(mine = before), turn, stubs, zoned, direct)
          .map(r => (r.shaped(shape), message))
      }

  /** A request of `system`: each of `parts` in order, then `own`, the turn's own messages. */
  final case class Unshaped(system: String, parts: Vector[Unshaped.Part], own: Vector[Message]) {

    /** This request shaped by `shape`, each document's pointer line shown only when the shaped
      * request offers the tool it names: a model offered no tool of that name could not follow
      * it. The tools offered are those `shape` gives the request with no pointer line shown.
      * Before `shape`, its [[ModelRequest.repeats]] name the first part's `before` and
      * documents as the task, every part's messages before the first `digest` as established,
      * and every part's messages as stable.
      */
    def shaped(shape: ModelRequest -> ModelRequest): ModelRequest = {
      val task = parts.headOption.fold(0)(p => p.before.size + p.documents.size)
      def drawn(p: Unshaped.Part) = p.before.size + p.documents.size + p.after.size
      def whole(p: Unshaped.Part) = drawn(p) + p.digest.size + p.carried.size
      val stable = parts.map(whole).sum
      val established = parts
        .foldLeft((0, false)) { case ((n, cut), p) =>
          if (cut) (n, cut)
          else if (p.digest.isDefined) (n + drawn(p), true)
          else (n + whole(p), false)
        }
        ._1
      def built(offered: Set[String]): ModelRequest =
        ModelRequest(
          system,
          parts.flatMap(part =>
            part.before ++ part.documents.map((document, label) =>
              Shown.document(
                document.copy(pointer =
                  document.pointer.filter(p => offered.contains(ToolName.value(p.tool)))
                ),
                label
              )
            ) ++ part.after ++ part.digest ++ part.carried
          ) ++ own,
          repeats = Repeats.of(task, established, stable)
        )
      shape(built(shape(built(Set.empty)).tools.map(_.name).toSet))
    }
  }

  object Unshaped {

    /** Part of a request: `before`, then each of `documents` as one user message, then
      * `after`, what it drew; then its session's `digest`, which each boundary draws afresh;
      * then `carried`, the lines a carried part's turn repeats.
      */
    final case class Part(
        before: Vector[Message],
        documents: Vector[(Document, DocLabel)],
        after: Vector[Message],
        digest: Option[Message] = None,
        carried: Vector[Message] = Vector.empty
    )
  }

  /** Each item a request shows the model of `showing`, in the order it shows them: part by
    * part, its brief, its task (an own entry), the entries its nearby sections show, section by section ([[Shown.section]]), its
    * documents, its pieces, its entries [[Shown.own]] shows, and a carried part's turn's own
    * entries [[Shown.turn]] shows; then the turn's own entries [[Shown.turn]] shows; an entry
    * kept as a tool's result by the call it answers, named by its tool, and any other by its
    * position, a person's named as [[Showing.named]] names them ("someone" when it does not),
    * grit's as "an earlier reply" and a closing as "a closing record". An entry the request
    * does not show (an earlier turn's tool exchange or result outside a carried turn, a window,
    * a query, a summary, an item, an ask, a message with no text nearby) is none.
    */
  def itemsOf(showing: Showing): Vector[Item] = {
    val Showing(parts, mine, named) = showing
    val tools: Map[ToolCallId, String] = (showing.window ++ mine)
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
    def turned(own: Vector[Entry]): Vector[Item] =
      own
        .filter(e => Shown.turn(Vector(e), named).nonEmpty)
        .flatMap(e => entry(e, ItemRef.Own(e.seq)))
    parts.flatMap { part =>
      // Each entry as its section shows it alone: shown there exactly when it is shown alone.
      val nearby = part.window.nearby.flatMap { section =>
        section.names.flatMap { seq =>
          part.near
            .find(e => e.conversationId == section.conversation && e.seq == seq)
            .filter(e => Shown.section(only(section, seq), Vector(e), named).isDefined)
            .flatMap(e => entry(e, ItemRef.Nearby(e.conversationId, e.seq)))
        }
      }
      val (task, drawn) = part.entries.partition(e => part.window.task.contains(e.seq))
      part.brief.toVector.map((d, label) =>
        Item(ItemRef.Document(d.version), d.integrity, DocLabel.value(label))
      ) ++
        task
          .filter(e => Shown.of(e, named).isDefined)
          .flatMap(e => entry(e, ItemRef.Own(e.seq))) ++
        nearby ++
        part.documents.map((d, label) =>
          Item(ItemRef.Document(d.version), d.integrity, DocLabel.value(label))
        ) ++
        part.pieces.map((k, label, _) =>
          Item(ItemRef.Piece(k.id), k.integrity, DocLabel.value(label))
        ) ++
        drawn
          .filter(e => Shown.of(e, named).isDefined)
          .flatMap(e => entry(e, ItemRef.Own(e.seq))) ++
        part.carried.fold(Vector.empty)(c => turned(c.own))
    } ++ turned(mine)
  }

  /** What a request would show of the item at `ref` among `showing` in full, as its one
    * message's text: a tool's result as [[Shown.result]] shows it, an entry as [[Shown.of]]
    * does, a nearby one as its section alone shows it ([[Shown.section]]), a document as
    * [[Shown.document]] does; `None` when `showing` no longer holds it.
    */
  def textOf(showing: Showing, ref: ItemRef): Option[String] = {
    def text(m: Message): String = m match {
      case Message.User(t) => t
      case Message.Assistant(blocks, _, _, _, _) =>
        AssistantBlock.text(blocks)
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
          section <- showing.nearby.find(s =>
            s.conversation == conversation && s.names.contains(seq)
          )
          e <- showing.near.find(e => e.conversationId == conversation && e.seq == seq)
          m <- Shown.section(only(section, seq), Vector(e), showing.named)
        } yield text(m)
      case ItemRef.Document(version) =>
        showing.parts
          .flatMap(_.brief)
          .collectFirst { case (d, _) if d.version == version => text(Shown.brief(d)) }
          .orElse(showing.documents.collectFirst {
            case (d, label) if d.version == version => text(Shown.document(d, label))
          })
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
      def document(d: Document, label: DocLabel): Option[(Document, DocLabel)] =
        stubs.get(ItemRef.Document(d.version)) match {
          case None => Some((d, label))
          case Some(stub) => DocText.of(stub).toOption.map(t => (d.copy(text = t), label))
        }
      showing.copy(
        parts = showing.parts.map(part =>
          part.copy(
            entries = part.entries.map(own),
            near = part.near.map(entry(e => ItemRef.Nearby(e.conversationId, e.seq))),
            documents = part.documents.flatMap(document(_, _)),
            brief = part.brief.flatMap(document(_, _)),
            pieces = part.pieces.map((k, label, more) =>
              stubs.get(ItemRef.Piece(k.id)) match {
                case None => (k, label, more)
                case Some(stub) => (k.copy(piece = k.piece.holding(stub)), label, more)
              }
            ),
            carried = part.carried.map(c => c.copy(own = c.own.map(own)))
          )
        ),
        mine = showing.mine.map(own)
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
