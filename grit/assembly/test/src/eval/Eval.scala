package grit.assembly.eval

import java.time.Instant

import grit.assembly.{LinearAssembler, TokenEstimate}
import grit.core.context.AssemblyRequest
import grit.core.id.{ConversationId, EntryId, TurnRef, TurnSeq}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.store.{Db, Entry, InMemoryEntryStore, Payload, StoreError, Tx}
import grit.dbos.sql.TestTx

/** Assembly eval v0 (roadmap/mechanisms/assembly-eval.md): every case in [[Cases]], under
  * every strategy, scored on how many of its `[must]` entries the window holds and what the
  * window costs in estimated tokens. A report, not a gate: the tests pin the harness, never
  * the scores.
  *
  * {{{./mill grit.assembly.test.runMain grit.assembly.eval.Eval}}}
  */
object Eval {

  /** What builds a window. `Oracle` is exactly the labelled entries: the cheapest complete
    * window, the floor the others are measured against.
    */
  enum Strategy {
    case Linear(budget: Tokens)
    case Oracle

    def label: String = this match {
      case Linear(b) => s"linear@${Tokens.value(b)}"
      case Oracle => "oracle"
    }
  }

  val Strategies: Vector[Strategy] =
    Vector(50L, 100L, 200L, 24_000L).map(b => Strategy.Linear(Tokens(b))) :+ Strategy.Oracle

  /** One window, scored: labelled entries it holds, of how many; its estimated size; and
    * the labelled entries it missed.
    */
  final case class Score(got: Int, of: Int, tokens: Tokens, missed: Vector[EntryId])

  /** A case written into a fresh store, one entry per line, ids `t{turn}:{seq}`. */
  final case class Loaded(
      store: InMemoryEntryStore,
      turn: TurnRef,
      must: Vector[EntryId],
      messages: Map[EntryId, Message]
  )

  private val Conversation = ConversationId("eval")

  private object FakeDb extends Db {
    def read[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using TestTx.fake)
  }

  def load(c: Case): Loaded = {
    val turns = c.turns :+ Vector(Case.Line(you = true, c.ask, must = false))
    val lines = turns.zipWithIndex
      .flatMap((turn: Vector[Case.Line], t: Int) => turn.map(line => (t, line)))
      .zipWithIndex
      .map { case ((t, line), seq) => (EntryId(s"t$t:$seq"), t, seq, line) }
    val store = new InMemoryEntryStore
    given Tx = TestTx.fake
    lines.foreach { (id, t, seq, line) =>
      val _ = store.insert(
        Entry(
          id,
          Conversation,
          TurnSeq(t.toLong),
          None,
          seq.toLong,
          Payload.Message(message(line)),
          Instant.EPOCH
        )
      )
    }
    Loaded(
      store,
      TurnRef(Conversation, TurnSeq(c.turns.size.toLong)),
      lines.collect { case (id, _, _, line) if line.must => id },
      lines.map((id, _, _, line) => id -> message(line)).toMap
    )
  }

  private def message(line: Case.Line): Message =
    if (line.you) Message.User(line.text)
    else
      Message.Assistant(
        Vector(AssistantBlock.Text(line.text)),
        StopReason.EndTurn,
        Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, None),
        "eval"
      )

  /** The window `strategy` builds for `loaded`, scored against its labels. */
  def run(loaded: Loaded, strategy: Strategy): Either[String, Score] = {
    val window = strategy match {
      case Strategy.Linear(budget) =>
        new LinearAssembler(loaded.store, budget)
          .assemble(AssemblyRequest(loaded.turn))(using FakeDb)
          .map(_.entries)
          .left
          .map(e => s"${strategy.label}: $e")
      case Strategy.Oracle => Right(loaded.must)
    }
    window.map(score(loaded, _))
  }

  def score(loaded: Loaded, window: Vector[EntryId]): Score = {
    val held = window.toSet
    Score(
      loaded.must.count(held),
      loaded.must.size,
      window.flatMap(loaded.messages.get).map(TokenEstimate.of).foldLeft(Tokens.Zero)(_ + _),
      loaded.must.filterNot(held)
    )
  }

  def main(args: Array[String]): Unit = {
    val _ = args
    Cases.all.map((name, text) => Case.parse(name, text)).foreach {
      case Left(e) => println(s"unreadable case: $e")
      case Right(c) => report(c)
    }
  }

  private def report(c: Case): Unit = {
    val loaded = load(c)
    println(s"${c.name}: ${c.about}")
    Strategies.foreach { s =>
      run(loaded, s) match {
        case Left(e) => println(f"  ${s.label}%-14s failed: $e")
        case Right(r) =>
          val missed =
            if (r.missed.isEmpty) ""
            else
              r.missed
                .map(id => s"${EntryId.value(id)} ${gist(loaded, id)}")
                .mkString("  missed: ", "; ", "")
          println(
            f"  ${s.label}%-14s ${r.got}/${r.of} must  ${Tokens.value(r.tokens)}%5d tokens$missed"
          )
      }
    }
    println()
  }

  private def gist(loaded: Loaded, id: EntryId): String =
    loaded.messages.get(id).fold("") {
      case Message.User(text) => s"\"${text.take(40)}\""
      case Message.Assistant(blocks, _, _, _) =>
        blocks.collect { case AssistantBlock.Text(t) => s"\"${t.take(40)}…\"" }.mkString
      case Message.ToolResult(_, content, _) => s"\"${content.take(40)}\""
    }
}
