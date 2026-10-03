package grit.eval

import java.time.Instant

import grit.core.id.{
  CloseRef,
  ConversationId,
  EntryId,
  EntrySeq,
  PeriodRef,
  PeriodSeq,
  PrincipalId,
  TurnRef,
  TurnSeq
}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.period.{Balance, CloseReason, Closing, Edit, Flows, Ground}
import grit.core.place.{Directory, Namespace, Place, Prefix, Scope}
import grit.core.store.{ConversationStore, Entry, EntryStore, Jot, Origin, Payload, PeriodStore}

/** A case as written, or with [[Variant.FillerPerGap]] filler turns after each of its turns. */
enum Variant {
  case Plain, Buried

  def label: String = this match {
    case Plain => "plain"
    case Buried => "buried"
  }
}

object Variant {

  /** The filler turns after each of a buried case's own turns; a quarter as many after each
    * of its other conversations' turns.
    */
  val FillerPerGap = 40
}

/** A case laid out as the eval writes it into a database, one entry per line, before the
  * database names its conversations: its own conversation, found by `own`, its turns and then
  * the ask (`rows`, the ask's turn `ask`); its other conversations, as written; and the scope a
  * window for it is drawn within, everywhere under the case's root unless the case writes one.
  * Ids are `{case}/{variant}/t{turn}:{seq}`, and `{case}/{variant}/p{i}/t{turn}:{seq}` in its
  * `i`-th other conversation; every place is under the case's root (`fs:/eval/{case}/{variant}`,
  * `task:eval/{case}/{variant}`), so two cases share no conversation.
  */
final case class Layout(
    c: Case,
    variant: Variant,
    own: Origin,
    rows: Vector[Layout.Row],
    ask: TurnSeq,
    others: Vector[Layout.Other],
    scope: Scope
) {

  /** Every row: its own conversation's, then each other's, as written. */
  def entries: Vector[Layout.Row] = rows ++ others.flatMap(o => o.first ++ o.second)

  /** The entries a window for the case must hold (its `[must]` lines), by the origin of their
    * conversation, each conversation's in seq order: its own first, then each other's as
    * written; none when the case labels none.
    */
  def must: Vector[(Origin, ::[EntrySeq])] =
    Layout.held(own, rows).toVector ++ others.flatMap(o =>
      Layout.held(o.origin, o.first ++ o.second)
    )
}

object Layout {

  /** One line of a case: its entry's id, turn and seq, and the line. */
  final case class Row(id: EntryId, turn: TurnSeq, seq: EntrySeq, line: Case.Line)

  /** Another conversation of a case, found by `origin`: its first period's rows, open unless
    * `closed` closes it after them, then its second period's rows, open.
    */
  final case class Other(
      origin: Origin,
      first: Vector[Row],
      closed: Option[Close],
      second: Vector[Row]
  )

  /** A first period closed (lapsed) after its turn `after`, its closing's flows saying `said`
    * and its balance standing on `carried`.
    */
  final case class Close(after: TurnSeq, said: String, carried: Vector[String])

  /** `c` laid out as `variant` writes it, its buried filler seeded by its name. `Left` when a
    * place it writes names no namespace, or one other than `fs:` or `task:`, or it closes a
    * place before any turn there.
    */
  def of(c: Case, variant: Variant): Either[String, Layout] = {
    val prefix = s"${c.name}/${variant.label}"
    val root = Vector("eval", c.name, variant.label)
    def buried(turns: Vector[Vector[Case.Line]], seed: Long, per: Int) = variant match {
      case Variant.Plain => turns
      case Variant.Buried =>
        turns.zipWithIndex.flatMap((turn: Vector[Case.Line], t: Int) =>
          turn +: Filler.turns(seed + t, per)
        )
    }
    val body = buried(c.turns, c.name.hashCode.toLong * 31, Variant.FillerPerGap)
    val turns = body :+ Vector(Case.Line(you = true, c.ask, must = false))
    val rows = laid(turns, 0, 0, t => s"$prefix/t$t")
    val others = c.elsewhere.zipWithIndex.map { (e: Case.Elsewhere, i: Int) =>
      val first = laid(
        buried(e.turns, c.name.hashCode.toLong * 17 + i, Variant.FillerPerGap / 4),
        0,
        0,
        t => s"$prefix/p$i/t$t"
      )
      val firstTurns = first.lastOption.fold(0L)(r => TurnSeq.value(r.turn) + 1)
      // The first period's closing takes the seq after its last row.
      val second = laid(
        buried(e.reopened, c.name.hashCode.toLong * 19 + i, Variant.FillerPerGap / 4),
        firstTurns,
        first.size.toLong + 1,
        t => s"$prefix/p$i/t$t"
      )
      for {
        origin <- rooted(root, e.place).flatMap(originAt)
        closed <-
          if (!e.closed) Right(None)
          else
            first.lastOption
              .toRight(s"${c.name}: ${e.place} is closed before any turn there")
              .map(last => Some(Close(last.turn, s"${e.place} closed.", e.carried)))
      } yield Other(origin, first, closed, second)
    }
    for {
      others <- others.foldLeft[Either[String, Vector[Other]]](Right(Vector.empty))((acc, o) =>
        acc.flatMap(done => o.map(done :+ _))
      )
      within <- c.scope.fold(
        for {
          fs <- rooted(root, "fs:/")
          task <- rooted(root, "task:")
        } yield Scope(Vector(Prefix.At(fs), Prefix.At(task)))
      )(written =>
        written
          .split("\\s+")
          .toVector
          .foldLeft[Either[String, Vector[Prefix]]](Right(Vector.empty))((acc, w) =>
            acc.flatMap(done => rooted(root, w).map(p => done :+ Prefix.At(p)))
          )
          .map(Scope(_))
      )
    } yield Layout(
      c,
      variant,
      Origin.Task("eval", prefix),
      rows,
      TurnSeq((turns.size - 1).toLong),
      others,
      within
    )
  }

  /** `line` as the message its entry holds: a user's, or a reply that cost nothing. */
  def message(line: Case.Line): Message =
    if (line.you) Message.User(line.text)
    else
      Message.Assistant(
        Vector(AssistantBlock.Text(line.text)),
        StopReason.EndTurn,
        Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, None),
        "eval"
      )

  /** `turns`' lines as rows, turns numbered from `fromTurn` and seqs from `fromSeq`, each id
    * `{named(turn)}:{seq}`.
    */
  private def laid(
      turns: Vector[Vector[Case.Line]],
      fromTurn: Long,
      fromSeq: Long,
      named: Long => String
  ): Vector[Row] =
    turns.zipWithIndex
      .flatMap((turn: Vector[Case.Line], t: Int) => turn.map(line => (fromTurn + t, line)))
      .zipWithIndex
      .map { case ((t, line), k) =>
        val seq = fromSeq + k
        Row(EntryId(s"${named(t)}:$seq"), TurnSeq(t), EntrySeq(seq), line)
      }

  /** `written` (a place as a case writes it) under the case's `root`: `fs:/a` as
    * `fs:/eval/{case}/{variant}/a`, `task:a` as `task:eval/{case}/{variant}/a`.
    */
  private def rooted(root: Vector[String], written: String): Either[String, Place] =
    Place.read(written).flatMap { p =>
      p.segments.headOption
        .flatMap(Namespace.of)
        .toRight(s"a case's place must name a namespace: $written")
        .map(ns => Place.under(ns, root ++ p.segments.drop(1)))
    }

  /** The origin whose place is `place`: a TUI session for `fs`, a task's run for `task`. */
  private def originAt(place: Place): Either[String, Origin] =
    place.segments.headOption.flatMap(Namespace.of) match {
      case Some(Namespace.Fs) =>
        Directory.of("/" + place.segments.drop(1).mkString("/")).map(Origin.Tui(_, "eval"))
      case Some(Namespace.Task) =>
        Right(
          Origin.Task(
            place.segments.drop(1).headOption.getOrElse("eval"),
            place.segments.drop(2).mkString("/")
          )
        )
      case _ => Left(s"a case's place is fs: or task:, not ${place.written}")
    }

  /** The seqs of `rows`' `[must]` lines, with `origin`; none when no line is. */
  private def held(origin: Origin, rows: Vector[Row]): Option[(Origin, ::[EntrySeq])] =
    rows.filter(_.line.must).map(_.seq).toList match {
      case first :: rest => Some(origin -> ::(first, rest))
      case Nil => None
    }
}

/** Cases written into a database. */
object Load {

  /** `layout` written through `jot`, one transaction for the case's own rows and one for each
    * period of each other conversation: each conversation found or created by its origin, begun
    * by the local principal; each row its conversation's entry at its turn and seq, its message
    * [[Layout.message]], written at the epoch; each other conversation's first period opened at
    * its first row and, when laid out closed, sealed (lapsed), then its second opened at its
    * first row. The turn the case asks in, or the store's error; what was written before an
    * error stays. A case's ids are unique in a database, so a case loaded twice fails.
    */
  def into(
      jot: Jot^,
      conversations: ConversationStore,
      entries: EntryStore,
      periods: PeriodStore
  )(layout: Layout): Either[String, TurnRef] = {
    def found(origin: Origin): Either[String, ConversationId] =
      jot
        .write(conversations.findOrCreate(origin, PrincipalId.Local))
        .map(_.id)
        .left
        .map(e => s"${layout.c.name}: $e")
    def insert(conversation: ConversationId, rows: Vector[Layout.Row], open: Boolean) =
      if (rows.isEmpty) Right(())
      else
        jot
          .write(
            for {
              _ <- rows.headOption
                .filter(_ => open)
                .fold[Either[grit.core.store.StoreError, Unit]](Right(()))(r =>
                  periods.openFor(conversation, r.turn, Instant.EPOCH).map(_ => ())
                )
              _ <- rows.foldLeft[Either[grit.core.store.StoreError, Unit]](Right(()))((acc, r) =>
                acc.flatMap(_ =>
                  entries.insert(
                    Entry(
                      r.id,
                      conversation,
                      r.turn,
                      None,
                      r.seq,
                      Payload.Message(Layout.message(r.line)),
                      Instant.EPOCH
                    )
                  )
                )
              )
            } yield ()
          )
          .left
          .map(e => s"${layout.c.name}: $e")
    def close(conversation: ConversationId, c: Layout.Close) =
      for {
        flows <- Flows.of(c.said, None, Vector.empty).toRight(s"${layout.c.name}: no flows")
        carried = Balance.empty
          .edit(c.carried.map(Edit.Stand(_, Ground.Person)), PeriodSeq.First)
          .balance
        _ <- jot
          .write(
            periods.seal(
              CloseRef(PeriodRef(conversation, PeriodSeq.First), c.after, Instant.EPOCH),
              CloseReason.Lapsed,
              Closing(flows, carried),
              Instant.EPOCH
            )
          )
          .left
          .map(e => s"${layout.c.name}: $e")
      } yield ()
    for {
      own <- found(layout.own)
      _ <- insert(own, layout.rows, open = false)
      _ <- layout.others.foldLeft[Either[String, Unit]](Right(())) { (acc, o) =>
        for {
          _ <- acc
          conversation <- found(o.origin)
          _ <- insert(conversation, o.first, open = true)
          _ <- o.closed.fold[Either[String, Unit]](Right(()))(close(conversation, _))
          _ <- insert(conversation, o.second, open = true)
        } yield ()
      }
    } yield TurnRef(own, layout.ask)
  }
}
