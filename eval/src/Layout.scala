package grit.eval

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.document.{DocLabel, DocText, DocWeight, DocumentKeeper, DocumentTerms}
import grit.core.id.{
  CloseRef,
  ConversationId,
  DocKey,
  DocumentVersion,
  EntryId,
  EntrySeq,
  PeriodRef,
  PeriodSeq,
  PluginName,
  PrincipalId,
  TurnRef,
  TurnSeq
}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.period.{Balance, CloseReason, Closing, Edit, Flows, Ground}
import grit.core.place.{Directory, Namespace, Place, Prefix, Scope}
import grit.core.store.{ConversationStore, Entry, EntryStore, Jot, Origin, Payload, PeriodStore}
import grit.core.visibility.Label

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
  * `task:eval/{case}/{variant}`), so two cases share no conversation. Its documents (`documents`)
  * are keyed `{case}/{variant}/d{i}`, in the order written, each at its place under the root.
  */
final case class Layout(
    c: Case,
    variant: Variant,
    own: Origin,
    rows: Vector[Layout.Row],
    ask: TurnSeq,
    others: Vector[Layout.Other],
    scope: Scope,
    documents: Vector[Layout.Doc] = Vector.empty
) {

  /** Every row: its own conversation's, then each other's, as written. */
  def entries: Vector[Layout.Row] = rows ++ others.flatMap(o => o.first ++ o.second)

  /** The entries a window for the case must hold (its `[must]` lines), by the origin of their
    * conversation, each conversation's in seq order: its own first, then each other's as
    * written; none when the case labels none.
    */
  def must: Vector[(Origin, ::[EntrySeq])] = labelled(_.must)

  /** The entries a window for the case must not hold (its `[never]` lines), as [[must]] gives
    * its own.
    */
  def never: Vector[(Origin, ::[EntrySeq])] = labelled(_.never)

  private def labelled(is: Case.Line => Boolean): Vector[(Origin, ::[EntrySeq])] =
    Layout.marked(own, rows, is).toVector ++ others.flatMap(o =>
      Layout.marked(o.origin, o.first ++ o.second, is)
    )
}

object Layout {

  /** One document of a case: its key, its place, its text, and whether the window must hold
    * it.
    */
  final case class Doc(key: DocKey, place: Place, text: DocText, must: Boolean)

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
    * place before any turn there, or a document of it holds no text.
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
    val documents = c.documents.zipWithIndex.map { (d: Case.Document, i: Int) =>
      (for {
        key <- DocKey.of(s"$prefix/d$i")
        place <- rooted(root, d.place)
        text <- DocText.of(d.lines.mkString("\n"))
      } yield Doc(key, place, text, d.must)).left.map(why =>
        s"${c.name}: the document at ${d.place}: $why"
      )
    }
    for {
      others <- others.foldLeft[Either[String, Vector[Other]]](Right(Vector.empty))((acc, o) =>
        acc.flatMap(done => o.map(done :+ _))
      )
      docs <- documents.foldLeft[Either[String, Vector[Doc]]](Right(Vector.empty))((acc, d) =>
        acc.flatMap(done => d.map(done :+ _))
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
      within,
      docs
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

  /** The seqs of `rows`' lines that `is`, with `origin`; none when no line is. */
  private def marked(
      origin: Origin,
      rows: Vector[Row],
      is: Case.Line => Boolean
  ): Option[(Origin, ::[EntrySeq])] =
    rows.filter(r => is(r.line)).map(_.seq).toList match {
      case first :: rest => Some(origin -> ::(first, rest))
      case Nil => None
    }
}

/** Cases written into a database. */
object Load {

  /** The plugin a case's documents are written as: `digest`, whose documents the eval's
    * cases are written as (the eval names no plugin's module).
    */
  val Plugin: PluginName =
    PluginName.of("digest").fold(why => throw new IllegalStateException(why), identity)

  /** The terms a case's documents are kept and drawn on under, at `weight`: the digest's label,
    * kept 30 days, at most 1000.
    */
  def terms(weight: DocWeight): DocumentTerms =
    // Literals both constructors accept; the weight is a DocWeight already.
    (for {
      label <- DocLabel.of("digest: conversations closed here most recently")
      terms <- DocumentTerms.of(label, weight, 30.days, 1000)
    } yield terms).fold(why => throw new IllegalStateException(why), identity)

  /** What loading a case wrote: the turn it asks in, and its documents' versions, in the order
    * laid out.
    */
  final case class Written(turn: TurnRef, documents: Vector[DocumentVersion])

  /** `layout` written through `jot`, one transaction for the case's own rows, one for each
    * period of each other conversation and one for each document: each conversation found or
    * created by its origin, begun by the local principal; each row its conversation's entry at
    * its turn and seq, its message [[Layout.message]], written at the epoch; each other
    * conversation's first period opened at its first row and, when laid out closed, sealed
    * (lapsed), then its second opened at its first row; each document written through
    * `keeper` a minute before the epoch, so a window as of the ask's root draws it. What was
    * written, or the store's error; what was written before an error stays. A case's ids are
    * unique in a database, so a case loaded twice fails.
    */
  def into(
      jot: Jot^,
      conversations: ConversationStore,
      entries: EntryStore,
      periods: PeriodStore,
      keeper: DocumentKeeper
  )(layout: Layout): Either[String, Written] = {
    def found(origin: Origin): Either[String, ConversationId] =
      jot
        .write(conversations.findOrCreate(origin, PrincipalId.Local, Label.Public))
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
      documents <- layout.documents.foldLeft[Either[String, Vector[DocumentVersion]]](
        Right(Vector.empty)
      ) { (acc, d) =>
        acc.flatMap(done =>
          jot
            .write(
              keeper.write(d.key, d.place, d.text, ujson.Obj(), Instant.EPOCH.minusSeconds(60))
            )
            .map {
              case grit.core.document.Written.Versioned(v, _) => done :+ v
              case grit.core.document.Written.Unchanged(v) => done :+ v
            }
            .left
            .map(e => s"${layout.c.name}: $e")
        )
      }
    } yield Load.Written(TurnRef(own, layout.ask), documents)
  }
}
