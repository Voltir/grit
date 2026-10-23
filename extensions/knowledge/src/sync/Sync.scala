package grit.knowledge.sync

import java.time.Instant

import scala.annotation.tailrec

import grit.core.act.{Answer, Called, Keeping, MoveError, MoveLimits, Pieced, Piecing, Posed}
import grit.core.document.DocumentKeeper
import grit.core.durable.Journaled
import grit.core.flow.{Tainted, Trusted}
import grit.core.id.{CorpusName, DocumentVersionId, FileHandle, JobName}
import grit.core.job.{JobRun, KeepingJob}
import grit.core.message.Message
import grit.core.place.Place
import grit.core.sanitise.{IntegrityTree, Reason, Verdict}
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.{Integrity, Label}
import grit.knowledge.card.{Card, CardMoves, CardSummary, Phase, UnitVersion}
import grit.knowledge.ref.CardKey

/** The job `knowledge-sync`, version 6, over `sources`. A run, for the source its parameters
  * name, walks it as its [[Source.walk]] says: lists what changed since the cards it holds;
  * writes a card at its own floor for each new or changed unit (a file's from one summary, with
  * search terms, of its first 8 KB, the model asked for at most 60 words; any other's as its walk
  * makes it, at no model cost, one whose read found it unreadable ([[Fetched.Unreadable]])
  * among them, so it is read again only once it changes), each pointing at the read of its
  * unit; copies up to
  * its floor, with no call or ask, the card of an unchanged unit held at another label, and to
  * its source's place ([[Source.at]]) the card of one kept elsewhere; and
  * withdraws the cards of units gone from directories it listed in full. Of a source that keeps
  * files ([[Source.keepsFiles]]), after a file's card is kept, it keeps the pieces of the file
  * when its read kept all of it in the file area ([[Source.file]]) believed at `Outside` or
  * above, as its card's key's at its version, believed as its card
  * ([[grit.core.act.Keeping.piece]]), and otherwise marks the file unread, saying why, so no
  * earlier revision's pieces stay current; a file's pieces move where its card is copied, and
  * are withdrawn where it is withdrawn. A file whose card is current at the floor but which has
  * neither pieces there nor a mark, or whose pieces a format the area no longer reads at that
  * version made, is read again for its pieces alone, within the run's `perRun` reads, and
  * counted unchanged. A held card at another label is withdrawn only in the keep that replaces
  * it, so a run stopped early leaves every card it did not replace current. A card is kept at no less than what its unit's answers were
  * read at, and believed at the meet of the run and what it read on its unit's way
  * ([[Step.Fetching.way]]: everything read before it for [[Way.Run]]; for [[Way.Through]], the
  * listings it was reached through, whose labels its own joins), its source's results (each as
  * [[grit.core.visibility.Visibility.believed]] believes it, so a repository's cap counts, not
  * only its service's) and the summaries asked of them; its call's
  * arguments and its summary's ask are made from the same. A listing or file answered below `Outside` is first checked by the integrity tree
  * ([[grit.core.act.Moves.vet]]), its paths or its head a window at a time: raised, it is
  * believed as raised; otherwise its unit is left unwritten, saying why (it holds hidden or
  * encoded text, it looks written to steer its reader, part of it looks out of place, it could
  * not be checked, or it is over [[Sync.MostWindows]] windows), and nothing beneath a withheld
  * directory is reached. Replies [[Synced.line]]: written, relabelled, withdrawn, unchanged, and
  * left for the next run (past `perRun`, or unwritten), or why it stopped: an ask or a check
  * refused or unread, a call failed (naming the service and why), a keep the store refused,
  * such as a key written by another run meanwhile (naming the card's path), a run believed less
  * than `Outside`, or parameters naming no source of its. Its `limits`: asks, the largest
  * `perRun` among its sources' syncs; calls, the largest of its sources' [[Source.calls]] of
  * that; judgments, [[Sync.MostWindows]] a call. Parameters asking for more stop at the limit,
  * and the reply says so.
  */
final class Sync(sources: Vector[Source[?]]) extends KeepingJob[SyncParams] {

  val name: JobName = Sync.Name

  val version: Int = 6

  def write(params: SyncParams): ujson.Value =
    ujson.Obj("source" -> CorpusName.value(params.source), "perRun" -> params.perRun)

  def read(params: ujson.Value): Either[String, SyncParams] = {
    val shown = ujson.write(params).take(100)
    for {
      o <- params.objOpt.toRight(s"a sync's parameters are an object: $shown")
      source <- o
        .get("source")
        .flatMap(_.strOpt)
        .toRight(s"a sync's parameters name its source: $shown")
        .flatMap(CorpusName.of)
      perRun <- o
        .get("perRun")
        .flatMap(_.numOpt)
        .filter(n => n.isWhole && n >= 1 && n <= Int.MaxValue)
        .toRight(s"a sync's parameters hold perRun, a whole number at least 1: $shown")
    } yield SyncParams(source, perRun.toInt)
  }

  override val limits: MoveLimits = {
    val asks = sources.flatMap(_.syncs.map(_.perRun)).maxOption.getOrElse(0)
    // Each at least zero (a source's perRun and maxUnits are at least 1), each an Int:
    // MoveLimits.of never refuses them.
    val calls = sources.map(_.calls(asks)).maxOption.getOrElse(0).toLong
    MoveLimits
      .of(asks, calls.toInt, (calls * Sync.MostWindows).min(Int.MaxValue.toLong).toInt)
      .fold(why => throw new IllegalStateException(why), identity)
  }

  def run(run: Trusted[JobRun[SyncParams]], moves: Keeping^): String =
    sources.find(_.corpus == run.extract.params.source) match {
      case None => s"${CorpusName.value(run.extract.params.source)}: no source of this sync's"
      case Some(source) =>
        Sync.walk(source, run.extract.params.perRun, run.extract.nominal, run.tainted, moves)
    }
}

object Sync {

  /** `knowledge-sync`. */
  val Name: JobName =
    // A literal of the job name's grammar: every test of the job throws here if not.
    JobName.of("knowledge-sync").fold(why => throw new IllegalStateException(why), identity)

  /** The ask summarising `head`, told first that it is `subject` ([[Step.Summarise]]). */
  private[sync] def summarising(subject: String, head: String): Posed.Json[CardSummary] =
    Posed.Json(System, Vector(Message.User(s"$subject:\n\n$head")), CardSummary.Reply)

  /** What the summary model is told, before the file. */
  private val System: String =
    "You summarise one file for a catalog that tells an assistant what each file holds, so it " +
      "can choose which to read. The line before the file says what it is and where it is " +
      "kept: call it what that line calls it, whatever its text looks like. The catalog shows " +
      "the file's path beside your summary, so do not repeat its path or its type (\"This CSV " +
      "file…\", \"README for…\"): begin with what it holds. Reply by calling " +
      "`reply` with `summary`, " +
      s"at most ${CardSummary.Words} words on what the file is and what it holds, and `terms`, " +
      s"at most ${CardSummary.Terms} search terms, each one to three words, for what it holds " +
      "that the summary leaves out."

  /** One run of `source`, asking for at most `perRun` summaries, its slot due at `due`, its run
    * `start`: its reply.
    */
  private def walk[L <: caps.Pure](
      source: Source[L],
      perRun: Int,
      due: Instant,
      start: Tainted[JobRun[SyncParams]],
      moves: Keeping^
  ): String = {
    val corpus = source.corpus
    moves.keep(CardMoves.Held, start)((_, keeper, _) => holding(corpus, source.at, keeper)) match {
      case Left(e) =>
        val none = Synced(corpus, 0, 0, 0, 0, 0, 0, Vector.empty, Vector.empty, None)
        none.copy(stopped = Some(s"reading the cards held: ${MoveError.said(e)}")).line
      case Right(h) =>
        val paths = h.held.map(c => c.key -> c.path).toMap

        /** What `step` came to, with `read` (what the run read so far) and `seen` (each unit's
          * path, to what the run saw of it) with what it read added.
          */
        def make(step: Step, read: Read, seen: Seens): (Outcome[L], Read, Seens) =
          step match {
            case list @ Step.List(path, _) =>
              reached(Phase.List, list, read, seen) match {
                case Left(why) => (Outcome.Stopped(s"listing $path: $why"), read, seen)
                case Right((answer, at, on)) =>
                  source.listing(Trusted.asIs(answer).extract) match {
                    case Left(why) => (Outcome.Stopped(s"listing $path: $why"), read, seen)
                    case Right(l) if !answer.integrity.belowOutside =>
                      (
                        Outcome.Listed(l, at),
                        heard(read, answer),
                        seen.updated(path, on.hearing(answer))
                      )
                    case Right(l) =>
                      // What of a listing reaches a card or steers the run past code's checks
                      // is what its source says is checked: that is what is checked, and
                      // believed as checked.
                      val paths = answer.map(_ => source.checked(l))
                      checked(path, paths, "directory listing") match {
                        case Left(why) => (Outcome.Stopped(s"listing $path: $why"), read, seen)
                        case Right(Left(why)) => (Outcome.NotTaken(why), read, seen)
                        case Right(Right(vetted)) =>
                          (
                            Outcome.Listed(l, at),
                            heard(read, vetted),
                            seen.updated(path, on.hearing(vetted))
                          )
                      }
                  }
              }
            case reading @ Step.Read(path, _) =>
              reached(Phase.Read, reading, read, seen) match {
                case Left(why) => (Outcome.Stopped(s"reading $path: $why"), read, seen)
                case Right((answer, at, reachedAs)) =>
                  val on = reachedAs.copy(whole = wholeOf(source, answer))
                  source.fetched(Trusted.asIs(answer).extract) match {
                    case Fetched.Text(whole, version) if answer.integrity.belowOutside =>
                      // Of a file, only its head reaches its card, through its summary: the head
                      // is what is checked, and believed as checked.
                      val head = Fetched.head(whole)
                      checked(path, answer.map(_ => head), "file") match {
                        case Left(why) => (Outcome.Stopped(s"reading $path: $why"), read, seen)
                        case Right(Left(why)) => (Outcome.NotTaken(why), read, seen)
                        case Right(Right(vetted)) =>
                          (
                            Outcome.Fetched(Fetched.Text(head, version), at),
                            heard(read, vetted),
                            seen.updated(path, on.hearing(vetted))
                          )
                      }
                    case fetched if !answer.integrity.belowOutside =>
                      (
                        Outcome.Fetched(fetched, at),
                        heard(read, answer),
                        seen.updated(path, on.hearing(answer))
                      )
                    // A read with no version is left unwritten; a file found unreadable is carded
                    // with words of the walk's own: none of either read reaches a card.
                    case fetched => (Outcome.Fetched(fetched, at), read, seen)
                  }
              }
            case piece @ Step.Piece(path, _, kind) =>
              val key = CardKey.of(corpus, path)
              val (ref, at) = (CardKey.value(key), source.at)
              val revision = h.best(key).fold("")(c => UnitVersion.value(c.version))
              reached(Phase.Read, piece, read, seen) match {
                case Left(why) =>
                  (Outcome.Stopped(s"reading $path for its pieces: $why"), read, seen)
                case Right((answer, _, on)) =>
                  // Nothing of the file reaches a card or steers the run: only its pieces are
                  // believed as what it was read as.
                  val piecing = wholeOf(source, answer).flatMap(file =>
                    source.pointer(path).map((file, _))
                  ) match {
                    case Right((file, pointer)) =>
                      heard(on.believed, answer).map(_ =>
                        Piecing.Read(file, kind, ref, revision, pointer, at, Vector())
                      )
                    case Left(why) => read.map(_ => Piecing.Mark(ref, revision, at, why, Vector()))
                  }
                  val piece = moves.piece(CardMoves.of(Phase.Piece, key), piecing)
                  (kept(piece, s"keeping the pieces of $path"), read, seen)
              }
            case Step.Summarise(path, subject, head, _) =>
              val asked = summarising(subject, head)
              val key = CardMoves.of(Phase.Summary, CardKey.of(corpus, path))
              val on = seen.getOrElse(path, Seen(h.floor, read, Way.Run))
              val from = believed(path, read, seen)
              moves.ask(key, from.map(_ => asked)) match {
                case Left(e) =>
                  (Outcome.Stopped(s"summarising $path: ${MoveError.said(e)}"), read, seen)
                case Right(a) =>
                  (
                    Outcome.Summarised(Trusted.asIs(a.reply).extract),
                    heard(read, a.reply),
                    seen.updated(
                      path,
                      on.copy(at = on.at.join(a.at), believed = heard(from, a.reply))
                    )
                  )
              }
            case Step.Write(card, replacing, kind) =>
              val on = seen.get(card.path)
              val label = on.fold(h.floor)(_.at)
              val (at, from) = (source.at, believed(card.path, read, seen))
              val written = kept(
                moves.keep(CardMoves.of(Phase.Write, card.key), from)((_, keeper, now) =>
                  replace(keeper, card, label, from.integrity, at, replacing, now)
                ),
                s"keeping the card of ${card.path}"
              )
              // A file's card written from a read that kept no whole file marks it unread, so
              // none of its earlier revision's pieces is served as current, and no run reads it
              // again for its pieces until it changes.
              val (ref, revision) = (CardKey.value(card.key), UnitVersion.value(card.version))
              val piecing = kind.filter(_ => source.keepsFiles).map { kind =>
                on.fold[Either[String, FileHandle]](Left(NotRead))(_.whole) match {
                  case Right(file) =>
                    Piecing.Read(file, kind, ref, revision, card.pointer, at, replacing.toVector)
                  case Left(why) => Piecing.Mark(ref, revision, at, why, replacing.toVector)
                }
              }
              (
                piecing.fold(written)(p => after(written, card.key, card.path, from.map(_ => p))),
                read,
                seen
              )
            case Step.Relabel(key, from) =>
              val (floor, at) = (h.floor, source.at)
              val path = paths.getOrElse(key, CardKey.value(key))
              // Withdrawals and copies are decided by the source's listings, so they are kept from
              // what the run has read, as its writes are. Today a listing believed below a keep's
              // need always stops the run at its directory's card first, since a walk emits the
              // drop last; keeping from `read` makes that hold whatever the order.
              val relabelled = kept(
                moves.keep(CardMoves.of(Phase.Relabel, key), read)((_, keeper, now) =>
                  relabel(keeper, key, from, floor, at, now)
                ),
                s"relabelling the card of $path"
              )
              // A file's pieces go where its card goes, in a move of their own; a directory's
              // card (one made under a scope) has none.
              val file = source.keepsFiles &&
                h.held.exists(c => c.key == key && c.label == from && c.scope.isEmpty)
              val moved = read.map(_ => Piecing.Relabel(CardKey.value(key), from, at))
              (if (file) after(relabelled, key, path, moved) else relabelled, read, seen)
            case Step.Drop(gone) =>
              val dropped = kept(
                moves.keep(CardMoves.Drop, read)((_, keeper, now) => drop(keeper, gone, now)),
                "withdrawing the cards gone"
              )
              val files = Piecing.Withdraw(gone.map(c => (CardKey.value(c.key), c.label)))
              (
                dropped match {
                  case Outcome.Kept if gone.nonEmpty && source.keepsFiles =>
                    kept(
                      moves.piece(CardMoves.DropPieces, read.map(_ => files)),
                      "keeping the pieces of the files gone"
                    )
                  case stopped => stopped
                },
                read,
                seen
              )
          }

        /** `done`, a keep about the unit `key` at `path`; then, when it was kept, the piece move
          * `piecing` of its file.
          */
        def after(
            done: Outcome[Nothing],
            key: CardKey,
            path: String,
            piecing: Tainted[Piecing]
        ): Outcome[Nothing] = done match {
          case Outcome.Kept =>
            kept(
              moves.piece(CardMoves.of(Phase.Piece, key), piecing),
              s"keeping the pieces of $path"
            )
          case stopped => stopped
        }

        /** `text`, read of `path`, a `source`, through the integrity tree a window at a time
          * ([[IntegrityTree.windows]]), each window its own vet: all of it, believed as its windows
          * were raised to; or, the first window not raised, or more than [[MostWindows]] of them,
          * why it is withheld (inner `Left`); or why the run stops (outer `Left`).
          */
        def checked(
            path: String,
            text: Tainted[String],
            source: String
        ): Either[String, Either[String, Tainted[String]]] = {
          val key = CardKey.of(corpus, path)
          val windows = IntegrityTree.windows(Trusted.asIs(text).extract)
          @tailrec def go(
              i: Int,
              rest: List[String],
              done: Tainted[String]
          ): Either[String, Either[String, Tainted[String]]] = rest match {
            case Nil => Right(Right(done))
            case w :: more =>
              moves.vet(CardMoves.vet(key, i), text.map(_ => w), source) match {
                case Left(e) => Left(MoveError.said(e))
                case Right(v) =>
                  v.verdict match {
                    case Verdict.Raised(_) => go(i + 1, more, done.zip(v.text).map(_ + _))
                    case Verdict.Suspicious(reasons) =>
                      val hiding = reasons.exists(r => r == Reason.Hidden || r == Reason.Encoded)
                      Right(Left(if (hiding) Hiding else Suspected))
                    case Verdict.Kept(reasons) =>
                      Right(Left(if (reasons.contains(Reason.Unjudged)) Unchecked else Misplaced))
                  }
              }
          }
          if (windows.size > MostWindows) Right(Left(TooLong))
          else go(0, windows.toList, Trusted.literal("").tainted)
        }

        /** The answer of the call making `step` in `phase`, its label, and what the run saw of
          * its unit before it: that label joined with its listing's ([[Way.Through]]), believed
          * as what its arguments were made from (`read` for [[Way.Run]], its listing's record
          * for [[Way.Through]]); or why not, such as a listing it was reached through that `seen`
          * lacks.
          */
        def reached(
            phase: Phase,
            step: Step.Fetching,
            read: Read,
            seen: Seens
        ): Either[String, (Tainted[Answer], Label, Seen)] = {
          val before: Either[String, (Read, Option[Label])] = step.way match {
            case Way.Run => Right((read, None))
            case Way.Through(listing) =>
              seen
                .get(listing)
                .map(s => (s.believed, Some(s.at)))
                .toRight(s"it was reached through $listing, which the run has not listed")
          }
          before.flatMap { (from, above) =>
            call(phase, step, from).map { (answer, at) =>
              (answer, at, Seen(above.fold(at)(_.join(at)), from, step.way))
            }
          }
        }

        /** The answer of the call making `step` in `phase`, its arguments made from `from`, and
          * its label; or why not.
          */
        def call(
            phase: Phase,
            step: Step.Fetching,
            from: Read
        ): Either[String, (Tainted[Answer], Label)] = {
          val (tool, arguments) = source.call(step)
          moves.call(
            CardMoves.of(phase, CardKey.of(corpus, step.path)),
            source.service,
            tool,
            from.map(_ => arguments)
          ) match {
            case Left(e) => Left(MoveError.said(e))
            case Right(Called.Done(answer, at)) => Right((answer, at))
            case Right(Called.Failed(why)) =>
              Left(s"the call to ${source.service.name} failed: $why")
            case Right(Called.Interrupted) =>
              Left(s"the call to ${source.service.name} went unanswered, and may have partly run")
          }
        }

        @tailrec def go(w: Walk[L], read: Read, seen: Seens): String = w.next match {
          case Left(synced) => synced.line
          case Right(step) =>
            val (outcome, after, next) = make(step, read, seen)
            go(w.after(outcome), after, next)
        }

        // The held files whose cards are current at the floor and whose pieces are due: read
        // again for them alone ([[Holding.repiece]]).
        val files = h.held.filter(c => c.label == h.floor && c.scope.isEmpty).map(_.key)
        val owed =
          if (!source.keepsFiles || files.isEmpty) Right(Vector.empty)
          else
            moves
              .piece(CardMoves.DuePieces, start.map(_ => Piecing.Survey(files.map(CardKey.value))))
              .map {
                case Pieced.Due(refs) => files.filter(k => refs.contains(CardKey.value(k)))
                case _ => Vector.empty
              }
        owed match {
          case Left(e) =>
            val none = Synced(corpus, 0, 0, 0, 0, 0, 0, Vector.empty, h.damaged, None)
            none.copy(stopped = Some(s"finding the pieces due: ${MoveError.said(e)}")).line
          case Right(repiece) =>
            go(
              source.walk(perRun, h.floor, h.held, due, repiece.toSet, h.damaged),
              start.map(_ => ()),
              Map.empty
            )
        }
    }
  }

  /** Why a file whose read is not what a run saw keeps no pieces: it was not read. */
  private val NotRead = "it was not read"

  /** The file `answer`, a read of `source`'s, kept whole in the file area, believed at `Outside` or above,
    * whose pieces are kept; or why none are.
    */
  private def wholeOf(source: Source[?], answer: Tainted[Answer]): Either[String, FileHandle] =
    source.file(Trusted.asIs(answer).extract) match {
      case None => Left("its read kept no whole file in the file area")
      // All of a file is kept as pieces only when all of it is believed at Outside or above: of
      // one below, only its head is checked.
      case Some(_) if answer.integrity.belowOutside =>
        Left("it was believed below Outside, so only its head was checked")
      case Some(file) => Right(file)
    }

  /** A keep's or a piece move's outcome: [[Outcome.Kept]] whatever it kept, or why `doing`
    * stopped.
    */
  private def kept(result: Either[MoveError, Kept | Pieced], doing: String): Outcome[Nothing] =
    result.fold(e => Outcome.Stopped(s"$doing: ${MoveError.said(e)}"), _ => Outcome.Kept)

  /** The floor its transaction keeps at, and `corpus`'s cards among the plugin's documents, by
    * key, each kept elsewhere when not at `at`, and those of its cards that do not read
    * ([[Held.read]]); a document of another source's, or of none, is passed over.
    */
  private def holding(corpus: CorpusName, at: Place, keeper: DocumentKeeper)(using
      tx: Tx^
  ): Either[StoreError, Holding] =
    // Every current document: the plugin's bound, counted over every label, holds them to the
    // sum of its sources' maxUnits.
    keeper.newest(Int.MaxValue).map { documents =>
      val read = documents.map(Held.read(corpus, at, _))
      Holding(
        Tx.floor(tx),
        read.collect { case Held.Read.Ours(h) => h }.sortBy(c => CardKey.value(c.key)),
        damaged = read.collect { case d: Held.Read.Damaged => d }.sortBy(d => CardKey.value(d.key))
      )
    }

  /** `card` kept at `label` at `place`, `now`, believed at `integrity`, the meet of its unit's
    * reads, its copy at `replacing` withdrawn first, in this one transaction.
    */
  private def replace(
      keeper: DocumentKeeper,
      card: Card,
      label: Label,
      integrity: Integrity,
      place: Place,
      replacing: Option[Label],
      now: Instant
  )(using Tx^): Either[StoreError, Kept] =
    for {
      // Withdrawn before the write, so the old copy never counts against the bound beside it.
      old <- replacing.fold[Either[StoreError, Option[DocumentVersionId]]](Right(None))(
        keeper.withdraw(card.docKey, _, now)
      )
      w <- keeper.write(
        card.docKey,
        label,
        integrity,
        place,
        card.text,
        card.data,
        Some(card.pointer),
        now
      )
    } yield Kept(old.size, Some(w.kept))

  /** The card under `key` at `from` copied, believed as it was, to `floor` at `place`, `now`,
    * and withdrawn at `from` when that is not `floor` (a write at a label replaces its version
    * there), in this one transaction. Refused when no card is kept at `from`.
    */
  private def relabel(
      keeper: DocumentKeeper,
      key: CardKey,
      from: Label,
      floor: Label,
      place: Place,
      now: Instant
  )(using Tx^): Either[StoreError, Kept] = {
    val doc = CardKey.docKey(key)
    keeper.current(doc, from).flatMap {
      case None =>
        Left(StoreError.Invalid(s"no card is kept at ${Label.shown(from)} to relabel"))
      case Some(d) =>
        for {
          old <-
            if (from == floor) Right(None)
            else keeper.withdraw(doc, from, now)
          w <- keeper.write(doc, floor, d.integrity, place, d.text, d.data, d.pointer, now)
        } yield Kept(old.size, Some(w.kept))
    }
  }

  /** Each of `gone` withdrawn at its label, `now`. */
  private def drop(keeper: DocumentKeeper, gone: Vector[CardCopy], now: Instant)(using
      Tx^
  ): Either[StoreError, Kept] =
    gone
      .foldLeft[Either[StoreError, Int]](Right(0)) { (acc, c) =>
        acc.flatMap(n => keeper.withdraw(CardKey.docKey(c.key), c.label, now).map(n + _.size))
      }
      .map(Kept(_, None))

  /** What a run read so far, believed at the meet of its parts. */
  private type Read = Tainted[Unit]

  /** What a run saw of one unit: the label its answers hold nothing above, what they are
    * believed at, the way it was reached, and the file whose pieces are kept: the file area kept
    * all of what its read read, believed at `Outside` or above. Through
    * [[Way.Run]], it is believed as everything the run read before it was; through
    * [[Way.Through]], as its listing's record was, and its label joins that record's.
    */
  private final case class Seen(
      at: Label,
      believed: Read,
      way: Way,
      whole: Either[String, FileHandle] = Left(NotRead)
  ) {

    /** This, `answer` heard after it. */
    def hearing(answer: Tainted[?]): Seen = copy(believed = heard(believed, answer))
  }

  /** Each unit's path, to what the run saw of it. */
  private type Seens = Map[String, Seen]

  /** What a step about the unit at `path` is made from: its own record, when it was reached
    * [[Way.Through]] a listing; otherwise `read`, everything the run read so far.
    */
  private def believed(path: String, read: Read, seen: Seens): Read =
    seen.get(path) match {
      case Some(Seen(_, from, Way.Through(_), _)) => from
      case Some(Seen(_, _, Way.Run, _)) | None => read
    }

  /** 10: the most windows of one listing's paths or one file's head a run checks
    * ([[IntegrityTree.windows]]); a longer one is left unwritten. A file's head, 8 KB, always
    * fits.
    */
  val MostWindows: Int = 10

  /** Why a unit is left unwritten when what it read looks written to steer its reader. */
  private val Suspected = "it looks written to steer whoever reads it"

  /** Why a unit is left unwritten when what it read holds characters a reader does not see, a
    * word of mixed scripts, or an encoded blob: what code found, which accuses it of nothing.
    */
  private val Hiding = "it holds hidden or encoded text, so it is not trusted enough to keep"

  /** Why a unit is left unwritten when what it read could not be checked. */
  private val Unchecked = "it could not be checked, so it is not trusted enough to keep"

  /** Why a unit is left unwritten when part of what it read looks out of place. */
  private val Misplaced = "part of it looks out of place, so it is not trusted enough to keep"

  /** Why a unit is left unwritten when what it read is over [[MostWindows]] windows. */
  private val TooLong = "it is too long to check"

  /** `read`, and `answer` read after it. */
  private def heard(read: Read, answer: Tainted[?]): Read = read.zip(answer).map(_ => ())
}

/** What a keep writing, relabelling or dropping cards did, as its move records it: how many
  * cards it withdrew, and the label it kept a card at, if it kept one.
  */
private[sync] final case class Kept(withdrawn: Int, at: Option[Label]) extends caps.Pure

private[sync] object Kept {

  /** Its record: `{"withdrawn"}`, and `"at"`, the label in its written form. */
  given Journaled[Kept] = Journaled.json(
    k => {
      val o = ujson.Obj("withdrawn" -> k.withdrawn)
      k.at.foreach(l => o("at") = Label.written(l))
      o
    },
    json =>
      for {
        o <- json.objOpt.toRight(s"a keep's record is an object: ${ujson.write(json)}")
        n <- o
          .get("withdrawn")
          .flatMap(_.numOpt)
          .filter(_.isWhole)
          .toRight(s"a keep's record counts what it withdrew: ${ujson.write(json)}")
        at <- o.get("at") match {
          case None => Right(None)
          case Some(v) =>
            v.strOpt
              .toRight(s"a keep's record names its label: ${ujson.write(json)}")
              .flatMap(Label.read)
              .map(Some(_))
        }
      } yield Kept(n.toInt, at)
  )
}
