package grit.knowledge.sync

import java.time.Instant

import scala.annotation.tailrec

import grit.core.act.{Answer, Called, Keeping, MoveError, MoveKind, MoveLimits, MoveName, Posed}
import grit.core.document.DocumentKeeper
import grit.core.durable.Journaled
import grit.core.flow.{Tainted, Trusted}
import grit.core.id.{CorpusName, DocumentVersion, JobName}
import grit.core.job.{JobRun, KeepingJob}
import grit.core.message.Message
import grit.core.place.{Place, Service}
import grit.core.sanitise.{IntegrityTree, Reason, Verdict}
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.Label
import grit.knowledge.card.{Card, CardKey, CardMoves, CardSummary, Phase}

/** The job `knowledge-sync`, version 3, over `sources`. A run, for the source its parameters
  * name, walks it as its [[Source.walk]] says: lists what changed since the cards it holds;
  * writes a card at its own floor for each new or changed unit (a file's from one summary, with
  * search terms, of its first 8 KB, the model asked for at most 60 words; any other's as its walk
  * makes it, at no model cost), each pointing at the read of its unit; copies up to
  * its floor, with no call or ask, the card of an unchanged unit held at another label; and
  * withdraws the cards of units gone from directories it listed in full. A held card at another
  * label is withdrawn only in the keep that replaces it, so a run stopped early leaves every
  * card it did not replace current. A card is kept at no less than what its unit's answers were
  * read at, and believed at no more than the run and what it read on its unit's way
  * ([[Step.Fetching.way]]: everything read before it for [[Way.Run]]; for [[Way.Through]], the
  * listings it was reached through, whose labels its own joins), its source's results
  * ([[grit.core.visibility.Visibility.outputOf]]) and the summaries asked of them; its call's
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

  val version: Int = 3

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

  /** What the summary model is told, before the file. */
  private val System: String =
    "You summarise one file of a repository for a catalog that tells an assistant what each " +
      "file holds, so it can choose which to read. Reply by calling `reply` with `summary`, " +
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
    moves.keep(CardMoves.Held, start)((_, keeper, _) => holding(corpus, keeper)) match {
      case Left(e) =>
        val none = Synced(corpus, 0, 0, 0, 0, 0, 0, Vector.empty, None)
        none.copy(stopped = Some(s"reading the cards held: ${reason(e)}")).line
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
                case Right((answer, at, on)) =>
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
                    // A read with no version is left unwritten: none of it reaches a card.
                    case fetched => (Outcome.Fetched(fetched, at), read, seen)
                  }
              }
            case Step.Summarise(path, head, _) =>
              val asked = Posed.Json(
                System,
                Vector(Message.User(s"${source.subject(path)}:\n\n$head")),
                CardSummary.Reply
              )
              val key = CardMoves.of(Phase.Summary, CardKey.of(corpus, path))
              val on = seen.getOrElse(path, Seen(h.floor, read, Way.Run))
              val from = believed(path, read, seen)
              moves.ask(key, from.map(_ => asked)) match {
                case Left(e) => (Outcome.Stopped(s"summarising $path: ${reason(e)}"), read, seen)
                case Right(a) =>
                  (
                    Outcome.Summarised(Trusted.asIs(a.reply).extract),
                    heard(read, a.reply),
                    seen.updated(path, Seen(on.at.join(a.at), heard(from, a.reply), on.way))
                  )
              }
            case Step.Write(card, replacing) =>
              val label = seen.get(card.path).fold(h.floor)(_.at)
              val (at, from) = (source.at, believed(card.path, read, seen))
              (
                kept(
                  moves.keep(CardMoves.of(Phase.Write, card.key), from)((_, keeper, now) =>
                    replace(keeper, card, label, source.service, at, replacing, now)
                  ),
                  s"keeping the card of ${card.path}"
                ),
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
              (
                kept(
                  moves.keep(CardMoves.of(Phase.Relabel, key), read)((_, keeper, now) =>
                    relabel(keeper, key, from, floor, at, now)
                  ),
                  s"relabelling the card of $path"
                ),
                read,
                seen
              )
            case Step.Drop(gone) =>
              (
                kept(
                  moves.keep(CardMoves.Drop, read)((_, keeper, now) => drop(keeper, gone, now)),
                  "withdrawing the cards gone"
                ),
                read,
                seen
              )
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
                case Left(e) => Left(reason(e))
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
            case Left(e) => Left(reason(e))
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

        go(source.walk(perRun, h.floor, h.held, due), start.map(_ => ()), Map.empty)
    }
  }

  /** A keep's outcome: [[Outcome.Kept]], or why `doing` stopped. */
  private def kept(result: Either[MoveError, Kept], doing: String): Outcome[Nothing] =
    result.fold(e => Outcome.Stopped(s"$doing: ${reason(e)}"), _ => Outcome.Kept)

  /** The floor its transaction keeps at, and `corpus`'s cards among the plugin's documents, by
    * key; or the first document that is no card, as the store's refusal.
    */
  private def holding(corpus: CorpusName, keeper: DocumentKeeper)(using
      tx: Tx^
  ): Either[StoreError, Holding] =
    // Every current document: the plugin's bound, counted over every label, holds them to the
    // sum of its sources' maxUnits.
    keeper.newest(Int.MaxValue).flatMap { documents =>
      documents
        .foldLeft[Either[String, Vector[Held]]](Right(Vector.empty)) { (acc, d) =>
          acc.flatMap(hs => Held.read(corpus, d).map(hs ++ _))
        }
        .left
        .map(StoreError.Invalid(_))
        .map(hs => Holding(Tx.floor(tx), hs.sortBy(c => CardKey.value(c.key))))
    }

  /** `card` kept at `label` at `place`, `now`, believed as what `service`, its source, returns
    * is ([[grit.core.visibility.Visibility.outputOf]]), its copy at `replacing` withdrawn
    * first, in this one transaction.
    */
  private def replace(
      keeper: DocumentKeeper,
      card: Card,
      label: Label,
      service: Service,
      place: Place,
      replacing: Option[Label],
      now: Instant
  )(using tx: Tx^): Either[StoreError, Kept] =
    for {
      // Withdrawn before the write, so the old copy never counts against the bound beside it.
      old <- replacing.fold[Either[StoreError, Option[DocumentVersion]]](Right(None))(
        keeper.withdraw(card.docKey, _, now)
      )
      w <- keeper.write(
        card.docKey,
        label,
        Tx.visibility(tx).outputOf(service),
        place,
        card.text,
        card.data,
        Some(card.pointer),
        now
      )
    } yield Kept(old.size, Some(w.kept))

  /** The card under `key` at `from` copied, believed as it was, to `floor` at `place`, `now`, and withdrawn at
    * `from`, in this one transaction. Refused when no card is kept there.
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
          old <- keeper.withdraw(doc, from, now)
          w <- keeper.write(doc, floor, d.integrity, place, d.text, d.data, d.pointer, now)
        } yield Kept(old.size, Some(w.kept))
    }
  }

  /** Each of `gone` withdrawn at its label, `now`. */
  private def drop(keeper: DocumentKeeper, gone: Vector[Held], now: Instant)(using
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
    * believed at, and the way it was reached. Through [[Way.Run]], it is believed as everything
    * the run read before it was; through [[Way.Through]], as its listing's record was, and its
    * label joins that record's.
    */
  private final case class Seen(at: Label, believed: Read, way: Way) {

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
      case Some(Seen(_, from, Way.Through(_))) => from
      case Some(Seen(_, _, Way.Run)) | None => read
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

  /** Why a move was not made, as a run's reply says it. */
  private def reason(error: MoveError): String = error match {
    case MoveError.Below(_) => "it came from somewhere not trusted enough to keep"
    case MoveError.Repeated(name) => s"the move ${MoveName.value(name)} was made already"
    case MoveError.OverLimit(kind, limit) => s"the run reached its limit of $limit ${plural(kind)}"
    case MoveError.Capped => "the day's recorded spend reached its cap"
    case MoveError.Model(why) => why
    case MoveError.Uncleared(at) => s"no model is cleared for ${Label.shown(at)}"
    case MoveError.Store(why) => why
    case MoveError.Diverged(name) =>
      s"the run's moves differ from its record at ${MoveName.value(name)}"
    case MoveError.Undeclared(node) => s"the job's code does not allow the $node it used"
  }

  private def plural(kind: MoveKind): String = kind match {
    case MoveKind.Ask => "asks"
    case MoveKind.Judge => "judgments"
    case MoveKind.Call => "calls"
    case MoveKind.Keep => "keeps"
  }
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
