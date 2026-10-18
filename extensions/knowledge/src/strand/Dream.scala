package grit.knowledge.strand

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.act.{Dreaming, MoveError, MoveLimits, MoveName}
import grit.core.document.DocumentKeeper
import grit.core.durable.Journaled
import grit.core.flow.Trusted
import grit.core.id.{ConversationId, JobName}
import grit.core.job.{DreamingJob, JobRun}
import grit.core.stitch.{Strand, StrandShelf}
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.{Integrity, Label}
import grit.knowledge.card.CardSummary

/** What a dream run is told: at most how many strands it summarises, and how far back a
  * strand's last message may be for it to be looked at when it holds no document.
  */
final case class DreamParams(perRun: Int, reach: FiniteDuration) extends caps.Pure

/** The job `strands-dream`, version 1, of a plugin whose schedules are declared at `labels`.
  * A run keeps, at its floor, one document per strand of [[Dream.Members]] or more members
  * and [[Dream.Least]] or more characters of their messages, in a room whose label in force
  * is its floor, keyed `strand:{root}`: [[StrandDocument.text]] of one summary
  * ([[CardSummary]]) of what they said, their latest [[Dream.Chars]] characters as an excerpt
  * shows them. A strand whose messages are unchanged since its document was written asks
  * nothing. It withdraws its floor's document of a strand under either least since, whose
  * members' messages are gone, or whose room is at another label now. It summarises at most
  * `perRun` strands (and at most `perRunMost`), oldest touched first, of those whose members
  * spoke within `reach` or that hold a document; the rest wait for the next run. Replies
  * [[Dreamt.line]]: written, withdrawn, unchanged and left for the next run, the rooms it
  * read at a label none of `labels` is, the strands not written and why (a summary refused
  * or unread, or a keep the store refused), or why it stopped (the scan refused, naming
  * why). Its `limits`: asks, `perRunMost`; no calls or judgments.
  */
final class Dream(perRunMost: Int, labels: Vector[Label]) extends DreamingJob[DreamParams] {

  val name: JobName = Dream.Name

  val version: Int = 1

  def write(params: DreamParams): ujson.Value =
    ujson.Obj(
      "perRun" -> params.perRun,
      "reach" -> java.time.Duration.ofSeconds(params.reach.toSeconds).toString
    )

  def read(params: ujson.Value): Either[String, DreamParams] = {
    val shown = ujson.write(params).take(100)
    for {
      o <- params.objOpt.toRight(s"a dream's parameters are an object: $shown")
      perRun <- o
        .get("perRun")
        .flatMap(_.numOpt)
        .filter(n => n.isWhole && n >= 1 && n <= Int.MaxValue)
        .toRight(s"a dream's parameters hold perRun, a whole number at least 1: $shown")
      reach <- o
        .get("reach")
        .flatMap(_.strOpt)
        .flatMap(t => scala.util.Try(java.time.Duration.parse(t)).toOption)
        .filter(d => !d.isNegative && !d.isZero)
        .toRight(s"a dream's parameters hold reach, a positive ISO-8601 duration: $shown")
    } yield DreamParams(perRun.toInt, reach.getSeconds.seconds)
  }

  override val limits: MoveLimits = MoveLimits.of(perRunMost.max(0), 0).getOrElse(MoveLimits.Zero)

  def run(run: Trusted[JobRun[DreamParams]], moves: Dreaming^): String = {
    val most = run.extract.params.perRun.min(limits.asks)
    moves.scan(Dream.Scan, run.tainted.map(_.params))((params, keeper, strands, _, at) =>
      Dream.scanned(params.reach, most, labels, keeper, strands, at)
    ) match {
      case Left(e) => Dreamt.stopped(s"scanning its strands: ${MoveError.said(e)}").line
      case Right(scan) =>
        scan.due.zipWithIndex
          .foldLeft(Dreamt.of(scan)) { case (done, (seen, i)) =>
            moves.summarise(
              Dream.move("summary", i),
              seen,
              run.tainted.map(_ => Dream.Asked)
            ) match {
              case Left(e) => done.unwritten(seen.root, MoveError.said(e))
              case Right(None) => done.changed
              case Right(Some(asked)) =>
                moves.keep(Dream.move("write", i), asked.reply)((summary, keeper, at) =>
                  Dream.written(seen, summary, keeper, at)
                ) match {
                  case Left(e) => done.unwritten(seen.root, MoveError.said(e))
                  case Right(_) => done.wrote
                }
            }
          }
          .line
    }
  }
}

object Dream {

  val Name: JobName =
    JobName.of("strands-dream").fold(e => throw new IllegalStateException(e), identity)

  /** 3: the fewest members, the root among them, a strand with a document has. */
  val Members: Int = 3

  /** 1,200: the fewest characters of its messages, as an excerpt shows them whole, a strand
    * with a document has.
    */
  val Least: Int = 1200

  /** 6,000: the most characters of a strand a summary is asked of. */
  val Chars: Int = 6000

  private val Scan: MoveName = move("scan")

  /** What the summary model is told, before the strand. */
  private val System: String =
    "You summarise one discussion in a team's chat room for a catalog that tells an " +
      "assistant what has been discussed there, so it can find it again. The discussion is " +
      "several threads on one topic, oldest first: how it opened, then its latest messages, " +
      "each under its speaker's name, a line of … standing for messages left out. Reply by " +
      s"calling `reply` with `summary`, at most ${CardSummary.Words} words on what the " +
      "discussion is about, what was decided and what is still open, naming people as the " +
      s"messages do, and `terms`, at most ${CardSummary.Terms} search terms, each one to three " +
      "words, for what it covers that the summary leaves out."

  private val Reply = CardSummary.replyOf(
    s"What the discussion is about, what was decided and what is open, in at most " +
      s"${CardSummary.Words} words.",
    "Search terms, each one to three words, for what the discussion covers that the summary " +
      "leaves out."
  )

  /** Every strand's summary ask. */
  private val Asked: Dreaming.Summary[CardSummary] = Dreaming.Summary(System, Chars, Reply)

  /** The move `{kind}-{i}`. */
  private def move(kind: String, i: Int): MoveName = move(s"$kind-$i")

  private def move(name: String): MoveName =
    // Lower case words and digits joined by `-`: MoveName.of never refuses them.
    MoveName.of(name).fold(e => throw new IllegalStateException(e), identity)

  /** Whether `seen` is big enough to hold a document, in a room at `floor`. */
  private def holds(seen: Strand.Digested, floor: Label): Boolean =
    seen.members.size >= Members && seen.chars >= Least && seen.roomLabel == floor

  /** The scan: its floor's documents read; each withdrawn whose strand no longer holds one
    * there, or whose data does not read; and the strands to summarise, at most `most`, oldest
    * touched first, of those at its floor whose members spoke within `reach` of `at` or that
    * hold a document, whose messages are not the ones their document was written from.
    */
  private def scanned(
      reach: FiniteDuration,
      most: Int,
      labels: Vector[Label],
      keeper: DocumentKeeper,
      strands: StrandShelf,
      at: Instant
  )(using tx: Tx^): Either[StoreError, Scanned] = {
    val floor = Tx.floor(tx)
    val from = at.minusSeconds(reach.toSeconds)
    for {
      documents <- keeper.newest(Int.MaxValue).map(_.filter(_.label == floor))
      read = documents.map(d => (d, StrandDocument.held(d)))
      held = read.collect { case (d, Right((root, digest))) => root -> (d, digest) }.toMap
      recent <- strands.atFloor(from, at)
      seen <- strands.of(held.keys.toVector, at)
      undeclared <- strands
        .touched(from, at)
        .map(_.filterNot(s => labels.contains(s.roomLabel)).map(_.room).distinct.size)
      kept = seen.filter(holds(_, floor))
      gone = read.collect {
        case (d, Left(_)) => d.key
        case (d, Right((root, _))) if !kept.exists(_.root == root) => d.key
      }
      withdrawn <- gone.foldLeft[Either[StoreError, Int]](Right(0))((n, key) =>
        n.flatMap(k => keeper.withdraw(key, floor, at).map(v => k + v.size))
      )
      candidates = (kept ++ recent.filter(holds(_, floor))).distinctBy(_.root).sortBy(_.touched)
      changed = candidates.filterNot(s => held.get(s.root).exists(_._2 == s.digest))
    } yield Scanned(
      changed.take(most),
      candidates.size - changed.size,
      withdrawn,
      (changed.size - most).max(0),
      undeclared
    )
  }

  /** `summary` of `seen` kept as its document, at the floor, in its room. */
  private def written(
      seen: Strand.Digested,
      summary: CardSummary,
      keeper: DocumentKeeper,
      at: Instant
  )(using Tx^): Either[StoreError, Wrote] =
    keeper
      .write(
        StrandDocument.key(seen.root),
        Label.Public,
        Integrity.Authority,
        seen.room,
        StrandDocument.text(seen, summary),
        StrandDocument.data(seen),
        None,
        at
      )
      .map(w => Wrote(Label.written(w.kept)))
}

/** What a dream's scan came to: the strands `due` a summary, how many held a document still
  * `unchanged`, how many documents it `withdrew`, how many strands it `left` past its most,
  * and how many rooms it read at a label no schedule of its plugin is declared at.
  */
private[strand] final case class Scanned(
    due: Vector[Strand.Digested],
    unchanged: Int,
    withdrew: Int,
    left: Int,
    undeclared: Int
) extends caps.Pure

private[strand] object Scanned {

  /** Its record: `{"due": [...], "unchanged", "withdrew", "left", "undeclared"}`, each strand
    * as [[Strand.Digested.write]] writes it.
    */
  given Journaled[Scanned] = Journaled.json(
    s =>
      ujson.Obj(
        "due" -> ujson.Arr.from(s.due.map(Strand.Digested.write)),
        "unchanged" -> s.unchanged,
        "withdrew" -> s.withdrew,
        "left" -> s.left,
        "undeclared" -> s.undeclared
      ),
    v => {
      def count(key: String): Either[String, Int] =
        v.objOpt
          .flatMap(_.get(key))
          .flatMap(_.numOpt)
          .map(_.toInt)
          .toRight(s"a dream's scan holds no count '$key'")
      for {
        due <- v.objOpt
          .flatMap(_.get("due"))
          .flatMap(_.arrOpt)
          .toRight("a dream's scan holds no strands")
          .flatMap(_.toVector.foldLeft[Either[String, Vector[Strand.Digested]]](Right(Vector())) {
            (acc, s) => acc.flatMap(done => Strand.Digested.read(s).map(done :+ _))
          })
        unchanged <- count("unchanged")
        withdrew <- count("withdrew")
        left <- count("left")
        undeclared <- count("undeclared")
      } yield Scanned(due, unchanged, withdrew, left, undeclared)
    }
  )
}

/** The label a strand's document was kept at, in its written form. */
private[strand] final case class Wrote(label: String) extends caps.Pure

private[strand] object Wrote {
  given Journaled[Wrote] =
    Journaled.json(w => ujson.Str(w.label), v => v.strOpt.map(Wrote(_)).toRight("no label"))
}

/** What a dream run came to: how many documents it `written`, `withdrawn` and found
  * `unchanged`; how many strands it `left` for the next run (past its most, or changed since
  * its scan); the rooms it read at a label no schedule declares (`undeclared`); the strands
  * not written, each by its root, and why; and why it stopped, if it did. [[line]] is its
  * reply.
  */
final case class Dreamt(
    written: Int,
    withdrawn: Int,
    unchanged: Int,
    left: Int,
    undeclared: Int,
    unwritten: Vector[(ConversationId, String)],
    stopped: Option[String]
) extends caps.Pure {

  /** `strands: {written} written, {withdrawn} withdrawn, {unchanged} unchanged, {left plus
    * unwritten} left for the next run`, then, when some were, `; {n} rooms at a label no
    * schedule declares`, then `; not written: {root} ({why})` for at most [[Dreamt.Shown]] of
    * them, separated by `, `, and `and {n} more` for the rest; then, when it stopped, `;
    * stopped: {why}`.
    */
  def line: String = {
    val counts = s"strands: $written written, $withdrawn withdrawn, $unchanged unchanged, " +
      s"${left + unwritten.size} left for the next run"
    val rooms =
      if (undeclared == 0) ""
      else if (undeclared == 1) "; 1 room at a label no schedule declares"
      else s"; $undeclared rooms at a label no schedule declares"
    val named = unwritten.take(Dreamt.Shown).map((r, why) => s"${ConversationId.value(r)} ($why)")
    val more = unwritten.size - named.size
    val notWritten =
      if (unwritten.isEmpty) ""
      else s"; not written: ${(named ++ Option.when(more > 0)(s"and $more more")).mkString(", ")}"
    counts + rooms + notWritten + stopped.fold("")(why => s"; stopped: $why")
  }

  private[strand] def wrote: Dreamt = copy(written = written + 1)

  private[strand] def changed: Dreamt = copy(left = left + 1)

  private[strand] def unwritten(root: ConversationId, why: String): Dreamt =
    copy(unwritten = unwritten :+ (root, why))
}

object Dreamt {

  /** 5: the most unwritten strands a reply names. */
  val Shown: Int = 5

  private[strand] def of(scan: Scanned): Dreamt =
    Dreamt(0, scan.withdrew, scan.unchanged, scan.left, scan.undeclared, Vector.empty, None)

  private[strand] def stopped(why: String): Dreamt =
    Dreamt(0, 0, 0, 0, 0, Vector.empty, Some(why))
}
