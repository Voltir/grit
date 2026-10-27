package grit.knowledge.declassify

import java.time.Instant

import grit.core.act.{Declassifying, Dreaming, MoveError, MoveLimits, MoveName}
import grit.core.document.{Declassified, DocumentKeeper, Pointer, PolicyApplication}
import grit.core.durable.Journaled
import grit.core.either.Traversal.traverse
import grit.core.flow.Trusted
import grit.core.id.{DocumentVersionId, JobName}
import grit.core.job.{DreamingJob, JobRun}
import grit.core.need.Exported
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.{DeclassificationPolicy, Integrity}
import grit.knowledge.card.{CardMoves, CardShelf, NotWritten, UnitVersion}

/** What a declassify run is told: at most how many cards it rewrites. */
final case class DeclassifyParams(perRun: Int) extends caps.Pure

/** The job `cards-declassify`, version 3, over the cards `cards` exports. A run reads at most
  * [[CardDeclassify.Cards]] current cards, most recent first, by their pointers and their files'
  * revisions alone, and the application in force for each
  * ([[grit.core.document.DocumentKeeper.declassifying]]). In its scan it withdraws every one of
  * its plugin's own documents but its withholding ([[Withheld]]) whose data does not read as a
  * rewrite of a card's current version and revision that stands under the application in force
  * for it ([[Declassified.Made.stands]]: made under its policy version in force, or, until that
  * version withholds the card, an earlier one it was published to keep), whatever wrote it, and
  * forgets each entry it withheld that is not at the versions in force. Then it declassifies ([[Dreaming.declassify]], `rewrite-{i}`, from
  * the card's file's text at its revision) each card in force that has neither a rewrite nor a
  * withholding at those versions, at most `perRun` (and at most `perRunMost`), most recent
  * first, the rest left for the next run: a rewrite made is kept below its floor (`keep-{i}`,
  * [[grit.core.document.DocumentKeeper.declassified]]), replacing its card's earlier one; one
  * the check kept back, or the model withheld, is remembered at its floor ([[Withheld]],
  * `withhold-{i}`) so it is not asked again at those versions; one whose file is not yet pieced
  * at its revision is tried again next run. Before any card, it benches each version in force
  * over its cards that has no bench of its checks as they are now ([[Dreaming.bench]],
  * `bench-{i}`; [[grit.core.act.Declassifying.unbenched]]), so a card is rewritten under it
  * in the same run once its bench passes. Replies [[Tally.line]]. Its `limits`, so that
  * `perRunMost` declassifies each retried fit beside the benches: asks, twice `perRunMost` (a
  * rewrite and its retry); judgments, nine times (both attempts' screens and their source's,
  * three; and room for the benches' asks, at most seven a version); no calls.
  */
final class CardDeclassify(perRunMost: Int, cards: Exported[CardShelf])
    extends DreamingJob[DeclassifyParams] {

  val name: JobName = CardDeclassify.Name

  val version: Int = 3

  def write(params: DeclassifyParams): ujson.Value = ujson.Obj("perRun" -> params.perRun)

  def read(params: ujson.Value): Either[String, DeclassifyParams] =
    params.objOpt
      .flatMap(_.get("perRun"))
      .flatMap(_.numOpt)
      .filter(n => n.isWhole && n >= 1 && n <= Int.MaxValue)
      .map(n => DeclassifyParams(n.toInt))
      .toRight(
        s"a declassify's parameters hold perRun, a whole number at least 1: ${ujson.write(params).take(100)}"
      )

  override val limits: MoveLimits = {
    val most = perRunMost.max(0)
    MoveLimits.of(2 * most, 0, 9 * most).getOrElse(MoveLimits.Zero)
  }

  def run(run: Trusted[JobRun[DeclassifyParams]], moves: Dreaming^): String = {
    val most = run.extract.params.perRun.min(limits.asks)
    moves.scan(CardDeclassify.Scan, run.tainted.map(_.params))((_, keeper, _, needs, at) =>
      needs.of(cards) match {
        case Left(u) =>
          Left(
            StoreError.Invalid(s"its plugin does not need ${u.dependency}, which keeps its cards")
          )
        case Right(shelf) => CardDeclassify.scanned(shelf, keeper, most, at)
      }
    ) match {
      case Left(e) => Tally.stopped(s"scanning its cards: ${MoveError.said(e)}").line
      case Right(scan) =>
        val benched = scan.bench.zipWithIndex.foldLeft(Tally.of(scan)) { case (done, (p, i)) =>
          done.benched(
            p,
            moves.bench(CardMoves.numbered("bench", i), p) match {
              case Left(e) => s"not benched: ${MoveError.said(e)}"
              case Right(Declassifying.Benching.Kept(b)) =>
                b.line.fold("benched")(l => s"unchecked: ${l.said}")
              case Right(
                    Declassifying.Benching.Unbenched(Declassifying.Unbenched.Unreadable(why))
                  ) =>
                s"unchecked: ${why.said}"
              case Right(Declassifying.Benching.Unbenched(Declassifying.Unbenched.Lapsed)) =>
                "not read"
            }
          )
        }
        scan.due.zipWithIndex
          .foldLeft(benched) { case (done, (Due(source, revision, applied), i)) =>
            moves.declassify(CardMoves.numbered("rewrite", i), source, revision, applied) match {
              case Left(e) => done.unwritten(source, MoveError.said(e))
              case Right(Declassifying.Outcome.Unapplied(Declassifying.Unread.Lapsed)) =>
                done.lapsed
              case Right(Declassifying.Outcome.Unapplied(Declassifying.Unread.Unpieced)) =>
                done.lagging
              case Right(Declassifying.Outcome.Unapplied(Declassifying.Unread.Unchecked)) =>
                done.unscreened
              case Right(Declassifying.Outcome.Kept(_)) =>
                val made = Declassified.Made.of(source, revision, applied)
                moves.keep(CardMoves.numbered("withhold", i), run.tainted.map(_ => made))(
                  (m, keeper, at) => Withheld.joined(keeper, m, at)
                ) match {
                  case Left(e) => done.unwritten(source, MoveError.said(e))
                  case Right(_) => done.heldBack
                }
              case Right(Declassifying.Outcome.Made(rewrite)) =>
                moves.keep(CardMoves.numbered("keep", i), rewrite)((d, keeper, at) =>
                  keeper.declassified(d, Integrity.Authority, at).map(_ => Kept)
                ) match {
                  case Left(e) => done.unwritten(source, MoveError.said(e))
                  case Right(_) => done.wrote
                }
            }
          }
          .line
    }
  }
}

object CardDeclassify {

  val Name: JobName =
    JobName.of("cards-declassify").fold(e => throw new IllegalStateException(e), identity)

  /** 1,000: the most current cards one run reads to find those a policy is applied to. */
  val Cards: Int = 1000

  private val Scan: MoveName = MoveName("scan")

  /** The scan, at its transaction's floor: each card in force (its version, revision and
    * application), every own document but the withholding that reads as no rewrite standing
    * under them withdrawn, every withholding entry not at them forgotten, and those due (with
    * neither a rewrite nor a withholding at them), at most `most`, most recent first; and the
    * versions over them due a bench.
    */
  private def scanned(shelf: CardShelf, keeper: DocumentKeeper, most: Int, at: Instant)(using
      tx: Tx^
  ): Either[StoreError, Scanned] = {
    val floor = Tx.floor(tx)
    for {
      pointed <- shelf.pointed(Cards)
      applied <- traverse(pointed)(c =>
        keeper
          .declassifying(c.label, c.pointer)
          .map(_.map(a => (Due(c.version, UnitVersion.value(c.revision), a.applied), a)))
      )
      inForce = applied.flatten.map(_._1)
      bench <- traverse(inForce.map(_.applied.policy.over).distinct)(
        Declassifying.unbenched(keeper, _)
      )
      current = inForce.map(_.made).toSet
      held <- Withheld.read(keeper, floor)
      decided = held.flatten.toSet
      cards = applied.flatten.map((d, a) => d.source -> (d, a)).toMap
      own <- keeper.newest(Int.MaxValue)
      (rewrites, stale) = own
        .filterNot(_.key == Withheld.Key)
        .partition(d =>
          Declassified.Made
            .read(d.data)
            .exists(m =>
              cards
                .get(m.source)
                .exists((due, a) =>
                  m.stands(a, due.source, due.revision, decided.contains(due.made))
                )
            )
        )
      withdrawn <- stale.foldLeft[Either[StoreError, Int]](Right(0))((n, d) =>
        n.flatMap(k => keeper.withdraw(d.key, d.label, at).map(v => k + v.size))
      )
      kept = held.flatten.filter(current.contains)
      _ <- if (kept.size == held.size) Right(Kept) else Withheld.kept(keeper, floor, kept, at)
      done = rewrites.flatMap(d => Declassified.Made.read(d.data)).toSet ++ kept
      due = inForce.filterNot(d => done.contains(d.made))
    } yield Scanned(due.take(most), withdrawn, (due.size - most).max(0), bench.flatten)
  }
}

/** A card due a rewrite: its `source` version, its file's `revision`, and the application in
  * force for it.
  */
private[declassify] final case class Due(
    source: DocumentVersionId,
    revision: String,
    applied: PolicyApplication
) extends caps.Pure {

  /** A rewrite of it, as its data would name it. */
  def made: Declassified.Made = Declassified.Made.of(source, revision, applied)
}

/** What a declassify's scan came to: the cards `due` a rewrite; how many rewrites it
  * `withdrew`; how many due it `left` past its most; and the policy versions due a `bench`.
  */
private[declassify] final case class Scanned(
    due: Vector[Due],
    withdrew: Int,
    left: Int,
    bench: Vector[DeclassificationPolicy]
) extends caps.Pure

private[declassify] object Scanned {

  /** Its record: `{"due": [{"source", "revision", "over", "policy", "application",
    * "pointer"}...], "withdrew", "left", "bench": [{"over", "policy"}...]}`, the pointer as
    * [[Pointer.written]] writes it; a record without `bench` benched none.
    */
  given Journaled[Scanned] = Journaled.json(
    s =>
      ujson.Obj(
        "due" -> ujson.Arr.from(s.due.map { d =>
          val due = d.made.written
          due("pointer") = d.applied.document.written
          due
        }),
        "withdrew" -> s.withdrew,
        "left" -> s.left,
        "bench" -> ujson.Arr.from(
          s.bench.map(p => ujson.Obj("over" -> p.over.written, "policy" -> p.version))
        )
      ),
    v => {
      def count(key: String): Either[String, Int] =
        v.objOpt
          .flatMap(_.get(key))
          .flatMap(_.numOpt)
          .map(_.toInt)
          .toRight(s"a declassify's scan holds no count '$key'")
      def due(d: ujson.Value): Either[String, Due] =
        for {
          made <- Declassified.Made.read(d).toRight("a declassify's due card does not read")
          pointer <- d.objOpt
            .flatMap(_.get("pointer"))
            .toRight("a declassify's due card has no pointer")
            .flatMap(Pointer.read)
        } yield Due(
          made.source,
          made.revision,
          PolicyApplication(
            DeclassificationPolicy(made.over, made.policy),
            pointer,
            made.application
          )
        )
      for {
        dues <- v.objOpt
          .flatMap(_.get("due"))
          .flatMap(_.arrOpt)
          .toRight("a declassify's scan holds no cards")
          .flatMap(ds => traverse(ds.toVector)(due))
        withdrew <- count("withdrew")
        left <- count("left")
        bench <- v.objOpt.flatMap(_.get("bench")).flatMap(_.arrOpt) match {
          case None => Right(Vector.empty)
          case Some(ps) =>
            traverse(ps.toVector)(p =>
              for {
                over <- p.objOpt
                  .flatMap(_.get("over"))
                  .flatMap(_.strOpt)
                  .toRight("a declassify's bench has no compartment")
                  .flatMap(DeclassificationPolicy.Over.read)
                version <- p.objOpt
                  .flatMap(_.get("policy"))
                  .flatMap(_.numOpt)
                  .map(_.toInt)
                  .toRight("a declassify's bench has no version")
              } yield DeclassificationPolicy(over, version)
            )
        }
      } yield Scanned(dues, withdrew, left, bench)
    }
  )
}

/** A keep's result: that it kept. */
private[declassify] case object Kept extends caps.Pure {
  given Journaled[Kept.type] =
    Journaled.json(_ => ujson.True, v => Either.cond(v == ujson.True, Kept, "no keep"))
}

/** What a declassify run came to: how many rewrites it `written`, `withheld` (the check kept
  * them back, or the model wrote nothing that may be shared), found `unapplied` since its scan,
  * found `unpieced` (no pieces of their file at their card's revision yet), found `unchecked`
  * (their policy version in force has no passing bench) and `withdrawn`; how
  * many cards it `left` for the next run past its most; the cards not rewritten, each by its
  * version, and why; each policy version it `benched`, and how; and why it stopped, if it did.
  * [[line]] is its reply.
  */
final case class Tally(
    written: Int,
    withheld: Int,
    unapplied: Int,
    unpieced: Int,
    unchecked: Int,
    withdrawn: Int,
    left: Int,
    unwritten: Vector[(DocumentVersionId, String)],
    benches: Vector[(DeclassificationPolicy, String)],
    stopped: Option[String]
) extends caps.Pure {

  /** `declassified cards: {written} written, {withheld} withheld, {unapplied} unapplied,
    * {unpieced} unpieced, {unchecked} unchecked, {withdrawn} withdrawn, {left plus unwritten}
    * left for the next run`; then for each bench `; policy v{n} {how}`: `benched` when it
    * passed, `unchecked: {its first failing example}` ([[grit.core.screen.Benched.Line]]) or
    * `unchecked: {its checks' fault}`, `not read` when the run does not read its checks, or
    * `not benched: {why}`; then `; not written: {version} ({why})` for at most
    * [[NotWritten.Shown]] of them and `and {n} more` for the rest; then, when it stopped,
    * `; stopped: {why}`.
    */
  def line: String =
    s"declassified cards: $written written, $withheld withheld, $unapplied unapplied, " +
      s"$unpieced unpieced, $unchecked unchecked, $withdrawn withdrawn, " +
      s"${left + unwritten.size} left for the next run" +
      benches.map((p, how) => s"; policy v${p.version} $how").mkString +
      NotWritten.tail(unwritten.map((v, why) => (DocumentVersionId.value(v).toString, why))) +
      NotWritten.stopped(stopped)

  private[declassify] def wrote: Tally = copy(written = written + 1)

  private[declassify] def heldBack: Tally = copy(withheld = withheld + 1)

  private[declassify] def lapsed: Tally = copy(unapplied = unapplied + 1)

  private[declassify] def lagging: Tally = copy(unpieced = unpieced + 1)

  private[declassify] def unscreened: Tally = copy(unchecked = unchecked + 1)

  private[declassify] def benched(policy: DeclassificationPolicy, how: String): Tally =
    copy(benches = benches :+ (policy, how))

  private[declassify] def unwritten(source: DocumentVersionId, why: String): Tally =
    copy(unwritten = unwritten :+ (source, why))
}

object Tally {
  private[declassify] def of(scan: Scanned): Tally =
    Tally(0, 0, 0, 0, 0, scan.withdrew, scan.left, Vector.empty, Vector.empty, None)

  private[declassify] def stopped(why: String): Tally =
    Tally(0, 0, 0, 0, 0, 0, 0, Vector.empty, Vector.empty, Some(why))
}
