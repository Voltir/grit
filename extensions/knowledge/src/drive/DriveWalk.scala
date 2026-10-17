package grit.knowledge.drive

import scala.annotation.tailrec

import grit.core.document.Pointer
import grit.core.visibility.Label
import grit.knowledge.card.{Card, CardKey, CardSummary, UnitVersion}
import grit.knowledge.sync.{Fetched, Held, Outcome, Step, Synced, Unwritten, Walk, Way}

/** One run's walk of a [[DriveCards]] source at its floor, as data: from the cards it holds,
  * each next step. It lists the shared drives first, [[Way.Run]] (the run's first read, so
  * nothing is read before it); then each drive [[Way.Through]] that listing, its files read
  * before the next drive; then the folders directly in the drives, each through its drive's
  * listing, in the order listed. A folder within a folder is not listed. A run lists at most
  * `maxUnits` folders: when the drives hold more, it lists the `maxUnits` from index
  * `turn × maxUnits` of them, counted around the end, so runs at consecutive turns list them
  * all within ⌈folders / maxUnits⌉ runs while the folders stay the same.
  *
  * A listing is the run's to card when [[DriveCards.carding]] of the label its answer was read
  * at is the floor; another's is that run's, and of it the walk only follows its folders. Of a
  * listing it cards, a Doc, a Sheet or a text file is read, through that listing, unless its card
  * held at the floor records its change time and that listing; its card is written at the
  * change time it was read at, recording the listing, titled with its name when the listing
  * shows one: from a summary of its first 8 KB, or, when its text is blank, as `An empty file.`
  * with no ask. A
  * card held only at another label is replaced by that write, its copy there withdrawn in the
  * same keep. A current card held at the floor but kept elsewhere than its source's place
  * ([[Held.elsewhere]]) is relabelled from the floor, so it is kept again at that place with no
  * read and no ask. It reads at most `perRun` files, so asks for at most as many summaries: past them
  * a changed file is left, and the listings go on. A new file (none of its cards held) is not taken once the units held and
  * those written new reach `maxUnits`. A file read with no change time, or whose list or read
  * came to [[Outcome.NotTaken]], is [[Unwritten]].
  *
  * Its last step is a [[Step.Drop]], when there is anything to drop, of the cards held at the
  * floor whose files another run's listing showed; of those no listing showed, only when the
  * listing each records was made and complete; of those held at a label not among the source's
  * whose [[DriveCards.carding]] is the floor; and, past `maxUnits` (when it was lowered), of the
  * files last by id, so the source holds no more. A step that came to [[Outcome.Stopped]], or to an
  * outcome not of its kind, ends the walk at once, with no drop.
  */
final class DriveWalk private (
    private val env: DriveWalk.Env,
    private val state: DriveWalk.State
) extends Walk[DriveListing] {

  def next: Either[Synced, Step] = state.next

  def after(outcome: Outcome[DriveListing]): DriveWalk =
    new DriveWalk(env, DriveWalk.after(env, state, outcome))
}

object DriveWalk {

  /** `/`: the path of the shared drives' listing, which no Drive id is. */
  val Drives: String = "/"

  /** The walk of `cards` for a run reading at most `perRun` files and keeping at `floor`,
    * holding `held`, its source's cards as [[Held.read]] reads them, at `turn`
    * ([[DriveCards.turn]] of its slot), which picks the folders it lists.
    */
  def start(
      cards: DriveCards,
      perRun: Int,
      floor: Label,
      held: Vector[Held],
      turn: Long
  ): DriveWalk = {
    val env = Env(cards, perRun, floor, held, held.groupBy(_.path), turn)
    new DriveWalk(env, advance(env, State.fresh(env)))
  }

  private val Empty: CardSummary =
    CardSummary
      .of("An empty file.", Vector.empty)
      .fold(why => throw new IllegalStateException(why), identity)

  private final case class Env(
      cards: DriveCards,
      perRun: Int,
      floor: Label,
      held: Vector[Held],
      byPath: Map[String, Vector[Held]],
      turn: Long
  ) extends caps.Pure {

    private def copies(id: String): Vector[Held] = byPath.getOrElse(id, Vector.empty)

    /** The label of the copy a write of `id` replaces: none when the floor holds one. */
    def replacing(id: String): Option[Label] =
      if (copies(id).exists(_.label == floor)) None else copies(id).headOption.map(_.label)

    /** Whether the copy of `entry` held at the floor records its change time and `within`, the
      * listing that shows it.
      */
    def current(entry: DriveEntry, within: String): Boolean =
      entry.changed.exists(t =>
        copies(entry.id).exists(h =>
          h.label == floor && h.version == UnitVersion.Changed(t) && h.within.contains(within)
        )
      )

    /** Whether the copy of `id` held at the floor is kept elsewhere than its source's place. */
    def elsewhere(id: String): Boolean = copies(id).exists(h => h.label == floor && h.elsewhere)

    def isNew(id: String): Boolean = !byPath.contains(id)

    /** Whether a listing read at `at` is this run's to card. */
    def carded(at: Label): Boolean = cards.carding(at).contains(floor)
  }

  /** What a listing lists. */
  private enum Container extends caps.Pure {
    case Shared
    case Drive(id: String)
    case Folder(id: String, drive: String)

    def path: String = this match {
      case Shared => Drives
      case Drive(id) => id
      case Folder(id, _) => id
    }

    def way: Way = this match {
      case Shared => Way.Run
      case Drive(_) => Way.Through(Drives)
      case Folder(_, drive) => Way.Through(drive)
    }
  }

  /** What is still to do, in walk order: [[Task.Folders]], once every drive is listed, picks
    * the folders to list.
    */
  private enum Task extends caps.Pure {
    case List(of: Container)
    case Read(entry: DriveEntry, within: String)
    case Relabel(id: String)
    case Folders
  }

  /** What the last step's outcome is read as. */
  private enum Waiting extends caps.Pure {
    case Done
    case Listing(of: Container)
    case Reading(entry: DriveEntry, within: String, pointer: Pointer)
    case Summary(entry: DriveEntry, within: String, version: UnitVersion, pointer: Pointer)
    case Wrote(id: String)
    case Relabelled(id: String)
    case Dropped(count: Int)
  }

  private final case class State(
      stack: List[Task],
      waiting: Waiting,
      next: Either[Synced, Step],
      reads: Int,
      fresh: Int,
      found: Vector[Container.Folder],
      written: Int,
      relabelled: Int,
      withdrawn: Int,
      unchanged: Int,
      left: Int,
      refused: Int,
      unwritten: Vector[Unwritten],
      seen: Set[String],
      foreign: Set[String],
      wrote: Set[String],
      complete: Set[String],
      stopped: Option[String]
  ) extends caps.Pure {
    def synced(env: Env): Synced =
      Synced(
        env.cards.corpus,
        written,
        relabelled,
        withdrawn,
        unchanged,
        left,
        refused,
        unwritten,
        stopped
      )
    def step(s: Step, w: Waiting): State = copy(next = Right(s), waiting = w)
  }

  private object State {
    def fresh(env: Env): State =
      State(
        List(Task.List(Container.Shared)),
        Waiting.Done,
        Left(Synced(env.cards.corpus, 0, 0, 0, 0, 0, 0, Vector.empty, None)),
        0,
        0,
        Vector.empty,
        0,
        0,
        0,
        0,
        0,
        0,
        Vector.empty,
        Set.empty,
        Set.empty,
        Set.empty,
        Set.empty,
        None
      )
  }

  private def after(env: Env, s: State, outcome: Outcome[DriveListing]): State =
    (s.waiting, outcome) match {
      case (Waiting.Done, _) => s
      case (_, Outcome.Stopped(why)) => halt(env, s, why)
      case (Waiting.Listing(of), Outcome.Listed(listing, at)) =>
        advance(env, listed(env, s, of, listing, at))
      case (Waiting.Listing(of), Outcome.NotTaken(why)) =>
        advance(env, unwritten(s, of.path, why))
      case (Waiting.Reading(entry, _, _), Outcome.NotTaken(why)) =>
        advance(env, unwritten(s, entry.id, why))
      case (Waiting.Reading(entry, within, pointer), Outcome.Fetched(fetched, _)) =>
        fetched match {
          case Fetched.Text(text, version) if text.isBlank =>
            write(env, s, card(env, entry, within, version, Empty, pointer))
          case Fetched.Text(text, version) =>
            s.step(
              Step.Summarise(entry.id, DriveCards.subject(entry), Fetched.head(text), version),
              Waiting.Summary(entry, within, version, pointer)
            )
          case Fetched.Unversioned(why) => advance(env, unwritten(s, entry.id, why))
        }
      case (Waiting.Summary(entry, within, version, pointer), Outcome.Summarised(summary)) =>
        write(env, s, card(env, entry, within, version, summary, pointer))
      case (Waiting.Wrote(id), Outcome.Kept) =>
        advance(
          env,
          s.copy(written = s.written + 1, fresh = if (env.isNew(id)) s.fresh + 1 else s.fresh)
        )
      case (Waiting.Relabelled(_), Outcome.Kept) =>
        advance(env, s.copy(relabelled = s.relabelled + 1))
      case (Waiting.Dropped(count), Outcome.Kept) => done(env, s.copy(withdrawn = count))
      case (waiting, other) => halt(env, s, s"${expected(waiting)} came to ${kind(other)}")
    }

  private def expected(waiting: Waiting): String = waiting match {
    case Waiting.Listing(_) => "a list"
    case Waiting.Reading(_, _, _) => "a read"
    case Waiting.Summary(_, _, _, _) => "a summary"
    case Waiting.Wrote(_) => "a write"
    case Waiting.Relabelled(_) => "a relabel"
    case Waiting.Dropped(_) | Waiting.Done => "a drop"
  }

  private def kind(outcome: Outcome[DriveListing]): String = outcome match {
    case Outcome.Listed(_, _) => "a listing"
    case Outcome.Fetched(_, _) => "a read's outcome"
    case Outcome.Summarised(_) => "a summary"
    case Outcome.Kept => "a keep's outcome"
    case Outcome.NotTaken(_) => "a withholding"
    case Outcome.Stopped(_) => "a stop"
  }

  /** `s` once `of` was listed, read at `at`: the drives it names, pushed in the order listed
    * before the pick of folders; or its files to read, and, a drive's, its folders found.
    */
  private def listed(
      env: Env,
      s: State,
      of: Container,
      listing: DriveListing,
      at: Label
  ): State = {
    val complete = if (listing.complete) s.complete + of.path else s.complete
    of match {
      case Container.Shared =>
        val drives = listing.entries
          .filter(_.kind == DriveEntry.Kind.Drive)
          .map(e => Task.List(Container.Drive(e.id)))
        s.copy(stack = drives.toList ++ (Task.Folders :: s.stack), complete = complete)
      case Container.Drive(id) =>
        val within = listing.entries
          .filter(_.kind == DriveEntry.Kind.Folder)
          .map(f => new Container.Folder(f.id, id))
        files(env, s.copy(found = s.found ++ within, complete = complete), id, listing, at)
      case Container.Folder(id, _) =>
        files(env, s.copy(complete = complete), id, listing, at)
    }
  }

  /** Of `found`, in the order found, the folders a run at `turn` lists: every one when they are
    * at most `most`; else the `most` from index `turn × most`, counted around the end.
    */
  private def picked(
      found: Vector[Container.Folder],
      most: Int,
      turn: Long
  ): Vector[Container.Folder] =
    if (found.size <= most) found
    else {
      val n = found.size.toLong
      val from = (turn % n) * most % n
      val at = (0 until most).map(i => (from + i) % n).toSet
      found.zipWithIndex.collect { case (f, i) if at(i.toLong) => f }
    }

  /** `s` with the files `listing` of `within` shows seen, and, when it is this run's to card,
    * those changed pushed to read and those current but kept elsewhere to relabel.
    */
  private def files(
      env: Env,
      s: State,
      within: String,
      listing: DriveListing,
      at: Label
  ): State = {
    val readable = listing.entries.filter(_.kind.readable)
    val seen = s.seen ++ readable.map(_.id)
    if (env.carded(at)) {
      val (current, changed) = readable.partition(env.current(_, within))
      val (moved, unchanged) = current.partition(e => env.elsewhere(e.id))
      s.copy(
        stack = (moved.map(e => Task.Relabel(e.id)) ++ changed.map(Task.Read(_, within))).toList ++
          s.stack,
        seen = seen,
        unchanged = s.unchanged + unchanged.size
      )
    } else
      s.copy(
        seen = seen,
        foreign = s.foreign ++ readable.map(_.id)
      )
  }

  private def card(
      env: Env,
      entry: DriveEntry,
      within: String,
      version: UnitVersion,
      summary: CardSummary,
      pointer: Pointer
  ): Card =
    Card.file(env.cards.corpus, entry.id, version, summary, pointer, entry.name, Some(within))

  private def unwritten(s: State, id: String, why: String): State =
    s.copy(unwritten = s.unwritten :+ Unwritten(id, why))

  private def write(env: Env, s: State, card: Card): State =
    s.copy(wrote = s.wrote + card.path)
      .step(Step.Write(card, env.replacing(card.path)), Waiting.Wrote(card.path))

  /** Pops tasks until one makes a step, or none is left. */
  @tailrec private def advance(env: Env, s: State): State = s.stack match {
    case Nil => end(env, s)
    case task :: rest =>
      val popped = s.copy(stack = rest)
      task match {
        case Task.List(of) => popped.step(Step.List(of.path, of.way), Waiting.Listing(of))
        case Task.Folders =>
          val lists = picked(s.found, env.cards.maxUnits, env.turn).map(Task.List(_))
          advance(env, popped.copy(stack = lists.toList ++ popped.stack))
        case Task.Read(entry, within) =>
          if (env.isNew(entry.id) && env.byPath.size + s.fresh >= env.cards.maxUnits)
            advance(env, popped.copy(refused = popped.refused + 1))
          else if (s.reads >= env.perRun) advance(env, popped.copy(left = popped.left + 1))
          else
            env.cards.pointer(entry.id) match {
              case Left(why) => advance(env, unwritten(popped, entry.id, why))
              case Right(pointer) =>
                popped
                  .copy(reads = popped.reads + 1)
                  .step(
                    Step.Read(entry.id, Way.Through(within)),
                    Waiting.Reading(entry, within, pointer)
                  )
            }
        case Task.Relabel(id) =>
          popped.step(
            Step.Relabel(CardKey.of(env.cards.corpus, id), env.floor),
            Waiting.Relabelled(id)
          )
      }
  }

  /** `s` come to its end: its drop, when anything is gone, and then its result. */
  private def end(env: Env, s: State): State = {
    val gone = drops(env, s)
    if (gone.isEmpty) done(env, s) else s.step(Step.Drop(gone), Waiting.Dropped(gone.size))
  }

  /** `s` stopped by a step's outcome, saying `why`: its result at once, dropping nothing, each
    * file still to read or relabel counted as left.
    */
  private def halt(env: Env, s: State, why: String): State = {
    val reads = s.stack.count {
      case Task.Read(_, _) | Task.Relabel(_) => true
      case Task.List(_) | Task.Folders => false
    }
    done(env, s.copy(stack = Nil, left = s.left + reads, stopped = Some(why)))
  }

  private def done(env: Env, s: State): State =
    s.copy(next = Left(s.synced(env)), waiting = Waiting.Done)

  /** The cards held at the floor of files another run's listing showed, or that no listing
    * showed though the one each records it was shown in was made and complete; those at a label
    * not among the source's that the floor's run cards; and every copy of the files past
    * `maxUnits`, last by id; but each copy a write replaced.
    */
  private def drops(env: Env, s: State): Vector[Held] = {
    def replaced(h: Held) = s.wrote(h.path) && env.replacing(h.path).contains(h.label)
    val gone = env.held.filter { h =>
      if (h.label == env.floor)
        s.foreign(h.path) || (h.within.exists(s.complete) && !s.seen(h.path))
      else
        !env.cards.labels.contains(h.label) &&
        env.cards.carding(h.label).contains(env.floor) &&
        !replaced(h)
    }
    // Past maxUnits (it was lowered): the files last by id go, so the source holds no more.
    val dropped = gone.toSet
    val kept = env.held.filterNot(dropped).map(_.path).distinct.sorted
    val over = kept.drop(math.max(env.cards.maxUnits - s.fresh, 0)).toSet
    env.held.filter(h => dropped(h) || (over(h.path) && !replaced(h)))
  }
}
