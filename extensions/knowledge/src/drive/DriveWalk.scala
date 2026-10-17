package grit.knowledge.drive

import scala.annotation.tailrec

import grit.core.document.Pointer
import grit.core.visibility.Label
import grit.knowledge.card.{Card, CardSummary, UnitVersion}
import grit.knowledge.sync.{Fetched, Held, Outcome, Step, Synced, Unwritten, Walk, Way}

/** One run's walk of a [[DriveCards]] source at its floor, as data: from the cards it holds,
  * each next step. It lists the shared drives first, [[Way.Run]] (the run's first read, so
  * nothing is read before it); then each drive [[Way.Through]] that listing, and each folder
  * directly in a drive through the drive's listing, in the order listed: a drive's files, then
  * its folders, before the next drive. A folder within a folder is not listed. At most
  * `maxUnits` folders are listed in a run; one past that is not listed.
  *
  * A listing is the run's to card when [[DriveCards.carding]] of the label its answer was read
  * at is the floor; another's is that run's, and of it the walk only follows its folders. Of a
  * listing it cards, a Doc, a Sheet or a text file is read, through that listing, unless its card
  * held at the floor records its change time and that listing; its card is written at the
  * change time it was read at, recording the listing, titled with its name when the listing
  * shows one: from a summary of its first 8 KB, or, when its text is blank, as `An empty file.`
  * with no ask. A
  * card held only at another label is replaced by that write, its copy there withdrawn in the
  * same keep. It asks for at most `perRun` summaries: past them a changed file is left, and the
  * listings go on. A new file (none of its cards held) is not taken once the units held and
  * those written new reach `maxUnits`. A file read with no change time, or whose list or read
  * came to [[Outcome.NotTaken]], is [[Unwritten]].
  *
  * Its last step is a [[Step.Drop]], when there is anything to drop, of the cards held at the
  * floor whose files another run's listing showed; of those no listing showed, only when the
  * listing each records was made and complete; and of those held at a label not among the source's whose
  * [[DriveCards.carding]] is the floor. A step that came to [[Outcome.Stopped]], or to an
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

  /** The walk of `cards` for a run asking for at most `perRun` summaries and keeping at `floor`,
    * holding `held`, its source's cards as [[Held.read]] reads them.
    */
  def start(cards: DriveCards, perRun: Int, floor: Label, held: Vector[Held]): DriveWalk = {
    val env = Env(cards, perRun, floor, held, held.groupBy(_.path))
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
      byPath: Map[String, Vector[Held]]
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

  /** What is still to do, in walk order. */
  private enum Task extends caps.Pure {
    case List(of: Container)
    case Read(entry: DriveEntry, within: String)
  }

  /** What the last step's outcome is read as. */
  private enum Waiting extends caps.Pure {
    case Done
    case Listing(of: Container)
    case Reading(entry: DriveEntry, within: String, pointer: Pointer)
    case Summary(entry: DriveEntry, within: String, version: UnitVersion, pointer: Pointer)
    case Wrote(id: String)
    case Dropped(count: Int)
  }

  private final case class State(
      stack: List[Task],
      waiting: Waiting,
      next: Either[Synced, Step],
      asked: Int,
      fresh: Int,
      folders: Int,
      written: Int,
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
      Synced(env.cards.corpus, written, 0, withdrawn, unchanged, left, refused, unwritten, stopped)
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
            s.copy(asked = s.asked + 1)
              .step(
                Step.Summarise(entry.id, Fetched.head(text), version),
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
      case (Waiting.Dropped(count), Outcome.Kept) => done(env, s.copy(withdrawn = count))
      case (waiting, other) => halt(env, s, s"${expected(waiting)} came to ${kind(other)}")
    }

  private def expected(waiting: Waiting): String = waiting match {
    case Waiting.Listing(_) => "a list"
    case Waiting.Reading(_, _, _) => "a read"
    case Waiting.Summary(_, _, _, _) => "a summary"
    case Waiting.Wrote(_) => "a write"
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

  /** `s` once `of` was listed, read at `at`: the drives it names, or its files to read and, a
    * drive's, its folders, pushed in the order listed.
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
        s.copy(stack = drives.toList ++ s.stack, complete = complete)
      case Container.Drive(id) =>
        val within = listing.entries.filter(_.kind == DriveEntry.Kind.Folder)
        val room = (env.cards.maxUnits - s.folders).max(0)
        val folders = within.take(room).map(f => Task.List(Container.Folder(f.id, id)))
        files(
          env,
          s.copy(folders = s.folders + folders.size, complete = complete),
          id,
          listing,
          at,
          folders
        )
      case Container.Folder(id, _) =>
        files(env, s.copy(complete = complete), id, listing, at, Vector.empty)
    }
  }

  /** `s` with the files `listing` of `within` shows seen, and, when it is this run's to card,
    * those changed pushed to read, then `then`.
    */
  private def files(
      env: Env,
      s: State,
      within: String,
      listing: DriveListing,
      at: Label,
      `then`: Vector[Task]
  ): State = {
    val readable = listing.entries.filter(_.kind.readable)
    val seen = s.seen ++ readable.map(_.id)
    if (env.carded(at)) {
      val (current, changed) = readable.partition(env.current(_, within))
      s.copy(
        stack = (changed.map(Task.Read(_, within)) ++ `then`).toList ++ s.stack,
        seen = seen,
        unchanged = s.unchanged + current.size
      )
    } else
      s.copy(
        stack = `then`.toList ++ s.stack,
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
        case Task.Read(entry, within) =>
          if (env.isNew(entry.id) && env.byPath.size + s.fresh >= env.cards.maxUnits)
            advance(env, popped.copy(refused = popped.refused + 1))
          else if (s.asked >= env.perRun) advance(env, popped.copy(left = popped.left + 1))
          else
            env.cards.pointer(entry.id) match {
              case Left(why) => advance(env, unwritten(popped, entry.id, why))
              case Right(pointer) =>
                popped.step(
                  Step.Read(entry.id, Way.Through(within)),
                  Waiting.Reading(entry, within, pointer)
                )
            }
      }
  }

  /** `s` come to its end: its drop, when anything is gone, and then its result. */
  private def end(env: Env, s: State): State = {
    val gone = drops(env, s)
    if (gone.isEmpty) done(env, s) else s.step(Step.Drop(gone), Waiting.Dropped(gone.size))
  }

  /** `s` stopped by a step's outcome, saying `why`: its result at once, dropping nothing, each
    * changed file still to read counted as left.
    */
  private def halt(env: Env, s: State, why: String): State = {
    val reads = s.stack.count {
      case Task.Read(_, _) => true
      case Task.List(_) => false
    }
    done(env, s.copy(stack = Nil, left = s.left + reads, stopped = Some(why)))
  }

  private def done(env: Env, s: State): State =
    s.copy(next = Left(s.synced(env)), waiting = Waiting.Done)

  /** The cards held at the floor of files another run's listing showed, or that no listing
    * showed though the one each records it was shown in was made and complete; and those at a
    * label not among the source's that the floor's run cards, but each copy a write replaced.
    */
  private def drops(env: Env, s: State): Vector[Held] =
    env.held.filter { h =>
      if (h.label == env.floor)
        s.foreign(h.path) || (h.within.exists(s.complete) && !s.seen(h.path))
      else
        !env.cards.labels.contains(h.label) &&
        env.cards.carding(h.label).contains(env.floor) &&
        !(s.wrote(h.path) && env.replacing(h.path).contains(h.label))
    }
}
