package grit.knowledge.github

import scala.annotation.tailrec

import grit.core.document.Pointer
import grit.core.visibility.Label
import grit.knowledge.card.{Card, CardKey, CardSummary, UnitKind, UnitVersion}
import grit.knowledge.sync.{Fetched, Held, Outcome, Step, Synced, Unwritten, Walk, Way}

/** One run's walk of a repository, as data: from the cards it holds, depth first by path from
  * the repository's root, `/`, each next step. Of the repository it walks the paths `include` names and,
  * of files, those whose extension is one of `extensions`; the directories on the way to an
  * included path are listed and carry no card.
  *
  * A directory whose held card records its tree SHA, made under the source's
  * [[grit.knowledge.github.GitHubCards.scope]], is not listed: each card held beneath it is
  * unchanged. One made under another scope is listed, so a changed `include` or `extensions`
  * reaches beneath it. A file is read only when its listed SHA is not its held card's, and its
  * card is written at the SHA it was read at: from a summary of its first 8 KB, or, when its
  * text is blank, as `An empty file.` with no ask. A directory's card is written from its
  * listing, with no ask, after its children, and only when each was brought up to date. A unit's
  * card held only at another label than the floor is relabelled when unchanged, and otherwise
  * written; either way its copy there is withdrawn in that keep, never before. A unit held at
  * the floor goes by that copy, whatever another label's records.
  *
  * It reads at most `perRun` files, so asks for at most as many summaries, and lists at most
  * `maxUnits` directories in scope (those on the way to an included path besides): at the next
  * read or listing past either, it stops, counting what it left. The
  * source holds at most `maxUnits` cards: a new unit (one with no card held) is not taken once
  * the units held and those written new reach it, and the walk goes on bringing the held up to
  * date. A file read with no version, a directory listed only in part (`listed only in part`),
  * a unit whose list or read came to [[Outcome.NotTaken]] (nothing beneath a withheld directory
  * is listed, read or dropped) and a unit whose path makes no pointer are [[Unwritten]]; they,
  * and a unit not taken, leave
  * the cards of the directories above them unwritten, so the next run lists those again.
  *
  * Its last step is a [[Step.Drop]], when there is anything to drop: of the cards of units gone
  * from directories it listed in full (or outside the source now, a directory it lists only as
  * on the way to an included path among them); of the copies at other labels of units it kept
  * current at the floor; and, past `maxUnits` (when it was lowered), of the units last in walk
  * order and the directories above them. A `Partial` listing, or a directory a stopped walk
  * never reached, drops nothing beneath it. A step that came to [[Outcome.Stopped]], or to an
  * outcome not of its kind, ends the walk at once, with no drop.
  */
final class RepositoryWalk private (
    private val env: RepositoryWalk.Env,
    private val state: RepositoryWalk.State
) extends Walk[Listing] {

  def next: Either[Synced, Step] = state.next

  def after(outcome: Outcome[Listing]): RepositoryWalk =
    new RepositoryWalk(env, RepositoryWalk.after(env, state, outcome))
}

object RepositoryWalk {

  /** The walk of `cards` for a run reading at most `perRun` files and keeping at `floor`,
    * holding `held`, its source's cards as [[Held.read]] reads them. Its first step lists the
    * repository's root, `/`; when `cards` includes nothing, it has none and holds nothing gone.
    */
  def start(cards: GitHubCards, perRun: Int, floor: Label, held: Vector[Held]): RepositoryWalk = {
    val env = Env(cards, perRun, floor, held, held.groupBy(_.key))
    val stack = if (cards.include.isEmpty) Nil else List(Task.Pass(Root))
    new RepositoryWalk(env, advance(env, State.fresh(env, stack)))
  }

  private val Root = "/"

  private val Empty: CardSummary = sure(CardSummary.of("An empty file.", Vector.empty))

  private final case class Env(
      cards: GitHubCards,
      perRun: Int,
      floor: Label,
      held: Vector[Held],
      byKey: Map[CardKey, Vector[Held]]
  ) extends caps.Pure {
    def keyOf(path: String): CardKey = CardKey.of(cards.corpus, path)

    /** The copy of `key` the walk goes by: the one at the floor, else the first held. */
    private def best(key: CardKey): Option[Held] = {
      val copies = byKey.getOrElse(key, Vector.empty)
      copies.find(_.label == floor).orElse(copies.headOption)
    }

    /** The label of the copy a write or relabel of `key` replaces: none when the floor holds it. */
    def replacing(key: CardKey): Option[Label] = best(key).map(_.label).filter(_ != floor)

    /** Whether the copy of `key` the walk goes by records `version`. */
    def holds(key: CardKey, version: UnitVersion): Boolean = best(key).exists(_.version == version)

    /** Whether no card of `key` is held. */
    def isNew(key: CardKey): Boolean = !byKey.contains(key)

    /** Whether `entry`'s held card is up to date: at its version and, a directory's, made under
      * the source's scope.
      */
    def current(entry: Entry): Boolean = {
      val key = keyOf(entry.path)
      holds(key, entry.version) &&
      (entry.kind == UnitKind.File || best(key).exists(_.scope.contains(cards.scope)))
    }

    private val include: Vector[String] = cards.include
    private val extensions: Set[String] = cards.extensions

    def inScope(entry: Entry): Boolean =
      include.exists(i => i.isEmpty || entry.path == i || entry.path.startsWith(i + "/")) &&
        (entry.kind == UnitKind.Directory || extensions.isEmpty || {
          val name = entry.path.substring(entry.path.lastIndexOf('/') + 1)
          val dot = name.lastIndexOf('.')
          dot > 0 && extensions.contains(name.substring(dot + 1))
        })

    /** Whether `entry` is a directory on the way to an included path. */
    def passage(entry: Entry): Boolean =
      entry.kind == UnitKind.Directory && include.exists(_.startsWith(entry.path + "/"))
  }

  /** What is still to do, in walk order. */
  private enum Task extends caps.Pure {

    /** A unit in scope, as its directory's listing gave it. */
    case Visit(entry: Entry)

    /** A directory on the way to an included path: listed, never carded. */
    case Pass(path: String)

    /** A held unit beneath a directory whose tree is unchanged. */
    case Known(key: CardKey)

    /** The card of the directory `path`, at `version`, holding `children`. */
    case Finish(path: String, version: UnitVersion, children: Vector[Entry])
  }

  /** What the last step's outcome is read as. */
  private enum Waiting extends caps.Pure {
    case Done
    case Listing(path: String, version: Option[UnitVersion])
    case Reading(path: String, pointer: Pointer)
    case Summary(path: String, version: UnitVersion, pointer: Pointer)
    case Wrote(key: CardKey)
    case Relabelled(key: CardKey)
    case Dropped(count: Int)
  }

  private final case class State(
      stack: List[Task],
      waiting: Waiting,
      next: Either[Synced, Step],
      reads: Int,
      lists: Int,
      fresh: Int,
      refused: Int,
      written: Int,
      relabelled: Int,
      withdrawn: Int,
      unchanged: Int,
      left: Int,
      unwritten: Vector[Unwritten],
      undone: Set[String],
      listed: Map[String, Set[String]],
      passed: Set[String],
      settled: Vector[CardKey],
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
    def fresh(env: Env, stack: List[Task]): State =
      State(
        stack,
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
        0,
        Vector.empty,
        Set.empty,
        Map.empty,
        Set.empty,
        Vector.empty,
        None
      )
  }

  private def after(env: Env, s: State, outcome: Outcome[Listing]): State =
    (s.waiting, outcome) match {
      case (Waiting.Done, _) => s
      case (_, Outcome.Stopped(why)) => halt(env, s, why)
      case (Waiting.Listing(path, version), Outcome.Listed(listing, _)) =>
        advance(env, listed(env, s, path, version, listing))
      case (Waiting.Listing(path, _), Outcome.NotTaken(why)) =>
        advance(env, unwritten(s, path, why))
      case (Waiting.Reading(path, _), Outcome.NotTaken(why)) =>
        advance(env, unwritten(s, path, why))
      case (Waiting.Reading(path, pointer), Outcome.Fetched(fetched, _)) =>
        fetched match {
          case Fetched.Text(text, sha) if text.isBlank =>
            write(env, s, Card.file(env.cards.corpus, path, sha, Empty, pointer))
          case Fetched.Text(text, sha) =>
            s.step(
              Step.Summarise(path, Fetched.head(text), sha),
              Waiting.Summary(path, sha, pointer)
            )
          case Fetched.Unversioned(why) => advance(env, unwritten(s, path, why))
        }
      case (Waiting.Summary(path, sha, pointer), Outcome.Summarised(summary)) =>
        write(env, s, Card.file(env.cards.corpus, path, sha, summary, pointer))
      case (Waiting.Wrote(key), Outcome.Kept) =>
        advance(
          env,
          s.copy(
            written = s.written + 1,
            fresh = if (env.isNew(key)) s.fresh + 1 else s.fresh,
            settled = s.settled :+ key
          )
        )
      case (Waiting.Relabelled(key), Outcome.Kept) =>
        advance(
          env,
          s.copy(relabelled = s.relabelled + 1, settled = s.settled :+ key)
        )
      case (Waiting.Dropped(count), Outcome.Kept) => done(env, s.copy(withdrawn = count))
      case (waiting, other) => halt(env, s, s"${expected(waiting)} came to ${kind(other)}")
    }

  private def expected(waiting: Waiting): String = waiting match {
    case Waiting.Listing(_, _) => "a list"
    case Waiting.Reading(_, _) => "a read"
    case Waiting.Summary(_, _, _) => "a summary"
    case Waiting.Wrote(_) => "a write"
    case Waiting.Relabelled(_) => "a relabel"
    case Waiting.Dropped(_) | Waiting.Done => "a drop"
  }

  private def kind(outcome: Outcome[Listing]): String = outcome match {
    case Outcome.Listed(_, _) => "a listing"
    case Outcome.Fetched(_, _) => "a read's outcome"
    case Outcome.Summarised(_) => "a summary"
    case Outcome.Kept => "a keep's outcome"
    case Outcome.NotTaken(_) => "a withholding"
    case Outcome.Stopped(_) => "a stop"
  }

  /** `s` once the directory `path` was listed: its children in scope, and those on the way to
    * one, pushed in path order, then its own card when it has one (`version`).
    */
  private def listed(
      env: Env,
      s: State,
      path: String,
      version: Option[UnitVersion],
      listing: Listing
  ): State = {
    val entries = listing match {
      case Listing.Complete(entries) => entries
      case Listing.Partial(shown) => shown
    }
    // Only the directory's own children: a listing naming anything else cannot loop the walk.
    val children = entries
      .filter(e => parent(e.path) == path)
      .filter(e => env.inScope(e) || env.passage(e))
      .sortBy(_.path)
    val carded = children.filter(env.inScope)
    val tasks =
      children.map(e => if (env.inScope(e)) Task.Visit(e) else Task.Pass(e.path)) ++
        version.map(Task.Finish(path, _, carded))
    val s1 = s.copy(
      stack = tasks.toList ++ s.stack,
      passed = if (version.isEmpty) s.passed + path else s.passed
    )
    listing match {
      case Listing.Complete(_) =>
        s1.copy(listed = s1.listed.updated(path, children.map(_.path).toSet))
      case Listing.Partial(_) =>
        if (version.isEmpty) s1.copy(undone = s1.undone + path)
        else unwritten(s1, path, "listed only in part")
    }
  }

  private def unwritten(s: State, path: String, why: String): State =
    s.copy(unwritten = s.unwritten :+ Unwritten(path, why), undone = s.undone + path)

  private def write(env: Env, s: State, card: Card): State =
    s.step(Step.Write(card, env.replacing(card.key)), Waiting.Wrote(card.key))

  /** Pops tasks until one makes a step, or none is left. */
  @tailrec private def advance(env: Env, s: State): State = s.stack match {
    case Nil => end(env, s)
    case task :: rest =>
      val popped = s.copy(stack = rest)
      task match {
        case Task.Pass(path) =>
          popped.step(Step.List(path, Way.Run), Waiting.Listing(path, None))
        case Task.Known(key) =>
          known(popped, env, key) match {
            case Some(next) => next
            case None => advance(env, unchanged(popped, key))
          }
        case Task.Visit(entry) =>
          val key = env.keyOf(entry.path)
          if (env.current(entry)) {
            entry.kind match {
              case UnitKind.File =>
                known(popped, env, key) match {
                  case Some(next) => next
                  case None => advance(env, unchanged(popped, key))
                }
              case UnitKind.Directory =>
                val beneath = env.held
                  .filter(h => h.path.startsWith(entry.path + "/"))
                  .sortBy(_.path)
                  .map(_.key)
                  .distinct
                advance(
                  env,
                  popped.copy(stack = (key +: beneath).map(Task.Known(_)).toList ++ rest)
                )
            }
          } else if (env.isNew(key) && !room(env, s)) advance(env, refuse(popped, entry.path))
          else
            entry.kind match {
              case UnitKind.Directory =>
                if (s.lists >= env.cards.maxUnits) end(env, s)
                else
                  popped
                    .copy(lists = popped.lists + 1)
                    .step(
                      Step.List(entry.path, Way.Run),
                      Waiting.Listing(entry.path, Some(entry.version))
                    )
              case UnitKind.File =>
                if (s.reads >= env.perRun) end(env, s)
                else
                  env.cards.pointer(entry.path) match {
                    case Left(why) => advance(env, unwritten(popped, entry.path, why))
                    case Right(pointer) =>
                      popped
                        .copy(reads = popped.reads + 1)
                        .step(
                          Step.Read(entry.path, Way.Run),
                          Waiting.Reading(entry.path, pointer)
                        )
                  }
            }
        case Task.Finish(path, version, children) =>
          if (s.undone.exists(u => u == path || u.startsWith(path + "/")))
            advance(env, popped.copy(undone = popped.undone + path))
          else if (env.isNew(env.keyOf(path)) && !room(env, s)) advance(env, refuse(popped, path))
          else
            env.cards.pointer(path) match {
              case Left(why) => advance(env, unwritten(popped, path, why))
              case Right(pointer) =>
                val summary = CardSummary.of(described(path, children), Vector.empty)
                summary match {
                  case Left(why) => advance(env, unwritten(popped, path, why))
                  case Right(summary) =>
                    write(
                      env,
                      popped,
                      Card.directory(
                        env.cards.corpus,
                        path,
                        version,
                        env.cards.scope,
                        summary,
                        pointer
                      )
                    )
                }
            }
      }
  }

  /** The relabel step of a held unit whose version is current, when it is held only at another
    * label; `None` when the floor holds it.
    */
  private def known(s: State, env: Env, key: CardKey): Option[State] =
    env.replacing(key).map(from => s.step(Step.Relabel(key, from), Waiting.Relabelled(key)))

  private def unchanged(s: State, key: CardKey): State =
    s.copy(unchanged = s.unchanged + 1, settled = s.settled :+ key)

  /** Whether a new unit's card would leave the source within `maxUnits`: the units it holds and
    * those this walk has written new.
    */
  private def room(env: Env, s: State): Boolean = env.byKey.size + s.fresh < env.cards.maxUnits

  /** `s` with the new unit at `path` not taken, so the directories above it are not written. */
  private def refuse(s: State, path: String): State =
    s.copy(refused = s.refused + 1, undone = s.undone + path)

  /** `s` come to its end: its drop, when anything is gone, and then its result. */
  private def end(env: Env, s: State): State = {
    val ended = unstacked(env, s)
    val gone = drops(env, ended)
    if (gone.isEmpty) done(env, ended)
    else ended.step(Step.Drop(gone), Waiting.Dropped(gone.size))
  }

  /** `s` stopped by a step's outcome, saying `why`: its result at once, dropping nothing. */
  private def halt(env: Env, s: State, why: String): State =
    done(env, unstacked(env, s).copy(stopped = Some(why)))

  private def done(env: Env, s: State): State =
    s.copy(next = Left(s.synced(env)), waiting = Waiting.Done)

  /** `s` with its tasks left counted: each unit seen changed or new, or held only at another
    * label, and not reached.
    */
  private def unstacked(env: Env, s: State): State = {
    val left = s.stack.count {
      case Task.Visit(entry) => !env.current(entry)
      case Task.Known(key) => env.replacing(key).nonEmpty
      case Task.Finish(_, _, _) => true
      case Task.Pass(_) => false
    }
    s.copy(stack = Nil, left = s.left + left)
  }

  /** The copies held of units gone from directories listed in full or listed as on the way to
    * an included path; the copies at other labels of units this walk kept current at the floor,
    * but the one each write or relabel replaced; and the units past `maxUnits`.
    */
  private def drops(env: Env, s: State): Vector[Held] = {
    val settled = s.settled.toSet
    val gone = env.held.filter { h =>
      if (settled.contains(h.key)) h.label != env.floor && !replacedBy(env, h)
      else
        s.passed.contains(h.path) || s.listed.exists { (dir, children) =>
          beneath(h.path, dir) && !children.contains(toward(dir, h.path))
        }
    }
    // Past maxUnits (it was lowered): the units last in walk order go, and the directories
    // above them, so the next run lists those again and takes the units back when there is room.
    val dropped = gone.toSet
    val kept = env.held
      .filterNot(dropped)
      .map(_.path)
      .distinct
      .sortBy(_.split('/').toVector)(using Ordering.Implicits.seqOrdering[Vector, String])
    val past = kept.drop(math.max(env.cards.maxUnits - s.fresh, 0))
    val over = (past ++ past.flatMap(ancestors)).toSet
    env.held.filter(h => dropped(h) || over(h.path))
  }

  /** The paths of the directories above `path`. */
  private def ancestors(path: String): Vector[String] =
    path.split('/').toVector.inits.toVector.drop(1).filter(_.nonEmpty).map(_.mkString("/"))

  /** Whether `h` is the copy a write or relabel of its unit replaced. */
  private def replacedBy(env: Env, h: Held): Boolean = env.replacing(h.key).contains(h.label)

  private def parent(path: String): String = path.lastIndexOf('/') match {
    case -1 => Root
    case i => path.substring(0, i)
  }

  private def beneath(path: String, dir: String): Boolean =
    if (dir == Root) path != Root else path.startsWith(dir + "/")

  /** The path of `dir`'s child on the way to `path`, beneath it. */
  private def toward(dir: String, path: String): String = {
    val rest = if (dir == Root) path else path.substring(dir.length + 1)
    val first = rest.takeWhile(_ != '/')
    if (dir == Root) first else s"$dir/$first"
  }

  /** What a directory holds, as its card's summary says it. */
  private def described(path: String, children: Vector[Entry]): String = {
    val files = children.count(_.kind == UnitKind.File)
    val dirs = children.size - files
    val names = children.map { e =>
      val name = e.path.substring(e.path.lastIndexOf('/') + 1)
      if (e.kind == UnitKind.Directory) s"$name/" else name
    }
    val holds = s"The directory $path holds ${count(files, "file", "files")} and " +
      count(dirs, "directory", "directories")
    if (names.isEmpty) s"$holds." else s"$holds: ${names.mkString(", ")}."
  }

  private def count(n: Int, one: String, many: String): String =
    s"$n ${if (n == 1) one else many}"

  // "An empty file." is three words and not blank: CardSummary.of never refuses it.
  private def sure[A](e: Either[String, A]): A =
    e.fold(why => throw new IllegalStateException(why), identity)
}
