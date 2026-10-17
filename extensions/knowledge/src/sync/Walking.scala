package grit.knowledge.sync

import scala.annotation.tailrec

import grit.core.document.Pointer
import grit.core.visibility.Label
import grit.knowledge.card.{Card, CardKey, CardSummary, UnitVersion}

/** A step-at-a-time reconciliation of the cards a run holds against a source's listings: one
  * run's [[Walk]] of a source listing as `L`, taking its [[Walking.Agenda]]'s tasks `T` off a
  * stack, the last pushed first, until one makes a step, the agenda keeping its own record `S`.
  *
  * A file read is summarised from its first 8 KB and its card written at the version its read
  * gave; a blank text's card says `An empty file.`, with no ask. A write withdraws, in its keep,
  * the copy [[Holding.replacing]] names. A list or a read that came to [[Outcome.NotTaken]], and
  * a read with no version, are [[Unwritten]], saying why. When no task is left, or the agenda
  * [[end]]s it, its last step is a [[Step.Drop]] of the agenda's `drops` but each copy a write or
  * relabel of this walk withdrew, when any are left. A step that came to [[Outcome.Stopped]], or
  * to an outcome not of its kind, ends it at once with no drop, saying why. Ended either way,
  * each task still stacked that the agenda's `left` holds is counted left.
  */
private[knowledge] final case class Walking[
    L <: caps.Pure,
    T <: caps.Pure,
    S <: caps.Pure
] private (
    private val agenda: Walking.Agenda[L, T, S],
    private val perRun: Int,
    holding: Holding,
    private val stack: List[T],
    private val waiting: Walking.Waiting[L, T, S],
    next: Either[Synced, Step],
    private val reads: Int,
    private val fresh: Int,
    tally: Synced,
    settled: Set[CardKey],
    own: S
) extends Walk[L] {
  import Walking.Waiting

  def after(outcome: Outcome[L]): Walking[L, T, S] = (waiting, outcome) match {
    case (Waiting.Done(), _) => this
    case (_, Outcome.Stopped(why)) => halt(why)
    case (Waiting.Listing(_, listed), Outcome.Listed(listing, at)) =>
      Walking.advance(listed(this, listing, at))
    case (Waiting.Listing(path, _), Outcome.NotTaken(why)) =>
      Walking.advance(unwritten(path, why))
    case (Waiting.Reading(path, _, _), Outcome.NotTaken(why)) =>
      Walking.advance(unwritten(path, why))
    case (Waiting.Reading(path, subject, card), Outcome.Fetched(fetched, _)) =>
      fetched match {
        case Fetched.Text(text, version) if text.isBlank => write(card(version, Walking.Empty))
        case Fetched.Text(text, version) =>
          step(
            Step.Summarise(path, subject, Fetched.head(text), version),
            Waiting.Summary(card(version, _))
          )
        case Fetched.Unversioned(why) => Walking.advance(unwritten(path, why))
      }
    case (Waiting.Summary(card), Outcome.Summarised(summary)) => write(card(summary))
    case (Waiting.Wrote(key), Outcome.Kept) =>
      Walking.advance(
        copy(
          tally = tally.copy(written = tally.written + 1),
          fresh = if (holding.isNew(key)) fresh + 1 else fresh,
          settled = settled + key
        )
      )
    case (Waiting.Relabelled(key), Outcome.Kept) =>
      Walking.advance(
        copy(tally = tally.copy(relabelled = tally.relabelled + 1), settled = settled + key)
      )
    case (Waiting.Dropped(count), Outcome.Kept) => copy(tally = tally.copy(withdrawn = count)).done
    case (waiting, other) => halt(s"${Walking.expected(waiting)} came to ${Walking.kind(other)}")
  }

  /** This walk, its agenda's record now `own`. */
  def recording(own: S): Walking[L, T, S] = copy(own = own)

  /** This walk with `tasks` stacked before those it has, the first taken first. */
  def push(tasks: Seq[T]): Walking[L, T, S] = copy(stack = tasks.toList ++ stack)

  /** This walk with the held unit `key` counted unchanged. */
  def unchanged(key: CardKey): Walking[L, T, S] =
    copy(tally = tally.copy(unchanged = tally.unchanged + 1), settled = settled + key)

  /** This walk with the unit at `path` [[Unwritten]], saying `why`. */
  def unwritten(path: String, why: String): Walking[L, T, S] =
    copy(tally = tally.copy(unwritten = tally.unwritten :+ Unwritten(path, why)))

  /** This walk with one more new unit not taken: the source holds its most cards. */
  def refuse: Walking[L, T, S] = copy(tally = tally.copy(refused = tally.refused + 1))

  /** This walk with one more unit left for the next run. */
  def leave: Walking[L, T, S] = copy(tally = tally.copy(left = tally.left + 1))

  /** Whether `key`, held at no label, would take the source past its `maxUnits` cards, counting
    * those this walk wrote new.
    */
  def full(key: CardKey): Boolean =
    holding.isNew(key) && holding.units + fresh >= agenda.source.maxUnits

  /** Whether it has read `perRun` files. */
  def spent: Boolean = reads >= perRun

  /** Whether `held` is the copy a write or relabel of this walk withdrew in its keep. */
  def replaced(held: Held): Boolean =
    settled(held.key) && holding.replacing(held.key).contains(held.label)

  /** The last of `paths`, the held units' paths in the order they keep their places: those past
    * `maxUnits` once the units this walk wrote new have theirs.
    */
  def past(paths: Vector[String]): Vector[String] =
    paths.drop(math.max(agenda.source.maxUnits - fresh, 0))

  /** This walk listing `path`, reached `way`; the listing, read at a label, then given to `listed`. */
  def list(path: String, way: Way)(
      listed: (Walking[L, T, S], L, Label) -> Walking[L, T, S]
  ): Walking[L, T, S] = step(Step.List(path, way), Waiting.Listing(path, listed))

  /** This walk reading the file `path`, reached `way`, through its source's pointer of it, the
    * summary model told it is `subject`; its card is `card` of the read's version, the summary
    * and the pointer. With no read made, checked in this order: [[refuse]]d when [[full]],
    * [[leave]]s it when [[spent]], unwritten when the path makes no pointer.
    */
  def read(path: String, way: Way, subject: String)(
      card: (UnitVersion, CardSummary, Pointer) -> Card
  ): Walking[L, T, S] =
    if (full(CardKey.of(agenda.source.corpus, path))) refuse
    else if (spent) leave
    else
      agenda.source.pointer(path) match {
        case Left(why) => unwritten(path, why)
        case Right(pointer) =>
          copy(reads = reads + 1).step(
            Step.Read(path, way),
            Waiting.Reading(path, subject, card(_, _, pointer))
          )
      }

  /** This walk relabelling `key` from `from` ([[Step.Relabel]]). */
  def relabel(key: CardKey, from: Label): Walking[L, T, S] =
    step(Step.Relabel(key, from), Waiting.Relabelled(key))

  /** This walk writing `card` at the floor. */
  def write(card: Card): Walking[L, T, S] =
    step(Step.Write(card, holding.replacing(card.key)), Waiting.Wrote(card.key))

  /** This walk ended here: no task more is taken. */
  def end: Walking[L, T, S] = {
    val ended = unstacked(None)
    val gone = agenda.drops(ended).filterNot(ended.replaced)
    if (gone.isEmpty) ended.done else ended.step(Step.Drop(gone), Waiting.Dropped(gone.size))
  }

  private def halt(why: String): Walking[L, T, S] = unstacked(Some(why)).done

  private def unstacked(stopped: Option[String]): Walking[L, T, S] = copy(
    stack = Nil,
    tally = tally.copy(left = tally.left + stack.count(agenda.left(_, this)), stopped = stopped)
  )

  private def done: Walking[L, T, S] = copy(next = Left(tally), waiting = Waiting.Done())

  private def step(s: Step, w: Waiting[L, T, S]): Walking[L, T, S] =
    copy(next = Right(s), waiting = w)
}

private[knowledge] object Walking {

  /** How `source`'s walk does its tasks. `take` is the walk once a task is off its stack: a step
    * made ([[Walking.list]], [[Walking.read]], [[Walking.relabel]], [[Walking.write]]), the walk
    * [[Walking.end]]ed, or neither, and the next task is taken. `left` says whether a task the
    * walk never took counts as left; `drops` is what it withdraws at its end.
    */
  final case class Agenda[L <: caps.Pure, T <: caps.Pure, S <: caps.Pure](
      source: Source[L],
      take: (T, Walking[L, T, S]) -> Walking[L, T, S],
      left: (T, Walking[L, T, S]) -> Boolean,
      drops: Walking[L, T, S] -> Vector[Held]
  ) extends caps.Pure

  /** The walk of `agenda` for a run reading at most `perRun` files, holding `holding`, its first
    * tasks `tasks` and its record `own`: its first step made, or its end.
    */
  def start[L <: caps.Pure, T <: caps.Pure, S <: caps.Pure](
      agenda: Agenda[L, T, S],
      perRun: Int,
      holding: Holding,
      tasks: List[T],
      own: S
  ): Walking[L, T, S] = {
    val none = Synced(agenda.source.corpus, 0, 0, 0, 0, 0, 0, Vector.empty, None)
    advance(
      Walking(
        agenda,
        perRun,
        holding,
        tasks,
        Waiting.Idle(),
        Left(none),
        0,
        0,
        none,
        Set.empty,
        own
      )
    )
  }

  /** What the last step's outcome is read as; `Idle` while a task is taken, `Done` once ended. */
  private enum Waiting[L <: caps.Pure, T <: caps.Pure, S <: caps.Pure] extends caps.Pure {
    case Idle()
    case Done()
    case Listing(path: String, listed: (Walking[L, T, S], L, Label) -> Walking[L, T, S])
    case Reading(path: String, subject: String, card: (UnitVersion, CardSummary) -> Card)
    case Summary(card: CardSummary -> Card)
    case Wrote(key: CardKey)
    case Relabelled(key: CardKey)
    case Dropped(count: Int)
  }

  private val Empty: CardSummary = sure(CardSummary.of("An empty file.", Vector.empty))

  /** Pops tasks until one makes a step or ends the walk, or none is left. */
  @tailrec private def advance[L <: caps.Pure, T <: caps.Pure, S <: caps.Pure](
      w: Walking[L, T, S]
  ): Walking[L, T, S] = w.stack match {
    case Nil => w.end
    case task :: rest =>
      val taken = w.agenda.take(task, w.copy(stack = rest, waiting = Waiting.Idle()))
      taken.waiting match {
        case Waiting.Idle() => advance(taken)
        case _ => taken
      }
  }

  private def expected(waiting: Waiting[?, ?, ?]): String = waiting match {
    case Waiting.Listing(_, _) => "a list"
    case Waiting.Reading(_, _, _) => "a read"
    case Waiting.Summary(_) => "a summary"
    case Waiting.Wrote(_) => "a write"
    case Waiting.Relabelled(_) => "a relabel"
    case _ => "a drop"
  }

  private def kind(outcome: Outcome[?]): String = outcome match {
    case Outcome.Listed(_, _) => "a listing"
    case Outcome.Fetched(_, _) => "a read's outcome"
    case Outcome.Summarised(_) => "a summary"
    case Outcome.Kept => "a keep's outcome"
    case Outcome.NotTaken(_) => "a withholding"
    case Outcome.Stopped(_) => "a stop"
  }

  // "An empty file." is three words and not blank: CardSummary.of never refuses it.
  private def sure[A](e: Either[String, A]): A =
    e.fold(why => throw new IllegalStateException(why), identity)
}
