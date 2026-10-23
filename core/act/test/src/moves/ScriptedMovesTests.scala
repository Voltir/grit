package grit.act.moves

import java.time.Instant

import grit.core.act.{
  Asked,
  Called,
  Dreaming,
  Keeping,
  MoveError,
  MoveLimits,
  Moves,
  ScriptedDreaming,
  ScriptedKeeping,
  ScriptedMoves
}
import grit.core.classify.{Answer as Judged, ClassifierError}
import grit.core.document.{Document, InMemoryDocuments}
import grit.core.files.{FileError, Files, Formats, InMemoryPieces, KeptPiece}
import grit.core.flow.TestTainted
import grit.core.id.FileHandle
import grit.core.message.{AssistantBlock, Message, StopReason, Usage}
import grit.core.stitch.StrandWorld
import grit.core.tool.Outcome
import grit.core.visibility.{Integrity, Recorded}

/** [[MovesContract]] against `ScriptedMoves` and its keeping `ScriptedKeeping`, scripted with
  * the contract's world.
  */
object ScriptedMovesTests extends MovesContract {
  import MovesContract.*

  def within[A <: caps.Pure](
      limits: MoveLimits,
      broken: Boolean,
      shapes: Vector[ujson.Value],
      judgment: Either[ClassifierError, Judged],
      integrity: Integrity
  )(use: Moves^ -> A): A = use(scripted(limits, broken, shapes, judgment, integrity))

  def keeping[A <: caps.Pure](limits: MoveLimits)(use: Keeping^ -> A): (A, Vector[Document]) = {
    val moves =
      scripted(limits, broken = false, Vector(), Right(Judged.YesNo(Yes)), Integrity.Authority)
    (use(moves), moves.kept)
  }

  def piecing[A <: caps.Pure](limits: MoveLimits, formats: Formats)(
      use: (Keeping^, FileHandle) -> A
  ): (A, Vector[KeptPiece]) = {
    val file = FileHandle.read("f1").getOrElse(throw new java.lang.AssertionError("f1"))
    val moves = new ScriptedKeeping(
      limits,
      Floor,
      MovesWorld.Notes,
      MovesWorld.NotesTerms,
      _ => Left(MoveError.Model("no ask scripted")),
      (_, _, _) => Right(Called.Failed("unadvertised")),
      (_, _) => Left(MoveError.Model("no ask scripted")),
      _ => Left(MoveError.Model("no judgment scripted")),
      new InMemoryDocuments,
      Instant.EPOCH,
      Integrity.Authority,
      Recorded.Empty,
      new InMemoryPieces,
      h => if (h == file) Right((Policy, Paged)) else Left(FileError.Gone(h, Files.Expiry).message),
      Some(formats)
    )
    (use(moves, file), moves.pieced)
  }

  def dreaming[A <: caps.Pure](
      limits: MoveLimits,
      world: StrandWorld,
      integrity: Integrity,
      shelves: MovesContract.Shelves
  )(
      use: Dreaming^ -> A
  ): (A, Vector[(String, Vector[String])]) = {
    val shown = new Shown
    val moves = new ScriptedDreaming(
      limits,
      Floor,
      MovesWorld.Notes,
      MovesWorld.NotesTerms,
      Dreaming.Reads(world.shelf, _ => shelves.needs, world.excerpts, shelves.documents),
      (system, messages) => {
        shown.add(system, messages.collect { case Message.User(t) => t })
        Right(Asked(TestTainted.at(Answered, Believed), Floor))
      },
      // Positional: a named argument lifts the capturing script into a val it cannot flow into.
      shelves.documents,
      Instant.EPOCH,
      integrity
    )
    (use(moves), shown.all)
  }

  /** Keeping within `limits` whose keeps keep [[MovesWorld.Notes]]' documents at
    * [[MovesContract.Floor]], scripted as [[within]] describes its world, its run believed at
    * `integrity`.
    */
  private def scripted(
      limits: MoveLimits,
      broken: Boolean,
      shapes: Vector[ujson.Value],
      judgment: Either[ClassifierError, Judged],
      integrity: Integrity
  ): ScriptedKeeping = {
    val replies = new Replies(shapes)
    val answer: Message.Assistant =
      Message.Assistant(Vector(AssistantBlock.Text(Answer)), StopReason.EndTurn, Usage.Zero, "m")
    new ScriptedKeeping(
      limits,
      Floor,
      MovesWorld.Notes,
      MovesWorld.NotesTerms,
      _ =>
        if (broken) Left(MoveError.Store("down"))
        else Right(Asked(TestTainted.authority(answer), Floor)),
      (service, tool, _) =>
        if (service == Probe && tool == Tool)
          Right(
            Called.Done(
              TestTainted.at(grit.core.act.Answer(Read, Outcome.Stated.None), ProbeOutput),
              ProbeAt
            )
          )
        else Right(Called.Failed("unadvertised")),
      (_, _) =>
        if (broken) Left(MoveError.Store("down"))
        else Right(Asked(TestTainted.authority(replies.next()), Floor)),
      request =>
        if (broken) Left(MoveError.Store("down"))
        else
          judgment.fold(
            e => Left(MoveError.Model(ScriptedMoves.unanswered(e))),
            j => Right(Asked(TestTainted.authority(request.questions.map(_ => j)), Floor))
          ),
      // Positional: a named argument here would bind the lambdas above to untyped temporaries.
      new InMemoryDocuments,
      Instant.EPOCH,
      integrity
    )
  }

  /** Each of `shapes` in turn, the last again once they run out; [[MovesContract.Answered]]
    * when there are none.
    */
  /** Each JSON ask's system text and user messages, in order. */
  private final class Shown {
    // Appended to by the fake's JSON script; one test's, read on its one thread.
    @caps.unsafe.untrackedCaptures
    var all: Vector[(String, Vector[String])] = Vector.empty

    def add(system: String, users: Vector[String]): Unit = all = all :+ (system, users)
  }

  private final class Replies(shapes: Vector[ujson.Value]) {
    // Counts the replies given; one test's, read on its one thread, so no other reader sees
    // it change.
    @caps.unsafe.untrackedCaptures
    private var told = 0

    def next(): ujson.Value = {
      val shape = shapes.lift(told).orElse(shapes.lastOption).getOrElse(Answered)
      told += 1
      shape
    }
  }
}
