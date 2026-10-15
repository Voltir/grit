package grit.act.moves

import java.time.Instant

import grit.core.act.{Asked, Called, Keeping, MoveError, MoveLimits, Moves, ScriptedKeeping}
import grit.core.classify.Answer as Judged
import grit.core.document.{Document, InMemoryDocuments}
import grit.core.flow.TestTainted
import grit.core.message.{AssistantBlock, Message, StopReason, Usage}
import grit.core.visibility.Integrity

/** [[MovesContract]] against `ScriptedMoves` and its keeping `ScriptedKeeping`, scripted with
  * the contract's world.
  */
object ScriptedMovesTests extends MovesContract {
  import MovesContract.*

  def within[A <: caps.Pure](
      limits: MoveLimits,
      broken: Boolean,
      shapes: Vector[ujson.Value],
      judgment: Judged,
      integrity: Integrity
  )(use: Moves^ -> A): A = use(scripted(limits, broken, shapes, judgment, integrity))

  def keeping[A <: caps.Pure](limits: MoveLimits)(use: Keeping^ -> A): (A, Vector[Document]) = {
    val moves = scripted(limits, broken = false, Vector(), Judged.YesNo(Yes), Integrity.Authority)
    (use(moves), moves.kept)
  }

  /** Keeping within `limits` whose keeps keep [[MovesWorld.Notes]]' documents at
    * [[MovesContract.Floor]], scripted as [[within]] describes its world, its run believed at
    * `integrity`.
    */
  private def scripted(
      limits: MoveLimits,
      broken: Boolean,
      shapes: Vector[ujson.Value],
      judgment: Judged,
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
          Right(Called.Done(TestTainted.at(Read, ProbeOutput), Floor))
        else Right(Called.Failed("unadvertised")),
      (_, _) =>
        if (broken) Left(MoveError.Store("down"))
        else Right(Asked(TestTainted.authority(replies.next()), Floor)),
      request =>
        if (broken) Left(MoveError.Store("down"))
        else
          Right(Asked(TestTainted.authority(request.questions.map(_ => judgment)), Floor)),
      // Positional: a named argument here would bind the lambdas above to untyped temporaries.
      new InMemoryDocuments,
      Instant.EPOCH,
      integrity
    )
  }

  /** Each of `shapes` in turn, the last again once they run out; [[MovesContract.Answered]]
    * when there are none.
    */
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
