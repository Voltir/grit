package grit.act.moves

import grit.core.act.{Asked, Called, MoveError, MoveLimits, Moves, ScriptedMoves}
import grit.core.classify.Answer as Judged
import grit.core.message.{AssistantBlock, Message, StopReason, Usage}

/** [[MovesContract]] against `ScriptedMoves`, scripted with the contract's world. */
object ScriptedMovesTests extends MovesContract {
  import MovesContract.*

  def within[A <: caps.Pure](
      limits: MoveLimits,
      broken: Boolean,
      shapes: Vector[ujson.Value],
      judgment: Judged
  )(use: Moves^ -> A): A = {
    val replies = new Replies(shapes)
    val answer: Message.Assistant =
      Message.Assistant(Vector(AssistantBlock.Text(Answer)), StopReason.EndTurn, Usage.Zero, "m")
    val moves = new ScriptedMoves(
      limits,
      _ => if (broken) Left(MoveError.Store("down")) else Right(Asked(answer, Floor)),
      (service, tool, _) =>
        if (service == Probe && tool == Tool) Right(Called.Done(Read, Floor))
        else Right(Called.Failed("unadvertised")),
      (_, _) =>
        if (broken) Left(MoveError.Store("down"))
        else Right(Asked(replies.next(), Floor)),
      _ =>
        if (broken) Left(MoveError.Store("down"))
        else Right(Asked(Vector(judgment), Floor))
    )
    use(moves)
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
