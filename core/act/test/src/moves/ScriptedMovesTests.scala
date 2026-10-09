package grit.act.moves

import grit.core.act.{Asked, Called, MoveError, MoveLimits, Moves, ScriptedMoves}
import grit.core.message.{AssistantBlock, Message, StopReason, Usage}

/** [[MovesContract]] against `ScriptedMoves`, scripted with the contract's world. */
object ScriptedMovesTests extends MovesContract {
  import MovesContract.*

  def within[A <: caps.Pure](limits: MoveLimits, broken: Boolean)(use: Moves^ -> A): A = {
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
        else Right(Asked(ujson.Obj("answer" -> Answer), Floor))
    )
    use(moves)
  }
}
