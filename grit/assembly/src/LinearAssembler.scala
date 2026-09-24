package grit.assembly

import grit.core.{AssemblyError, AssemblyRequest, ContextAssembler, Db, EntryStore, TurnSeq, Window}

/** The window with no choosing: every entry recorded before the turn, oldest first. The
  * baseline every smarter assembler is measured against; unbounded, so a long
  * conversation outgrows the model's context.
  */
final class LinearAssembler(entries: EntryStore) extends ContextAssembler {

  def assemble(request: AssemblyRequest)(using db: Db^): Either[AssemblyError, Window] =
    db.read(entries.list(request.turn.conversationId))
      .map { all =>
        Window(
          all
            .filter(e => TurnSeq.value(e.turnSeq) < TurnSeq.value(request.turn.turnSeq))
            .map(_.id)
        )
      }
      .left
      .map(AssemblyError.Store(_))
}
