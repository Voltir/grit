package grit.lifecycle.moves

import grit.act.moves.{MoveRecords, MovesEnv}
import grit.core.act.MovesFixtures.{Answering, PerChar}
import grit.core.classify.Classifier
import grit.core.clock.Clock
import grit.core.document.InMemoryDocuments
import grit.core.edge.InMemoryEdges
import grit.core.id.TurnRef
import grit.core.identity.Principal
import grit.core.job.InMemorySchedules
import grit.core.store.{Askers, Db, InMemoryToolSets, InMemoryUsageLedger, StoreError, Tx}

/** What a lifecycle workflow's asks are made with in a test. */
object TestMoves {

  /** `classifier`, cleared as the test classifiers are, its costs kept in `ledger`, read
    * through `db` at `clock`, and nothing else a move could reach (no model answers, no edge,
    * no tool set, no schedule, no asker).
    */
  def of(
      ledger: InMemoryUsageLedger,
      classifier: Classifier^,
      db: Db^,
      clock: Clock^
  ): MovesEnv^ = {
    val edges = new InMemoryEdges
    MovesEnv(
      MoveRecords(
        ledger,
        ledger,
        edges,
        edges,
        new InMemoryToolSets,
        new InMemorySchedules(),
        NoAskers,
        PerChar,
        new InMemoryDocuments().savepoints
      ),
      new Answering("unused"),
      grit.core.classify.TestClassifiers.cleared(classifier),
      db,
      clock
    )
  }

  /** No turn's asker: a lifecycle ask is made for nobody's request. */
  object NoAskers extends Askers {
    def of(turn: TurnRef)(using Tx^): Either[StoreError, Option[Principal]] = Right(None)
  }
}
