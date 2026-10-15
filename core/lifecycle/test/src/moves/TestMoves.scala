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
import grit.core.provider.Models
import grit.core.spend.Spending
import grit.core.store.{
  Askers,
  Db,
  InMemoryToolSets,
  InMemoryUsageLedger,
  StoreError,
  Tx,
  UsageLedger
}

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
  ): MovesEnv^ = withModels(ledger, ledger, classifier, db, clock, new Answering("unused"))

  /** As [[of]], with `models` answering its model asks, their costs recorded in `recording`
    * and admitted against what `spending` read.
    */
  def withModels(
      recording: UsageLedger,
      spending: Spending,
      classifier: Classifier^,
      db: Db^,
      clock: Clock^,
      models: Models^
  ): MovesEnv^ = {
    val edges = new InMemoryEdges
    MovesEnv(
      MoveRecords(
        recording,
        spending,
        edges,
        edges,
        new InMemoryToolSets,
        new InMemorySchedules(),
        NoAskers,
        PerChar,
        new InMemoryDocuments().savepoints
      ),
      models,
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
