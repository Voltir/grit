package grit.dbos.sql

import java.sql.ResultSet

import grit.core.store.{StoreError, Tx}

/** The read of `grit.bench_trajectories` that its label filters (ADR 0030), for the bench's rows,
  * which `grit.dbos.internal` lends the eval harness.
  */
private[dbos] object SqlTrialRows {

  /** The part stored as `kind` of the trial whose session is `harness`'s `local`, read by `read`
    * from its `sha256`, `bytes` and `clipped` columns and its label's ([[SqlLabels.read]]), when
    * this transaction reads it.
    */
  def kept[A](harness: String, local: String, kind: String)(read: ResultSet => A)(using
      Tx^
  ): Either[StoreError, Option[A]] =
    SqlStatements
      .cleared(
        s"""WITH ${SqlClearance.With}
           |SELECT j.sha256, j.bytes, j.clipped, ${SqlLabels.columns("l")}
           |  FROM grit.bench_trajectories j
           |  JOIN grit.bench_trials t ON t.id = j.trial_id
           |  JOIN grit.classifications l ON l.id = j.classification_id
           | WHERE t.harness = ? AND t.local = ? AND j.kind = ? AND ${SqlClearance.kept(
            "j"
          )}""".stripMargin
      ) { ps =>
        ps.setString(SqlClearance.Params + 1, harness)
        ps.setString(SqlClearance.Params + 2, local)
        ps.setString(SqlClearance.Params + 3, kind)
      }(read)
      .map(_.headOption)
}
