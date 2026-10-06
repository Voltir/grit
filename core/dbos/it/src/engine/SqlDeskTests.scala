package grit.dbos.engine

import grit.core.job.{DeskContract, ScheduleContract}

/** The desk contract, kept by the SQL store's desks against a real Postgres. */
object SqlDeskTests extends DeskContract {
  protected def fresh(): ScheduleContract.Under = SqlSchedulesUnder("sql_desk")
}
