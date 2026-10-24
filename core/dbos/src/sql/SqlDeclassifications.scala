package grit.dbos.sql

import grit.core.store.{StoreError, Tx}

/** The operator's declassification rows (`grit.declassification_policies`,
  * `grit.declassification_applications`): when a version is no longer current, and those the
  * collector forgets.
  */
private[dbos] object SqlDeclassifications {

  /** True of the `grit.declassification_policies` row aliased `p` when its compartment has a
    * higher version: a policy version is current while this is false.
    */
  val PolicySuperseded: String =
    """EXISTS (SELECT 1 FROM grit.declassification_policies q
      |         WHERE q.over = p.over AND q.version > p.version)""".stripMargin

  /** True of the `grit.declassification_applications` row aliased `a` when its document has a
    * higher version: an application version is current while this is false.
    */
  val ApplicationSuperseded: String =
    """EXISTS (SELECT 1 FROM grit.declassification_applications b
      |         WHERE b.pointer = a.pointer AND b.version > a.version)""".stripMargin

  /** Every policy and application version neither current (the highest of its compartment's, or
    * of its document's) nor named by a `grit.declassifications` row, deleted; how many.
    */
  def forget(using Tx^): Either[StoreError, Int] =
    for {
      policies <- SqlStatements.update(
        s"""DELETE FROM grit.declassification_policies p
          | WHERE $PolicySuperseded
          |   AND NOT EXISTS (SELECT 1 FROM grit.declassifications d
          |                    WHERE d.over = p.over AND d.policy_version = p.version)""".stripMargin
      )(_ => ())
      applications <- SqlStatements.update(
        s"""DELETE FROM grit.declassification_applications a
          | WHERE $ApplicationSuperseded
          |   AND NOT EXISTS (SELECT 1 FROM grit.declassifications d
          |                    WHERE d.pointer = a.pointer
          |                      AND d.application_version = a.version)""".stripMargin
      )(_ => ())
    } yield policies + applications
}
