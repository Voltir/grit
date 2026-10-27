package grit.dbos.sql

import grit.core.store.{StoreError, Tx}

/** The operator's declassification rows (`grit.declassification_policies`,
  * `grit.declassification_applications`): which version is in force, from which version
  * rewrites stand under it, and the versions the collector forgets.
  */
private[dbos] object SqlDeclassifications {

  /** True of the `grit.declassification_policies` row aliased `p` when its compartment has a
    * higher published version.
    */
  val PolicyOutranked: String =
    """EXISTS (SELECT 1 FROM grit.declassification_policies q
      |         WHERE q.over = p.over AND q.version > p.version
      |           AND q.status = 'published')""".stripMargin

  /** True of the `grit.declassification_policies` row aliased `p` when it is the version in
    * force, the one written and rewritten under: its compartment's newest published version,
    * not a revocation.
    */
  val PolicyInForce: String =
    s"p.status = 'published' AND p.text IS NOT NULL AND NOT $PolicyOutranked"

  /** Of the `grit.declassification_policies` row aliased `p`, the version in force, the lowest
    * version whose rewrites stand under it (`PolicyApplication.InForce.keptFrom`): the highest
    * at or below it published to withdraw (as every revocation is), or 1.
    */
  val KeptFrom: String =
    """COALESCE((SELECT max(k.version) FROM grit.declassification_policies k
      |           WHERE k.over = p.over AND k.version <= p.version
      |             AND k.on_publish = 'withdraw'), 1)""".stripMargin

  /** True of the `grit.declassification_applications` row aliased `a` when its document has a
    * higher version: an application version is current while this is false.
    */
  val ApplicationSuperseded: String =
    """EXISTS (SELECT 1 FROM grit.declassification_applications b
      |         WHERE b.pointer = a.pointer AND b.version > a.version)""".stripMargin

  /** Every policy version below a published one (a draft never published, or a published one
    * since outranked) and every application version but its document's highest, deleted, unless
    * a `grit.declassifications` row names it, or it is its compartment's highest published to
    * withdraw, which [[KeptFrom]] reads; how many.
    */
  def forget(using Tx^): Either[StoreError, Int] =
    for {
      policies <- SqlStatements.update(
        s"""DELETE FROM grit.declassification_policies p
          | WHERE $PolicyOutranked
          |   AND NOT EXISTS (SELECT 1 FROM grit.declassifications d
          |                    WHERE d.over = p.over AND d.policy_version = p.version)
          |   AND NOT (p.on_publish = 'withdraw'
          |            AND NOT EXISTS (SELECT 1 FROM grit.declassification_policies w
          |                             WHERE w.over = p.over AND w.version > p.version
          |                               AND w.on_publish = 'withdraw'))""".stripMargin
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
