package grit.dbos.sql

import java.time.{Instant, LocalDate}

import grit.core.id.{DocKey, PluginName}
import grit.core.identity.Account
import grit.core.place.Place
import grit.core.review.{DocumentLabel, DocumentReview, DocumentReviews, ReviewWord, Reviews}
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.Label

/** [[DocumentReviews]] over `grit.document_reviews`, a ledger keyed by a document's key, label
  * and day, its version set NULL when the version is deleted; the documents read as
  * [[SqlDocuments]] reads them. The rows are put up by [[SqlDocuments.Keeper.forReview]].
  */
final class SqlDocumentReviews extends DocumentReviews {
  import SqlDocuments.{Columns, LabelId, Labelled, cleared, instant, rows, update, utc}

  def of(plugin: PluginName): Reviews[DocumentReview, ReviewWord] = new Of(plugin)

  def labels(plugin: PluginName, from: LocalDate)(using
      tx: Tx^
  ): Either[StoreError, Vector[DocumentLabel]] =
    rows(
      s"""SELECT r.key, r.day, r.version, r.verdict, r.rater, r.labelled_at,
         |       ${SqlLabels.columns("l")}
         |  FROM grit.document_reviews r JOIN grit.classifications l ON l.id = r.classification_id
         | WHERE r.plugin = ? AND r.verdict IS NOT NULL AND r.day >= ?
         | ORDER BY r.day, r.labelled_at, r.key COLLATE "C"""".stripMargin
    ) { ps =>
      ps.setString(1, PluginName.value(plugin))
      ps.setObject(2, from)
    } { rs =>
      for {
        key <- DocKey.of(rs.getString("key"))
        verdict <- ReviewWord.of(rs.getString("verdict"))
        rater <- SqlIdentities.read(rs.getString("rater")).left.map(_.toString)
        version <- Option(rs.getObject("version")) match {
          case None => Right(None)
          case Some(_) => SqlDocuments.versionOf(rs).map(Some(_))
        }
      } yield DocumentLabel(
        key,
        SqlLabels.read(rs),
        rs.getObject("day", classOf[LocalDate]),
        version,
        verdict,
        rater,
        instant(rs, "labelled_at")
      )
    }

  def forget(before: Instant)(using tx: Tx^): Either[StoreError, Int] =
    update("DELETE FROM grit.document_reviews WHERE queued_at < ?")(_.setObject(1, utc(before)))

  /** `plugin`'s reviews. */
  private final class Of(plugin: PluginName) extends Reviews[DocumentReview, ReviewWord] {

    def unposted(to: Place)(using tx: Tx^): Either[StoreError, Vector[DocumentReview]] =
      Tx.writable(to).fold(Right(Vector.empty))(waiting)

    /** Every review waiting whose label `at` dominates, its version read by the transaction. */
    private def waiting(at: Label)(using tx: Tx^): Either[StoreError, Vector[DocumentReview]] =
      cleared(
        s"""WITH ${SqlClearance.With}
           |SELECT $Columns, day FROM (
           |  SELECT d.*, r.day, r.queued_at
           |    FROM $Labelled d JOIN grit.document_reviews r ON r.version = d.version
           |    JOIN grit.classifications cl ON cl.id = r.classification_id
           |   WHERE r.plugin = ? AND r.address IS NULL AND r.dropped_at IS NULL
           |     AND ${SqlClearance.kept("d")}
           |     AND grit.classification_dominates(${SqlLabels.Arg},
           |           ROW(cl.level, cl.compartments)::grit.classification)
           |) waiting
           |ORDER BY queued_at, key COLLATE "C", day, version""".stripMargin
      ) { ps =>
        ps.setString(SqlClearance.Params + 1, PluginName.value(plugin))
        SqlLabels.bind(ps, SqlClearance.Params + 2, at)
      } { rs =>
        SqlDocuments
          .document(rs)
          .map(d =>
            DocumentReview(plugin, d.key, d.label, rs.getObject("day", classOf[LocalDate]), d)
          )
      }

    def posted(subject: DocumentReview, address: String, at: Instant)(using
        tx: Tx^
    ): Either[StoreError, Boolean] =
      update(
        s"""UPDATE grit.document_reviews SET address = ?, posted_at = ?
           | WHERE plugin = ? AND key = ? AND classification_id = $LabelId AND day = ?
           |   AND version IS NOT NULL AND address IS NULL AND dropped_at IS NULL
           |   AND NOT EXISTS (SELECT 1 FROM grit.document_reviews WHERE address = ?)""".stripMargin
      ) { ps =>
        ps.setString(1, address)
        ps.setObject(2, utc(at))
        named(ps, 3, subject)
        ps.setString(8, address)
      }.map(_ == 1)

    def dropped(subject: DocumentReview, at: Instant)(using
        tx: Tx^
    ): Either[StoreError, Boolean] =
      update(
        s"""UPDATE grit.document_reviews SET dropped_at = ?
           | WHERE plugin = ? AND key = ? AND classification_id = $LabelId AND day = ?
           |   AND version IS NOT NULL AND address IS NULL AND dropped_at IS NULL""".stripMargin
      ) { ps =>
        ps.setObject(1, utc(at))
        named(ps, 2, subject)
      }.map(_ == 1)

    def reacted(address: String, rater: Account, verdict: ReviewWord, at: Instant)(using
        tx: Tx^
    ): Either[StoreError, Boolean] =
      update(
        """UPDATE grit.document_reviews SET verdict = ?, rater = ?, labelled_at = ?
          | WHERE plugin = ? AND address = ?""".stripMargin
      ) { ps =>
        ps.setString(1, verdict.word)
        ps.setString(2, SqlIdentities.written(rater))
        ps.setObject(3, utc(at))
        ps.setString(4, PluginName.value(plugin))
        ps.setString(5, address)
      }.map(_ == 1)

    def unreacted(address: String, rater: Account, verdict: ReviewWord)(using
        tx: Tx^
    ): Either[StoreError, Boolean] =
      update(
        """UPDATE grit.document_reviews SET verdict = NULL, rater = NULL, labelled_at = NULL
          | WHERE plugin = ? AND address = ? AND verdict = ? AND rater = ?""".stripMargin
      ) { ps =>
        ps.setString(1, PluginName.value(plugin))
        ps.setString(2, address)
        ps.setString(3, verdict.word)
        ps.setString(4, SqlIdentities.written(rater))
      }.map(_ == 1)

    /** Sets the plugin, `subject`'s key, label (two parameters) and day, from `at`. */
    private def named(ps: java.sql.PreparedStatement, at: Int, subject: DocumentReview): Unit = {
      ps.setString(at, PluginName.value(plugin))
      ps.setString(at + 1, DocKey.value(subject.key))
      SqlLabels.bind(ps, at + 2, subject.label)
      ps.setObject(at + 4, subject.day)
    }
  }
}
