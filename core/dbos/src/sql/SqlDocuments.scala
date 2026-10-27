package grit.dbos.sql

import java.sql.{PreparedStatement, ResultSet}
import java.time.{Instant, LocalDate, OffsetDateTime, ZoneOffset}

import grit.core.document.{
  Declassified,
  DocLabel,
  DocRef,
  DocText,
  DocWeight,
  Document,
  DocumentKeeper,
  DocumentSearch,
  DocumentShelf,
  DocumentStore,
  DocumentTerms,
  Following,
  Placement,
  Pointer,
  PolicyApplication,
  Shelved,
  Written
}
import grit.core.id.{DocKey, DocumentVersionId, PluginName}
import grit.core.period.Probability
import grit.core.place.{Namespace, Place}
import grit.core.retention.Target
import grit.core.screen.{Benched, Checks}
import grit.core.store.{StoreError, Tombstones, Tx}
import grit.core.visibility.{Clearance, DeclassificationPolicy, Integrity, Label}

/** [[DocumentStore]] over `grit.documents`, `grit.document_refs` and `grit.document_terms`, its
  * keepers putting up reviews in `grit.document_reviews` ([[SqlDocumentReviews]]), reading as
  * [[SqlDocumentSearch]] does, marking in `tombstones` the versions its keepers leave for
  * deletion. Keys order bytewise (`COLLATE "C"`), as the
  * in-memory fake orders them; a retention is kept to the microsecond.
  */
final class SqlDocuments(tombstones: Tombstones) extends DocumentStore {
  import SqlDocuments.*

  /** `plugin`'s documents as it writes them, under `terms`. */
  def keeper(plugin: PluginName, terms: DocumentTerms): DocumentKeeper =
    new Keeper(plugin, terms, tombstones)

  /** `plugin`'s documents as it reads them. */
  def shelf(plugin: PluginName): DocumentShelf = new Shelf(plugin)

  private val reads = new SqlDocumentSearch

  def declared()(using Tx^): Either[StoreError, Vector[(PluginName, DocumentTerms)]] =
    reads.declared()

  def shelved(plugins: Vector[PluginName], at: Instant)(using
      Tx^
  ): Either[StoreError, Vector[Shelved]] = reads.shelved(plugins, at)

  def search(shelves: Vector[Shelved], query: String, limit: Int, at: Instant)(using
      Tx^
  ): Either[StoreError, Vector[DocumentSearch.Hit]] = reads.search(shelves, query, limit, at)

  def read(versions: Vector[DocumentVersionId])(using Tx^): Either[StoreError, Vector[Document]] =
    reads.read(versions)

  def declare(enabled: Vector[(PluginName, DocumentTerms)])(using
      tx: Tx^
  ): Either[StoreError, Unit] =
    for {
      _ <- update("UPDATE grit.document_terms SET enabled = false WHERE enabled")(_ => ())
      _ <- enabled.foldLeft[Either[StoreError, Unit]](Right(())) { case (acc, (plugin, terms)) =>
        acc.flatMap(_ =>
          update(
            """INSERT INTO grit.document_terms (plugin, label, weight, retention, bound, enabled)
              |VALUES (?, ?, ?, ? * interval '1 microsecond', ?, true)
              |ON CONFLICT (plugin) DO UPDATE
              |   SET label = EXCLUDED.label, weight = EXCLUDED.weight,
              |       retention = EXCLUDED.retention, bound = EXCLUDED.bound, enabled = true""".stripMargin
          ) { ps =>
            ps.setString(1, PluginName.value(plugin))
            ps.setString(2, DocLabel.value(terms.label))
            ps.setDouble(3, DocWeight.value(terms.weight))
            ps.setLong(4, terms.retention.toMicros)
            ps.setInt(5, terms.bound)
          }.map(_ => ())
        )
      }
    } yield ()

  def placed(versions: Vector[DocumentVersionId], at: Instant)(using
      tx: Tx^
  ): Either[StoreError, Unit] =
    if (versions.isEmpty) Right(())
    else
      update(
        """UPDATE grit.documents d
          |   SET placed = d.placed + c.n, last_placed = greatest(d.last_placed, ?)
          |  FROM (SELECT v::bigint AS version, count(*) AS n
          |          FROM jsonb_array_elements_text(?::jsonb) AS e(v) GROUP BY v) c
          | WHERE d.version = c.version""".stripMargin
      ) { ps =>
        ps.setObject(1, utc(at))
        ps.setString(2, numbers(versions))
      }.map(_ => ())

  def followed(versions: Vector[DocumentVersionId], at: Instant)(using
      tx: Tx^
  ): Either[StoreError, Unit] =
    if (versions.isEmpty) Right(())
    else {
      val clearance = Tx.clearance(tx)
      update(
        s"""WITH ${SqlClearance.With}
           |UPDATE grit.documents d
           |   SET followed = d.followed + c.n,
           |       last_followed = greatest(d.last_followed, ?)
           |  FROM (SELECT v::bigint AS version, count(*) AS n
           |          FROM jsonb_array_elements_text(?::jsonb) AS e(v) GROUP BY v) c
           | WHERE d.version = c.version AND ${SqlClearance.kept("d")}""".stripMargin
      ) { ps =>
        SqlClearance.bind(ps, 1, clearance)
        ps.setObject(SqlClearance.Params + 1, utc(at))
        ps.setString(SqlClearance.Params + 2, numbers(versions))
      }.map(_ => ())
    }

  def kept()(using tx: Tx^): Either[StoreError, Vector[PluginName]] =
    rows(
      """SELECT plugin FROM (
        |  SELECT plugin FROM grit.documents UNION SELECT plugin FROM grit.document_terms
        |) kept ORDER BY plugin COLLATE "C"""".stripMargin
    )(_ => ())(rs => PluginName.of(rs.getString("plugin")))

  def forget(version: DocumentVersionId)(using tx: Tx^): Either[StoreError, Unit] =
    SqlKept.Documents.forget(DocumentVersionId.value(version))

  def remove(plugin: PluginName, at: Instant)(using tx: Tx^): Either[StoreError, Unit] =
    for {
      _ <- SqlKept.Documents.delete("plugin = ?", at)(_.setString(_, PluginName.value(plugin)))
      _ <- update("DELETE FROM grit.document_terms WHERE plugin = ?")(
        _.setString(1, PluginName.value(plugin))
      )
      _ <- update("DELETE FROM grit.document_reviews WHERE plugin = ?")(
        _.setString(1, PluginName.value(plugin))
      )
    } yield ()
}

private[sql] object SqlDocuments {

  val Labelled: String = SqlKept.Documents.labelled

  /** A version's columns, as [[document]] reads them, from [[Labelled]]. */
  val Columns: String =
    "version, plugin, key, array_to_json(place)::text AS place, body, data::text AS data, " +
      "pointer::text AS pointer, written_at, placed, last_placed, followed, last_followed, " +
      "label_level, label_compartments, integrity_level, integrity_compartments"

  /** A label's id: the parameters of [[SqlLabels.Arg]], looked up without interning it. */
  val LabelId: String =
    s"(SELECT l.id FROM grit.classifications l WHERE ROW(l.level, l.compartments)::grit.classification = ${SqlLabels.Arg})"

  /** A room's `grit.places` id, its path one JSON array parameter; NULL for none. */
  val RoomId: String =
    "(SELECT p.id FROM grit.places p WHERE p.path = ARRAY(SELECT jsonb_array_elements_text(?::jsonb)))"

  /** The path of `clearance`'s own room, as [[RoomId]] binds it: none when it has none. */
  def room(clearance: Clearance): String =
    ujson.Arr.from(clearance.own.toVector.flatMap(_.room.segments).map(ujson.Str(_))).render()

  // pg_textsearch scores are negative, more negative is better. The planner may filter to the
  // shelves first, scoring rows that do not match 0; the guard drops them outside the LIMIT,
  // as SqlEntrySearch's queries do.
  val Ranked: String =
    s"""WITH ${SqlClearance.With}
       |SELECT $Columns, s FROM (
       |  SELECT d.*, d.body <@> to_bm25query(?, 'grit.idx_documents_bm25') AS s
       |    FROM $Labelled d
       |   WHERE ${SqlClearance.kept(
        "d"
      )} AND EXISTS (SELECT 1 FROM jsonb_to_recordset(?::jsonb) AS r(p text, place jsonb)
       |                  WHERE d.plugin = r.p
       |                    AND d.place = ARRAY(SELECT jsonb_array_elements_text(r.place)))
       |     AND d.body IS NOT NULL
       |     AND ${SqlKept.currentAt("d")}
       |   ORDER BY s, d.written_at DESC, d.version DESC
       |   LIMIT ?
       |) ranked
       |WHERE s < 0
       |ORDER BY s, written_at DESC, version DESC""".stripMargin

  def utc(at: Instant): OffsetDateTime = at.atOffset(ZoneOffset.UTC)

  def instant(rs: ResultSet, column: String): Instant =
    rs.getObject(column, classOf[OffsetDateTime]).toInstant

  /** `values` as a JSON array of strings, so no Java array crosses JDBC. */
  def strings(values: Vector[String]): String = ujson.Arr.from(values.map(ujson.Str(_))).render()

  /** `versions` as a JSON array of their numbers written as strings: a JSON number past 2^53
    * would lose digits.
    */
  def numbers(versions: Vector[DocumentVersionId]): String =
    strings(versions.map(v => DocumentVersionId.value(v).toString))

  def path(place: Place): ujson.Arr = ujson.Arr.from(place.segments.map(ujson.Str(_)))

  /** The place whose segments `json` holds. */
  def placeOf(json: String): Either[String, Place] =
    ujson.read(json).arrOpt.fold(Vector.empty[String])(_.toVector.flatMap(_.strOpt)) match {
      case ns +: rest =>
        Namespace.of(ns).map(Place.under(_, rest)).toRight(s"a document's place: no namespace $ns")
      case _ => Right(Place.Everywhere)
    }

  /** `pointer` as its column holds it: `{"arguments":{…},"tool":…}`, compact, every key sorted. */
  def pointerJson(pointer: Pointer): String = pointer.written.render()

  /** The pointer `json` holds ([[pointerJson]]), or why not; JSONB keeps its keys in its own
    * order, so its arguments are sorted again.
    */
  def pointerOf(json: String): Either[String, Pointer] =
    Pointer.read(ujson.read(json)).left.map(why => s"a document's $why")

  /** The version `rs` holds ([[Columns]]), or why not: one withdrawn holds nothing. */
  def document(rs: ResultSet): Either[String, Document] =
    for {
      version <- DocumentVersionId.of(rs.getLong("version")).toRight("a document's version")
      plugin <- PluginName.of(rs.getString("plugin"))
      key <- DocKey.of(rs.getString("key"))
      place <- placeOf(rs.getString("place"))
      text <- Option(rs.getString("body")).toRight("a withdrawal").flatMap(DocText.of)
      pointer <- Option(rs.getString("pointer")) match {
        case None => Right(None)
        case Some(json) => pointerOf(json).map(Some(_))
      }
    } yield Document(
      version,
      plugin,
      key,
      SqlLabels.read(rs),
      SqlIntegrity.read(rs),
      place,
      text,
      ujson.read(rs.getString("data")),
      pointer,
      instant(rs, "written_at"),
      Placement(rs.getLong("placed"), instant(rs, "last_placed")),
      Option(rs.getObject("last_followed", classOf[OffsetDateTime]))
        .flatMap(last => Following.of(rs.getLong("followed"), last.toInstant))
    )

  export SqlStatements.update

  /** As [[rows]], for a statement beginning `WITH ${SqlClearance.With}`: the clearance's
    * parameters are set first, to the transaction's, then `bind`'s.
    */
  def cleared[A](
      sql: String
  )(bind: PreparedStatement => Unit)(read: ResultSet => Either[String, A])(using
      tx: Tx^
  ): Either[StoreError, Vector[A]] =
    SqlStatements.cleared(sql)(bind)(read).flatMap(SqlStatements.valid)

  /** What `sql`, its parameters set by `bind`, selects, each row read by `read`; a row it
    * cannot read is `Invalid`.
    */
  def rows[A](sql: String)(bind: PreparedStatement => Unit)(read: ResultSet => Either[String, A])(
      using tx: Tx^
  ): Either[StoreError, Vector[A]] =
    SqlStatements.many(sql)(bind)(read).flatMap(SqlStatements.valid)

  /** One plugin's current documents, as it reads them. */
  class Shelf(plugin: PluginName) extends DocumentShelf {

    def current(key: DocKey, label: Label)(using tx: Tx^): Either[StoreError, Option[Document]] =
      cleared(
        s"""WITH ${SqlClearance.With}
           |SELECT $Columns FROM $Labelled d
           | WHERE plugin = ? AND key = ? AND classification_id = $LabelId
           |   AND superseded_at IS NULL AND body IS NOT NULL AND ${SqlClearance.kept(
            "d"
          )}""".stripMargin
      ) { ps =>
        ps.setString(SqlClearance.Params + 1, PluginName.value(plugin))
        ps.setString(SqlClearance.Params + 2, DocKey.value(key))
        SqlLabels.bind(ps, SqlClearance.Params + 3, label)
      }(document).map(_.headOption)

    def newest(n: Int)(using tx: Tx^): Either[StoreError, Vector[Document]] =
      if (n <= 0) Right(Vector.empty)
      else
        cleared(
          s"""WITH ${SqlClearance.With}
             |SELECT $Columns FROM $Labelled d
             | WHERE plugin = ? AND superseded_at IS NULL AND body IS NOT NULL
             |   AND ${SqlClearance.kept("d")}
             | ORDER BY written_at DESC, version DESC LIMIT ?""".stripMargin
        ) { ps =>
          ps.setString(SqlClearance.Params + 1, PluginName.value(plugin))
          ps.setInt(SqlClearance.Params + 2, n)
        }(document)

    def referring(ref: DocRef, n: Int)(using tx: Tx^): Either[StoreError, Vector[Document]] =
      if (n <= 0) Right(Vector.empty)
      else
        cleared(
          s"""WITH ${SqlClearance.With}
             |SELECT $Columns FROM $Labelled d
             | WHERE plugin = ? AND superseded_at IS NULL AND body IS NOT NULL
             |   AND ${SqlClearance.kept("d")}
             |   AND EXISTS (SELECT 1 FROM grit.document_refs r
             |                WHERE r.version = d.version AND r.kind = ?
             |                  AND md5(r.ref) = md5(?) AND r.ref = ?)
             | ORDER BY written_at DESC, version DESC LIMIT ?""".stripMargin
        ) { ps =>
          ps.setString(SqlClearance.Params + 1, PluginName.value(plugin))
          ps.setString(SqlClearance.Params + 2, ref.kind)
          ps.setString(SqlClearance.Params + 3, ref.ref)
          ps.setString(SqlClearance.Params + 4, ref.ref)
          ps.setInt(SqlClearance.Params + 5, n)
        }(document)
  }

  /** `plugin`'s documents as it writes them under `terms`, marking in `tombstones` each
    * version it leaves.
    */
  final class Keeper(plugin: PluginName, terms: DocumentTerms, tombstones: Tombstones)
      extends Shelf(plugin)
      with DocumentKeeper {

    def write(
        key: DocKey,
        label: Label,
        integrity: Integrity,
        place: Place,
        text: DocText,
        data: ujson.Value,
        pointer: Option[Pointer],
        at: Instant,
        refs: Set[DocRef]
    )(using tx: Tx^): Either[StoreError, Written] =
      put(key, label.join(Tx.clearance(tx).floor), integrity, place, text, data, pointer, at, refs)

    /** As [[write]], kept at exactly `kept`. */
    private def put(
        key: DocKey,
        kept: Label,
        integrity: Integrity,
        place: Place,
        text: DocText,
        data: ujson.Value,
        pointer: Option[Pointer],
        at: Instant,
        refs: Set[DocRef]
    )(using tx: Tx^): Either[StoreError, Written] = {
      val clearance: Clearance = Tx.clearance(tx)
      val held = refsJson(refs)
      if (refs.size > DocRef.Most)
        Left(StoreError.Invalid(s"a document holds at most ${DocRef.Most} refs, not ${refs.size}"))
      else
        SqlLabels.intern(kept).flatMap { id =>
          rows(
            s"""SELECT version,
            |       (body IS NOT DISTINCT FROM ?
            |        AND place = ARRAY(SELECT jsonb_array_elements_text(?::jsonb))
            |        AND data = ?::jsonb
            |        AND pointer IS NOT DISTINCT FROM ?::jsonb
            |        AND integrity_id = ${SqlIntegrity.Interned}
            |        AND (SELECT count(*) FROM grit.document_refs x
            |              WHERE x.version = d.version) = ?
            |        AND NOT EXISTS (
            |              SELECT 1 FROM grit.document_refs x
            |               WHERE x.version = d.version
            |                 AND NOT EXISTS (SELECT 1 FROM jsonb_to_recordset(?::jsonb) AS r(kind text, ref text)
            |                                  WHERE r.kind = x.kind AND r.ref = x.ref))) AS same
            |  FROM grit.documents d
            | WHERE plugin = ? AND key = ? AND classification_id = ? AND superseded_at IS NULL
            |   FOR UPDATE""".stripMargin
          ) { ps =>
            ps.setString(1, DocText.value(text))
            ps.setString(2, path(place).render())
            ps.setString(3, data.render())
            ps.setString(4, pointer.map(pointerJson).orNull)
            SqlIntegrity.bind(ps, 5, integrity)
            ps.setInt(7, refs.size)
            ps.setString(8, held)
            ps.setString(9, PluginName.value(plugin))
            ps.setString(10, DocKey.value(key))
            ps.setInt(11, id)
          }(rs => versionOf(rs).map(_ -> rs.getBoolean("same"))).flatMap {
            // The parameter's type written out: inferred, it is Vector's array-holding refinement.
            (head: Vector[(DocumentVersionId, Boolean)]) =>
              head match {
                case Vector((current, true)) => Right(Written.Unchanged(current, kept))
                case before =>
                  for {
                    _ <- before.foldLeft[Either[StoreError, Unit]](Right(())) {
                      case (acc, (v, _)) =>
                        acc.flatMap(_ => supersede(v, at))
                    }
                    written <- rows(
                      s"""INSERT INTO grit.documents
                      |       (plugin, key, place, body, data, pointer, written_at, last_placed,
                      |        classification_id, room_id, integrity_id)
                      |VALUES (?, ?, ARRAY(SELECT jsonb_array_elements_text(?::jsonb)), ?, ?::jsonb,
                      |        ?::jsonb, ?, ?, ?, $RoomId, ${SqlIntegrity.Interned})
                      |RETURNING version""".stripMargin
                    ) { ps =>
                      ps.setString(1, PluginName.value(plugin))
                      ps.setString(2, DocKey.value(key))
                      ps.setString(3, path(place).render())
                      ps.setString(4, DocText.value(text))
                      ps.setString(5, data.render())
                      ps.setString(6, pointer.map(pointerJson).orNull)
                      ps.setObject(7, utc(at))
                      ps.setObject(8, utc(at))
                      ps.setInt(9, id)
                      ps.setString(10, room(clearance))
                      SqlIntegrity.bind(ps, 11, integrity)
                    }(versionOf)
                    version <- written.headOption.toRight(
                      StoreError.Invalid("a write returned no version")
                    )
                    _ <-
                      if (refs.isEmpty) Right(0)
                      else
                        update(
                          """INSERT INTO grit.document_refs (version, kind, ref)
                          |SELECT ?, r.kind, r.ref FROM jsonb_to_recordset(?::jsonb) AS r(kind text, ref text)""".stripMargin
                        ) { ps =>
                          ps.setLong(1, DocumentVersionId.value(version))
                          ps.setString(2, held)
                        }
                    gone <- excess(key, id)
                    _ <- gone.foldLeft[Either[StoreError, Unit]](Right(())) { (acc, g) =>
                      acc.flatMap(_ => withdrawn(g._1, g._3, at).map(_ => ()))
                    }
                  } yield Written.Versioned(version, kept, gone.map(g => (g._1, g._2)))
              }
          }
        }
    }

    def withdraw(key: DocKey, label: Label, at: Instant)(using
        tx: Tx^
    ): Either[StoreError, Option[DocumentVersionId]] =
      SqlLabels.intern(label).flatMap(withdrawn(key, _, at))

    def declassifying(label: Label, pointer: Pointer)(using
        tx: Tx^
    ): Either[StoreError, Option[PolicyApplication.InForce]] =
      cleared(
        s"""WITH ${SqlClearance.With}
          |SELECT a.version AS application, a.over, p.version AS policy,
          |       ${SqlDeclassifications.KeptFrom} AS kept_from,
          |       CASE WHEN p.classification_id = ANY ((SELECT everywhere FROM clearance)::smallint[])
          |            THEN p.text END AS text,
          |       CASE WHEN p.classification_id = ANY ((SELECT everywhere FROM clearance)::smallint[])
          |            THEN p.checks END AS checks
          |  FROM grit.declassification_applications a
          |  JOIN grit.declassification_policies p ON p.over = a.over
          | WHERE a.pointer = ?::jsonb AND NOT ${SqlDeclassifications.ApplicationSuperseded}
          |   AND ${SqlDeclassifications.PolicyInForce}""".stripMargin
      ) { ps =>
        ps.setString(SqlClearance.Params + 1, pointerJson(pointer))
      }(rs =>
        DeclassificationPolicy.Over
          .read(rs.getString("over"))
          .map(over =>
            new PolicyApplication.InForce(
              PolicyApplication(
                DeclassificationPolicy(over, rs.getInt("policy")),
                pointer,
                rs.getInt("application")
              ),
              rs.getInt("kept_from"),
              // The text is NOT NULL here, so it is NULL only when the clearance does not read it.
              Option(rs.getString("text"))
                .map(text => (text, Checks.read(Option(rs.getString("checks")).map(ujson.read(_)))))
            )
          )
      ).map(_.headOption.filter(_.applied.policy.lowered(label).isDefined))

    def applying(label: Label, pointer: Pointer)(using
        tx: Tx^
    ): Either[StoreError, Option[DeclassificationPolicy.Over]] =
      rows(
        s"""SELECT a.over FROM grit.declassification_applications a
          | WHERE a.pointer = ?::jsonb AND a.over IS NOT NULL
          |   AND NOT ${SqlDeclassifications.ApplicationSuperseded}""".stripMargin
      )(ps => ps.setString(1, pointerJson(pointer)))(rs =>
        DeclassificationPolicy.Over.read(rs.getString("over"))
      ).map(_.headOption.filter(_.lowered(label).isDefined))

    def keepDeclassified(row: Declassified.Row, at: Instant)(using
        tx: Tx^
    ): Either[StoreError, Written] = {
      val applied = row.applied
      val over = applied.policy.over.written
      for {
        current <- rows(
          s"""SELECT EXISTS (
            |  SELECT 1 FROM grit.declassification_policies p, grit.declassification_applications a
            |   WHERE p.over = ? AND p.version = ? AND ${SqlDeclassifications.PolicyInForce}
            |     AND a.pointer = ?::jsonb AND a.version = ? AND a.over = p.over
            |     AND NOT ${SqlDeclassifications.ApplicationSuperseded}
            |) AS current""".stripMargin
        ) { ps =>
          ps.setString(1, over)
          ps.setInt(2, applied.policy.version)
          ps.setString(3, pointerJson(applied.document))
          ps.setInt(4, applied.version)
        }(rs => Right(rs.getBoolean("current")))
        _ <- Either.cond(
          current.contains(true),
          (),
          StoreError.Invalid("the declassification's policy or application is no longer current")
        )
        written <- put(
          row.key,
          row.label,
          row.integrity,
          row.place,
          row.text,
          row.data,
          None,
          at,
          Set.empty
        )
        id <- SqlLabels.intern(row.label)
        others <- rows(
          """SELECT classification_id FROM grit.documents
            | WHERE plugin = ? AND key = ? AND classification_id <> ?
            |   AND superseded_at IS NULL AND body IS NOT NULL""".stripMargin
        ) { ps =>
          ps.setString(1, PluginName.value(plugin))
          ps.setString(2, DocKey.value(row.key))
          ps.setInt(3, id)
        }(rs => Right(rs.getInt("classification_id")))
        _ <- others.foldLeft[Either[StoreError, Unit]](Right(())) { (acc, other) =>
          acc.flatMap(_ => withdrawn(row.key, other, at).map(_ => ()))
        }
        _ <- written match {
          case Written.Versioned(version, _, _) =>
            update(
              """INSERT INTO grit.declassifications
                |       (document_version, source_version, pointer, over, policy_version,
                |        application_version, judged, attempt, made_at)
                |VALUES (?, ?, ?::jsonb, ?, ?, ?, ?, ?, ?)""".stripMargin
            ) { ps =>
              ps.setLong(1, DocumentVersionId.value(version))
              ps.setLong(2, DocumentVersionId.value(row.source))
              ps.setString(3, pointerJson(applied.document))
              ps.setString(4, over)
              ps.setInt(5, applied.policy.version)
              ps.setInt(6, applied.version)
              ps.setFloat(7, Probability.value(row.judged).toFloat)
              ps.setShort(8, row.attempt.number.toShort)
              ps.setObject(9, utc(at))
            }
          case Written.Unchanged(_, _) => Right(0)
        }
      } yield written
    }

    def checks(policy: DeclassificationPolicy)(using
        tx: Tx^
    ): Either[StoreError, Option[Declassified.PolicyChecks]] =
      cleared(
        s"""WITH ${SqlClearance.With}
          |SELECT p.checks
          |  FROM grit.declassification_policies p
          | WHERE p.over = ? AND p.version = ? AND p.text IS NOT NULL
          |   AND p.classification_id = ANY ((SELECT everywhere FROM clearance)::smallint[])""".stripMargin
      ) { ps =>
        ps.setString(SqlClearance.Params + 1, policy.over.written)
        ps.setInt(SqlClearance.Params + 2, policy.version)
      }(rs =>
        Right(
          new Declassified.PolicyChecks(
            policy,
            Checks.read(Option(rs.getString("checks")).map(ujson.read(_)))
          )
        )
      ).map(_.headOption)

    def benchable(over: DeclassificationPolicy.Over)(using
        tx: Tx^
    ): Either[StoreError, Vector[DeclassificationPolicy]] =
      rows(
        s"""SELECT p.version FROM grit.declassification_policies p
          | WHERE p.over = ?
          |   AND (${SqlDeclassifications.PolicyInForce}
          |        OR (p.status = 'draft' AND NOT ${SqlDeclassifications.PolicyOutranked}
          |            AND NOT EXISTS (SELECT 1 FROM grit.declassification_policies l
          |                             WHERE l.over = p.over AND l.version > p.version)))
          | ORDER BY p.version""".stripMargin
      )(ps => ps.setString(1, over.written))(rs =>
        Right(DeclassificationPolicy(over, rs.getInt("version")))
      )

    def benched(policy: DeclassificationPolicy)(using
        tx: Tx^
    ): Either[StoreError, Option[(Declassified.Screening, Benched)]] =
      rows(
        """SELECT screening, passed, examples::text AS examples
          |  FROM grit.declassification_benches
          | WHERE over = ? AND policy_version = ?""".stripMargin
      ) { ps =>
        ps.setString(1, policy.over.written)
        ps.setInt(2, policy.version)
      }(rs =>
        Right(
          for {
            screening <- Declassified.Screening.read(rs.getString("screening"))
            benched <- Benched
              .read(rs.getBoolean("passed"), ujson.read(rs.getString("examples")))
              .toOption
          } yield (screening, benched)
        )
      ).map(_.headOption.flatten)

    def keepBenched(row: Declassified.BenchRow, at: Instant)(using
        tx: Tx^
    ): Either[StoreError, Unit] =
      update(
        """INSERT INTO grit.declassification_benches
          |       (over, policy_version, screening, passed, examples, benched_at)
          |SELECT p.over, p.version, ?, ?, ?::jsonb, ?
          |  FROM grit.declassification_policies p
          | WHERE p.over = ? AND p.version = ?
          |ON CONFLICT (over, policy_version) DO UPDATE
          |   SET screening = EXCLUDED.screening, passed = EXCLUDED.passed,
          |       examples = EXCLUDED.examples, benched_at = EXCLUDED.benched_at""".stripMargin
      ) { ps =>
        ps.setString(1, row.screening.written)
        ps.setBoolean(2, row.benched.passed)
        ps.setString(3, Benched.write(row.benched).render())
        ps.setObject(4, utc(at))
        ps.setString(5, row.policy.over.written)
        ps.setInt(6, row.policy.version)
      }.flatMap(n => Either.cond(n > 0, (), StoreError.Invalid("no such policy version is kept")))

    def forReview(version: DocumentVersionId, day: LocalDate, at: Instant)(using
        tx: Tx^
    ): Either[StoreError, Boolean] = {
      val clearance = Tx.clearance(tx)
      update(
        s"""WITH ${SqlClearance.With}
           |INSERT INTO grit.document_reviews (plugin, key, classification_id, day, version, queued_at)
           |SELECT d.plugin, d.key, d.classification_id, ?, d.version, ?
           |  FROM grit.documents d
           | WHERE d.version = ? AND d.plugin = ? AND d.superseded_at IS NULL
           |   AND d.body IS NOT NULL AND ${SqlClearance.kept("d")}
           |ON CONFLICT DO NOTHING""".stripMargin
      ) { ps =>
        SqlClearance.bind(ps, 1, clearance)
        ps.setObject(SqlClearance.Params + 1, day)
        ps.setObject(SqlClearance.Params + 2, utc(at))
        ps.setLong(SqlClearance.Params + 3, DocumentVersionId.value(version))
        ps.setString(SqlClearance.Params + 4, PluginName.value(plugin))
      }.map(_ == 1)
    }

    /** As [[withdraw]], at the label whose id is `label`. */
    private def withdrawn(key: DocKey, label: Int, at: Instant)(using
        tx: Tx^
    ): Either[StoreError, Option[DocumentVersionId]] =
      SqlKept.Documents
        .current(plugin, DocKey.value(key), label, "body IS NOT NULL")(versionOf)
        .flatMap(_.fold(Right(None)) { current =>
          for {
            _ <- supersede(current, at)
            written <- rows(
              """INSERT INTO grit.documents
                |       (plugin, key, place, body, written_at, last_placed, classification_id, room_id,
                |        integrity_id)
                |SELECT plugin, key, place, NULL, ?, ?, classification_id, room_id, integrity_id
                |  FROM grit.documents
                | WHERE version = ?
                |RETURNING version""".stripMargin
            ) { ps =>
              ps.setObject(1, utc(at))
              ps.setObject(2, utc(at))
              ps.setLong(3, DocumentVersionId.value(current))
            }(versionOf)
            withdrawal <- written.headOption.toRight(
              StoreError.Invalid("a withdrawal returned no version")
            )
            _ <- tombstones.write(Target.Document(withdrawal), at)
          } yield Some(withdrawal)
        })

    private def supersede(version: DocumentVersionId, at: Instant)(using Tx^) =
      SqlKept.Documents.supersede(Target.Document(version), tombstones, at)

    /** The keys and labels past the bound once one is written under `kept` at the label whose
      * id is `label`, with that label's id: the current documents but it, of every label,
      * placed least recently first, ties by key, then label in its written form, bytewise.
      */
    private def excess(kept: DocKey, label: Int)(using
        tx: Tx^
    ): Either[StoreError, Vector[(DocKey, Label, Int)]] =
      rows(
        SqlKept.cut(
          s"""SELECT d.key, d.last_placed, d.classification_id, d.label_level, d.label_compartments,
            |       concat_ws('+', VARIADIC (ARRAY[(ARRAY['public', 'internal', 'confidential',
            |                                             'restricted'])[d.label_level + 1]]
            |                                || l.compartments)) AS written, 1 AS measure
            |  FROM $Labelled d JOIN grit.classifications l ON l.id = d.classification_id
            | WHERE d.plugin = ? AND d.superseded_at IS NULL AND d.body IS NOT NULL""".stripMargin,
          """last_placed, key COLLATE "C", written COLLATE "C"""",
          "key = ? AND classification_id = ?"
        )
      ) { ps =>
        ps.setString(1, PluginName.value(plugin))
        ps.setString(2, DocKey.value(kept))
        ps.setInt(3, label)
        ps.setInt(4, terms.bound)
      }(rs =>
        DocKey
          .of(rs.getString("key"))
          .map(k => (k, SqlLabels.read(rs), rs.getInt("classification_id")))
      )
  }

  /** `refs` as a JSON array of `{"kind":…,"ref":…}`, for `jsonb_to_recordset`. */
  def refsJson(refs: Set[DocRef]): String =
    ujson.Arr
      .from(refs.toVector.map(r => ujson.Obj("kind" -> r.kind, "ref" -> r.ref)))
      .render()

  def versionOf(rs: ResultSet): Either[String, DocumentVersionId] =
    DocumentVersionId.of(rs.getLong("version")).toRight("a document's version")
}
