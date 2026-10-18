package grit.dbos.sql

import java.sql.{PreparedStatement, ResultSet}
import java.time.{Instant, OffsetDateTime, ZoneOffset}

import grit.core.document.{
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
  Shelved,
  Written
}
import grit.core.id.{DocKey, DocumentVersion, PluginName}
import grit.core.place.{Namespace, Place}
import grit.core.retention.Target
import grit.core.store.{StoreError, Tombstones, Tx}
import grit.core.tool.ToolName
import grit.core.visibility.{Clearance, Integrity, Label}

/** [[DocumentStore]] over `grit.documents` and `grit.document_terms`, reading as
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

  def read(versions: Vector[DocumentVersion])(using Tx^): Either[StoreError, Vector[Document]] =
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

  def placed(versions: Vector[DocumentVersion], at: Instant)(using
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

  def followed(versions: Vector[DocumentVersion], at: Instant)(using
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

  def forget(version: DocumentVersion)(using tx: Tx^): Either[StoreError, Unit] =
    update("DELETE FROM grit.documents WHERE version = ?")(
      _.setLong(1, DocumentVersion.value(version))
    ).map(_ => ())

  def remove(plugin: PluginName, at: Instant)(using tx: Tx^): Either[StoreError, Unit] =
    for {
      _ <- update(
        """WITH gone AS (DELETE FROM grit.documents WHERE plugin = ? RETURNING version)
          |UPDATE grit.tombstones SET collected_at = ?
          | WHERE kind = ? AND collected_at IS NULL
          |   AND target IN (SELECT version::text FROM gone)""".stripMargin
      ) { ps =>
        ps.setString(1, PluginName.value(plugin))
        ps.setObject(2, utc(at))
        ps.setString(3, Target.Kind.Document.name)
      }
      _ <- update("DELETE FROM grit.document_terms WHERE plugin = ?")(
        _.setString(1, PluginName.value(plugin))
      )
    } yield ()
}

private[sql] object SqlDocuments {

  /** `grit.documents` with each version's label's columns ([[SqlLabels.columns]]), for a read
    * to select [[Columns]] from.
    */
  val Labelled: String =
    s"(SELECT d.*, ${SqlLabels.columns("l")}, ${SqlIntegrity.columns("d.integrity_id")} " +
      "FROM grit.documents d JOIN grit.classifications l ON l.id = d.classification_id)"

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

  /** Current at the instant bound twice here: written before it, superseded by none written
    * before it.
    */
  val CurrentAt: String = "written_at < ? AND (superseded_at IS NULL OR superseded_at >= ?)"

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
       |     AND d.written_at < ? AND (d.superseded_at IS NULL OR d.superseded_at >= ?)
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
  def numbers(versions: Vector[DocumentVersion]): String =
    strings(versions.map(v => DocumentVersion.value(v).toString))

  def path(place: Place): ujson.Arr = ujson.Arr.from(place.segments.map(ujson.Str(_)))

  /** The place whose segments `json` holds. */
  def placeOf(json: String): Either[String, Place] =
    ujson.read(json).arrOpt.fold(Vector.empty[String])(_.toVector.flatMap(_.strOpt)) match {
      case ns +: rest =>
        Namespace.of(ns).map(Place.under(_, rest)).toRight(s"a document's place: no namespace $ns")
      case _ => Right(Place.Everywhere)
    }

  /** `pointer` as its column holds it: `{"arguments":{…},"tool":…}`, compact, every key sorted. */
  def pointerJson(pointer: Pointer): String =
    ujson.Obj("arguments" -> pointer.arguments, "tool" -> ToolName.value(pointer.tool)).render()

  /** The pointer `json` holds ([[pointerJson]]), or why not; JSONB keeps its keys in its own
    * order, so its arguments are sorted again.
    */
  def pointerOf(json: String): Either[String, Pointer] =
    for {
      obj <- ujson.read(json).objOpt.toRight("a document's pointer: not an object")
      tool <- obj.get("tool").flatMap(_.strOpt).toRight("a document's pointer: no tool")
      name <- ToolName.of(tool)
      arguments <- obj
        .get("arguments")
        .flatMap(_.objOpt)
        .toRight("a document's pointer: no arguments")
      pointer <- arguments.toVector match {
        case first +: rest => Pointer.of(name, first, rest*)
        case _ => Left("a document's pointer: no arguments")
      }
    } yield pointer

  /** The version `rs` holds ([[Columns]]), or why not: one withdrawn holds nothing. */
  def document(rs: ResultSet): Either[String, Document] =
    for {
      version <- DocumentVersion.of(rs.getLong("version")).toRight("a document's version")
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
             |                WHERE r.version = d.version AND r.kind = ? AND r.ref = ?)
             | ORDER BY written_at DESC, version DESC LIMIT ?""".stripMargin
        ) { ps =>
          ps.setString(SqlClearance.Params + 1, PluginName.value(plugin))
          ps.setString(SqlClearance.Params + 2, ref.kind)
          ps.setString(SqlClearance.Params + 3, ref.ref)
          ps.setInt(SqlClearance.Params + 4, n)
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
    )(using tx: Tx^): Either[StoreError, Written] = {
      val clearance: Clearance = Tx.clearance(tx)
      val kept = label.join(clearance.floor)
      val room =
        ujson.Arr.from(clearance.own.toVector.flatMap(_.room.segments).map(ujson.Str(_))).render()
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
            (head: Vector[(DocumentVersion, Boolean)]) =>
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
                      ps.setString(10, room)
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
                          ps.setLong(1, DocumentVersion.value(version))
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
    ): Either[StoreError, Option[DocumentVersion]] =
      SqlLabels.intern(label).flatMap(withdrawn(key, _, at))

    /** As [[withdraw]], at the label whose id is `label`. */
    private def withdrawn(key: DocKey, label: Int, at: Instant)(using
        tx: Tx^
    ): Either[StoreError, Option[DocumentVersion]] =
      rows(
        """SELECT version FROM grit.documents
          | WHERE plugin = ? AND key = ? AND classification_id = ? AND superseded_at IS NULL
          |   AND body IS NOT NULL
          |   FOR UPDATE""".stripMargin
      ) { ps =>
        ps.setString(1, PluginName.value(plugin))
        ps.setString(2, DocKey.value(key))
        ps.setInt(3, label)
      }(versionOf).flatMap { (found: Vector[DocumentVersion]) =>
        // At most one: idx_documents_current allows one per key and label.
        found.headOption match {
          case None => Right(None)
          case Some(current) =>
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
                ps.setLong(3, DocumentVersion.value(current))
              }(versionOf)
              withdrawal <- written.headOption.toRight(
                StoreError.Invalid("a withdrawal returned no version")
              )
              _ <- tombstones.write(Target.Document(withdrawal), at)
            } yield Some(withdrawal)
        }
      }

    /** `version` no longer current from `at`, and marked for deletion. */
    private def supersede(version: DocumentVersion, at: Instant)(using
        tx: Tx^
    ): Either[StoreError, Unit] =
      for {
        _ <- update("UPDATE grit.documents SET superseded_at = ? WHERE version = ?") { ps =>
          ps.setObject(1, utc(at))
          ps.setLong(2, DocumentVersion.value(version))
        }
        _ <- tombstones.write(Target.Document(version), at)
      } yield ()

    /** The keys and labels past the bound once one is written under `kept` at the label whose
      * id is `label`, with that label's id: the current documents but it, of every label,
      * placed least recently first, ties by key, then label in its written form, bytewise.
      */
    private def excess(kept: DocKey, label: Int)(using
        tx: Tx^
    ): Either[StoreError, Vector[(DocKey, Label, Int)]] =
      rows(
        s"""WITH holding AS (
          |  SELECT d.key, d.last_placed, d.classification_id, d.label_level, d.label_compartments,
          |         concat_ws('+', VARIADIC (ARRAY[(ARRAY['public', 'internal', 'confidential',
          |                                               'restricted'])[d.label_level + 1]]
          |                                  || l.compartments)) AS written
          |    FROM $Labelled d JOIN grit.classifications l ON l.id = d.classification_id
          |   WHERE d.plugin = ? AND d.superseded_at IS NULL AND d.body IS NOT NULL
          |     AND NOT (d.key = ? AND d.classification_id = ?))
          |SELECT key, classification_id, label_level, label_compartments FROM holding
          | ORDER BY last_placed, key COLLATE "C", written COLLATE "C"
          | LIMIT greatest((SELECT count(*) FROM holding) + 1 - ?, 0)""".stripMargin
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

  def versionOf(rs: ResultSet): Either[String, DocumentVersion] =
    DocumentVersion.of(rs.getLong("version")).toRight("a document's version")
}
