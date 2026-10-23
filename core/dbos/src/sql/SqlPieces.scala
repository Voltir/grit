package grit.dbos.sql

import java.sql.ResultSet
import java.time.Instant

import grit.core.files.{
  Anchor,
  FormatName,
  KeptAs,
  KeptPiece,
  Piece,
  PieceIndex,
  PieceKeeper,
  PieceSearch,
  PieceStore,
  PiecedFile
}
import grit.core.id.{PieceId, PieceSetId, PluginName}
import grit.core.message.Tokens
import grit.core.place.Place
import grit.core.retention.Target
import grit.core.store.{StoreError, Tombstones, Tx}
import grit.core.visibility.{Clearance, Integrity, Label}

/** [[PieceStore]] over `grit.piece_sets` and `grit.pieces`, ranked by their own BM25 index, its
  * keepers marking in `tombstones` the sets they leave for deletion.
  */
final class SqlPieces(tombstones: Tombstones) extends PieceStore {
  import SqlPieces.*
  import SqlDocuments.{RoomId, cleared, path, rows, strings, update, utc}

  /** `plugin`'s pieces as it writes them, bounded at `bound` bytes. */
  def keeper(plugin: PluginName, bound: Long = PieceKeeper.Bound): PieceKeeper =
    new Keeper(plugin, bound, tombstones)

  def search(plugins: Vector[PluginName], query: String, limit: Int, at: Instant)(using
      tx: Tx^
  ): Either[StoreError, Vector[PieceSearch.Hit]] =
    if (plugins.isEmpty || query.isBlank || limit <= 0) Right(Vector.empty)
    else
      // pg_textsearch scores are negative, more negative better; a row the planner scored
      // without matching scores 0, and is dropped outside the LIMIT, as SqlDocuments' are.
      cleared(
        s"""WITH ${SqlClearance.With},
           |matched AS (
           |  SELECT p.id, p.set_id, p.search_text <@> to_bm25query(?, 'grit.idx_pieces_bm25') AS s
           |    FROM grit.pieces p JOIN grit.piece_sets d ON d.id = p.set_id
           |   WHERE ${SqlClearance.kept("d")}
           |     AND d.plugin IN (SELECT jsonb_array_elements_text(?::jsonb))
           |     AND d.written_at < ? AND (d.superseded_at IS NULL OR d.superseded_at >= ?)
           |   ORDER BY s, p.id
           |   LIMIT ?
           |), ranked AS (
           |  SELECT id, s, row_number() OVER (PARTITION BY set_id ORDER BY s, id) AS r,
           |         count(*) OVER (PARTITION BY set_id) AS n
           |    FROM matched WHERE s < 0
           |)
           |SELECT $Columns, p.set_id, ranked.s, ranked.n
           |  FROM ranked JOIN grit.pieces p ON p.id = ranked.id JOIN $Labelled d ON d.id = p.set_id
           | WHERE ranked.r <= ?
           | ORDER BY ranked.s, p.id
           | LIMIT ?""".stripMargin
      ) { ps =>
        val p = SqlClearance.Params
        ps.setString(p + 1, PieceIndex.query(query))
        ps.setString(p + 2, strings(plugins.map(PluginName.value)))
        ps.setObject(p + 3, utc(at))
        ps.setObject(p + 4, utc(at))
        ps.setInt(p + 5, limit * PieceSearch.PerFile)
        ps.setInt(p + 6, PieceSearch.PerFile)
        ps.setInt(p + 7, limit)
      }(rs =>
        kept(rs).map(k =>
          (rs.getLong("set_id"), rs.getInt("n"), PieceSearch.Hit(k, -rs.getDouble("s"), 0))
        )
      ).map { (found: Vector[(Long, Int, PieceSearch.Hit)]) =>
        // How many more of each file matched than are answered.
        val shown = found.groupBy(_._1).view.mapValues(_.size).toMap
        found.map((set, matched, hit) => hit.copy(more = matched - shown.getOrElse(set, 0)))
      }

  def holding(plugins: Vector[PluginName], at: Instant)(using
      tx: Tx^
  ): Either[StoreError, Vector[PluginName]] =
    if (plugins.isEmpty) Right(Vector.empty)
    else
      cleared(
        s"""WITH ${SqlClearance.With}
           |SELECT DISTINCT d.plugin FROM grit.piece_sets d
           | WHERE d.plugin IN (SELECT jsonb_array_elements_text(?::jsonb))
           |   AND d.written_at < ? AND (d.superseded_at IS NULL OR d.superseded_at >= ?)
           |   AND ${SqlClearance.kept("d")}
           |   AND EXISTS (SELECT 1 FROM grit.pieces p WHERE p.set_id = d.id)""".stripMargin
      ) { ps =>
        ps.setString(SqlClearance.Params + 1, strings(plugins.map(PluginName.value)))
        ps.setObject(SqlClearance.Params + 2, utc(at))
        ps.setObject(SqlClearance.Params + 3, utc(at))
      }(rs => PluginName.of(rs.getString("plugin"))).map(found => plugins.filter(found.contains))

  def read(ids: Vector[PieceId])(using tx: Tx^): Either[StoreError, Vector[KeptPiece]] =
    if (ids.isEmpty) Right(Vector.empty)
    else
      cleared(
        s"""WITH ${SqlClearance.With}
           |SELECT $Columns FROM grit.pieces p JOIN $Labelled d ON d.id = p.set_id
           | WHERE p.id IN (SELECT v::bigint FROM jsonb_array_elements_text(?::jsonb) AS e(v))
           |   AND ${SqlClearance.kept("d")}""".stripMargin
      )(_.setString(SqlClearance.Params + 1, strings(ids.map(i => PieceId.value(i).toString))))(
        kept
      ).map { found =>
        val byId = found.map(k => k.id -> k).toMap
        ids.flatMap(byId.get)
      }

  def placed(ids: Vector[PieceId], at: Instant)(using tx: Tx^): Either[StoreError, Unit] =
    if (ids.isEmpty) Right(())
    else
      update(
        """UPDATE grit.piece_sets d SET last_placed = greatest(d.last_placed, ?)
          | WHERE d.id IN (SELECT p.set_id FROM grit.pieces p
          |                 WHERE p.id IN (SELECT v::bigint
          |                                  FROM jsonb_array_elements_text(?::jsonb) AS e(v)))""".stripMargin
      ) { ps =>
        ps.setObject(1, utc(at))
        ps.setString(2, strings(ids.map(i => PieceId.value(i).toString)))
      }.map(_ => ())

  def forget(set: PieceSetId)(using tx: Tx^): Either[StoreError, Unit] =
    update("DELETE FROM grit.piece_sets WHERE id = ?")(_.setLong(1, PieceSetId.value(set)))
      .map(_ => ())

  def remove(plugin: PluginName, at: Instant)(using tx: Tx^): Either[StoreError, Unit] =
    update(
      """WITH gone AS (DELETE FROM grit.piece_sets WHERE plugin = ? RETURNING id)
        |UPDATE grit.tombstones SET collected_at = ?
        | WHERE kind = ? AND collected_at IS NULL
        |   AND target IN (SELECT id::text FROM gone)""".stripMargin
    ) { ps =>
      ps.setString(1, PluginName.value(plugin))
      ps.setObject(2, utc(at))
      ps.setString(3, Target.Kind.PieceSet.name)
    }.map(_ => ())

  /** `plugin`'s pieces as it writes them under `bound`, marking in `tombstones` each set it
    * leaves.
    */
  private final class Keeper(plugin: PluginName, bound: Long, tombstones: Tombstones)
      extends PieceKeeper {

    def keep(
        file: PiecedFile,
        place: Place,
        integrity: Integrity,
        pieces: Vector[Piece],
        cut: Int,
        at: Instant
    )(using tx: Tx^): Either[StoreError, Int] = {
      val clearance: Clearance = Tx.clearance(tx)
      val room = rooms(clearance)
      for {
        label <- SqlLabels.intern(clearance.floor)
        before <- current(file.ref, label)
        same <- before.fold(Right(false)) { set =>
          rows(
            s"""SELECT place = ARRAY(SELECT jsonb_array_elements_text(?::jsonb))
               |       AND integrity_id = ${SqlIntegrity.Interned}
               |       AND format = ? AND format_version = ? AND target = ? AS same
               |  FROM grit.piece_sets WHERE id = ?""".stripMargin
          ) { ps =>
            ps.setString(1, path(place).render())
            SqlIntegrity.bind(ps, 2, integrity)
            ps.setString(4, file.format.value)
            ps.setInt(5, file.version)
            ps.setLong(6, Tokens.value(file.target))
            ps.setLong(7, set)
          }(rs => Right(rs.getBoolean("same"))).flatMap { (found: Vector[Boolean]) =>
            if (!found.contains(true)) Right(false)
            else pieced(set).map(_.map(_._3) == pieces)
          }
        }
        kept <-
          if (same)
            before.fold(Right(pieces.size)) { set =>
              update(
                "UPDATE grit.piece_sets SET name = ?, revision = ?, pointer = ?::jsonb WHERE id = ?"
              ) { ps =>
                ps.setString(1, file.name)
                ps.setString(2, file.revision)
                ps.setString(3, SqlDocuments.pointerJson(file.pointer))
                ps.setLong(4, set)
              }.map(_ => pieces.size)
            }
          else
            for {
              olds <- before.fold(Right(Vector.empty[(Long, String, Piece)]))(pieced)
              _ <- before.fold(Right(()))(supersede(_, at))
              made <- rows(
                s"""INSERT INTO grit.piece_sets
                   |       (plugin, ref, name, revision, format, format_version, place,
                   |        classification_id, room_id, integrity_id, written_at, last_placed, cut,
                   |        pointer, target)
                   |VALUES (?, ?, ?, ?, ?, ?, ARRAY(SELECT jsonb_array_elements_text(?::jsonb)),
                   |        ?, $RoomId, ${SqlIntegrity.Interned}, ?, ?, ?, ?::jsonb, ?)
                   |RETURNING id""".stripMargin
              ) { ps =>
                ps.setString(1, PluginName.value(plugin))
                ps.setString(2, file.ref)
                ps.setString(3, file.name)
                ps.setString(4, file.revision)
                ps.setString(5, file.format.value)
                ps.setInt(6, file.version)
                ps.setString(7, path(place).render())
                ps.setInt(8, label)
                ps.setString(9, room)
                SqlIntegrity.bind(ps, 10, integrity)
                ps.setObject(12, utc(at))
                ps.setObject(13, utc(at))
                ps.setInt(14, cut)
                ps.setString(15, SqlDocuments.pointerJson(file.pointer))
                ps.setLong(16, Tokens.value(file.target))
              }(rs => Right(rs.getLong("id")))
              set <- made.headOption.toRight(StoreError.Invalid("a keep returned no set"))
              _ <- written(set, file.name, pieces, olds)
              _ <- bounded(set, at)
            } yield pieces.size
      } yield kept
    }

    def withdraw(ref: String, label: Label, at: Instant)(using Tx^): Either[StoreError, Boolean] =
      for {
        interned <- SqlLabels.intern(label)
        before <- current(ref, interned)
        _ <- before.fold(Right(()))(supersede(_, at))
      } yield before.nonEmpty

    def relabel(ref: String, from: Label, place: Place, most: Integrity, at: Instant)(using
        tx: Tx^
    ): Either[StoreError, Int] = {
      val clearance: Clearance = Tx.clearance(tx)
      for {
        interned <- SqlLabels.intern(from)
        before <- current(ref, interned)
        moved <- before.fold(Right(0))(old => relabelled(old, ref, clearance, place, most, at))
      } yield moved
    }

    def unread(ref: String, revision: String, place: Place, why: String, at: Instant)(using
        tx: Tx^
    ): Either[StoreError, Unit] = {
      val clearance: Clearance = Tx.clearance(tx)
      for {
        label <- SqlLabels.intern(clearance.floor)
        before <- current(ref, label)
        _ <- before.fold(Right(()))(supersede(_, at))
        // Believed at the least: it holds nothing read of the file.
        _ <- update(
          s"""INSERT INTO grit.piece_sets
             |       (plugin, ref, name, revision, place, classification_id, room_id, integrity_id,
             |        written_at, last_placed, unread)
             |VALUES (?, ?, ?, ?, ARRAY(SELECT jsonb_array_elements_text(?::jsonb)), ?, $RoomId,
             |        ${SqlIntegrity.Interned}, ?, ?, ?)""".stripMargin
        ) { ps =>
          ps.setString(1, PluginName.value(plugin))
          ps.setString(2, ref)
          ps.setString(3, ref)
          ps.setString(4, revision)
          ps.setString(5, path(place).render())
          ps.setInt(6, label)
          ps.setString(7, rooms(clearance))
          SqlIntegrity.bind(ps, 8, Integrity.Untrusted)
          ps.setObject(10, utc(at))
          ps.setObject(11, utc(at))
          ps.setString(12, why)
        }
      } yield ()
    }

    def kept(refs: Vector[String])(using tx: Tx^): Either[StoreError, Map[String, KeptAs]] =
      if (refs.isEmpty) Right(Map.empty)
      else
        for {
          label <- SqlLabels.intern(Tx.clearance(tx).floor)
          found <- rows(
            """SELECT ref, format, format_version, target, unread FROM grit.piece_sets
              | WHERE plugin = ? AND classification_id = ? AND superseded_at IS NULL
              |   AND ref = ANY (ARRAY(SELECT jsonb_array_elements_text(?::jsonb)))""".stripMargin
          ) { ps =>
            ps.setString(1, PluginName.value(plugin))
            ps.setInt(2, label)
            ps.setString(3, ujson.Arr.from(refs.map(ujson.Str(_))).render())
          } { rs =>
            val as = Option(rs.getString("unread")) match {
              case Some(why) => KeptAs.Unread(why)
              case None =>
                KeptAs.Read(
                  FormatName(rs.getString("format")),
                  rs.getInt("format_version"),
                  Tokens(rs.getLong("target"))
                )
            }
            Right(rs.getString("ref") -> as)
          }
        } yield found.toMap

    /** As [[relabel]], the set `old` of `ref` moved to `clearance`'s floor: how many pieces. */
    private def relabelled(
        old: Long,
        ref: String,
        clearance: Clearance,
        place: Place,
        most: Integrity,
        at: Instant
    )(using tx: Tx^): Either[StoreError, Int] =
      for {
        believed <- rows(
          s"SELECT d.integrity_level, d.integrity_compartments FROM $Labelled d WHERE d.id = ?"
        )(_.setLong(1, old))(rs => Right(SqlIntegrity.read(rs)))
        integrity <- believed.headOption.toRight(StoreError.Invalid(s"no piece set $old"))
        _ <- supersede(old, at)
        label <- SqlLabels.intern(clearance.floor)
        there <- current(ref, label)
        _ <- there.fold(Right(()))(supersede(_, at))
        made <- rows(
          s"""INSERT INTO grit.piece_sets
             |       (plugin, ref, name, revision, format, format_version, pointer, target, place,
             |        classification_id, room_id, integrity_id, written_at, last_placed, cut, unread)
             |SELECT plugin, ref, name, revision, format, format_version, pointer, target,
             |       ARRAY(SELECT jsonb_array_elements_text(?::jsonb)), ?, $RoomId,
             |       ${SqlIntegrity.Interned}, ?, last_placed, cut, unread
             |  FROM grit.piece_sets WHERE id = ?
             |RETURNING id""".stripMargin
        ) { ps =>
          ps.setString(1, path(place).render())
          ps.setInt(2, label)
          ps.setString(3, rooms(clearance))
          SqlIntegrity.bind(ps, 4, integrity.meet(most))
          ps.setObject(6, utc(at))
          ps.setLong(7, old)
        }(rs => Right(rs.getLong("id")))
        set <- made.headOption.toRight(StoreError.Invalid("a relabel returned no set"))
        n <- update("UPDATE grit.pieces SET set_id = ? WHERE set_id = ?") { ps =>
          ps.setLong(1, set)
          ps.setLong(2, old)
        }
      } yield n

    /** The path of `clearance`'s own room, as [[RoomId]] binds it: none when it has none. */
    private def rooms(clearance: Clearance): String =
      ujson.Arr.from(clearance.own.toVector.flatMap(_.room.segments).map(ujson.Str(_))).render()

    /** The current set of `ref` at the label whose id is `label`, locked. */
    private def current(ref: String, label: Int)(using tx: Tx^): Either[StoreError, Option[Long]] =
      rows(
        """SELECT id FROM grit.piece_sets
          | WHERE plugin = ? AND ref = ? AND classification_id = ? AND superseded_at IS NULL
          |   FOR UPDATE""".stripMargin
      ) { ps =>
        ps.setString(1, PluginName.value(plugin))
        ps.setString(2, ref)
        ps.setInt(3, label)
      }(rs => Right(rs.getLong("id"))).map(_.headOption)

    /** `set`'s pieces in order: each one's id, fingerprint and piece. */
    private def pieced(
        set: Long
    )(using tx: Tx^): Either[StoreError, Vector[(Long, String, Piece)]] =
      rows(
        """SELECT id, encode(fingerprint, 'hex') AS fingerprint, array_to_json(heading)::text AS heading,
          |       tree::text AS tree, anchor
          |  FROM grit.pieces WHERE set_id = ? ORDER BY ordinal""".stripMargin
      )(_.setLong(1, set))(rs =>
        piece(rs).map(p => (rs.getLong("id"), rs.getString("fingerprint"), p))
      )

    /** `set` no longer current from `at`, and marked for deletion. */
    private def supersede(set: Long, at: Instant)(using tx: Tx^): Either[StoreError, Unit] =
      for {
        _ <- update("UPDATE grit.piece_sets SET superseded_at = ? WHERE id = ?") { ps =>
          ps.setObject(1, utc(at))
          ps.setLong(2, set)
        }
        id <- PieceSetId.of(set).toRight(StoreError.Invalid(s"a piece set's id: $set"))
        _ <- tombstones.write(Target.PieceSet(id), at)
      } yield ()

    /** `pieces` written into `set`, each whose fingerprint is among `olds` (once each) moved
      * there from its old set, keeping its id.
      */
    private def written(
        set: Long,
        name: String,
        pieces: Vector[Piece],
        olds: Vector[(Long, String, Piece)]
    )(using tx: Tx^): Either[StoreError, Unit] = {
      var left = olds
      val records = pieces.zipWithIndex.map { (p, i) =>
        val fingerprint = p.fingerprint.hex
        val moved = left.indexWhere(_._2 == fingerprint) match {
          case -1 => None
          case k =>
            val id = left(k)._1
            left = left.patch(k, Nil, 1)
            Some(id)
        }
        ujson.Obj(
          "id" -> moved.fold[ujson.Value](ujson.Null)(id => ujson.Str(id.toString)),
          "ordinal" -> i,
          "heading" -> ujson.Arr.from(p.heading.map(ujson.Str(_))),
          "tree" -> Piece.tree(p),
          "fingerprint" -> fingerprint,
          "anchor" -> p.anchor.written,
          "search_text" -> PieceIndex.of(name, p),
          "bytes" -> Piece.size(p, name).toDouble
        )
      }
      val (moves, adds) = records.partition(r => !r("id").isNull)
      for {
        _ <-
          if (moves.isEmpty) Right(0)
          else
            update(
              """UPDATE grit.pieces p
                |   SET set_id = ?, ordinal = r.ordinal,
                |       heading = ARRAY(SELECT jsonb_array_elements_text(r.heading)),
                |       tree = r.tree, anchor = r.anchor, search_text = r.search_text,
                |       bytes = r.bytes
                |  FROM jsonb_to_recordset(?::jsonb)
                |       AS r(id text, ordinal int, heading jsonb, tree jsonb, anchor text,
                |            search_text text, bytes int)
                | WHERE p.id = r.id::bigint""".stripMargin
            ) { ps =>
              ps.setLong(1, set)
              ps.setString(2, ujson.Arr.from(moves).render())
            }
        _ <-
          if (adds.isEmpty) Right(0)
          else
            update(
              """INSERT INTO grit.pieces
                |       (set_id, ordinal, heading, tree, fingerprint, anchor, search_text, bytes)
                |SELECT ?, r.ordinal, ARRAY(SELECT jsonb_array_elements_text(r.heading)), r.tree,
                |       decode(r.fingerprint, 'hex'), r.anchor, r.search_text, r.bytes
                |  FROM jsonb_to_recordset(?::jsonb)
                |       AS r(ordinal int, heading jsonb, tree jsonb, fingerprint text, anchor text,
                |            search_text text, bytes int)
                | ORDER BY r.ordinal""".stripMargin
            ) { ps =>
              ps.setLong(1, set)
              ps.setString(2, ujson.Arr.from(adds).render())
            }
      } yield ()
    }

    /** The plugin's sets but `kept` deleted, at once, while its pieces take more than the bound:
      * those no longer current first, the oldest first, then the current ones least recently
      * placed, ties by ref then id; their pending tombstones ended at `at`.
      */
    private def bounded(kept: Long, at: Instant)(using tx: Tx^): Either[StoreError, Unit] =
      update(
        """WITH sized AS (
          |  SELECT s.id, s.superseded_at, s.last_placed, s.ref,
          |         coalesce((SELECT sum(p.bytes) FROM grit.pieces p WHERE p.set_id = s.id), 0) AS bytes
          |    FROM grit.piece_sets s WHERE s.plugin = ?
          |), total AS (SELECT sum(bytes) AS all_bytes FROM sized),
          |ordered AS (
          |  SELECT id, sum(bytes) OVER (ORDER BY superseded_at IS NULL,
          |                                       coalesce(superseded_at, last_placed),
          |                                       ref COLLATE "C", id
          |                              ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS freed
          |    FROM sized WHERE id <> ?
          |), gone AS (
          |  DELETE FROM grit.piece_sets
          |   WHERE id IN (SELECT o.id FROM ordered o, total t
          |                 WHERE t.all_bytes - coalesce(o.freed, 0) > ?)
          |  RETURNING id
          |)
          |UPDATE grit.tombstones SET collected_at = ?
          | WHERE kind = ? AND collected_at IS NULL
          |   AND target IN (SELECT id::text FROM gone)""".stripMargin
      ) { ps =>
        ps.setString(1, PluginName.value(plugin))
        ps.setLong(2, kept)
        ps.setLong(3, bound)
        ps.setObject(4, utc(at))
        ps.setString(5, Target.Kind.PieceSet.name)
      }.map(_ => ())
  }
}

private[sql] object SqlPieces {

  /** `grit.piece_sets` with each set's label's and integrity's columns. */
  val Labelled: String =
    s"(SELECT d.*, ${SqlLabels.columns("l")}, ${SqlIntegrity.columns("d.integrity_id")} " +
      "FROM grit.piece_sets d JOIN grit.classifications l ON l.id = d.classification_id)"

  /** A piece's columns, as [[kept]] reads them, from `grit.pieces p` joined to [[Labelled]] `d`. */
  val Columns: String =
    "p.id, array_to_json(p.heading)::text AS heading, p.tree::text AS tree, p.anchor, d.plugin, " +
      "d.ref, array_to_json(d.place)::text AS place, d.pointer::text AS pointer, " +
      "d.name, d.revision, d.format, d.format_version, d.target, d.written_at, d.label_level, " +
      "d.label_compartments, d.integrity_level, d.integrity_compartments"

  /** The piece `rs` holds in its `heading`, `tree` and `anchor` columns, or why not. */
  def piece(rs: ResultSet): Either[String, Piece] =
    Anchor
      .read(rs.getString("anchor"))
      .left
      .map(why => s"a piece's anchor: $why")
      .flatMap(anchor =>
        Piece.read(
          ujson
            .read(rs.getString("heading"))
            .arrOpt
            .fold(Vector.empty[String])(_.toVector.flatMap(_.strOpt)),
          ujson.read(rs.getString("tree")),
          anchor
        )
      )

  /** The kept piece `rs` holds ([[Columns]]), or why not. */
  def kept(rs: ResultSet): Either[String, KeptPiece] =
    for {
      id <- PieceId.of(rs.getLong("id")).toRight("a piece's id")
      plugin <- PluginName.of(rs.getString("plugin"))
      p <- piece(rs)
      place <- SqlDocuments.placeOf(rs.getString("place"))
      pointer <- Option(rs.getString("pointer"))
        .toRight("a read piece set's pointer: none")
        .flatMap(SqlDocuments.pointerOf)
    } yield KeptPiece(
      id,
      plugin,
      PiecedFile(
        rs.getString("ref"),
        rs.getString("name"),
        rs.getString("revision"),
        FormatName(rs.getString("format")),
        rs.getInt("format_version"),
        pointer,
        Tokens(rs.getLong("target"))
      ),
      place,
      SqlLabels.read(rs),
      SqlIntegrity.read(rs),
      SqlDocuments.instant(rs, "written_at"),
      p
    )
}
