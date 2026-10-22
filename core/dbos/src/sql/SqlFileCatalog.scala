package grit.dbos.sql

import java.sql.ResultSet
import java.time.{Instant, ZoneOffset}

import grit.core.files.{FileCatalog, FileItem, FileOrigin, FileRecord, PutKey}
import grit.core.id.{EdgeName, FileHandle, Sha256}
import grit.core.identity.Email
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.{Provenance, Readers}

/** [[FileCatalog]] over `grit.files`. */
final class SqlFileCatalog extends FileCatalog {
  import SqlStatements.{many, one, update, valid}
  import SqlFileCatalog.*

  def lock()(using tx: Tx^): Either[StoreError, Unit] =
    one("SELECT pg_advisory_xact_lock(?)")(_.setLong(1, AreaLock))(_ => ()).map(_ => ())

  def keyed(key: PutKey)(using tx: Tx^): Either[StoreError, Option[FileRecord]] =
    many(s"SELECT $Columns FROM grit.files f WHERE f.put_key = ?")(
      _.setString(1, PutKey.value(key))
    )(record).flatMap(valid).map(_.headOption)

  def get(handle: FileHandle)(using tx: Tx^): Either[StoreError, Option[FileRecord]] =
    many(s"SELECT $Columns FROM grit.files f WHERE f.id = ?")(
      _.setLong(1, FileHandle.value(handle))
    )(record).flatMap(valid).map(_.headOption)

  def all()(using tx: Tx^): Either[StoreError, Vector[FileRecord]] =
    many(s"SELECT $Columns FROM grit.files f ORDER BY f.id")(_ => ())(record).flatMap(valid)

  def blobs()(using tx: Tx^): Either[StoreError, Map[Sha256, Long]] =
    many("SELECT DISTINCT encode(sha256, 'hex'), bytes FROM grit.files")(_ => ()) { rs =>
      Sha256.read(rs.getString(1)).map(_ -> rs.getLong(2)).toRight("a blob's digest")
    }.flatMap(valid).map(_.toMap)

  def insert(
      key: PutKey,
      name: String,
      bytes: Long,
      sha256: Sha256,
      claimed: Option[String],
      origin: FileOrigin,
      item: FileItem,
      expires: Instant
  )(using tx: Tx^): Either[StoreError, FileRecord] =
    for {
      _ <- update("UPDATE grit.files SET put_key = NULL WHERE put_key = ?")(
        _.setString(1, PutKey.value(key))
      )
      id <- one(
        s"""INSERT INTO grit.files (put_key, sha256, bytes, name, claimed, edge, ref, revision,
           |    fetched_at, expires_at, place, originator, vouched_id)
           |VALUES (?, decode(?, 'hex'), ?, ?, ?, ?, ?, ?, ?, ?,
           |    ARRAY(SELECT jsonb_array_elements_text(?::jsonb)), ?, ${SqlIntegrity.Interned})
           |RETURNING id""".stripMargin
      ) { ps =>
        ps.setString(1, PutKey.value(key))
        ps.setString(2, sha256.hex)
        ps.setLong(3, bytes)
        ps.setString(4, name)
        ps.setString(5, claimed.orNull)
        ps.setString(6, EdgeName.value(origin.edge))
        ps.setString(7, origin.ref)
        ps.setString(8, origin.revision.orNull)
        ps.setObject(9, origin.fetched.atOffset(ZoneOffset.UTC))
        ps.setObject(10, expires.atOffset(ZoneOffset.UTC))
        ps.setString(11, SqlPlaces.path(item.item.place))
        ps.setString(12, originator(item.readers).orNull)
        SqlIntegrity.bind(ps, 13, item.item.vouched)
      }(_.getLong(1))
      handle <- id
        .flatMap(FileHandle.of)
        .toRight(StoreError.Invalid("a file was kept with no handle"))
    } yield FileRecord(handle, name, bytes, sha256, claimed, origin, item, expires)

  def forget(handle: FileHandle)(using tx: Tx^): Either[StoreError, Unit] =
    update("DELETE FROM grit.files WHERE id = ?")(_.setLong(1, FileHandle.value(handle)))
      .map(_ => ())
}

private object SqlFileCatalog {

  /** The area's lock (`pg_advisory_xact_lock`): "gritfile". */
  val AreaLock = 0x67726974_66696c65L

  /** What [[record]] reads of a row of `grit.files f`. */
  val Columns: String =
    "f.id, f.name, f.bytes, encode(f.sha256, 'hex') AS sha, f.claimed, f.edge, f.ref, " +
      "f.revision, f.fetched_at, f.expires_at, array_to_json(f.place)::text AS place, " +
      s"f.originator, ${SqlIntegrity.columns("f.vouched_id")}"

  /** Who reads a file's item, as its `originator` column holds it: `None` for [[Readers.AtPlace]]. */
  def originator(readers: Readers): Option[String] = readers match {
    case Readers.AtPlace => None
    case Readers.Originator(email) => Some(Email.value(email))
  }

  /** The file the current row of `rs` holds, or why it is none. */
  def record(rs: ResultSet): Either[String, FileRecord] =
    for {
      handle <- FileHandle.of(rs.getLong("id")).toRight("a file's handle")
      sha <- Sha256.read(rs.getString("sha")).toRight("a file's digest")
      place <- SqlPlaces.read(rs.getString("place"))
      readers <- Option(rs.getString("originator")) match {
        case None => Right(Readers.AtPlace)
        case Some(e) => Email.of(e).map(Readers.Originator(_))
      }
    } yield FileRecord(
      handle,
      rs.getString("name"),
      rs.getLong("bytes"),
      sha,
      Option(rs.getString("claimed")),
      FileOrigin(
        EdgeName(rs.getString("edge")),
        rs.getString("ref"),
        Option(rs.getString("revision")),
        rs.getObject("fetched_at", classOf[java.time.OffsetDateTime]).toInstant
      ),
      FileItem(Provenance(place, SqlIntegrity.read(rs)), readers),
      rs.getObject("expires_at", classOf[java.time.OffsetDateTime]).toInstant
    )
}
