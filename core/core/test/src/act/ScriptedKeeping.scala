package grit.core.act

import java.time.Instant

import grit.core.classify.{Answer, Request}
import grit.core.document.{Document, DocumentKeeper, DocumentTerms, InMemoryDocuments}
import grit.core.durable.Journaled
import grit.core.files.{FileError, Files, InMemoryPieces, KeptPiece, PieceKeeper, Understood}
import grit.core.flow.{Tainted, Trusted}
import grit.core.id.{EntryId, FileHandle, PluginName}
import grit.core.message.Message
import grit.core.place.Service
import grit.core.provider.ModelRequest
import grit.core.store.{StoreError, Tx}
import grit.core.tool.ToolName
import grit.core.visibility.{Clearance, Integrity, Label, Recorded, Visibility}
import grit.dbos.sql.TestTx

/** [[Keeping]] for a job's tests: [[ScriptedMoves]], scripted as it is, whose keeps write
  * `plugin`'s documents in `documents` under `terms`, each in a transaction opened at `floor`
  * whose rooms are labelled as `rooms` records them, at `at`. Under the rules every [[Keeping]] keeps, which `grit.act.moves.MovesContract` holds
  * it to beside the durable moves: a keep whose body is `Left` keeps none of its writes, is
  * [[MoveError.Store]] in the durable moves' words, and its name is spent; a keep writes at no
  * less than `floor`, believed at no more than what it keeps from, and from what is believed
  * less than `Outside` is [[MoveError.Below]] and writes nothing; keeps are not limited. Its documents keep `documents`' rules: two of
  * these over one `documents` are two runs of a plugin, the second reading what the first kept.
  * Its piece moves keep `plugin`'s pieces in `pieces`, each file read as `files` reads its
  * handle (by default none is kept), cut with [[MovesFixtures.PerChar]].
  */
class ScriptedKeeping(
    limits: MoveLimits,
    floor: Label,
    plugin: PluginName,
    terms: DocumentTerms,
    asks: ModelRequest -> Either[MoveError, Asked[Message.Assistant]],
    calls: (Service, ToolName, ujson.Obj) -> Either[MoveError, Called],
    jsons: (String, Vector[Message]) -> Either[MoveError, Asked[ujson.Value]] = (_, _) =>
      Left(MoveError.Model("no JSON reply scripted")),
    judgments: Request -> Either[MoveError, Asked[Vector[Answer]]] = _ =>
      Left(MoveError.Model("no judgment scripted")),
    documents: InMemoryDocuments = new InMemoryDocuments,
    at: Instant = Instant.EPOCH,
    integrity: Integrity = Integrity.Authority,
    rooms: Recorded = Recorded.Empty,
    val pieces: InMemoryPieces = new InMemoryPieces,
    files: FileHandle -> Either[String, (String, Understood)] = (h: FileHandle) =>
      Left(FileError.Gone(h, Files.Expiry).message)
) extends ScriptedMoves(limits, asks, calls, jsons, judgments, integrity),
      Keeping {

  private val keeper = documents.keeper(plugin, terms)

  private val pieceKeeper = pieces.keeper(plugin, PieceKeeper.Bound)

  def keep[I, A <: caps.Pure: Journaled](name: MoveName, from: Tainted[I])(
      body: (I, DocumentKeeper, Instant) -> Tx^ ?-> Either[StoreError, A]
  ): Either[MoveError, A] =
    // Keeps are not limited (`MoveLimits`): no count reaches `Int.MaxValue` in one test.
    making(name, MoveKind.Keep, Int.MaxValue) {
      if (from.integrity.belowOutside) Left(MoveError.Below(from.integrity))
      else {
        given Tx = opened
        val kept = DocumentKeeper.believedAtMost(keeper, from.integrity)
        documents.savepoints
          .atomic(body(Trusted.asIs(from).extract, kept, at))
          .left
          .map(e => MoveError.Store(described(e)))
      }
    }

  def piece(name: MoveName, from: Tainted[Piecing]): Either[MoveError, Pieced] =
    making(name, MoveKind.Keep, Int.MaxValue) {
      if (from.integrity.belowOutside) Left(MoveError.Below(from.integrity))
      else {
        given Tx = opened
        val done = Trusted.asIs(from).extract match {
          case Piecing.Withdraw(gone) =>
            Piecing.withdrawn(gone, pieceKeeper, at).map(Pieced.Withdrawn(_))
          case read: Piecing.Read =>
            Piecing.kept(
              read,
              files(read.file),
              pieceKeeper,
              from.integrity,
              MovesFixtures.PerChar,
              at
            )
        }
        done.left.map(e => MoveError.Store(described(e)))
      }
    }

  /** `plugin`'s current pieces of files, as a run at `floor` reads them. */
  def pieced: Vector[KeptPiece] = pieces.current(plugin)(using opened)

  /** `plugin`'s current documents, as a run at `floor` reads them: every key's and label's
    * that `floor` reads, most recently written first.
    */
  def kept: Vector[Document] =
    keeper
      .newest(Int.MaxValue)(using opened)
      .fold(e => throw new java.lang.AssertionError(s"$e"), identity)

  /** A transaction of a run at `floor`, its rooms labelled as `rooms` records them. */
  protected def opened: Tx = TestTx.fake(Clearance.of(floor), Visibility.Shipped, rooms)

  /** A store's failure, in the durable moves' words. */
  protected def described(error: StoreError): String = error match {
    case StoreError.DuplicateId(id) => s"entry ${EntryId.value(id)} already exists"
    case StoreError.DatabaseError(cause) => cause
    case StoreError.Invalid(cause) => cause
  }
}
