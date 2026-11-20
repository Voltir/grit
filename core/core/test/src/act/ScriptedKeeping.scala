package grit.core.act

import java.time.Instant

import grit.core.classify.{Answer, Request}
import grit.core.document.{Document, DocumentKeeper, DocumentTerms, InMemoryDocuments}
import grit.core.durable.Journaled
import grit.core.files.{
  FileError,
  Files,
  Formats,
  InMemoryPieces,
  KeptPiece,
  PieceKeeper,
  Understood
}
import grit.core.flow.{Tainted, Trusted}
import grit.core.id.{ConversationId, FileHandle, JobName, PluginName, ScheduleId, TurnRef, TurnSeq}
import grit.core.inbox.InMemoryInbox
import grit.core.job.{Grace, InMemorySchedules}
import grit.core.message.Message
import grit.core.occasion.Occasion
import grit.core.place.{Place, Service}
import grit.core.provider.ModelRequest
import grit.core.speech.Speaking
import grit.core.spend.Budget
import grit.core.store.{StoreError, Tx}
import grit.core.tool.ToolName
import grit.core.visibility.{Clearance, Integrity, Label, Recorded, Visibility}
import grit.dbos.sql.TestTx

/** [[Keeping]] for a job's tests: [[ScriptedMoves]], scripted as it is, whose keeps write
  * `plugin`'s documents in `documents` under `terms`, each in a transaction opened at `floor`
  * under `visibility` whose rooms are labelled as `rooms` records them, at `at`. Under the rules every [[Keeping]] keeps, which `grit.act.moves.MovesContract` holds
  * it to beside the durable moves: a keep whose body is `Left` keeps none of its writes, is
  * [[MoveError.Store]] in the durable moves' words, and its name is spent; a keep writes at no
  * less than `floor`, believed at no more than what it keeps from, and from what is believed
  * less than `Outside` is [[MoveError.Below]] and writes nothing; keeps are not limited. Its documents keep `documents`' rules: two of
  * these over one `documents` are two runs of a plugin, the second reading what the first kept.
  * Its piece moves keep `plugin`'s pieces in `pieces`, each file read as `files` reads its
  * handle (by default none is kept), cut with [[MovesFixtures.PerChar]]; its surveys take the
  * area's readers to be `formats`, as the durable moves take theirs ([[Formats.reads]],
  * [[Formats.first]]), or, with none given, every format at every version to be read and no
  * kind to have a first. Its occasions are made over `occasions`, as the durable moves make
  * theirs ([[Occasioned.found]], [[Occasioned.decided]]), at `at`; by default in an empty
  * inbox, grit never speaking.
  */
class ScriptedKeeping(
    limits: MoveLimits,
    floor: Label,
    plugin: PluginName,
    terms: DocumentTerms,
    asks: ModelRequest -> Either[MoveError, ScriptedMoves.Replied[Message.Assistant]],
    calls: (Service, ToolName, ujson.Obj) -> Either[MoveError, Called],
    jsons: (String, Vector[Message]) -> Either[MoveError, ScriptedMoves.Replied[ujson.Value]] =
      (_, _) => Left(MoveError.Model("no JSON reply scripted")),
    judgments: Request -> Either[MoveError, ScriptedMoves.Replied[Vector[Answer]]] = _ =>
      Left(MoveError.Model("no judgment scripted")),
    documents: InMemoryDocuments = new InMemoryDocuments,
    at: Instant = Instant.EPOCH,
    integrity: Integrity = Integrity.Authority,
    rooms: Recorded = Recorded.Empty,
    val pieces: InMemoryPieces = new InMemoryPieces,
    files: FileHandle -> Either[String, (String, Understood)] = (h: FileHandle) =>
      Left(FileError.Gone(h, Files.Expiry).message),
    formats: Option[Formats] = None,
    visibility: Visibility = Visibility.Shipped,
    // The test's own in-memory inbox, which this run's occasions write and the test reads back on
    // its one thread once the run returns: nothing reaches it after the test.
    @caps.unsafe.untrackedCaptures
    private val occasions: ScriptedKeeping.Occasions^ = ScriptedKeeping.Occasions.Unscripted,
    val schedules: InMemorySchedules = new InMemorySchedules(),
    owns: JobName -> Boolean = (_: JobName) => true
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
          .map(e => MoveError.Store(e.described))
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
          case relabel: Piecing.Relabel =>
            Piecing.relabelled(relabel, pieceKeeper, from.integrity, at)
          case mark: Piecing.Mark => Piecing.marked(mark, pieceKeeper, at)
          case survey: Piecing.Survey =>
            formats match {
              case Some(f) => Piecing.due(survey, pieceKeeper, f.reads, f.first)
              case None => Piecing.due(survey, pieceKeeper, (_, _) => true, _ => None)
            }
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
        done.left.map(e => MoveError.Store(e.described))
      }
    }

  // The rooms this run named occasions in: immutable values, read on the test's one thread.
  @caps.unsafe.untrackedCaptures
  private var named: Set[Place] = Set.empty

  def occasion(name: MoveName, from: Tainted[Occasion]): Either[MoveError, Occasioned] = {
    val room = Trusted.asIs(from).extract.room
    if (!made.contains(name) && named.contains(room)) Left(MoveError.RoomTaken(room))
    else
      making(name, MoveKind.Occasion, limits.occasions) {
        named = named + room
        {
          val o = occasions
          given Tx = opened
          val (occasion, believed) = (Trusted.asIs(from).extract, from.integrity)
          val speaking = o.speaking
          val speech = o.inbox.speech
          val made: Either[String, Occasioned] = for {
            found <- Occasioned
              .found(occasion, keeper, speech, speaking, believed, at)
              .left
              .map(_.described)
            ended <- found match {
              case Left(ended) => Right(ended)
              case Right((origin, to, most)) =>
                for {
                  turn <- o.inbox
                    .occasion(origin, plugin, occasion, at, most)
                    .left
                    .map(_.toString)
                  decided <- Occasioned
                    .decided(
                      occasion,
                      turn,
                      to,
                      most,
                      speaking,
                      o.budget,
                      speech,
                      o.inbox.ledger,
                      at
                    )
                    .left
                    .map(_.described)
                  ended <- decided match {
                    case Left(ended) => Right(ended)
                    case Right(()) =>
                      o.refusing match {
                        case None =>
                          o.inbox
                            .startTurn(turn)
                            .left
                            .map(_.toString)
                            .map(_ => Occasioned.Started(turn))
                        case Some(why) =>
                          Occasioned.unstarted(turn, why, speech, at).left.map(_.described)
                      }
                  }
                } yield ended
            }
          } yield ended
          made.left.map(MoveError.Store(_))
        }
      }
  }

  def later[P <: caps.Pure](
      name: MoveName,
      from: Tainted[Booked[P]]
  ): Either[MoveError, ScheduleId] = {
    val booked = Trusted.asIs(from).extract
    val job = booked.job.name
    if (!made.contains(name) && !owns(job)) Left(MoveError.NotOwn(job))
    else
      making(name, MoveKind.Later, limits.laters) {
        if (from.integrity.belowOutside) Left(MoveError.Below(from.integrity))
        else {
          given Tx = opened
          val id = ScheduleId.booked(ScriptedKeeping.Turn, MoveName.value(name))
          schedules
            .book(
              id,
              job,
              booked.job.write(booked.params),
              booked.at,
              Grace.between(booked.at, booked.until),
              from.integrity,
              at
            )
            .map(_ => id)
            .left
            .map(e => MoveError.Store(e.described))
        }
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

  /** A transaction of a run at `floor` under `visibility`, its rooms labelled as `rooms` records
    * them.
    */
  protected def opened: Tx = TestTx.fake(Clearance.of(floor), visibility, rooms)

}

object ScriptedKeeping {

  /** The turn a scripted run's bookings are keyed by. */
  val Turn: TurnRef = TurnRef(ConversationId("scripted"), TurnSeq.First)

  /** What a scripted run's occasions are made with: `inbox`, which records each, keeps its
    * decision ([[InMemoryInbox.speech]]), reads the day's spend from its ledger and starts each
    * turn grit drafts, unless the start is `refusing`, with why; `speaking` and `budget`, what
    * grit speaks within.
    */
  final case class Occasions(
      inbox: InMemoryInbox,
      speaking: Speaking,
      budget: Budget,
      refusing: Option[String] = None
  )

  object Occasions {

    /** An empty inbox's, grit never speaking. */
    def Unscripted: Occasions^ =
      Occasions(InMemoryInbox.fresh(), Speaking.Off, Budget(java.time.ZoneOffset.UTC, None))
  }
}
