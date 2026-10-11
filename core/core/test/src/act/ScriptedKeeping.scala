package grit.core.act

import java.time.Instant

import grit.core.classify.{Answer, Request}
import grit.core.document.{Document, DocumentKeeper, DocumentTerms, InMemoryDocuments}
import grit.core.durable.Journaled
import grit.core.id.{EntryId, PluginName}
import grit.core.message.Message
import grit.core.place.Service
import grit.core.provider.ModelRequest
import grit.core.store.{StoreError, Tx}
import grit.core.tool.ToolName
import grit.core.visibility.{Clearance, Label}
import grit.dbos.sql.TestTx

/** [[Keeping]] for a job's tests: [[ScriptedMoves]], scripted as it is, whose keeps write
  * `plugin`'s documents in `documents` under `terms`, each in a transaction opened at `floor`,
  * at `at`. Under the rules every [[Keeping]] keeps, which `grit.act.moves.MovesContract` holds
  * it to beside the durable moves: a keep whose body is `Left` keeps none of its writes, is
  * [[MoveError.Store]] in the durable moves' words, and its name is spent; a keep writes at no
  * less than `floor`; keeps are not limited. Its documents keep `documents`' rules: two of
  * these over one `documents` are two runs of a plugin, the second reading what the first kept.
  */
final class ScriptedKeeping(
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
    at: Instant = Instant.EPOCH
) extends ScriptedMoves(limits, asks, calls, jsons, judgments),
      Keeping {

  private val keeper = documents.keeper(plugin, terms)

  def keep[A <: caps.Pure: Journaled](name: MoveName)(
      body: (DocumentKeeper, Instant) -> Tx^ ?-> Either[StoreError, A]
  ): Either[MoveError, A] =
    // Keeps are not limited (`MoveLimits`): no count reaches `Int.MaxValue` in one test.
    making(name, MoveKind.Keep, Int.MaxValue) {
      given Tx = TestTx.fake(Clearance.of(floor))
      documents.savepoints.atomic(body(keeper, at)).left.map(e => MoveError.Store(described(e)))
    }

  /** `plugin`'s current documents, as a run at `floor` reads them: every key's and label's
    * that `floor` reads, most recently written first.
    */
  def kept: Vector[Document] =
    keeper
      .newest(Int.MaxValue)(using TestTx.fake(Clearance.of(floor)))
      .fold(e => throw new java.lang.AssertionError(s"$e"), identity)

  /** A store's failure, in the durable moves' words. */
  private def described(error: StoreError): String = error match {
    case StoreError.DuplicateId(id) => s"entry ${EntryId.value(id)} already exists"
    case StoreError.DatabaseError(cause) => cause
    case StoreError.Invalid(cause) => cause
  }
}
