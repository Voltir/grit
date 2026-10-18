package grit.core.act

import java.time.Instant

import grit.core.document.{DocumentKeeper, DocumentTerms, InMemoryDocuments}
import grit.core.durable.Journaled
import grit.core.flow.{Tainted, TestTainted, Trusted}
import grit.core.id.PluginName
import grit.core.message.Message
import grit.core.stitch.{Strand, StrandExcerpts, StrandShelf}
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.{Integrity, Label, Recorded}

/** [[Dreaming]] for a job's tests: [[ScriptedKeeping]], scripted as it is, whose scans read
  * `strands` beside its documents, and whose summaries read what a strand's members said
  * through `excerpts`, in a transaction opened as its keeps' are, and ask it of `jsons` as one user
  * message after the summary's system text. Under the rules every [[Dreaming]] keeps, which
  * `grit.act.moves.MovesContract` holds it to beside the durable moves: a summary is an ask,
  * named and limited as one; one whose strand's messages changed since its scan asks nothing
  * and is `None`; its reply is believed at the meet of the summary, the run and every message
  * read.
  */
final class ScriptedDreaming(
    limits: MoveLimits,
    floor: Label,
    plugin: PluginName,
    terms: DocumentTerms,
    strands: StrandShelf,
    excerpts: StrandExcerpts,
    jsons: (String, Vector[Message]) -> Either[MoveError, Asked[ujson.Value]],
    documents: InMemoryDocuments = new InMemoryDocuments,
    at: Instant = Instant.EPOCH,
    integrity: Integrity = Integrity.Authority,
    rooms: Recorded = Recorded.Empty
) extends ScriptedKeeping(
      limits,
      floor,
      plugin,
      terms,
      _ => Left(MoveError.Model("no text reply scripted")),
      (_, _, _) => Left(MoveError.Model("no call scripted")),
      jsons,
      documents = documents,
      at = at,
      integrity = integrity,
      rooms = rooms
    ),
      Dreaming {

  def scan[I, A <: caps.Pure: Journaled](name: MoveName, from: Tainted[I])(
      body: (I, DocumentKeeper, StrandShelf, Instant) -> Tx^ ?-> Either[StoreError, A]
  ): Either[MoveError, A] =
    keep(name, from)((i, kept, now) => body(i, kept, strands, now))

  def summarise[T](
      name: MoveName,
      seen: Strand.Digested,
      summary: Tainted[Dreaming.Summary[T]]
  ): Either[MoveError, Option[Asked[T]]] = {
    val Dreaming.Summary(system, chars, reply) = Trusted.asIs(summary).extract
    excerpts.excerpt(seen, chars)(using opened) match {
      case Left(e) => making(name, MoveKind.Ask, limits.asks)(Left(MoveError.Store(described(e))))
      case Right(None) => making(name, MoveKind.Ask, limits.asks)(Right(None))
      case Right(Some(shown)) =>
        val believed = summary.integrity.meet(integrity).meet(shown.integrity)
        val posed = Posed.Json(system, Vector(Message.User(shown.excerpt.text)), reply)
        ask(name, TestTainted.at(posed, believed)).map(Some(_))
    }
  }
}
