package grit.core.act

import java.time.Instant

import grit.core.document.{DocumentKeeper, DocumentTerms, InMemoryDocuments}
import grit.core.durable.Journaled
import grit.core.either.Traversal.sequence
import grit.core.flow.{Tainted, TestTainted, Trusted}
import grit.core.id.{DocumentVersion, PluginName}
import grit.core.message.Message
import grit.core.need.Needs
import grit.core.stitch.{Strand, StrandExcerpts, StrandShelf}
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.{Integrity, Label, Recorded}

/** [[Dreaming]] for a job's tests: [[ScriptedKeeping]], scripted as it is, whose scans read
  * `strands` and `needs` beside its documents, and whose summaries read what strands' members
  * said through `excerpts` and the documents shown in `documents` (`plugin`'s and its needs'),
  * in a transaction opened as its keeps' are, and ask it of `jsons` as one user message
  * ([[Dreaming.shown]]) after the summary's system text. Under the rules every [[Dreaming]]
  * keeps, which `grit.act.moves.MovesContract` holds it to beside the durable moves: a summary
  * is an ask, named and limited as one; one whose strand's messages changed since its scan
  * asks nothing and is `None`; its reply is believed at the meet of the summary, the run, every
  * message read and every document shown.
  */
final class ScriptedDreaming(
    limits: MoveLimits,
    floor: Label,
    plugin: PluginName,
    terms: DocumentTerms,
    strands: StrandShelf,
    needs: Needs,
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
      body: (I, DocumentKeeper, StrandShelf, Needs, Instant) -> Tx^ ?-> Either[StoreError, A]
  ): Either[MoveError, A] =
    keep(name, from)((i, kept, now) => body(i, kept, strands, needs, now))

  def summarise[T](
      name: MoveName,
      seen: Vector[Strand.Digested],
      shown: Vector[DocumentVersion],
      summary: Tainted[Dreaming.Summary[T]]
  ): Either[MoveError, Option[Dreaming.Summarised[T]]] = {
    val Dreaming.Summary(system, chars, documentChars, reply) = Trusted.asIs(summary).extract
    val readable = plugin +: needs.plugins
    val read = for {
      excerpts <- sequence(seen.map(s => excerpts.excerpt(s, chars)(using opened)))
      docs <- documents.labelled(shown)(using opened)
    } yield (
      sequence(excerpts.map(_.toRight(()))).toOption,
      docs.filter((d, _) => readable.contains(d.plugin))
    )
    read match {
      case Left(e) => making(name, MoveKind.Ask, limits.asks)(Left(MoveError.Store(described(e))))
      case Right((None, _)) => making(name, MoveKind.Ask, limits.asks)(Right(None))
      case Right((Some(strands), docs)) =>
        val believed = (strands.map(_.integrity) ++ docs.map(_._1.integrity))
          .foldLeft(summary.integrity.meet(integrity))(_.meet(_))
        val text = Dreaming.shown(
          strands.map(_.excerpt.text),
          docs.map((d, label) => (label, d.text)),
          documentChars
        )
        val posed = Posed.Json(system, Vector(Message.User(text)), reply)
        ask(name, TestTainted.at(posed, believed))
          .map(a => Some(Dreaming.Summarised(a, docs.map(_._1.version))))
    }
  }
}
