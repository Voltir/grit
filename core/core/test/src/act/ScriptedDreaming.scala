package grit.core.act

import java.time.Instant

import grit.core.classify.{Answer, Request}
import grit.core.document.{
  Declassified,
  DocText,
  DocumentKeeper,
  DocumentTerms,
  InMemoryDocuments,
  PolicyApplication
}
import grit.core.durable.Journaled
import grit.core.either.Traversal.sequence
import grit.core.flow.{Tainted, TestTainted, Trusted}
import grit.core.id.{DocumentVersionId, PluginName}
import grit.core.message.Message
import grit.core.need.Needs
import grit.core.provider.ModelRequest
import grit.core.screen.{Screen, Screened}
import grit.core.stitch.{Strand, StrandShelf}
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.{DeclassificationPolicy, Integrity, Label, Recorded}

/** [[Dreaming]] for a job's tests: [[ScriptedKeeping]], scripted as it is, whose scans read
  * `reads`' strands and `plugin`'s needs beside its documents, and whose summaries read what
  * strands' members said and the documents shown (`plugin`'s and its needs') through `reads`,
  * in a transaction opened as its keeps' are, and ask it of `jsons` as one user message
  * ([[Dreaming.shown]]) under [[Dreaming.told]] of the summary's system text. Under the rules every [[Dreaming]]
  * keeps, which `grit.act.moves.MovesContract` holds it to beside the durable moves: a summary
  * is an ask, named and limited as one; one whose strand's messages changed since its scan
  * asks nothing and is `None`; its reply is believed at the meet of the summary, the run, every
  * message read and every document shown. Its declassifies read as its summaries do
  * ([[Declassifying.read]]), ask the rewrite of `asks` and the check of `judgments`, and are
  * always declared.
  */
final class ScriptedDreaming(
    limits: MoveLimits,
    floor: Label,
    plugin: PluginName,
    terms: DocumentTerms,
    reads: Dreaming.Reads,
    jsons: (String, Vector[Message]) -> Either[MoveError, Asked[ujson.Value]],
    documents: InMemoryDocuments = new InMemoryDocuments,
    at: Instant = Instant.EPOCH,
    integrity: Integrity = Integrity.Authority,
    rooms: Recorded = Recorded.Empty,
    asks: ModelRequest -> Either[MoveError, Asked[Message.Assistant]] = (_: ModelRequest) =>
      Left(MoveError.Model("no text reply scripted")),
    judgments: Request -> Either[MoveError, Asked[Vector[Answer]]] = (_: Request) =>
      Left(MoveError.Model("no judgment scripted"))
) extends ScriptedKeeping(
      limits,
      floor,
      plugin,
      terms,
      asks,
      (_, _, _) => Left(MoveError.Model("no call scripted")),
      jsons,
      judgments,
      documents = documents,
      at = at,
      integrity = integrity,
      rooms = rooms
    ),
      Dreaming {

  private val needs = reads.needs(plugin)

  def scan[I, A <: caps.Pure: Journaled](name: MoveName, from: Tainted[I])(
      body: (I, DocumentKeeper, StrandShelf, Needs, Instant) -> Tx^ ?-> Either[StoreError, A]
  ): Either[MoveError, A] =
    keep(name, from)((i, kept, now) => body(i, kept, reads.strands, needs, now))

  def summarise[T](
      name: MoveName,
      seen: Vector[Strand.Digested],
      shown: Vector[DocumentVersionId],
      summary: Tainted[Dreaming.Summary[T]]
  ): Either[MoveError, Option[Dreaming.Summarised[T]]] = {
    val Dreaming.Summary(system, chars, documentChars, reply) = Trusted.asIs(summary).extract
    val readable = plugin +: needs.plugins
    val read = for {
      excerpts <- sequence(seen.map(s => reads.excerpts.excerpt(s, chars)(using opened)))
      docs <- reads.documents.labelled(shown)(using opened)
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
        val posed = Posed.Json(Dreaming.told(system), Vector(Message.User(text)), reply)
        ask(name, TestTainted.at(posed, believed))
          .map(a => Some(Dreaming.Summarised(a, docs.map(_._1.version))))
    }
  }

  protected def declassifier: Option[Declassifying.Raising^] =
    Some(TestTainted.raise(Declassified.declassification))

  protected def rewrite(
      name: MoveName,
      source: DocumentVersionId,
      revision: String,
      applied: PolicyApplication,
      again: Option[Declassifying.Again]
  ): Either[MoveError, Tainted[Declassifying.Rewritten]] =
    shown(source, revision, applied) match {
      case Left(e) => making(name, MoveKind.Ask, limits.asks)(Left(MoveError.Store(described(e))))
      case Right(Left(why)) =>
        making(name, MoveKind.Ask, limits.asks)(
          Right(TestTainted.at(Declassifying.Rewritten.Unapplied(why), integrity))
        )
      case Right(Right(shown)) =>
        val posed = Posed.Text(Declassifying.request(shown, again))
        ask(name, TestTainted.at(posed, integrity.meet(shown.integrity))).flatMap { a =>
          val reply = Trusted.asIs(a.reply).extract
          Declassifying
            .written(reply)
            .left
            .map(MoveError.Model(_))
            .map(text =>
              a.reply.map(_ =>
                text.fold(Declassifying.Rewritten.Withheld)(
                  Declassifying.Rewritten.Made(_, shown.source, reply.model)
                )
              )
            )
        }
    }

  protected def screen(
      name: MoveName,
      source: DocumentVersionId,
      revision: String,
      applied: PolicyApplication,
      rewrite: DocText,
      attempt: Declassified.Attempt
  ): Either[MoveError, Either[Declassifying.Unread, Screened]] =
    Declassifying.checked(name, attempt).left.map(MoveError.Model(_)).flatMap { checking =>
      screening(checking, source, revision, applied) { shown =>
        Right(
          Posed.judge(Screen.Text(DocText.value(rewrite)), Declassifying.screen(shown.checks).ask)
        )
      }
    }

  protected def baseline(
      name: MoveName,
      source: DocumentVersionId,
      revision: String,
      applied: PolicyApplication
  ): Either[MoveError, Either[Declassifying.Unread, Screen.Baseline]] =
    Declassifying.sourced(name).left.map(MoveError.Model(_)).flatMap { sourcing =>
      screening(sourcing, source, revision, applied) { shown =>
        Right(Posed.judge(Screen.Text(shown.text), Declassifying.Baseline))
      }
    }

  protected def example(
      name: MoveName,
      policy: DeclassificationPolicy,
      at: Int
  ): Either[MoveError, Either[Declassifying.Unbenched, Tainted[Declassifying.Examined]]] =
    Declassifying.exhibit(documents.keeper(plugin, terms), policy, at)(using opened) match {
      case Left(e) =>
        making(name, MoveKind.Judge, limits.judgments)(Left(MoveError.Store(described(e))))
      case Right(Left(why)) => making(name, MoveKind.Judge, limits.judgments)(Right(Left(why)))
      case Right(Right(Declassifying.Exhibit(screening, None))) =>
        making(name, MoveKind.Judge, limits.judgments)(
          Right(Right(TestTainted.at(Declassifying.Examined(screening, None), integrity)))
        )
      case Right(Right(Declassifying.Exhibit(screening, Some(e)))) =>
        ask(name, TestTainted.at(Posed.judge(e.text, e.screen.ask), integrity)).map(a =>
          Right(
            a.reply.map(s => Declassifying.Examined(screening, Some((kind = e.kind, screened = s))))
          )
        )
    }

  /** The judgment `name` that `posed` makes of what a rewrite of `source` is made from, as its
    * rewrite read it; why not, when it read nothing, made as a judgment all the same.
    */
  private def screening[S](
      name: MoveName,
      source: DocumentVersionId,
      revision: String,
      applied: PolicyApplication
  )(
      posed: Declassifying.Shown -> Either[MoveError, Posed[S]]
  ): Either[MoveError, Either[Declassifying.Unread, S]] =
    shown(source, revision, applied) match {
      case Left(e) =>
        making(name, MoveKind.Judge, limits.judgments)(Left(MoveError.Store(described(e))))
      case Right(Left(why)) =>
        making(name, MoveKind.Judge, limits.judgments)(Right(Left(why)))
      case Right(Right(shown)) =>
        posed(shown).flatMap(p =>
          ask(name, TestTainted.at(p, integrity)).map(a => Right(Trusted.asIs(a.reply).extract))
        )
    }

  /** What a rewrite of `source`, its file at `revision`, under `applied` is made from, read as
    * its keeps read.
    */
  private def shown(
      source: DocumentVersionId,
      revision: String,
      applied: PolicyApplication
  ): Either[StoreError, Either[Declassifying.Unread, Declassifying.Shown]] =
    Declassifying.read(
      Declassifying.Reading(
        source,
        revision,
        applied,
        reads.documents,
        reads.pieces,
        documents.keeper(plugin, terms),
        plugin +: needs.plugins
      )
    )(using opened)
}
