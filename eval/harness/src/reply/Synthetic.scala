package grit.eval.harness.reply

import java.time.Instant

import scala.util.control.NonFatal

import grit.core.context.Width
import grit.core.document.{DocWeight, DocumentKeeper, DocumentStore, DocumentTerms}
import grit.core.either.Traversal
import grit.core.id.{ConversationId, PluginName, TurnRef}
import grit.core.period.LifecycleSettings
import grit.core.place.Weight
import grit.core.recipe.Rooted
import grit.core.store.{ConversationStore, EntryStore, Jot, Origin, PeriodStore}
import grit.core.visibility.Subject
import grit.dbos.internal.{Reader, Scratch}
import grit.dbos.sql.DbConfig
import grit.eval.harness.capture.{Capture, Parts, TurnCapture}
import grit.eval.harness.label.Locator
import grit.eval.{Case, Cases, Layout, Load, Variant}
import grit.models.Seed

/** grit.eval's cases as a reference: each case, in each [[Variant]], a turn said to grit in a
  * database of its own ([[apart]], [[Layout]]), expected to hold its `[must]` entries and none
  * of its `[never]` entries.
  */
object Synthetic {

  /** One case's turn found in the synthetic database: `name` (`{case}/{variant}`), the case it
    * is a variant `of`, the turn it asks in, each of its conversations' `[must]` entries
    * expected held ([[Expect.Holds]] of a [[Locator.Messages]]) and then its `[never]` entries
    * expected omitted ([[Expect.Omits]]), and the query its window is searched with.
    */
  final case class Asked(
      name: String,
      of: String,
      turn: TurnRef,
      expected: ::[Expect],
      query: String
  )

  /** The cases found: each turn `asked`, and the names (`{case}/{variant}`) of those
    * `unlabelled`, which label no `[must]` or `[never]` entry, so expect nothing, and are left
    * out.
    */
  final case class Found(asked: Vector[Asked], unlabelled: Vector[String])

  /** Each of `laid`, laid out ([[Layout.of]]) and found through `reader` as [[build]] wrote it,
    * in order. `Left` naming the case when one cannot be laid out, has no query, or a
    * conversation of it is not in the database.
    */
  def found(reader: Reader^, laid: Vector[(Case, Variant)]): Either[String, Found] =
    Traversal
      .traverse(laid) { (c, v) =>
        val name = s"${c.name}/${v.label}"
        for {
          layout <- Layout.of(c, v)
          query <- c.query.toRight(s"$name has no query:")
          own <- conversation(reader, name, layout.own)
          must <- Traversal.traverse(layout.must)((o, seqs) =>
            conversation(reader, name, o).map(id => Expect.Holds(Locator.Messages(id, seqs)))
          )
          never <- Traversal.traverse(layout.never)((o, seqs) =>
            conversation(reader, name, o).map(id => Expect.Omits(Locator.Messages(id, seqs)))
          )
        } yield (must ++ never).toList match {
          case first :: rest =>
            Right(
              Asked(name, c.name, TurnRef(own, layout.ask), ::(first, rest), query)
            )
          case Nil => Left(name)
        }
      }
      .map(all => Found(all.collect { case Right(a) => a }, all.collect { case Left(n) => n }))

  /** Each of `cases` in each variant, in order. */
  def laid(cases: Vector[Case]): Vector[(Case, Variant)] =
    cases.flatMap(c => Variant.values.toVector.map(c -> _))

  /** `each` of every one of `laid`, alone: its database made empty beside `beside`, named
    * `{prefix}_{i}` for its index `i` in `laid` ([[Scratch.create]]), and dropped after `each`
    * returns, so no case's window can draw on another's. `Left` at the first that cannot be
    * made, `each` refuses, or cannot be dropped; those before it are dropped.
    */
  def apart[A](beside: DbConfig, prefix: String, laid: Vector[(Case, Variant)])(
      each: (DbConfig, (Case, Variant)) => Either[String, A]
  ): Either[String, Vector[A]] =
    Traversal.traverse(laid.zipWithIndex) { (one, i) =>
      Scratch.create(beside, s"${prefix}_$i") match {
        case Left(why) => Left(why)
        case Right(scratch) =>
          val done =
            try each(scratch.config, one)
            catch { case NonFatal(e) => Left(s"${one._1.name}: ${e.getClass.getName}") }
          scratch.drop().flatMap(_ => done)
      }
    }

  /** Every case of `grit.eval` ([[Cases.all]], then [[Cases.crossPlace]], then
    * [[Cases.documented]]), read; `Left` naming the first that does not read.
    */
  def cases: Either[String, Vector[Case]] =
    Traversal.traverse(Cases.all ++ Cases.crossPlace ++ Cases.documented)((name, text) =>
      Case.parse(name, text)
    )

  /** What a build wrote: its cases, its turns (a case's in each variant), its entries, and the
    * names of the cases that label no `[must]` or `[never]` entry.
    */
  final case class Built(cases: Int, turns: Int, entries: Int, unlabelled: Vector[String])

  /** `laid`, each laid out ([[Layout.of]]) and written through `jot` and the stores
    * ([[Load.into]]), its documents through `keeper`, whose terms are first declared the ones
    * in force in `documents` ([[Load.Plugin]]'s, unscaled), into an empty database ([[apart]]).
    * `Left` naming the case that cannot be laid out or written; those before it stay written.
    */
  def build(
      laid: Vector[(Case, Variant)],
      jot: Jot^,
      conversations: ConversationStore,
      entries: EntryStore,
      periods: PeriodStore,
      documents: DocumentStore,
      keeper: (PluginName, DocumentTerms) -> DocumentKeeper
  ): Either[String, Built] = {
    val terms = Load.terms(DocWeight.Unscaled)
    for {
      layouts <- Traversal.traverse(laid)((c, v) => Layout.of(c, v))
      _ <- jot
        .write(Subject.Public)(documents.declare(Vector(Load.Plugin -> terms)))
        .left
        .map(e => s"the eval's document terms: $e")
      written <- Traversal.traverse(layouts)(l =>
        Load
          .into(jot, conversations, entries, periods, keeper(Load.Plugin, terms))(l)
          .map(_ => l.entries.size)
      )
    } yield Built(
      laid.map(_._1.name).distinct.size,
      layouts.size,
      written.sum,
      layouts.filter(l => l.must.isEmpty && l.never.isEmpty).map(_.c.name).distinct
    )
  }

  /** The width `v` draws a synthetic case's turn at: one said to grit. */
  def width(v: TurnVariant): Width = v.recipe.at(Rooted.Addressed).width

  /** `asked`'s window, `width` wide, drawn by the shipped retrieval assembler built as
    * `assembled` says ([[Rebuild.window]]) over `reader`'s database as it stands, searched with
    * its query, at [[Weight.Default]] under otherwise default lifecycle
    * settings, whatever settings the database holds; by part ([[TurnCapture.costed]]). `Left`
    * naming what could not be read.
    */
  def window(
      reader: Reader^,
      asked: Asked,
      assembled: Assembled,
      width: Width
  ): Either[String, Parts] = {
    val d = LifecycleSettings.Default
    for {
      settings <- LifecycleSettings
        .of(
          d.windows,
          d.balance,
          d.settle,
          d.resolveAt,
          d.asks,
          Weight.Default
        )
        .left
        .map(why => s"${asked.name}: $why")
      // Every row of the synthetic database is in the view: none is written after the end of
      // time.
      view = AsOf(reader, Instant.MAX).settled(settings)
      seed <- Seed.catalog
      // The writer replays the case's query, and sends nothing.
      drawn <- Rebuild.window(
        view,
        asked.turn,
        assembled,
        new Rebuild.Replayed(Some(asked.query)),
        width,
        Some(seed.pin.query.first)
      )(using reader.db)
      parts <- TurnCapture.costed(reader, asked.turn, drawn)
    } yield parts
  }

  /** The conversation `origin` names in `reader`'s database. */
  private def conversation(
      reader: Reader^,
      name: String,
      origin: Origin
  ): Either[String, ConversationId] =
    reader.all
      .read(reader.conversations.find(origin))
      .left
      .map(e => s"$name: its conversations unread: ${e.kind}")
      .flatMap(
        _.map(_.id).toRight(
          s"$name: ${origin.place.written} is not in the database"
        )
      )
}
