package grit.eval.harness.reply

import java.time.Instant

import scala.collection.immutable.VectorMap

import grit.core.context.Width
import grit.core.id.{ConversationId, TurnRef}
import grit.core.period.LifecycleSettings
import grit.core.place.{Locality, Scope, Weight}
import grit.core.store.Origin
import grit.dbos.engine.Reader
import grit.eval.harness.corpus.{Capture, Fields, Parts, TurnCapture}
import grit.eval.harness.label.Locator
import grit.eval.{Case, Layout, Variant}
import grit.turn.TurnOffer

/** grit.eval's cases as a reference: each case, in each [[Variant]], a turn said to grit in the
  * synthetic database `scripts/eval reference-build` writes ([[Layout]]), expected to hold its
  * `[must]` entries.
  */
object Synthetic {

  /** One case's turn found in the synthetic database: `name` (`{case}/{variant}`), the turn it
    * asks in, each of its conversations' `[must]` entries expected held ([[Expect.Holds]] of a
    * [[Locator.Messages]]), the query its window is searched with, and the scope it is drawn
    * within.
    */
  final case class Asked(
      name: String,
      turn: TurnRef,
      expected: ::[Expect],
      query: String,
      scope: Scope
  )

  /** Each of `cases` in each variant, laid out ([[Layout.of]]) and found through `reader` as
    * `scripts/eval reference-build` wrote it, in order; and the names of those that label no
    * `[must]` entry, so expect nothing, left out. `Left` naming the case when one cannot be laid
    * out, has no query, or a conversation of it is not in the database.
    */
  def found(reader: Reader^, cases: Vector[Case]): Either[String, (Vector[Asked], Vector[String])] =
    Fields
      .each(cases.flatMap(c => Variant.values.toVector.map(c -> _))) { (c, v) =>
        val name = s"${c.name}/${v.label}"
        for {
          layout <- Layout.of(c, v)
          query <- c.query.toRight(s"$name has no query:")
          own <- conversation(reader, name, layout.own)
          must <- Fields.each(layout.must)((o, seqs) =>
            conversation(reader, name, o).map(id => Expect.Holds(Locator.Messages(id, seqs)))
          )
        } yield must.toList match {
          case first :: rest =>
            Right(Asked(name, TurnRef(own, layout.ask), ::(first, rest), query, layout.scope))
          case Nil => Left(name)
        }
      }
      .map(all => (all.collect { case Right(a) => a }, all.collect { case Left(n) => n }))

  /** The width `v` draws a synthetic case's turn at: one said to grit (addressed), with no
    * triage answer.
    */
  def width(v: TurnVariant, assembled: Assembled): Width =
    TurnVariant.width(v, TurnOffer.Root.Addressed, VectorMap.empty, assembled)

  /** `asked`'s window, `width` wide, drawn by the shipped retrieval assembler built as
    * `assembled` says ([[Rebuild.window]]) over `reader`'s database as it stands, searched with
    * its query, within its scope at [[Weight.Default]] under otherwise default lifecycle
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
          Locality(asked.scope, Weight.Default)
        )
        .left
        .map(why => s"${asked.name}: $why")
      // Every row of the synthetic database is in the view: none is written after the end of
      // time.
      view = AsOf(reader, Instant.MAX).settled(settings)
      drawn <- Rebuild.window(
        view,
        asked.turn,
        assembled,
        new Rebuild.Replayed(Some(asked.query)),
        width
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
    reader.db
      .read(reader.conversations.find(origin))
      .left
      .map(e => s"$name: its conversations unread: ${Capture.kind(e)}")
      .flatMap(
        _.map(_.id).toRight(
          s"$name: ${origin.place.written} is not in the database: build it with scripts/eval reference-build"
        )
      )
}
