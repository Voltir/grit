package grit.act.moves

import java.time.Instant

import grit.core.act.{Keeping, MoveError, MoveLimits}
import grit.core.act.MovesFixtures.Answering
import grit.core.document.{DocText, DocumentKeeper, Written}
import grit.core.durable.{InMemoryDurable, Journaled}
import grit.core.id.DocKey
import grit.core.place.{Namespace, Place}
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.{Label, Level}
import grit.dbos.sql.TestTx

import utest.*

/** [[DurableMoves.keeping]]'s keeps, over [[MovesWorld]]'s documents. */
object DurableKeepingTests extends TestSuite {
  import MovesContract.name
  import MovesWorld.*

  /** The label a keep wrote at, journaled as its written form. */
  final case class Kept(label: String) extends caps.Pure
  object Kept {
    given Journaled[Kept] =
      Journaled.json(k => ujson.Str(k.label), v => v.strOpt.map(Kept(_)).toRight("no label"))
  }

  private val Here: Place = Place.under(Namespace.Task, Vector("notes"))

  private def key(k: String): DocKey =
    DocKey.of(k).fold(e => throw new java.lang.AssertionError(e), identity)

  private def text(t: String): DocText =
    DocText.of(t).fold(e => throw new java.lang.AssertionError(e), identity)

  /** Writes `t` under `k` at public through `keeper`, at `at`: the label it was kept at. */
  private def write(keeper: DocumentKeeper, k: String, t: String, at: Instant)(using
      Tx^
  ): Either[StoreError, Kept] =
    keeper
      .write(key(k), Label.Public, Here, text(t), ujson.Obj(), at)
      .map(w => Kept(Label.written(w.kept)))

  /** Every current document's key and text in `w`'s notes, as their plugin reads them. */
  private def kept(w: MovesWorld): Vector[(String, String)] =
    w.keeper
      .newest(10)(using TestTx.fake(grit.core.visibility.Clearance.of(Label.at(Level.Restricted))))
      .fold(e => throw new java.lang.AssertionError(s"$e"), identity)
      .map(d => (DocKey.value(d.key), DocText.value(d.text)))
      .sorted

  /** A keeper that writes as `inner` does, then fails as a raced key's write does. */
  private final class Raced(inner: DocumentKeeper) extends DocumentKeeper {
    export inner.{current, newest, withdraw}
    def write(k: DocKey, label: Label, place: Place, t: DocText, data: ujson.Value, at: Instant)(
        using Tx^
    ): Either[StoreError, Written] =
      inner
        .write(k, label, place, t, data, at)
        .flatMap(_ =>
          Left(StoreError.DatabaseError("duplicate key value violates unique constraint"))
        )
  }

  val tests = Tests {
    test("a keep writes at the run's floor, and commits with its record: a rerun gets it back") {
      val floor = Label.at(Level.Internal)
      val w = new MovesWorld(broken = false, floor)
      val keep = (m: Keeping^) =>
        Got(m.keep[Kept](name("note"))((keeper, at) => write(keeper, "k", "noted", at)))
      val first = w.keeping(MoveLimits.Zero)(keep)
      val steps = w.durable.recordedSteps(Turn.workflowId)
      // Replayed, the keep's body never runs: what it wrote is not written again.
      val again =
        new InMemoryDurable().replay(Turn.workflowId, w.durable.history(Turn.workflowId)) { _ =>
          val models = new Answering(MovesContract.Answer)
          DurableMoves
            .keeping(w.acting, MoveLimits.Zero, w.env(models), new Raced(w.keeper))(m =>
              Seen(keep(m).kept.fold(_.toString, _.label))
            )
            .said
        }
      (first, steps, kept(w), again) ==> (
        Got(Right(Kept(Label.written(floor)))),
        Vector("move:note"),
        Vector(("k", "noted")),
        Right(Label.written(floor))
      )
    }

    test("a keep whose body is Left keeps nothing, and its name is spent") {
      val w = new MovesWorld(broken = false)
      val got = w.keeping(MoveLimits.Zero) { m =>
        val refused = m.keep[Kept](name("note")) { (keeper, at) =>
          write(keeper, "k", "noted", at).flatMap(_ => Left(StoreError.Invalid("not today")))
        }
        val again = m.keep[Kept](name("note"))((keeper, at) => write(keeper, "k", "again", at))
        Seen(Vector(refused, again).map(_.fold(_.toString, _.label)).mkString("; "))
      }
      (got.said, kept(w)) ==> (
        s"${MoveError.Store("not today")}; ${MoveError.Repeated(name("note"))}",
        Vector()
      )
    }

    test("a keep whose key another run wrote meanwhile is Store, and keeps nothing") {
      val w = new MovesWorld(broken = false)
      val got = w.keeping(MoveLimits.Zero, new Raced(w.keeper)) { m =>
        Seen(
          m.keep[Kept](name("note"))((keeper, at) => write(keeper, "k", "raced", at))
            .fold(_.toString, _.label)
        )
      }
      (got.said, kept(w)) ==> (
        MoveError.Store("duplicate key value violates unique constraint").toString,
        Vector()
      )
    }
  }

  /** What a run's keeps came to. */
  final case class Seen(said: String) extends caps.Pure

  /** What a keep came to. */
  final case class Got(kept: Either[MoveError, Kept]) extends caps.Pure
}
