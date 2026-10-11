package grit.act.moves

import java.time.Instant

import grit.core.act.MovesFixtures.Answering
import grit.core.act.{Keeping, MoveError, MoveLimits}
import grit.core.document.{DocText, DocumentKeeper, Pointer, Written}
import grit.core.durable.InMemoryDurable
import grit.core.id.DocKey
import grit.core.place.Place
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.{Label, Level}
import grit.dbos.sql.TestTx

import utest.*

/** [[DurableMoves.keeping]]'s keeps as durable steps, over [[MovesWorld]]'s documents, which
  * roll nothing back: that a keep commits with its record, or not at all, is the live tier's to
  * show. The rules every keep keeps are [[MovesContract]]'s.
  */
object DurableKeepingTests extends TestSuite {
  import MovesContract.{Kept, name, write}
  import MovesWorld.*

  /** Every current document's key and text in `w`'s notes, as their plugin reads them. */
  private def kept(w: MovesWorld): Vector[(String, String)] =
    w.keeper
      .newest(10)(using TestTx.fake(grit.core.visibility.Clearance.of(Label.at(Level.Restricted))))
      .fold(e => throw new java.lang.AssertionError(s"$e"), identity)
      .map(d => (DocKey.value(d.key), DocText.value(d.text)))
      .sorted

  /** A keeper that writes as `inner` does, counting its writes. */
  private final class Counting(inner: DocumentKeeper) extends DocumentKeeper {
    export inner.{current, newest, withdraw}
    @caps.unsafe.untrackedCaptures
    var writes = 0
    def write(
        k: DocKey,
        label: Label,
        place: Place,
        t: DocText,
        data: ujson.Value,
        pointer: Option[Pointer],
        at: Instant
    )(using Tx^): Either[StoreError, Written] = {
      writes += 1
      inner.write(k, label, place, t, data, pointer, at)
    }
  }

  val tests = Tests {
    test(
      "a keep is one step named for it; replayed, it returns what it recorded without running its body"
    ) {
      val floor = Label.at(Level.Internal)
      val w = new MovesWorld(broken = false, floor)
      val keep = (m: Keeping^) =>
        Got(m.keep[Kept](name("note"))((keeper, at) => write(keeper, "k", Label.Public, at)))
      val first = w.keeping(MoveLimits.Zero)(keep)
      val steps = w.durable.recordedSteps(Turn.workflowId)
      val counting = new Counting(w.keeper)
      val again =
        new InMemoryDurable().replay(Turn.workflowId, w.durable.history(Turn.workflowId)) { _ =>
          val models = new Answering(MovesContract.Answer)
          DurableMoves
            .keeping(w.acting, MoveLimits.Zero, w.env(models), counting)(m =>
              Seen(keep(m).kept.fold(_.toString, _.label))
            )
            .said
        }
      (first, steps, kept(w), again, counting.writes) ==> (
        Got(Right(Kept(Label.written(floor)))),
        Vector("move:note"),
        Vector(("k", "k")),
        Right(Label.written(floor)),
        0
      )
    }
  }

  /** What a run's keeps came to. */
  final case class Seen(said: String) extends caps.Pure

  /** What a keep came to. */
  final case class Got(kept: Either[MoveError, Kept]) extends caps.Pure
}
