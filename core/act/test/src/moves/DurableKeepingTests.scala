package grit.act.moves

import java.time.Instant

import grit.core.act.MovesFixtures.Answering
import grit.core.act.{Keeping, MoveError, MoveLimits, MovesFixtures}
import grit.core.document.{DocRef, DocText, DocumentKeeper, Pointer, Written}
import grit.core.durable.{Flows, InMemoryDurable}
import grit.core.id.DocKey
import grit.core.place.Place
import grit.core.stitch.StrandWorld
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.Integrity
import grit.core.visibility.{Label, Level}
import grit.dbos.sql.TestTx

import utest.*

/** [[DurableMoves.keeping]]'s keeps as durable steps, over [[MovesWorld]]'s in-memory
  * documents, whose savepoints roll a keep's writes back: that a keep commits with its record,
  * or not at all, is the live tier's to show. The rules every keep keeps are
  * [[MovesContract]]'s.
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
    export inner.{current, forReview, newest, referring, withdraw}
    // A counter only the test's assertions read; the keeper's writes bump it on the test's thread.
    @caps.unsafe.untrackedCaptures
    var writes = 0
    def write(
        k: DocKey,
        label: Label,
        integrity: Integrity,
        place: Place,
        t: DocText,
        data: ujson.Value,
        pointer: Option[Pointer],
        at: Instant,
        refs: Set[DocRef]
    )(using Tx^): Either[StoreError, Written] = {
      writes += 1
      inner.write(k, label, integrity, place, t, data, pointer, at, refs)
    }
  }

  val tests = Tests {
    test(
      "a keep is one step named for it; replayed, it returns what it recorded without running its body"
    ) {
      val floor = Label.at(Level.Internal)
      val w = new MovesWorld(broken = false, floor)
      val keep = (m: Keeping^) =>
        Got(
          m.keep(name("note"), MovesContract.From)((_, keeper, at) =>
            write(keeper, "k", Label.Public, at)
          )
        )
      val first = w.keeping(MoveLimits.Zero)(keep)
      val steps = w.durable.recordedSteps(Turn.workflowId)
      val counting = new Counting(w.keeper)
      val again =
        new InMemoryDurable().replay(Turn.workflowId, w.durable.history(Turn.workflowId)) { _ =>
          val models = new Answering(MovesContract.Answer)
          val keeps = MovesWorld.keeps(w.acting, MoveLimits.Zero, counting)(m =>
            Seen(keep(m).kept.fold(_.toString, _.label))
          )
          Flows.run(keeps, w.kept(models), ()).merge.said
        }
      (first, steps, kept(w), again, counting.writes) ==> (
        Got(Right(Kept(Label.written(floor)))),
        Vector("keep:note"),
        Vector(("k", "k")),
        Right(Label.written(floor)),
        0
      )
    }

    test("a strand's summary is asked of the dream role's pair, a JSON ask of the summary's") {
      val w = new MovesWorld(broken = false, MovesContract.Floor)
      val (strands, root) = MovesContract.strand()
      val seen = StrandWorld.right(
        strands.shelf.of(Vector(root), StrandWorld.at(600))(using TestTx.fake)
      )
      val models = new Answering(MovesContract.Answer)
      val dreams = MovesWorld.dreams(w.acting, MovesContract.limits(2, 0), w.keeper, strands)(m =>
        MovesContract.Seen(
          Vector(MovesContract.summarised(m, "s", seen), MovesContract.shaped(m, "j"))
        )
      )
      w.durable.run(Turn.workflowId) { _ =>
        val _ = Flows.run(dreams, w.kept(models), ()).merge
        "done"
      }
      models.pairs ==> Vector(MovesFixtures.Dream, MovesFixtures.Summary)
    }
  }

  /** What a run's keeps came to. */
  final case class Seen(said: String) extends caps.Pure

  /** What a keep came to. */
  final case class Got(kept: Either[MoveError, Kept]) extends caps.Pure
}
