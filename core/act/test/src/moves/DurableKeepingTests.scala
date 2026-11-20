package grit.act.moves

import java.time.Instant

import grit.core.act.MovesFixtures.{Answering, Judging}
import grit.core.act.{Dreaming, Keeping, MoveError, MoveLimits, MovesFixtures}
import grit.core.document.{DocRef, DocText, DocumentKeeper, Pointer, Written}
import grit.core.durable.{Flows, InMemoryDurable}
import grit.core.id.{DocKey, DocumentVersionId}
import grit.core.place.Place
import grit.core.stitch.{Strand, StrandWorld}
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
    export inner.{
      applying,
      asOf,
      believing,
      benchable,
      benched,
      checks,
      current,
      declassifying,
      forReview,
      holds,
      keepBenched,
      keepDeclassified,
      labels,
      newest,
      referring,
      search,
      versions,
      withdraw
    }
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

    test(
      "a run resumed after a sort asks the classifier nothing, and reads its answer as the first run did"
    ) {
      val s = Sorts()
      val first = s.world
        .answering(
          s.limits,
          s.strands,
          s.shelves.needs,
          MovesContract.Answer,
          new Judging(),
          DurableMoves.Within
        )(s.sort)
        ._1
      val steps = s.world.durable.recordedSteps(Turn.workflowId)
      val again = new Judging()
      val replayed =
        new InMemoryDurable().replay(Turn.workflowId, s.world.durable.history(Turn.workflowId)) {
          _ =>
            val dreams = MovesWorld.dreams(
              s.world.acting,
              s.limits,
              s.world.keeper,
              s.strands,
              s.shelves.needs,
              s.shelves.documents
            )(s.sort)
            val env = KeepingEnv(
              s.world.env(new Answering(MovesContract.Answer), again),
              MovesWorld.Fileless,
              s.world.jot
            )
            Flows.run(dreams, env, ()).merge.moves.mkString
        }
      (first, steps, replayed, again.calls) ==> (
        MovesContract.Seen(
          Vector(Right(s"c 1.0 at outside among ${DocumentVersionId.value(s.card)}"))
        ),
        Vector("ask:s", "ask:s:record"),
        Right(first.moves.mkString),
        0
      )
    }

    test("a sort is not asked of a classifier not cleared for what it read: it is Uncleared") {
      val s = Sorts(judgedAt = Some(Label.Public))
      val judging = new Judging()
      val (got, _) = s.world.answering(
        s.limits,
        s.strands,
        s.shelves.needs,
        MovesContract.Answer,
        judging,
        DurableMoves.Within
      )(s.sort)
      (got, judging.calls) ==> (
        MovesContract.Seen(Vector(Left(MoveError.Uncleared(MovesContract.Floor)))),
        0
      )
    }

    test(
      "a sort's record holds none of its item's text, nor its candidates': ids, a digest and answers"
    ) {
      val s = Sorts()
      val _ = s.world.answering(
        s.limits,
        s.strands,
        s.shelves.needs,
        MovesContract.Answer,
        new Judging(),
        DurableMoves.Within
      )(s.sort)
      val recorded = s.world.durable.history(Turn.workflowId).collect {
        case InMemoryDurable.Step("ask:s", InMemoryDurable.Outcome.Output(v)) => v
      }
      val judged = recorded.map(r => ujson.read(r)("judged").obj)
      (
        recorded.map(r => Vector("deploy", "stuck", "friday").filter(r.contains)),
        judged.map(_.keys.toVector),
        judged.map(j => (ujson.write(j("answers")), ujson.write(j("shown"))))
      ) ==> (
        Vector(Vector()),
        Vector(
          Vector(
            "digest",
            "answers",
            "model",
            "usage",
            "estimate",
            "upstream",
            "at",
            "level",
            "shown"
          )
        ),
        Vector(
          (
            """[{"choice":"1","weights":[{"key":"1","p":1},{"key":"new","p":0},{"key":"aside","p":0}]}]""",
            s"[${DocumentVersionId.value(s.card)}]"
          )
        )
      )
    }
  }

  /** A world sorting the contract's strand, its deploy stuck, among one card, "deploys stay
    * stuck by friday", by [[MovesContract.Choosing]], once (`s`), its classifier cleared for
    * `judgedAt` when given.
    */
  private final case class Sorts(judgedAt: Option[Label] = None) {
    val (strands, root) = MovesContract.strand()
    val seen: Strand.Digested = MovesContract.one(
      StrandWorld.right(strands.shelf.of(Vector(root), StrandWorld.at(600))(using TestTx.fake))
    )
    val shelves: MovesContract.Shelves = MovesContract.Shelves(needed = Vector(MovesContract.Cards))
    MovesContract.declared(shelves)
    val card: DocumentVersionId = MovesContract.put(
      shelves,
      MovesContract.Cards.name,
      "c",
      "deploys stay stuck by friday",
      Integrity.Member
    )
    val world: MovesWorld = new MovesWorld(
      broken = false,
      MovesContract.Floor,
      documents = shelves.documents,
      judgedAt = judgedAt
    )
    val limits: MoveLimits = MovesContract.limits(1, 0)
    val sort: Dreaming^ -> MovesContract.Seen = m =>
      MovesContract.Seen(
        Vector(
          MovesContract.sorted(
            m,
            "s",
            Dreaming.Item.Discussion(seen),
            Vector(MovesContract.Pick("c") -> card),
            MovesContract.Choosing
          )(d => s"${d.choice.of} ${d.top}")
        )
      )
  }

  /** What a run's keeps came to. */
  final case class Seen(said: String) extends caps.Pure

  /** What a keep came to. */
  final case class Got(kept: Either[MoveError, Kept]) extends caps.Pure
}
