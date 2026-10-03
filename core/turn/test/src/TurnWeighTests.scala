package grit.turn

import java.time.Instant

import grit.core.durable.InMemoryDurable
import grit.core.id.EntryId
import grit.core.message.Usage
import grit.core.store.{StoreError, Tx}
import grit.core.triage.{Tags, TriageStore}

import utest.*

/** The `weigh` step: what a turn's offer is given of its root's answers, behind its patch. */
object TurnWeighTests extends TestSuite {
  import TurnFixtures.*

  /** A triage store that cannot be read. */
  private object Unreadable extends TriageStore {
    def record(entry: EntryId, tags: Tags, at: Instant)(using Tx^): Either[StoreError, Boolean] =
      Left(StoreError.DatabaseError("down"))
    def of(entries: Vector[EntryId])(using Tx^): Either[StoreError, Map[EntryId, Tags]] =
      Left(StoreError.DatabaseError("down"))
    def tagged(from: Instant, until: Instant)(using
        Tx^
    ): Either[StoreError, Vector[TriageStore.Tagged]] = Left(StoreError.DatabaseError("down"))
  }

  val tests = Tests {
    test(
      "a heard turn is offered none of its workspace's tools when its kept triage reads every source below the recipe's threshold, and is offered them when one reaches it"
    ) {
      val below = new Sourced.Thread(heard = true, Some(Sourced.repoReads(0.1)))
      below.run()
      below.offered ==> Vector.empty
      val at = new Sourced.Thread(heard = true, Some(Sourced.repoReads(0.2)))
      at.run()
      at.offered ==> Vector("github_search")
    }

    test(
      "the weigh step records the tags triage kept for a heard root, under kept; nothing for a message said to grit, none kept, or a store it cannot read"
    ) {
      // Pinned: a turn in flight reads back this output on every later build.
      val heard = new Sourced.Thread(heard = true, Some(Sourced.repoReads(0.1)))
      heard.run()
      heard.weighed ==> Some(
        """{"kept":{"answers":[{"name":"source:repo","yes":0.1}],"model":"jev","usage":""" +
          grit.core.store.PayloadJson.writeUsage(Usage.Zero).render() + "}}"
      )
      val said = new Sourced.Thread(heard = false, Some(Sourced.repoReads(0.1)))
      said.run()
      said.weighed ==> Some("null")
      val none = new Sourced.Thread(heard = true, None)
      none.run()
      none.weighed ==> Some("null")
      val down = new Sourced.Thread(heard = true, Some(Sourced.repoReads(0.1)))
      down.run(Unreadable)
      // Never fails the turn: unweighed, the service is offered.
      (down.weighed, down.offered) ==> (Some("null"), Vector("github_search"))
    }

    test("a turn that recorded its offer before the weigh step shipped replays without weighing") {
      val before = new Sourced.Thread(
        heard = true,
        Some(Sourced.repoReads(0.1)),
        unpatched = Set(Turn.Patches.Weigh)
      )
      before.run()
      val history = before.durable.history(before.turn.workflowId)
      assert(!history.exists(_.name == Turn.Step.Weigh))
      // Before the patch, nothing was withheld.
      before.offered ==> Vector("github_search")
      val today = new Sourced.Thread(heard = true, Some(Sourced.repoReads(0.1)))
      today.durable
        .replay(today.turn.workflowId, history)(today.body(today.triage))
        .isRight ==> true
      today.durable.recordedSteps(today.turn.workflowId) ==> history.map(_.name)
    }

    test("a turn resumed after only its pinned models weighs, then offers, both fresh") {
      val before = new Sourced.Thread(
        heard = true,
        Some(Sourced.repoReads(0.1)),
        unpatched = Set(Turn.Patches.Weigh)
      )
      before.run()
      val pinned = before.durable.history(before.turn.workflowId).take(1)
      pinned.map(_.name) ==> Vector(Turn.Step.PinModels)
      val today = new Sourced.Thread(heard = true, Some(Sourced.repoReads(0.1)))
      today.durable.replay(today.turn.workflowId, pinned)(today.body(today.triage)).isRight ==> true
      today.durable.recordedSteps(today.turn.workflowId).take(4) ==> Vector(
        Turn.Step.PinModels,
        InMemoryDurable.patchMarker(Turn.Patches.Weigh),
        Turn.Step.Weigh,
        Turn.Step.Offer
      )
      today.offered ==> Vector.empty
    }
  }
}
