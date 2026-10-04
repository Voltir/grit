package grit.turn

import java.time.Instant

import grit.core.durable.InMemoryDurable
import grit.core.id.EntryId
import grit.core.message.{Cost, Tokens, Usage}
import grit.core.period.Probability
import grit.core.place.{Namespace, Place}
import grit.core.recipe.Offering
import grit.core.spend.Day
import grit.core.store.{StoreError, Tx}
import grit.core.triage.{KnowledgeSource, KnowledgeSources, Tags, TriageStore, Weighing}
import grit.dbos.sql.TestTx

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

  /** What a mention's asking answered: `repo` at 0.1, from a priced call. */
  private val Asked = Weighing.Weighed(
    Sourced.repoReads(0.1).copy(usage = Priced),
    Tokens(321)
  )

  private lazy val Priced: Usage =
    Usage(Tokens(700), Tokens(0), Tokens.Zero, Some(BigDecimal("0.0000294")))

  private val BySource: Offering = Offering.BySource(Probability.clamped(0.2))

  val tests = Tests {
    test(
      "a message said to grit is weighed once when the recipe offers its workspace's service by a source covering its place, and its tools withheld below the threshold"
    ) {
      val gated =
        new Sourced.Thread(heard = false, None, addressed = BySource, asked = Right(Asked))
      gated.run()
      (gated.asks.n, gated.offered) ==> (1, Vector.empty)
      // Pinned: a turn in flight reads back this output on every later build.
      gated.weighed ==> Some(
        """{"asked":{"answers":[{"name":"source:repo","yes":0.1}],"model":"jev","usage":""" +
          grit.core.store.PayloadJson.writeUsage(Priced).render() + """},"estimate":321}"""
      )
    }

    test("a message said to grit is never weighed when the recipe offers every service to it") {
      val all = new Sourced.Thread(heard = false, None, asked = Right(Asked))
      all.run()
      (all.asks.n, all.weighed, all.offered) ==> (0, Some("null"), Vector("github_search"))
      all.ledger.rows.map(_._1).contains(TurnWeighing.id(all.turn)) ==> false
    }

    test(
      "a message said to grit is not weighed when no source supplying a service it links covers its place"
    ) {
      val elsewhere = KnowledgeSources
        .of(
          Vector(
            KnowledgeSource(
              Sourced.repo,
              "the repository",
              Place.under(Namespace.Slack, Vector("T9", "C9")),
              Some(Sourced.github)
            )
          )
        )
        .fold(n => throw new java.lang.AssertionError(n), identity)
      val t = new Sourced.Thread(
        heard = false,
        None,
        addressed = BySource,
        asked = Right(Asked),
        sources = elsewhere
      )
      t.run()
      (t.asks.n, t.weighed, t.offered) ==> (0, Some("null"), Vector("github_search"))
    }

    test(
      "a weighing that fails records why, by kind alone, and offers everything with no ledger row"
    ) {
      Weighing.Unweighed.values.toVector.map { why =>
        val t = new Sourced.Thread(heard = false, None, addressed = BySource, asked = Left(why))
        t.run()
        assert(!t.ledger.rows.map(_._1).contains(TurnWeighing.id(t.turn)))
        (t.asks.n, t.weighed, t.offered)
      } ==> Vector(
        "message",
        "placement",
        "placement-timeout",
        "classifier",
        "unreadable",
        "timeout"
      ).map(kind => (1, Some(s"""{"failed":"$kind"}"""), Vector("github_search")))
    }

    test(
      "a weighed mention's call is in the usage ledger under its role and counted in the day's spend, and no triage is kept for it"
    ) {
      val gated =
        new Sourced.Thread(heard = false, None, addressed = BySource, asked = Right(Asked))
      gated.run()
      val id = TurnWeighing.id(gated.turn)
      gated.ledger.rows.collect { case (`id`, w, model, usage, estimate, _) =>
        (w, model, usage, estimate)
      } ==> Vector((gated.turn.workflowId, "jev", Priced, Tokens(321)))
      TurnRecord.role(gated.turn, id) ==> Some(TurnRecord.Role.Weigh)
      val all = new Sourced.Thread(heard = false, None, asked = Right(Asked))
      all.run()
      val day = Day.at(Instant.EPOCH, java.time.ZoneOffset.UTC)
      def spent(t: Sourced.Thread) = t.ledger.on(day)(using TestTx.fake)
      spent(gated).map(_.calls) ==> spent(all).map(_.calls + 1)
      spent(gated).map(_.cost) ==> spent(all).map(_.cost + Cost.of(Priced))
      gated.triage.of(Vector(gated.root))(using TestTx.fake) ==> Right(Map.empty)
    }

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
