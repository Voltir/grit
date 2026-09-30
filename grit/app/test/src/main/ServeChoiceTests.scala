package grit.app.main

import grit.core.edge.{EdgeRefusal, EdgeStores, ServedEdge, Variable}
import grit.core.id.EdgeName
import grit.core.place.Scope as PlaceScope
import grit.core.spend.DailyCap
import grit.kit.deployment.Offered

import utest.*

/** What the reference deployment reads for `grit serve` and `grit backfill`, before it
  * touches the database or Slack.
  */
object ServeChoiceTests extends TestSuite {

  /** An edge that is never opened here. */
  private object Quiet extends ServedEdge {
    def name: EdgeName = EdgeName("slack")
    def needs: Vector[Variable] = Vector.empty
    def answersAsks: Boolean = false
    def open(
        stores: EdgeStores^,
        env: Map[String, String],
        log: String => Unit
    ): Either[EdgeRefusal, ServedEdge.Open^{stores, log, caps.any}] =
      Left(EdgeRefusal.Refused("never opened here"))
  }

  val tests = Tests {
    test(
      "serving an edge seeds scope room and caps a day at $1.00 when their variables are unset; the chat does neither"
    ) {
      def of(edges: Vector[ServedEdge]) =
        Main
          .deployment(Map.empty, Offered.Read, edges, java.time.ZoneOffset.UTC)
          .map(d => (d.seed.locality.scope, d.budget.cap))
      (of(Vector(Quiet)), of(Vector.empty)) ==> (
        Right((PlaceScope.Room, DailyCap.of("1.00").toOption)),
        Right((grit.core.period.LifecycleSettings.Default.locality.scope, None))
      )
    }

    test("a listened channel that is not a channel id is refused, naming it") {
      Main.listening(Map("GRIT_SLACK_LISTEN" -> "C123ABC456, #general")) ==>
        Left(
          "GRIT_SLACK_LISTEN: #general is not a channel id (C…, as Slack's channel details show it)"
        )
    }

    test(
      "the days backfill reads are GRIT_BACKFILL_DAYS, 2 when unset, refused unless a whole number above zero, or when no channel is listened in"
    ) {
      val listen = Map("GRIT_SLACK_LISTEN" -> "C123ABC456")
      (
        Main.backfillDays(listen),
        Main.backfillDays(listen + ("GRIT_BACKFILL_DAYS" -> " 7 ")),
        Main.backfillDays(listen + ("GRIT_BACKFILL_DAYS" -> "0")),
        Main.backfillDays(listen + ("GRIT_BACKFILL_DAYS" -> "1.5")),
        Main.backfillDays(Map.empty)
      ) ==> (
        Right(2),
        Right(7),
        Left("GRIT_BACKFILL_DAYS is a whole number of days above zero, not '0'"),
        Left("GRIT_BACKFILL_DAYS is a whole number of days above zero, not '1.5'"),
        Left("GRIT_SLACK_LISTEN names no channel: there is nothing to backfill")
      )
    }
  }
}
