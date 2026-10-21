package grit.eval.harness.reply

import java.time.Instant

import grit.core.context.{Results, Retrieved}
import grit.core.document.{DocText, Document, Placement}
import grit.core.id.{DocKey, DocumentVersionId, PluginName, TurnSeq, WorkflowId}
import grit.core.place.{Namespace, Place, Service}
import grit.core.visibility.{Integrity, Label}

import Refound.{Current, Followed, Found, Rank}
import utest.*

/** The documents recorded turns followed, found again in a search run again: ranked by their
  * document's current version, and summed over turns by source.
  */
object RefoundTests extends TestSuite {

  private def got[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)
  private def version(n: Long): DocumentVersionId = got(DocumentVersionId.of(n).toRight("version"))
  private val at = Instant.parse("2026-10-17T12:00:00Z")

  /** Card `key` of plugin "cards" at `place`, its version `n`. */
  private def document(n: Long, key: String, place: String): Document =
    Document(
      version(n),
      got(PluginName.of("cards")),
      got(DocKey.of(key)),
      Label.Public,
      Integrity.Member,
      got(Place.read(place)),
      got(DocText.of(s"card $key")),
      ujson.Obj(),
      None,
      at,
      Placement(0, at),
      None
    )

  /** Ranked results: a turn of its own first, then each of `docs` in order. */
  private def results(docs: Long*): Results =
    Results(
      (Retrieved.Own(TurnSeq(0)) +: docs.map(n => Retrieved.Doc(version(n)))).toVector.map(_ -> 1.0)
    )

  private val github = "service:github"
  private val Github = KeptAt.Service(got(Service.of("github")))
  private val Drive = KeptAt.Service(got(Service.of("drive")))

  val tests = Tests {
    test(
      "a followed document is ranked where the search ranks its current version, by plugin and key"
    ) {
      // card-1 was followed at version 1, ranked second by the turn's own search, and resynced
      // as 11; the search run again ranks 11 second.
      val followed = document(1, "card-1", github)
      val current = Current.of(Vector(document(11, "card-1", github)))
      Refound.rank(results(1), results(11), Vector(version(1)), Vector(followed), current) ==>
        Vector(Followed(version(1), Found.Kept(Github, Some(2), Rank.At(2))))
    }
    test(
      "a followed document its own search never ranked is told from one it ranked: it reached the window otherwise"
    ) {
      val placed = document(6, "card-6", github)
      val found = document(7, "card-7", github)
      Refound.rank(
        results(7),
        results(7, 6),
        Vector(version(6), version(7)),
        Vector(placed, found),
        Current.of(Vector(placed, found))
      ) ==>
        Vector(
          Followed(version(6), Found.Kept(Github, None, Rank.At(3))),
          Followed(version(7), Found.Kept(Github, Some(2), Rank.At(2)))
        )
    }
    test("a current document the search did not rank is missed, not gone") {
      val card = document(4, "card-4", github)
      Refound.rank(
        results(5),
        results(5),
        Vector(version(4)),
        Vector(card),
        Current.of(Vector(card))
      ) ==>
        Vector(Followed(version(4), Found.Kept(Github, None, Rank.Missed)))
    }
    test("a followed document no current version of which is held is gone, not missed") {
      val card = document(2, "card-2", github)
      Refound.rank(
        results(2),
        results(2),
        Vector(version(2)),
        Vector(card),
        Current.of(Vector.empty)
      ) ==>
        Vector(Followed(version(2), Found.Kept(Github, Some(2), Rank.Gone)))
    }
    test("a followed version retention no longer keeps is forgotten, never dropped") {
      val card = document(3, "card-3", github)
      Refound.rank(
        results(3),
        results(3),
        Vector(version(9), version(3)),
        Vector(card),
        Current.of(Vector(card))
      ) ==>
        Vector(
          Followed(version(9), Found.Forgotten),
          Followed(version(3), Found.Kept(Github, Some(2), Rank.At(2)))
        )
    }
    test("a source is the place's service, else its namespace, else everywhere") {
      KeptAt.of(got(Place.read("service:drive"))) ==> Drive
      KeptAt.of(got(Place.read("slack:T1/C1"))) ==> KeptAt.Namespace(Namespace.Slack)
      KeptAt.of(Place.Everywhere) ==> KeptAt.Everywhere
    }
    test(
      "recall at a cut counts each ranked or missed document ranked at or above it, by source and over all, clustered by turn"
    ) {
      def kept(s: KeptAt, recorded: Option[Int], r: Rank) =
        Followed(version(1), Found.Kept(s, recorded, r))
      val turns = Vector(
        WorkflowId("w1:0") -> Vector(
          kept(Github, Some(3), Rank.At(3)),
          kept(Drive, None, Rank.At(10))
        ),
        WorkflowId("w2:0") -> Vector(
          kept(Github, Some(12), Rank.At(12)),
          kept(Github, Some(4), Rank.Missed)
        ),
        WorkflowId("w3:0") -> Vector(
          kept(Drive, None, Rank.At(1)),
          kept(Github, Some(1), Rank.Gone),
          Followed(version(2), Found.Forgotten)
        )
      )
      val s = Refound.summary(turns)
      def counts(a: Refound.At) =
        (
          a.cuts.toVector.map((cut, p) => (cut, p.hits, p.n)),
          (a.ranked.hits, a.ranked.n, a.ranked.clusters),
          (a.recorded.hits, a.recorded.n)
        )
      // Ranked or missed: w1 github 3, drive 10; w2 github 12, github missed; w3 drive 1 — five
      // documents over three turns. At 5: github 3, drive 1; at 10: and drive 10, a cut holding
      // its own rank; at 30: and github 12; ranked at all: all but the missed one. Ranked by
      // the turn's own search: the three github ones, neither drive one.
      counts(s.all) ==>
        (Vector((5, 2L, 5L), (10, 3L, 5L), (30, 4L, 5L)), (4L, 5L, 3), (3L, 5L))
      s.bySource.keys.toVector ==> Vector(Drive, Github)
      // Github: 3, 12, missed, over w1 and w2.
      s.bySource.get(Github).map(counts) ==>
        Some((Vector((5, 1L, 3L), (10, 1L, 3L), (30, 2L, 3L)), (2L, 3L, 2), (3L, 3L)))
      // Drive: 10 and 1, over w1 and w3.
      s.bySource.get(Drive).map(counts) ==>
        Some((Vector((5, 1L, 2L), (10, 2L, 2L), (30, 2L, 2L)), (2L, 2L, 2), (0L, 2L)))
      (s.gone, s.forgotten, s.turns) ==> (1, 1, 3)
    }
  }
}
