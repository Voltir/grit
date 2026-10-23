package grit.core.files

import java.time.Instant

import grit.core.id.{PieceId, PluginName}
import grit.core.place.{Namespace, Place}
import grit.core.retention.Target
import grit.core.store.{StoreError, Tombstones, Tx}
import grit.core.visibility.{Clearance, Integrity, Label, TestLabels}
import grit.prose.form.{Block, Doc, Span, Text}

import utest.*

/** The contract every [[PieceStore]] and its plugins' [[PieceKeeper]]s keep, run against the
  * in-memory fake in core and the SQL store in grit.dbos. Tests share the store, so each names
  * its own plugins and never assumes an id's number.
  */
abstract class PieceContract extends TestSuite {

  /** The store under test. */
  protected def store: PieceStore

  /** `plugin`'s pieces as it writes them, bounded at `bound` bytes, in the store under test. */
  protected def keeper(plugin: PluginName, bound: Long): PieceKeeper

  /** Where the store marks the sets it leaves for deletion. */
  protected def tombstones: Tombstones

  /** Runs `body` in one transaction opened at `clearance`, committed when it returns. */
  protected def opened[A](clearance: Clearance)(body: (Tx^) ?=> A): A

  private def transaction[A](body: (Tx^) ?=> A): A = opened(Clearance.of(Label.Public))(body)

  private def ok[A](e: Either[StoreError, A]): A =
    e.fold(err => throw new java.lang.AssertionError(s"$err"), identity)

  private def name(s: String): PluginName =
    PluginName.of(s).fold(why => throw new java.lang.AssertionError(why), identity)

  private val Here: Place = Place.under(Namespace.Task, Vector("pieces"))

  private def at(minute: Int): Instant =
    Instant.parse("2026-03-01T00:00:00Z").plusSeconds(60L * minute)

  private def piece(text: String, page: Int = 1): Piece =
    Piece(
      Vector("Policy"),
      Doc(Vector(Block.Paragraph(Text(Vector(Span(text, Set.empty, Vector(Locus.Page(page))))))))
    )

  private def file(ref: String, revision: String = "r1"): PiecedFile =
    PiecedFile(ref, s"$ref.pdf", revision, FormatName("test"), 1)

  /** Keeps `texts` as `ref`'s pieces under `k` at minute `m`; how many were kept. */
  private def keep(k: PieceKeeper, ref: String, m: Int, texts: String*): Int =
    transaction(
      ok(k.keep(file(ref), Here, Integrity.Member, texts.map(piece(_)).toVector, 0, at(m)))
    )

  private def search(
      plugin: PluginName,
      query: String,
      m: Int,
      limit: Int = 30
  ): Vector[PieceSearch.Hit] =
    transaction(ok(store.search(Vector(plugin), query, limit, at(m))))

  /** The id of the one piece of `plugin`'s matching `query` at minute `m`. */
  private def idOf(plugin: PluginName, query: String, m: Int): PieceId =
    search(plugin, query, m).map(_.piece.id) match {
      case Vector(id) => id
      case other => throw new java.lang.AssertionError(s"$query: $other")
    }

  private def words(h: PieceSearch.Hit): String = h.piece.piece.doc.blocks.collect {
    case Block.Paragraph(t) => t.plain
  }.mkString

  val tests = Tests {

    test(
      "kept pieces are found by their words; a later keep of the file supersedes them, marked for deletion"
    ) {
      val p = name("pieces-supersede")
      val k = keeper(p, PieceKeeper.Bound)
      (keep(k, "a", 1, "alpha one", "beta two"), keep(k, "a", 5, "gamma three")) ==> (2, 1)
      val first = search(p, "alpha", 3).map(words)
      val later = search(p, "alpha", 9).map(words)
      val marked =
        transaction(ok(tombstones.declaredDue(Target.Kind.PieceSet, p, at(10), 10))).size == 1
      (first, later, search(p, "gamma", 9).map(words), marked) ==>
        (Vector("alpha one"), Vector(), Vector("gamma three"), true)
    }

    test(
      "a piece whose words are kept again keeps its id; one no longer kept is read until collected"
    ) {
      val p = name("pieces-identity")
      val k = keeper(p, PieceKeeper.Bound)
      keep(k, "a", 1, "alpha one", "beta two")
      val (alpha, beta) = (idOf(p, "alpha", 2), idOf(p, "beta", 2))
      keep(k, "a", 5, "Alpha  one", "delta four")
      val read = transaction(ok(store.read(Vector(beta, alpha)))).map(_.id)
      (idOf(p, "alpha", 6) == alpha, search(p, "beta", 6), read) ==>
        (true, Vector(), Vector(beta, alpha))
    }

    test("a keep of the same pieces keeps nothing new, only the file's revision") {
      val p = name("pieces-unchanged")
      val k = keeper(p, PieceKeeper.Bound)
      keep(k, "a", 1, "alpha one")
      val kept = transaction(
        ok(k.keep(file("a", "r2"), Here, Integrity.Member, Vector(piece("alpha one")), 0, at(5)))
      )
      val hit = search(p, "alpha", 6)
      val marked = transaction(ok(tombstones.declaredDue(Target.Kind.PieceSet, p, at(10), 10)))
      (kept, hit.map(_.piece.file.revision), marked) ==> (1, Vector("r2"), Vector())
    }

    test(
      "past its bound, a keeper deletes whole files, least recently placed first, never the one written"
    ) {
      val p = name("pieces-bound")
      val one = Piece.size(piece("alpha one"), "a.pdf")
      val k = keeper(p, 2 * one + one / 2)
      keep(k, "a", 1, "alpha one")
      keep(k, "b", 2, "bravo one")
      transaction(ok(store.placed(Vector(idOf(p, "alpha", 3)), at(3))))
      keep(k, "c", 4, "charl one")
      (search(p, "alpha", 5).size, search(p, "bravo", 5).size, search(p, "charl", 5).size) ==>
        (1, 0, 1)
    }

    test("past its bound, a keeper deletes the files no longer current first") {
      val p = name("pieces-bound-old")
      val one = Piece.size(piece("alpha one"), "a.pdf")
      val k = keeper(p, 2 * one + one / 2)
      keep(k, "a", 1, "alpha one")
      val old = idOf(p, "one", 2)
      keep(k, "a", 3, "alpha two")
      keep(k, "b", 4, "bravo one")
      (
        transaction(ok(store.read(Vector(old)))),
        search(p, "alpha", 5).map(words),
        search(p, "bravo", 5).size
      ) ==> (Vector(), Vector("alpha two"), 1)
    }

    test("a search answers at most PerFile pieces of one file, each saying how many more matched") {
      val p = name("pieces-per-file")
      val k = keeper(p, PieceKeeper.Bound)
      keep(k, "a", 1, (1 to 12).map(i => s"alpha a$i")*)
      keep(k, "b", 1, "alpha b")
      val hits = search(p, "alpha", 2)
      Vector("a", "b").map { ref =>
        val of = hits.filter(_.piece.file.ref == ref)
        (ref, of.size, of.map(_.more).distinct)
      } ==>
        Vector(("a", PieceSearch.PerFile, Vector(12 - PieceSearch.PerFile)), ("b", 1, Vector(0)))
    }

    test("a piece is found by an identifier's alias") {
      val p = name("pieces-alias")
      val k = keeper(p, PieceKeeper.Bound)
      keep(k, "a", 1, "Accounts are disabled under AC-2(3).", "Nothing else here.")
      search(p, "AC2x3", 2).map(words) ==> Vector("Accounts are disabled under AC-2(3).")
    }

    test("pieces kept at a transaction's floor are read only by one that reads it") {
      val p = name("pieces-labelled")
      val k = keeper(p, PieceKeeper.Bound)
      opened(Clearance.of(TestLabels.Trial))(
        ok(k.keep(file("a"), Here, Integrity.Member, Vector(piece("alpha one")), 0, at(1)))
      )
      val high =
        opened(Clearance.of(TestLabels.Trial))(ok(store.search(Vector(p), "alpha", 5, at(2))))
      (search(p, "alpha", 2), high.map(_.piece.label)) ==> (Vector(), Vector(TestLabels.Trial))
    }

    test("a plugin holds pieces while one is current that its transaction reads") {
      val (p, q, r) =
        (name("pieces-holding"), name("pieces-holding-high"), name("pieces-holding-none"))
      keep(keeper(p, PieceKeeper.Bound), "a", 2, "alpha one")
      opened(Clearance.of(TestLabels.Trial))(
        ok(
          keeper(q, PieceKeeper.Bound)
            .keep(file("a"), Here, Integrity.Member, Vector(piece("x")), 0, at(2))
        )
      )
      def holding(m: Int) = transaction(ok(store.holding(Vector(r, q, p), at(m))))
      (holding(1), holding(3)) ==> (Vector(), Vector(p))
    }

    test(
      "a file's pieces withdrawn at their label are no longer current, and marked for deletion"
    ) {
      val p = name("pieces-withdraw")
      val k = keeper(p, PieceKeeper.Bound)
      keep(k, "a", 1, "alpha one")
      def withdraw(label: Label, m: Int) = transaction(ok(k.withdraw("a", label, at(m))))
      val withdrawn =
        (withdraw(TestLabels.Trial, 2), withdraw(Label.Public, 3), withdraw(Label.Public, 4))
      val marked = transaction(ok(tombstones.declaredDue(Target.Kind.PieceSet, p, at(10), 10)))
      (withdrawn, search(p, "alpha", 5), marked.size) ==> ((false, true, false), Vector(), 1)
    }

    test(
      "a file's pieces relabelled are current at the floor and place, each keeping its id and the file's name, belief no more than given, those left marked for deletion"
    ) {
      val p = name("pieces-relabel")
      val k = keeper(p, PieceKeeper.Bound)
      keep(k, "a", 1, "alpha one")
      val id = idOf(p, "alpha", 2)
      val There = Place.under(Namespace.Task, Vector("pieces", "there"))
      val trial = Clearance.of(TestLabels.Trial)
      val moved = opened(trial)(ok(k.relabel("a", Label.Public, There, Integrity.Outside, at(3))))
      val none = opened(trial)(ok(k.relabel("b", Label.Public, There, Integrity.Outside, at(3))))
      val hit = opened(trial)(ok(store.search(Vector(p), "alpha", 5, at(4))))
      val marked = transaction(ok(tombstones.declaredDue(Target.Kind.PieceSet, p, at(10), 10)))
      (
        (moved, none),
        hit.map(h => (h.piece.id, h.piece.label, h.piece.place, h.piece.integrity, h.piece.file)),
        search(p, "alpha", 4),
        marked.size
      ) ==> (
        (1, 0),
        Vector((id, TestLabels.Trial, There, Integrity.Outside, file("a"))),
        Vector(),
        1
      )
    }

    test("a file's pieces relabelled keep when they were last placed, which the bound orders by") {
      val p = name("pieces-relabel-placed")
      val one = Piece.size(piece("alpha one"), "a.pdf")
      val k = keeper(p, 2 * one + one / 2)
      keep(k, "a", 1, "alpha one")
      keep(k, "b", 2, "bravo one")
      transaction(ok(store.placed(Vector(idOf(p, "bravo", 3)), at(3))))
      transaction(ok(k.relabel("a", Label.Public, Here, Integrity.Member, at(4))))
      val relabelled = search(p, "alpha", 5).size
      keep(k, "c", 5, "charl one")
      val after =
        (search(p, "alpha", 6).size, search(p, "bravo", 6).size, search(p, "charl", 6).size)
      (relabelled, after) ==> (1, (0, 1, 1))
    }

    test("a plugin's pieces removed are read no more, their tombstones ended") {
      val p = name("pieces-removed")
      val k = keeper(p, PieceKeeper.Bound)
      keep(k, "a", 1, "alpha one")
      val old = idOf(p, "one", 2)
      keep(k, "a", 3, "alpha two")
      transaction(ok(store.remove(p, at(4))))
      val marked = transaction(ok(tombstones.declaredDue(Target.Kind.PieceSet, p, at(10), 10)))
      (search(p, "alpha", 5), transaction(ok(store.read(Vector(old)))), marked) ==>
        (Vector(), Vector(), Vector())
    }
  }
}
