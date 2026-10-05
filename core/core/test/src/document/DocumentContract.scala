package grit.core.document

import java.time.Instant

import scala.concurrent.duration.*

import grit.core.id.{DocKey, DocumentVersion, PluginName}
import grit.core.place.{Namespace, Place}
import grit.core.retention.{Target, Tombstone}
import grit.core.store.{StoreError, Tombstones, Tx}

import utest.*

/** The contract every [[DocumentStore]] and its plugins' [[DocumentKeeper]]s keep, run against
  * the in-memory fake in core and the SQL store in grit.dbos. Tests share the store, so each
  * names its own plugins and never assumes a version's number, only its order; only the
  * terms test declares.
  */
abstract class DocumentContract extends TestSuite {

  /** The store under test. */
  protected def store: DocumentStore

  /** `plugin`'s documents as it writes them, under `terms`, in the store under test. */
  protected def keeper(plugin: PluginName, terms: DocumentTerms): DocumentKeeper

  /** `plugin`'s documents as it reads them, in the store under test. */
  protected def shelf(plugin: PluginName): DocumentShelf

  /** Where the store marks the versions it leaves for deletion. */
  protected def tombstones: Tombstones

  /** Runs `body` in one transaction, committed when it returns. */
  protected def transaction[A](body: (Tx^) ?=> A): A

  private def ok[A](e: Either[StoreError, A]): A =
    e.fold(err => throw new java.lang.AssertionError(s"$err"), identity)
  private def got[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private def name(s: String): PluginName = got(PluginName.of(s))
  private def key(s: String): DocKey = got(DocKey.of(s))
  private def text(s: String): DocText = got(DocText.of(s))
  private def place(s: String): Place = Place.under(Namespace.Task, Vector("documents", s))

  private def at(minute: Int): Instant =
    Instant.parse("2026-03-01T00:00:00Z").plusSeconds(60L * minute)

  private def terms(bound: Int = 100, label: String = "notes"): DocumentTerms =
    got(DocumentTerms.of(got(DocLabel.of(label)), DocWeight.Unscaled, 30.days, bound))

  private val Empty: ujson.Value = ujson.Obj()

  /** Writes `body` under `k` at `where`, written at minute `m`; the version written. */
  private def put(
      k: DocumentKeeper,
      name: String,
      body: String,
      m: Int,
      where: Place = place("here"),
      data: ujson.Value = Empty
  ): DocumentVersion =
    transaction(ok(k.write(key(name), where, text(body), data, at(m)))) match {
      case Written.Versioned(v, _) => v
      case w => throw new java.lang.AssertionError(s"$name: $w")
    }

  private def keys(docs: Vector[Document]): Vector[String] = docs.map(d => DocKey.value(d.key))

  private def texts(docs: Vector[Document]): Vector[String] = docs.map(d => DocText.value(d.text))

  private def hits(
      shelves: Vector[Shelved],
      query: String,
      m: Int,
      limit: Int = 10
  ): Vector[DocumentSearch.Hit] =
    transaction(ok(store.search(shelves, query, limit, at(m))))

  val tests = Tests {
    test("a write supersedes the key's version: the new one is current, the old still read") {
      val p = name("doc-supersede")
      val k = keeper(p, terms())
      val one = put(k, "k", "first words", 1)
      val two = put(k, "k", "second words", 2)
      assert(DocumentVersion.value(one) < DocumentVersion.value(two))
      texts(transaction(ok(shelf(p).current(key("k")))).toVector) ==> Vector("second words")
      texts(transaction(ok(store.read(Vector(two, one))))) ==> Vector("second words", "first words")
      val shelves = Vector(Shelved(p, place("here")))
      texts(hits(shelves, "first", 2).map(_.document)) ==> Vector("first words")
      texts(hits(shelves, "first", 3).map(_.document)) ==> Vector()
      texts(hits(shelves, "second", 3).map(_.document)) ==> Vector("second words")
    }

    test("a withdrawn key holds nothing, its withdrawal is never read, and its old version is") {
      val p = name("doc-withdraw")
      val k = keeper(p, terms())
      val kept = put(k, "gone", "withdrawn words", 1)
      put(k, "stays", "kept words", 1)
      val withdrawal = transaction(ok(k.withdraw(key("gone"), at(2))))
      assert(withdrawal.nonEmpty)
      transaction(ok(shelf(p).current(key("gone")))) ==> None
      keys(transaction(ok(shelf(p).newest(10)))) ==> Vector("stays")
      texts(transaction(ok(store.read(withdrawal.toVector :+ kept)))) ==> Vector("withdrawn words")
      transaction(ok(k.withdraw(key("gone"), at(3)))) ==> None
      transaction(ok(k.withdraw(key("never"), at(3)))) ==> None
    }

    test("a write the current version already holds writes nothing; another place or data does") {
      val p = name("doc-unchanged")
      val k = keeper(p, terms())
      val first = put(k, "k", "same words", 1, data = ujson.Obj("n" -> 1))
      transaction(
        ok(k.write(key("k"), place("here"), text("same words"), ujson.Obj("n" -> 1), at(2)))
      ) ==>
        Written.Unchanged(first)
      // Each `put` fails unless the write is Versioned.
      put(k, "k", "same words", 3, where = place("there"), data = ujson.Obj("n" -> 1))
      put(k, "k", "same words", 4, where = place("there"), data = ujson.Obj("n" -> 2))
      transaction(ok(k.withdraw(key("k"), at(5))))
      put(k, "k", "same words", 6, where = place("there"), data = ujson.Obj("n" -> 2))
    }

    test("past the bound a write withdraws the least recently placed, ties by key, never itself") {
      val p = name("doc-bound")
      val k = keeper(p, terms(bound = 2))
      put(k, "z", "withdrawn first", 0)
      transaction(ok(k.withdraw(key("z"), at(0))))
      put(k, "b", "words b", 1)
      put(k, "a", "words a", 1)

      /** The keys withdrawn by writing `name` at minute `m`. */
      def withdrew(name: String, m: Int): Vector[String] =
        transaction(
          ok(k.write(key(name), place("here"), text(s"words $name"), Empty, at(m)))
        ) match {
          case Written.Versioned(_, gone) => gone.map(DocKey.value)
          case w => throw new java.lang.AssertionError(s"$name: $w")
        }
      // a and b tie on their placement: a goes, by key; the withdrawn z is not counted.
      withdrew("c", 2) ==> Vector("a")
      val b = transaction(ok(shelf(p).current(key("b")))).map(_.version).toVector
      transaction(ok(store.placed(b, at(5))))
      // b was placed since c was written: c goes.
      withdrew("d", 3) ==> Vector("c")
      // e, written earliest of all, is the one written: d goes in its stead.
      withdrew("e", -1) ==> Vector("d")
      keys(transaction(ok(shelf(p).newest(10)))) ==> Vector("b", "e")
    }

    test("placed counts each window and keeps the latest time; a version not kept is skipped") {
      val p = name("doc-placed")
      val k = keeper(p, terms())
      val v = put(k, "k", "placed words", 1)
      def placement = transaction(ok(store.read(Vector(v)))).map(_.placement)
      placement ==> Vector(Placement(0, at(1)))
      val unknown = DocumentVersion.of(Long.MaxValue).toVector
      transaction(ok(store.placed(v +: unknown, at(3))))
      placement ==> Vector(Placement(1, at(3)))
      transaction(ok(store.placed(Vector(v), at(2))))
      placement ==> Vector(Placement(2, at(3)))
    }

    test("search ranks the shelves' documents current at its time, best first, ties latest first") {
      val (p, q) = (name("doc-search"), name("doc-search-other"))
      val k = keeper(p, terms())
      put(k, "b", "apple pear plum", 1)
      put(k, "a", "apple apple pear", 1)
      put(k, "c", "apple pear plum", 2)
      put(k, "old", "apple apple apple", 1)
      put(k, "old", "nothing like it", 2)
      put(k, "gone", "apple apple apple", 1)
      transaction(ok(k.withdraw(key("gone"), at(2))))
      put(k, "late", "apple apple apple", 6)
      put(k, "away", "apple apple apple", 1, where = place("elsewhere"))
      put(keeper(q, terms()), "theirs", "apple apple apple", 1)
      val found = hits(Vector(Shelved(p, place("here"))), "apple", 5)
      keys(found.map(_.document)) ==> Vector("a", "c", "b")
      val scores = found.map(_.score)
      assert(scores.forall(_ > 0), scores == scores.sorted.reverse, scores(0) > scores(1))
      keys(hits(Vector(Shelved(p, place("here"))), "apple", 5, limit = 2).map(_.document)) ==>
        Vector("a", "c")
    }

    test("a blank query, a limit under one or no shelves finds nothing") {
      val p = name("doc-search-none")
      put(keeper(p, terms()), "k", "apple words", 1)
      val shelves = Vector(Shelved(p, place("here")))
      hits(shelves, "apple", 2).size ==> 1
      hits(shelves, "  ", 2) ==> Vector()
      hits(shelves, "apple", 2, limit = 0) ==> Vector()
      hits(Vector(), "apple", 2) ==> Vector()
    }

    test("shelved is each place a plugin asked for keeps a document current at its time, once") {
      val (p, q, r) = (name("doc-shelved"), name("doc-shelved-q"), name("doc-shelved-r"))
      val kp = keeper(p, terms())
      put(kp, "a", "words", 1, where = place("x"))
      put(kp, "b", "words", 1, where = place("x"))
      put(kp, "c", "words", 1, where = place("withdrawn"))
      transaction(ok(kp.withdraw(key("c"), at(2))))
      put(kp, "d", "words", 9, where = place("later"))
      put(keeper(q, terms()), "a", "words", 1, where = place("z"))
      put(keeper(r, terms()), "a", "words", 1, where = place("unasked"))
      transaction(ok(store.shelved(Vector(p, q), at(5))))
        .sortBy(s => (PluginName.value(s.plugin), s.place.written)) ==>
        Vector(Shelved(p, place("x")), Shelved(q, place("z")))
    }

    test("newest is a plugin's current documents, latest written first, at most n") {
      val (p, q) = (name("doc-newest"), name("doc-newest-other"))
      val k = keeper(p, terms())
      put(k, "first", "words", 1)
      put(k, "third", "words", 3)
      put(k, "second", "words", 2)
      put(keeper(q, terms()), "theirs", "words", 4)
      keys(transaction(ok(shelf(p).newest(2)))) ==> Vector("third", "second")
      transaction(ok(shelf(p).newest(0))) ==> Vector()
      transaction(ok(shelf(q).current(key("first")))) ==> None
    }

    test("each version that stops being current, and each withdrawal, is marked when it does") {
      val (p, q) = (name("doc-tombstone"), name("doc-tombstone-other"))
      val k = keeper(p, terms())
      val one = put(k, "k", "one", 1)
      val two = put(k, "k", "two", 2)
      val withdrawal = transaction(ok(k.withdraw(key("k"), at(3)))).toVector
      val qk = keeper(q, terms())
      put(qk, "k", "one", 1)
      put(qk, "k", "two", 2)
      transaction(ok(tombstones.documentsDue(p, at(10), 10))).toSet ==>
        (Tombstone(Target.Document(one), at(2)) +:
          (two +: withdrawal).map(v => Tombstone(Target.Document(v), at(3)))).toSet
      transaction(ok(tombstones.documentsDue(p, at(3), 10))) ==>
        Vector(Tombstone(Target.Document(one), at(2)))
    }

    test("versions are never repeated across plugins") {
      val (p, q) = (name("doc-versions-p"), name("doc-versions-q"))
      val (kp, kq) = (keeper(p, terms()), keeper(q, terms()))
      val versions = Vector(put(kp, "k", "one", 1), put(kq, "k", "one", 1), put(kp, "k", "two", 2))
      versions.distinct.size ==> 3
    }

    test("declare puts the terms given in force; one left out is no longer declared, but kept") {
      val (p, q) = (name("doc-declare-p"), name("doc-declare-q"))
      transaction(ok(store.declare(Vector(p -> terms(label = "p's"), q -> terms(bound = 3)))))
      transaction(ok(store.declared())).sortBy((n, _) => PluginName.value(n)) ==>
        Vector(p -> terms(label = "p's"), q -> terms(bound = 3))
      transaction(ok(store.declare(Vector(q -> terms(bound = 4)))))
      transaction(ok(store.declared())) ==> Vector(q -> terms(bound = 4))
      // p has terms and no document: kept until removed, so the sweep can mark it.
      transaction(ok(store.kept())).filter(Set(p, q)).toSet ==> Set(p, q)
      transaction(ok(store.remove(p, at(1))))
      transaction(ok(store.kept())).filter(Set(p, q)).toSet ==> Set(q)
      transaction(ok(store.declared())) ==> Vector(q -> terms(bound = 4))
    }

    test("forget deletes one version, and the key's others stay") {
      val p = name("doc-forget")
      val k = keeper(p, terms())
      val one = put(k, "k", "first words", 1)
      val two = put(k, "k", "second words", 2)
      transaction(ok(store.forget(one)))
      texts(transaction(ok(store.read(Vector(one, two))))) ==> Vector("second words")
      texts(transaction(ok(shelf(p).current(key("k")))).toVector) ==> Vector("second words")
      transaction(ok(store.forget(one)))
    }

    test("remove deletes a plugin's every version and ends their tombstones; another's stay") {
      val (p, q) = (name("doc-remove-p"), name("doc-remove-q"))
      val (kp, kq) = (keeper(p, terms()), keeper(q, terms()))
      val ps = Vector(put(kp, "k", "one", 1), put(kp, "k", "two", 2), put(kp, "j", "three", 2))
      val withdrawal = transaction(ok(kp.withdraw(key("j"), at(3)))).toVector
      val qs = Vector(put(kq, "k", "one", 1), put(kq, "k", "two", 2))
      transaction(ok(store.remove(p, at(4))))
      transaction(ok(store.read(ps ++ withdrawal))) ==> Vector()
      transaction(ok(shelf(p).newest(10))) ==> Vector()
      transaction(ok(store.kept())).filter(Set(p, q)).toSet ==> Set(q)
      val mine = (ps ++ withdrawal).map(Target.Document(_)).toSet[Target]
      val theirs = qs.take(1).map(Target.Document(_)).toSet[Target]
      val pending = transaction(ok(tombstones.due(Target.Kind.Document, at(10), 1000)))
        .map(_.target)
        .toSet
      (pending.intersect(mine), pending.intersect(theirs)) ==> (Set(), theirs)
      texts(transaction(ok(store.read(qs)))) ==> Vector("one", "two")
    }

    test("search reads a document's text, never its key, place or data") {
      val p = name("doc-search-text")
      val k = keeper(p, terms())
      put(
        k,
        "zebra",
        "plain words",
        1,
        where = place("zebra"),
        data = ujson.Obj("zebra" -> "zebra")
      )
      val shelves = Vector(Shelved(p, place("zebra")))
      hits(shelves, "zebra", 2) ==> Vector()
      keys(hits(shelves, "plain", 2).map(_.document)) ==> Vector("zebra")
    }

    test("past the bound, keys tied on their placement go in bytewise order") {
      val p = name("doc-bound-bytes")
      val k = keeper(p, terms(bound = 2))
      put(k, "a", "words a", 1)
      put(k, "B", "words B", 1)
      transaction(ok(k.write(key("c"), place("here"), text("words c"), Empty, at(2)))) match {
        case Written.Versioned(_, gone) => gone.map(DocKey.value) ==> Vector("B")
        case w => throw new java.lang.AssertionError(s"c: $w")
      }
    }

    test("kept is every plugin with documents, withdrawn or not") {
      val (p, q, r) = (name("doc-kept-p"), name("doc-kept-q"), name("doc-kept-r"))
      put(keeper(p, terms()), "k", "words", 1)
      val kq = keeper(q, terms())
      put(kq, "k", "words", 1)
      transaction(ok(kq.withdraw(key("k"), at(2))))
      transaction(ok(store.kept())).filter(Set(p, q, r)).toSet ==> Set(p, q)
    }
  }
}
