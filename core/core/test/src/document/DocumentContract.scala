package grit.core.document

import java.time.{Instant, LocalDate}

import scala.concurrent.duration.*

import grit.core.id.{DocKey, DocumentVersionId, PluginName, Sha256, ShortHash}
import grit.core.identity.TestAccounts
import grit.core.place.{Namespace, Place}
import grit.core.retention.{Target, Tombstone}
import grit.core.review.{DocumentLabel, DocumentReviews, ReviewWord}
import grit.core.screen.{Benched, Checks}
import grit.core.store.{Origin, Savepoints, StoreError, Tombstones, Tx}
import grit.core.tool.ToolName
import grit.core.visibility.Integrity
import grit.core.visibility.{
  Clearance,
  Compartments,
  DeclassificationPolicy,
  Label,
  Labelled,
  Labeller,
  Level,
  Room,
  TestLabels,
  Visibility
}

import utest.*

/** The contract every [[DocumentStore]] and its plugins' [[DocumentKeeper]]s keep, run against
  * the in-memory fake in core and the SQL store in grit.dbos. Tests share the store, so each
  * names its own plugins and never assumes a version's number, only its order; only the
  * terms test declares.
  */
abstract class DocumentContract extends TestSuite {

  /** The store under test. */
  protected def store: DocumentStore

  /** What the read half runs over: the store itself, unless an implementation reads apart. */
  protected def search: DocumentSearch = store

  /** `plugin`'s documents as it writes them, under `terms`, in the store under test. */
  protected def keeper(plugin: PluginName, terms: DocumentTerms): DocumentKeeper

  /** `plugin`'s documents as it reads them, in the store under test. */
  protected def shelf(plugin: PluginName): DocumentShelf

  /** Where the store marks the versions it leaves for deletion. */
  protected def tombstones: Tombstones

  /** What undoes part of a transaction in the store under test. */
  protected def savepoints: Savepoints

  /** Runs `body` in one transaction opened at `clearance`, committed when it returns. */
  protected def opened[A](clearance: Clearance)(body: (Tx^) ?=> A): A

  /** Makes `origin`'s room one the store knows, as its first conversation does. */
  protected def room(origin: Origin): Unit

  /** The reviews of the documents the store's keepers put up. */
  protected def reviews: DocumentReviews

  /** Runs `body` as [[opened]] does, labelling places as `visibility` does. */
  protected def openedUnder[A](clearance: Clearance, visibility: Visibility)(body: (Tx^) ?=> A): A

  /** Records the `version`th of `over`'s policy, kept at `kept`, as the operator's row does,
    * committed: `text`, or a revocation when `None`, and `checks` as its column holds them.
    */
  protected def policyAt(
      over: DeclassificationPolicy.Over,
      version: Int,
      text: Option[String],
      kept: Label,
      checks: Option[ujson.Value]
  ): Unit

  /** As [[policyAt]], publicly, with [[Instructions]] unless `revoked`, and checks that read. */
  private def policy(over: DeclassificationPolicy.Over, version: Int, revoked: Boolean): Unit =
    policyAt(
      over,
      version,
      Option.unless(revoked)(Instructions),
      Label.Public,
      Some(InMemoryDocuments.Checks)
    )

  /** A policy's text, as an operator writes one. */
  private val Instructions: String = "remove the team's details"

  /** Records the `version`th application to the document `pointer` reads, of `over`'s policy or
    * a removal, as the operator's row does, committed.
    */
  protected def application(
      pointer: Pointer,
      version: Int,
      over: Option[DeclassificationPolicy.Over]
  ): Unit

  /** The audit row of the declassified version `v`: the version it was made from, the policy
    * and application versions it was made under, and which attempt wrote it; `None` when it has
    * none.
    */
  protected def audited(v: DocumentVersionId): Option[InMemoryDocuments.Audit]

  /** Runs `body` in one transaction opened publicly, committed when it returns: what every
    * case but the labelled ones writes and reads at.
    */
  protected final def transaction[A](body: (Tx^) ?=> A): A =
    opened(Clearance.of(Label.Public))(body)

  private def ok[A](e: Either[StoreError, A]): A =
    e.fold(err => throw new java.lang.AssertionError(s"$err"), identity)
  private def got[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private def name(s: String): PluginName = got(PluginName.of(s))
  private def key(s: String): DocKey = got(DocKey.of(s))
  private def ref(kind: String, s: String): DocRef = got(DocRef.of(kind, s))
  private def text(s: String): DocText = got(DocText.of(s))
  private def place(s: String): Place = Place.under(Namespace.Task, Vector("documents", s))

  private def at(minute: Int): Instant =
    Instant.parse("2026-03-01T00:00:00Z").plusSeconds(60L * minute)

  private def terms(bound: Int = 100, label: String = "notes"): DocumentTerms =
    got(DocumentTerms.of(got(DocLabel.of(label)), DocWeight.Unscaled, 30.days, bound))

  private val Empty: ujson.Value = ujson.Obj()

  /** The policy over the compartment `c`, a name no other test's policy uses. */
  private def over(c: String): DeclassificationPolicy.Over =
    DeclassificationPolicy.Over.Compartment(TestLabels.compartment(c))

  /** A pointer at the drive file `id`, an id no other test's application names. */
  private def file(id: String): Pointer =
    got(Pointer.of(ToolName("drive_read_file"), "file" -> ujson.Str(id)))

  /** A rewrite of `body`, of the `cards` document `card` at `label` that `read` reads. */
  private def declassified(
      body: String,
      label: Label,
      read: Pointer,
      applied: PolicyApplication,
      attempt: Declassified.Attempt = Declassified.Attempt.First
  ): Declassified =
    TestDeclassified.of(
      body,
      "cards",
      "card",
      place("cards"),
      label,
      read,
      Revision,
      applied,
      attempt
    )

  /** The revision of the file a test's rewrite is made from. */
  private val Revision: String = "r7"

  /** A pointer at README.md as read at `ref`. */
  private def readme(ref: String): Pointer =
    got(
      Pointer.of(
        ToolName("github_get_file_contents"),
        "path" -> ujson.Str("README.md"),
        "ref" -> ujson.Str(ref)
      )
    )

  /** Writes `body` under `k` at `where`, written at minute `m`; the version written. */
  private def put(
      k: DocumentKeeper,
      name: String,
      body: String,
      m: Int,
      where: Place = place("here"),
      data: ujson.Value = Empty,
      pointer: Option[Pointer] = None,
      refs: Set[DocRef] = Set.empty
  ): DocumentVersionId =
    transaction(
      ok(
        k.write(
          key(name),
          Label.Public,
          Integrity.Member,
          where,
          text(body),
          data,
          pointer,
          at(m),
          refs
        )
      )
    ) match {
      case Written.Versioned(v, _, _) => v
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
    transaction(ok(search.search(shelves, query, limit, at(m))))

  /** Where [[posting]] maps the trial's review place, and the public one. */
  private val trialPlace = Place.under(Namespace.Slack, Vector("T", "CTRIAL"))
  private val publicPlace = Place.under(Namespace.Slack, Vector("T", "CPUBLIC"))

  /** [[TestLabels.trial]] declared; [[trialPlace]] mapped at [[TestLabels.Trial]],
    * [[publicPlace]] at public, every other place unmapped.
    */
  private val posting: Visibility = {
    val rooms: Labeller[Room] = new Labeller[Room] {
      def label(item: Room): Labelled =
        if (item.place == trialPlace) Labelled.Mapped(TestLabels.Trial)
        else if (item.place == publicPlace) Labelled.Mapped(Label.Public)
        else Labelled.Unmapped(Label.Public)
      def requires: Vector[grit.core.visibility.Compartment] = Vector(TestLabels.trial)
    }
    (for {
      compartments <- Compartments.of(Vector(TestLabels.trial)).left.map(_.toString)
      v <- Visibility.of(compartments, rooms, Vector.empty, Vector.empty).left.map(_.toString)
    } yield v).fold(e => throw new java.lang.AssertionError(e), identity)
  }

  private val Day = LocalDate.parse("2026-03-01")
  private val rater = TestAccounts.account("slack:T/U1")
  private def word(s: String): ReviewWord = got(ReviewWord.of(s))

  /** The keys `p`'s reviews offer `to`, in a transaction opened at `clearance` under [[posting]]. */
  private def offered(
      p: PluginName,
      to: Place,
      clearance: Clearance = Clearance.of(Label.Public)
  ): Vector[String] =
    openedUnder(clearance, posting)(ok(reviews.of(p).unposted(to))).map(r => DocKey.value(r.key))

  /** `p`'s one waiting review, as it is offered to [[publicPlace]]. */
  private def waiting(p: PluginName) =
    openedUnder(Clearance.of(Label.Public), posting)(
      ok(reviews.of(p).unposted(publicPlace))
    ) match {
      case Vector(only) => only
      case other => throw new java.lang.AssertionError(s"not one waiting: $other")
    }

  private def labelled(p: PluginName): Vector[DocumentLabel] =
    transaction(ok(reviews.labels(p, Day)))

  val tests = Tests {
    test("a write supersedes the key's version: the new one is current, the old still read") {
      val p = name("doc-supersede")
      val k = keeper(p, terms())
      val one = put(k, "k", "first words", 1)
      val two = put(k, "k", "second words", 2)
      assert(DocumentVersionId.value(one) < DocumentVersionId.value(two))
      texts(transaction(ok(shelf(p).current(key("k"), Label.Public))).toVector) ==> Vector(
        "second words"
      )
      texts(transaction(ok(search.read(Vector(two, one))))) ==> Vector(
        "second words",
        "first words"
      )
      val shelves = Vector(Shelved(p, place("here")))
      texts(hits(shelves, "first", 2).map(_.document)) ==> Vector("first words")
      texts(hits(shelves, "first", 3).map(_.document)) ==> Vector()
      texts(hits(shelves, "second", 3).map(_.document)) ==> Vector("second words")
    }

    test(
      "a document is believed at the integrity its writer gave; the same words at another are a new version"
    ) {
      val p = name("doc-integrity")
      val k = keeper(p, terms())
      val endorsed = Integrity.at(Integrity.Level.Authority, TestLabels.trial)
      def write(integrity: Integrity, m: Int) = transaction(
        ok(
          k.write(
            key("k"),
            Label.Public,
            integrity,
            place("here"),
            text("words"),
            Empty,
            None,
            at(m)
          )
        )
      )
      val first = write(endorsed, 1) match {
        case Written.Versioned(v, _, _) => v
        case w => throw new java.lang.AssertionError(s"$w")
      }
      write(endorsed, 2) ==> Written.Unchanged(first, Label.Public)
      assertMatch(write(Integrity.Untrusted, 3)) { case Written.Versioned(_, _, _) => }
      transaction(ok(shelf(p).current(key("k"), Label.Public))).map(_.integrity) ==>
        Some(Integrity.Untrusted)
      transaction(ok(search.read(Vector(first)))).map(_.integrity) ==> Vector(endorsed)
    }

    test("a withdrawn key holds nothing, its withdrawal is never read, and its old version is") {
      val p = name("doc-withdraw")
      val k = keeper(p, terms())
      val kept = put(k, "gone", "withdrawn words", 1)
      put(k, "stays", "kept words", 1)
      val withdrawal = transaction(ok(k.withdraw(key("gone"), Label.Public, at(2))))
      assert(withdrawal.nonEmpty)
      transaction(ok(shelf(p).current(key("gone"), Label.Public))) ==> None
      keys(transaction(ok(shelf(p).newest(10)))) ==> Vector("stays")
      texts(transaction(ok(search.read(withdrawal.toVector :+ kept)))) ==> Vector("withdrawn words")
      transaction(ok(k.withdraw(key("gone"), Label.Public, at(3)))) ==> None
      transaction(ok(k.withdraw(key("never"), Label.Public, at(3)))) ==> None
    }

    test("a write the current version already holds writes nothing; another place or data does") {
      val p = name("doc-unchanged")
      val k = keeper(p, terms())
      val first = put(k, "k", "same words", 1, data = ujson.Obj("n" -> 1))
      transaction(
        ok(
          k.write(
            key("k"),
            Label.Public,
            Integrity.Member,
            place("here"),
            text("same words"),
            ujson.Obj("n" -> 1),
            None,
            at(2)
          )
        )
      ) ==>
        Written.Unchanged(first, Label.Public)
      // Each `put` fails unless the write is Versioned.
      put(k, "k", "same words", 3, where = place("there"), data = ujson.Obj("n" -> 1))
      put(k, "k", "same words", 4, where = place("there"), data = ujson.Obj("n" -> 2))
      transaction(ok(k.withdraw(key("k"), Label.Public, at(5))))
      put(k, "k", "same words", 6, where = place("there"), data = ujson.Obj("n" -> 2))
    }

    test("a document's pointer is read back as it was written, sorted in its line") {
      val p = name("doc-pointer")
      val k = keeper(p, terms())
      val v = put(k, "k", "pointed words", 1, pointer = Some(readme("main")))
      transaction(ok(search.read(Vector(v)))).map(_.pointer.map(_.shown)) ==> Vector(
        Some("""Read it with github_get_file_contents {"path":"README.md","ref":"main"}""")
      )
      transaction(ok(shelf(p).current(key("k"), Label.Public))).map(_.pointer) ==>
        Some(Some(readme("main")))
    }

    test(
      "a pointer whose argument holds objects is read back equal, their keys sorted at every depth in its line"
    ) {
      val p = name("doc-pointer-nested")
      val k = keeper(p, terms())
      // A JSON object column may keep shorter keys first: "b" before "aa".
      val nested = got(
        Pointer.of(
          ToolName("github_get_file_contents"),
          "at" -> ujson.Obj(
            "aa" -> ujson.Obj("dd" -> ujson.Num(1), "c" -> ujson.Num(2)),
            "b" -> ujson.Num(3)
          )
        )
      )
      val v = put(k, "k", "pointed words", 1, pointer = Some(nested))
      transaction(ok(search.read(Vector(v)))).map(d => d.pointer.map(_.shown) -> d.pointer) ==>
        Vector(
          Some(
            """Read it with github_get_file_contents {"at":{"aa":{"c":2,"dd":1},"b":3}}"""
          ) -> Some(nested)
        )
    }

    test(
      "a write changing only the pointer is a new version; the same one in another order is not"
    ) {
      val p = name("doc-pointer-unchanged")
      val k = keeper(p, terms())
      val first = put(k, "k", "pointed words", 1, pointer = Some(readme("main")))
      def again(pointer: Option[Pointer], m: Int): Written =
        transaction(
          ok(
            k.write(
              key("k"),
              Label.Public,
              Integrity.Member,
              place("here"),
              text("pointed words"),
              Empty,
              pointer,
              at(m)
            )
          )
        )
      val reordered = got(
        Pointer.of(
          ToolName("github_get_file_contents"),
          "ref" -> ujson.Str("main"),
          "path" -> ujson.Str("README.md")
        )
      )
      again(Some(reordered), 2) ==> Written.Unchanged(first, Label.Public)
      // Each `put` fails unless the write is Versioned.
      put(k, "k", "pointed words", 3, pointer = Some(readme("dev")))
      val none = put(k, "k", "pointed words", 4)
      again(None, 5) ==> Written.Unchanged(none, Label.Public)
    }

    test("past the bound a write withdraws the least recently placed, ties by key, never itself") {
      val p = name("doc-bound")
      val k = keeper(p, terms(bound = 2))
      put(k, "z", "withdrawn first", 0)
      transaction(ok(k.withdraw(key("z"), Label.Public, at(0))))
      put(k, "b", "words b", 1)
      put(k, "a", "words a", 1)

      /** The keys withdrawn by writing `name` at minute `m`. */
      def withdrew(name: String, m: Int): Vector[String] =
        transaction(
          ok(
            k.write(
              key(name),
              Label.Public,
              Integrity.Member,
              place("here"),
              text(s"words $name"),
              Empty,
              None,
              at(m)
            )
          )
        ) match {
          case Written.Versioned(_, _, gone) => gone.map((k, _) => DocKey.value(k))
          case w => throw new java.lang.AssertionError(s"$name: $w")
        }
      // a and b tie on their placement: a goes, by key; the withdrawn z is not counted.
      withdrew("c", 2) ==> Vector("a")
      val b = transaction(ok(shelf(p).current(key("b"), Label.Public))).map(_.version).toVector
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
      def placement = transaction(ok(search.read(Vector(v)))).map(_.placement)
      placement ==> Vector(Placement(0, at(1)))
      val unknown = DocumentVersionId.of(Long.MaxValue).toVector
      transaction(ok(store.placed(v +: unknown, at(3))))
      placement ==> Vector(Placement(1, at(3)))
      transaction(ok(store.placed(Vector(v), at(2))))
      placement ==> Vector(Placement(2, at(3)))
    }

    test(
      "followed counts each call and keeps the latest follow; a version never followed has none"
    ) {
      val p = name("doc-followed")
      val k = keeper(p, terms())
      val v = put(k, "k", "followed words", 1)
      def following = transaction(ok(search.read(Vector(v)))).map(_.following)
      following ==> Vector(None)
      transaction(ok(store.followed(Vector(v), at(3))))
      following ==> Vector(Following.of(1, at(3)))
      transaction(ok(store.followed(Vector(v), at(2))))
      following ==> Vector(Following.of(2, at(3)))
    }

    test(
      "followed skips a version no longer kept, and one kept above its transaction's clearance"
    ) {
      val p = name("doc-followed-skipped")
      val k = keeper(p, terms())
      val gone = put(k, "gone", "gone words", 1)
      transaction(ok(store.forget(gone)))
      val high = opened(Clearance.of(TestLabels.Trial))(
        ok(
          k.write(
            key("high"),
            Label.Public,
            Integrity.Member,
            place("here"),
            text("high words"),
            Empty,
            None,
            at(1)
          )
        )
      ) match {
        case Written.Versioned(v, _, _) => v
        case w => throw new java.lang.AssertionError(s"$w")
      }
      val unknown = DocumentVersionId.of(Long.MaxValue).toVector
      transaction(ok(store.followed(Vector(gone, high) ++ unknown, at(3))))
      opened(Clearance.of(TestLabels.Trial))(ok(search.read(Vector(gone, high))))
        .map(d => DocKey.value(d.key) -> d.following) ==> Vector("high" -> None)
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
      transaction(ok(k.withdraw(key("gone"), Label.Public, at(2))))
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
      transaction(ok(kp.withdraw(key("c"), Label.Public, at(2))))
      put(kp, "d", "words", 9, where = place("later"))
      put(keeper(q, terms()), "a", "words", 1, where = place("z"))
      put(keeper(r, terms()), "a", "words", 1, where = place("unasked"))
      transaction(ok(search.shelved(Vector(p, q), at(5))))
        .sortBy(s => (PluginName.value(s.plugin), s.place.written)) ==>
        Vector(Shelved(p, place("x")), Shelved(q, place("z")))
    }
    test(
      "nearby: the best matches among enabled plugins' documents current at its time that the transaction reads, best first, at most limit, each with its plugin's label"
    ) {
      val (p, q, r) = (name("doc-nearby"), name("doc-nearby-q"), name("doc-nearby-r"))
      transaction(ok(store.declare(Vector(p -> terms(), q -> terms(label = "decisions")))))
      val kp = keeper(p, terms())
      put(kp, "a", "apple apple pear", 1, where = place("here"))
      put(kp, "b", "apple pear plum", 1, where = place("elsewhere"))
      put(kp, "late", "apple apple apple", 6)
      put(keeper(q, terms()), "theirs", "apple apple apple", 1, where = place("there"))
      put(keeper(r, terms()), "undeclared", "apple apple apple", 1)
      def near(query: String, limit: Int) =
        transaction(ok(search.nearby(query, limit, at(5)))).map((h, l) =>
          DocKey.value(h.document.key) -> DocLabel.value(l)
        )
      (near("apple", 5), near("apple", 2), near("quince", 5), near("apple", 0)) ==> (
        Vector("theirs" -> "decisions", "a" -> "notes", "b" -> "notes"),
        Vector("theirs" -> "decisions", "a" -> "notes"),
        Vector(),
        Vector()
      )
    }
    test(
      "labelOf keeps those whose plugin is enabled, in their order, each with its plugin's label"
    ) {
      val (p, q, r) = (name("doc-label-of-p"), name("doc-label-of-q"), name("doc-label-of-r"))
      transaction(ok(store.declare(Vector(p -> terms(), q -> terms(label = "decisions")))))
      val held = Vector("b" -> q, "x" -> r, "a" -> p, "c" -> q)
      transaction(ok(search.labelOf(held)(_._2))).map((h, l) => h._1 -> DocLabel.value(l)) ==>
        Vector("b" -> "decisions", "a" -> "notes", "c" -> "decisions")
    }
    test(
      "a document the transaction does not read is never returned by nearby and never displaces one it does"
    ) {
      val p = name("doc-nearby-above")
      transaction(ok(store.declare(Vector(p -> terms()))))
      val k = keeper(p, terms())
      put(k, "low", "apple low", 1)
      opened(Clearance.of(TestLabels.Trial))(
        ok(
          k.write(
            key("high"),
            Label.Public,
            Integrity.Member,
            place("here"),
            text("apple apple high"),
            Empty,
            None,
            at(1)
          )
        )
      )
      def near(clearance: Clearance) =
        keys(opened(clearance)(ok(search.nearby("apple", 1, at(5)))).map(_._1.document))
      (near(Clearance.of(Label.Public)), near(Clearance.of(TestLabels.Trial))) ==> (
        Vector("low"),
        Vector("high")
      )
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
      transaction(ok(shelf(q).current(key("first"), Label.Public))) ==> None
    }

    test(
      "newestWhere reads back past the documents it passes over for n kept, and no further than past"
    ) {
      val p = name("doc-newest-where")
      val k = keeper(p, terms())
      (1 to 6).foreach(i => put(k, s"k$i", "words", i))
      def where(n: Int, before: Int) =
        transaction(
          ok(
            shelf(p).newestWhere(n, _.written.isBefore(at(before)))(
              d => Option.when(DocKey.value(d.key).last.asDigit % 2 == 0)(DocKey.value(d.key)),
              _ => true
            )
          )
        )
      (where(2, 0), where(10, 5), where(0, 0)) ==> (Vector("k6", "k4"), Vector("k6"), Vector())
    }

    test(
      "newestWhere reads back for n counted, keeping those it does not count between, and none past the nth"
    ) {
      val p = name("doc-newest-counted")
      val k = keeper(p, terms())
      (1 to 6).foreach(i => put(k, s"k$i", "words", i))
      def where(n: Int) =
        transaction(
          ok(
            shelf(p).newestWhere(n, _ => false)(
              d => Some(DocKey.value(d.key)),
              _.last.asDigit % 3 == 0
            )
          )
        )
      (where(1), where(2), where(3)) ==> (
        Vector("k6"),
        Vector("k6", "k5", "k4", "k3"),
        Vector("k6", "k5", "k4", "k3", "k2", "k1")
      )
    }

    test("each version that stops being current, and each withdrawal, is marked when it does") {
      val (p, q) = (name("doc-tombstone"), name("doc-tombstone-other"))
      val k = keeper(p, terms())
      val one = put(k, "k", "one", 1)
      val two = put(k, "k", "two", 2)
      val withdrawal = transaction(ok(k.withdraw(key("k"), Label.Public, at(3)))).toVector
      val qk = keeper(q, terms())
      put(qk, "k", "one", 1)
      put(qk, "k", "two", 2)
      transaction(ok(tombstones.declaredDue(Target.Kind.Document, p, at(10), 10))).toSet ==>
        (Tombstone(Target.Document(one), at(2)) +:
          (two +: withdrawal).map(v => Tombstone(Target.Document(v), at(3)))).toSet
      transaction(ok(tombstones.declaredDue(Target.Kind.Document, p, at(3), 10))) ==>
        Vector(Tombstone(Target.Document(one), at(2)))
    }

    test("a savepoint whose body is Left keeps none of its writes; what follows it commits") {
      val p = name("doc-savepoint")
      val k = keeper(p, terms())
      val one = put(k, "k", "one", 1)
      val undone = transaction {
        val inner = savepoints.atomic[Unit] {
          for {
            _ <- k.write(
              key("k"),
              Label.Public,
              Integrity.Member,
              place("here"),
              text("two"),
              Empty,
              None,
              at(2)
            )
            _ <- k.write(
              key("new"),
              Label.Public,
              Integrity.Member,
              place("here"),
              text("new"),
              Empty,
              None,
              at(2)
            )
            _ <- Left(StoreError.Invalid("refused"))
          } yield ()
        }
        ok(
          k.write(
            key("after"),
            Label.Public,
            Integrity.Member,
            place("here"),
            text("after"),
            Empty,
            None,
            at(3)
          )
        )
        inner
      }
      undone ==> Left(StoreError.Invalid("refused"))
      texts(transaction(ok(shelf(p).newest(10)))).sorted ==> Vector("after", "one")
      transaction(ok(tombstones.declaredDue(Target.Kind.Document, p, at(10), 10))) ==> Vector()
      texts(transaction(ok(search.read(Vector(one))))) ==> Vector("one")
    }

    test("a savepoint whose body is Right keeps its writes") {
      val p = name("doc-savepoint-kept")
      val k = keeper(p, terms())
      val kept = transaction {
        savepoints.atomic[Unit](
          k.write(
            key("k"),
            Label.Public,
            Integrity.Member,
            place("here"),
            text("kept"),
            Empty,
            None,
            at(1)
          ).map(_ => ())
        )
      }
      kept ==> Right(())
      texts(transaction(ok(shelf(p).newest(10)))) ==> Vector("kept")
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
      transaction(ok(search.declared())).sortBy((n, _) => PluginName.value(n)) ==>
        Vector(p -> terms(label = "p's"), q -> terms(bound = 3))
      transaction(ok(store.declare(Vector(q -> terms(bound = 4)))))
      transaction(ok(search.declared())) ==> Vector(q -> terms(bound = 4))
      // p has terms and no document: kept until removed, so the sweep can mark it.
      transaction(ok(store.kept())).filter(Set(p, q)).toSet ==> Set(p, q)
      transaction(ok(store.remove(p, at(1))))
      transaction(ok(store.kept())).filter(Set(p, q)).toSet ==> Set(q)
      transaction(ok(search.declared())) ==> Vector(q -> terms(bound = 4))
    }

    test("forget deletes one version, and the key's others stay") {
      val p = name("doc-forget")
      val k = keeper(p, terms())
      val one = put(k, "k", "first words", 1)
      val two = put(k, "k", "second words", 2)
      transaction(ok(store.forget(one)))
      texts(transaction(ok(search.read(Vector(one, two))))) ==> Vector("second words")
      texts(transaction(ok(shelf(p).current(key("k"), Label.Public))).toVector) ==> Vector(
        "second words"
      )
      transaction(ok(store.forget(one)))
    }

    test("remove deletes a plugin's every version and ends their tombstones; another's stay") {
      val (p, q) = (name("doc-remove-p"), name("doc-remove-q"))
      val (kp, kq) = (keeper(p, terms()), keeper(q, terms()))
      val ps = Vector(put(kp, "k", "one", 1), put(kp, "k", "two", 2), put(kp, "j", "three", 2))
      val withdrawal = transaction(ok(kp.withdraw(key("j"), Label.Public, at(3)))).toVector
      val qs = Vector(put(kq, "k", "one", 1), put(kq, "k", "two", 2))
      transaction(ok(store.remove(p, at(4))))
      transaction(ok(search.read(ps ++ withdrawal))) ==> Vector()
      transaction(ok(shelf(p).newest(10))) ==> Vector()
      transaction(ok(store.kept())).filter(Set(p, q)).toSet ==> Set(q)
      val mine = (ps ++ withdrawal).map(Target.Document(_)).toSet[Target]
      val theirs = qs.take(1).map(Target.Document(_)).toSet[Target]
      val pending = transaction(ok(tombstones.due(Target.Kind.Document, at(10), 1000)))
        .map(_.target)
        .toSet
      (pending.intersect(mine), pending.intersect(theirs)) ==> (Set(), theirs)
      texts(transaction(ok(search.read(qs)))) ==> Vector("one", "two")
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
      transaction(
        ok(
          k.write(
            key("c"),
            Label.Public,
            Integrity.Member,
            place("here"),
            text("words c"),
            Empty,
            None,
            at(2)
          )
        )
      ) match {
        case Written.Versioned(_, _, gone) => gone.map((k, _) => DocKey.value(k)) ==> Vector("B")
        case w => throw new java.lang.AssertionError(s"c: $w")
      }
    }

    test(
      "written under a {trial} floor at public, it is kept and keyed at {trial}, and Written says so"
    ) {
      val p = name("doc-floored")
      val k = keeper(p, terms())
      val floored = Clearance.of(TestLabels.Trial)
      opened(floored)(
        ok(
          k.write(
            key("k"),
            Label.Public,
            Integrity.Member,
            place("here"),
            text("floored"),
            Empty,
            None,
            at(1)
          )
        )
      ) match {
        case Written.Versioned(_, kept, _) => kept ==> TestLabels.Trial
        case w => throw new java.lang.AssertionError(s"$w")
      }
      opened(floored)(
        (
          ok(shelf(p).current(key("k"), TestLabels.Trial)).map(d =>
            (d.label, DocText.value(d.text))
          ),
          ok(shelf(p).current(key("k"), Label.Public))
        )
      ) ==> (Some((TestLabels.Trial, "floored")), None)
    }

    test(
      "two labels under one key are two current documents, and a write at one leaves the other current"
    ) {
      val p = name("doc-variants")
      val k = keeper(p, terms())
      val trialled = Clearance.of(TestLabels.Trial)
      put(k, "k", "public one", 1)
      opened(trialled)(
        ok(
          k.write(
            key("k"),
            TestLabels.Trial,
            Integrity.Member,
            place("here"),
            text("trial one"),
            Empty,
            None,
            at(2)
          )
        )
      )
      put(k, "k", "public two", 3)
      opened(trialled)(
        Vector(Label.Public, TestLabels.Trial)
          .flatMap(l => ok(shelf(p).current(key("k"), l)))
          .map(d => DocText.value(d.text))
      ) ==> Vector("public two", "trial one")
    }

    test("a withdrawal at one label leaves the key's document at another current") {
      val p = name("doc-withdraw-variant")
      val k = keeper(p, terms())
      val trialled = Clearance.of(TestLabels.Trial)
      put(k, "k", "public one", 1)
      opened(trialled)(
        ok(
          k.write(
            key("k"),
            TestLabels.Trial,
            Integrity.Member,
            place("here"),
            text("trial one"),
            Empty,
            None,
            at(2)
          )
        )
      )
      assert(opened(trialled)(ok(k.withdraw(key("k"), TestLabels.Trial, at(3)))).nonEmpty)
      opened(trialled)(
        Vector(Label.Public, TestLabels.Trial).map(l =>
          ok(shelf(p).current(key("k"), l)).map(d => DocText.value(d.text))
        )
      ) ==> Vector(Some("public one"), None)
      opened(trialled)(
        ok(
          k.write(
            key("k"),
            TestLabels.Trial,
            Integrity.Member,
            place("here"),
            text("trial two"),
            Empty,
            None,
            at(4)
          )
        )
      ) match {
        case Written.Versioned(_, kept, _) => kept ==> TestLabels.Trial
        case w => throw new java.lang.AssertionError(s"$w")
      }
    }

    test(
      "a variant above Tx.cleared is None from current and absent from newest, search and read, though kept"
    ) {
      val p = name("doc-above")
      val k = keeper(p, terms())
      put(k, "low", "apple low", 1)
      val high = opened(Clearance.of(TestLabels.Trial))(
        ok(
          k.write(
            key("high"),
            Label.Public,
            Integrity.Member,
            place("here"),
            text("apple apple high"),
            Empty,
            None,
            at(1)
          )
        )
      ) match {
        case Written.Versioned(v, _, _) => v
        case w => throw new java.lang.AssertionError(s"$w")
      }
      val shelves = Vector(Shelved(p, place("here")))
      def seen(clearance: Clearance) = opened(clearance)(
        (
          ok(shelf(p).current(key("high"), TestLabels.Trial)).map(d => DocText.value(d.text)),
          keys(ok(shelf(p).newest(10))),
          keys(ok(search.search(shelves, "apple", 1, at(5))).map(_.document)),
          texts(ok(search.read(Vector(high))))
        )
      )
      (seen(Clearance.of(Label.Public)), seen(Clearance.of(TestLabels.Trial))) ==> (
        (None, Vector("low"), Vector("low"), Vector()),
        (
          Some("apple apple high"),
          Vector("high", "low"),
          Vector("high"),
          Vector("apple apple high")
        )
      )
    }

    test("an uncleared asker reads a document kept in its own room") {
      val p = name("doc-own-room")
      val k = keeper(p, terms())
      val origin = Origin.Slack("T1", "D1", "1.0")
      room(origin)
      opened(Clearance.inRoom(origin.room, TestLabels.Trial, TestLabels.Trial))(
        ok(
          k.write(
            key("ours"),
            Label.Public,
            Integrity.Member,
            place("here"),
            text("our room's"),
            Empty,
            None,
            at(1)
          )
        )
      )
      opened(Clearance.inRoom(origin.room, TestLabels.Trial, Label.Public))(
        (
          ok(shelf(p).current(key("ours"), TestLabels.Trial)).map(d => DocText.value(d.text)),
          keys(ok(shelf(p).newest(10))),
          ok(search.shelved(Vector(p), at(5)))
        )
      ) ==> (Some("our room's"), Vector("ours"), Vector(Shelved(p, place("here"))))
    }

    test("it reads none kept in another room, or in no room") {
      val p = name("doc-other-room")
      val k = keeper(p, terms())
      val (here, there) = (Origin.Slack("T1", "D2", "1.0"), Origin.Slack("T1", "D3", "1.0"))
      room(here)
      room(there)
      opened(Clearance.inRoom(there.room, TestLabels.Trial, TestLabels.Trial))(
        ok(
          k.write(
            key("theirs"),
            Label.Public,
            Integrity.Member,
            place("here"),
            text("their room's"),
            Empty,
            None,
            at(1)
          )
        )
      )
      opened(Clearance.of(TestLabels.Trial))(
        ok(
          k.write(
            key("nobody's"),
            Label.Public,
            Integrity.Member,
            place("here"),
            text("no room's"),
            Empty,
            None,
            at(1)
          )
        )
      )
      opened(Clearance.inRoom(here.room, TestLabels.Trial, Label.Public))(
        (
          Vector("theirs", "nobody's").flatMap(n => ok(shelf(p).current(key(n), TestLabels.Trial))),
          keys(ok(shelf(p).newest(10))),
          ok(search.shelved(Vector(p), at(5)))
        )
      ) ==> (Vector(), Vector(), Vector())
    }

    test("a sweep withdraws each current document its plugin no longer declares, at any label") {
      val p = name("doc-sweep")
      val declaring = terms()
      val k = keeper(p, declaring)
      val trialled = Clearance.of(TestLabels.Trial)
      val gone = ujson.Obj("gone" -> true)
      put(k, "stays", "kept on", 1)
      put(k, "public", "dropped", 2, data = gone)
      opened(trialled)(
        ok(
          k.write(
            key("trial"),
            TestLabels.Trial,
            Integrity.Member,
            place("here"),
            text("dropped"),
            gone,
            None,
            at(3)
          )
        )
      )
      val documents = new grit.core.plugin.Documents {
        val terms: DocumentTerms = declaring
        val posting: Option[grit.core.plugin.DocumentPosting] = None
        override def undeclared(document: Document): Boolean =
          document.data.objOpt.exists(_.contains("gone"))
      }
      val swept = opened(trialled)(ok(documents.sweep(k, at(4))))
      (swept, opened(trialled)(keys(ok(shelf(p).newest(10))))) ==> (2, Vector("stays"))
    }

    test("kept is every plugin with documents, withdrawn or not") {
      val (p, q, r) = (name("doc-kept-p"), name("doc-kept-q"), name("doc-kept-r"))
      put(keeper(p, terms()), "k", "words", 1)
      val kq = keeper(q, terms())
      put(kq, "k", "words", 1)
      transaction(ok(kq.withdraw(key("k"), Label.Public, at(2))))
      transaction(ok(store.kept())).filter(Set(p, q, r)).toSet ==> Set(p, q)
    }

    test(
      "referring finds a plugin's own current documents holding a reference, latest written first, at most n"
    ) {
      val (p, q) = (name("doc-refs-p"), name("doc-refs-q"))
      val (kp, kq) = (keeper(p, terms()), keeper(q, terms()))
      val (a, b) = (ref("strand", "s-1"), ref("strand", "s-2"))
      put(kp, "one", "first", 1, refs = Set(a))
      put(kp, "two", "second", 2, refs = Set(a, b))
      put(kp, "three", "third", 3)
      put(kp, "four", "fourth", 4, refs = Set(a))
      put(kp, "four", "fourth again", 5, refs = Set(b))
      put(kq, "theirs", "theirs", 6, refs = Set(a))
      // Stored last but written earliest: latest written is by its date, not by when it was stored.
      put(kp, "five", "fifth", 1, refs = Set(b))
      def referring(of: DocRef, n: Int) = transaction(keys(ok(shelf(p).referring(of, n))))
      (
        referring(a, 10),
        referring(b, 10),
        referring(a, 1),
        referring(a, 0),
        referring(ref("strand", "s-3"), 10),
        transaction(keys(ok(shelf(q).referring(a, 10))))
      ) ==> (
        Vector("two", "one"),
        Vector("four", "two", "five"),
        Vector("two"),
        Vector(),
        Vector(),
        Vector("theirs")
      )
    }

    test("a document holding a reference above Tx.cleared is not found by it") {
      val p = name("doc-refs-above")
      val k = keeper(p, terms())
      val a = ref("card", "c-1")
      put(k, "low", "low", 1, refs = Set(a))
      opened(Clearance.of(TestLabels.Trial))(
        ok(
          k.write(
            key("high"),
            Label.Public,
            Integrity.Member,
            place("here"),
            text("high"),
            Empty,
            None,
            at(2),
            Set(a)
          )
        )
      )
      (
        transaction(keys(ok(shelf(p).referring(a, 10)))),
        opened(Clearance.of(TestLabels.Trial))(keys(ok(shelf(p).referring(a, 10))))
      ) ==> (Vector("low"), Vector("high", "low"))
    }

    test(
      "a write differing only in its references is a new version; the same references, in any order, are Unchanged"
    ) {
      val p = name("doc-refs-unchanged")
      val k = keeper(p, terms())
      val (a, b) = (ref("topic", "t-1"), ref("mention", "m-1"))
      def write(refs: Set[DocRef], m: Int) = transaction(
        ok(
          k.write(
            key("k"),
            Label.Public,
            Integrity.Member,
            place("here"),
            text("words"),
            Empty,
            None,
            at(m),
            refs
          )
        )
      )
      val first = put(k, "k", "words", 1, refs = Set(a, b))
      val again = write(Set(b, a), 2)
      val fewer = write(Set(a), 3)
      val none = write(Set.empty, 4)
      assertMatch((again, fewer, none)) {
        case (
              Written.Unchanged(`first`, _),
              Written.Versioned(_, _, _),
              Written.Versioned(_, _, _)
            ) =>
      }
      transaction(keys(ok(shelf(p).referring(a, 10)))) ==> Vector()
    }

    test("a write holding more than DocRef.Most references is refused, writing nothing") {
      val p = name("doc-refs-most")
      val k = keeper(p, terms())
      val most = (0 until DocRef.Most).map(i => ref("strand", s"s-$i")).toSet
      val over = ref("strand", "s-over")
      val many = most + over
      val refused = transaction(
        k.write(
          key("k"),
          Label.Public,
          Integrity.Member,
          place("here"),
          text("words"),
          Empty,
          None,
          at(1),
          many
        )
      )
      put(k, "j", "most", 2, refs = most)
      assertMatch(refused) { case Left(StoreError.Invalid(_)) => }
      (
        transaction(ok(shelf(p).current(key("k"), Label.Public))),
        transaction(keys(ok(shelf(p).referring(ref("strand", "s-0"), 10)))),
        transaction(keys(ok(shelf(p).referring(over, 10))))
      ) ==> (None, Vector("j"), Vector())
    }

    test(
      "a document put up is offered only to a place written at a label dominating its own, and only to a transaction reading it"
    ) {
      val p = name("doc-review-to")
      val k = keeper(p, terms())
      val low = put(k, "low", "low", 1)
      val high = opened(Clearance.of(TestLabels.Trial))(
        ok(
          k.write(
            key("high"),
            Label.Public,
            Integrity.Member,
            place("here"),
            text("high"),
            Empty,
            None,
            at(2)
          )
        )
      ) match {
        case Written.Versioned(v, _, _) => v
        case w => throw new java.lang.AssertionError(s"$w")
      }
      val up = opened(Clearance.of(TestLabels.Trial))(
        Vector(ok(k.forReview(low, Day, at(3))), ok(k.forReview(high, Day, at(4))))
      )
      val trial = Clearance.of(TestLabels.Trial)
      (
        up,
        offered(p, publicPlace, trial),
        offered(p, trialPlace, trial),
        offered(p, trialPlace),
        offered(p, place("unmapped"), trial),
        offered(name("doc-review-other"), trialPlace, trial)
      ) ==> (
        Vector(true, true),
        Vector("low"),
        Vector("low", "high"),
        Vector("low"),
        Vector(),
        Vector()
      )
    }

    test(
      "a version is put up once a day, only while it is one of the plugin's current documents"
    ) {
      val (p, q) = (name("doc-review-once"), name("doc-review-once-q"))
      val k = keeper(p, terms())
      val first = put(k, "k", "first", 1)
      val second = put(k, "k", "second", 2)
      val theirs = put(keeper(q, terms()), "k", "theirs", 2)
      val gone = put(k, "gone", "gone", 2)
      transaction(ok(k.withdraw(key("gone"), Label.Public, at(3))))
      val answers = transaction(
        Vector(
          ok(k.forReview(first, Day, at(4))),
          ok(k.forReview(theirs, Day, at(4))),
          ok(k.forReview(gone, Day, at(4))),
          ok(k.forReview(second, Day, at(4))),
          ok(k.forReview(second, Day, at(5))),
          ok(k.forReview(second, Day.plusDays(1), at(5)))
        )
      )
      answers ==> Vector(false, false, false, true, false, true)
      openedUnder(Clearance.of(Label.Public), posting)(ok(reviews.of(p).unposted(publicPlace)))
        .map(r => (DocText.value(r.document.text), r.day)) ==>
        Vector(("second", Day), ("second", Day.plusDays(1)))
    }

    test(
      "a review is offered until posted, posted once at an address no other holds, and dropped only while waiting"
    ) {
      val p = name("doc-review-posted")
      val k = keeper(p, terms())
      val (a, b) = (put(k, "a", "a", 1), put(k, "b", "b", 1))
      transaction {
        ok(k.forReview(a, Day, at(2)))
        ok(k.forReview(b, Day, at(3)))
      }
      val Vector(ra, rb) =
        openedUnder(Clearance.of(Label.Public), posting)(
          ok(reviews.of(p).unposted(publicPlace))
        ): @unchecked
      val r = reviews.of(p)
      val answers = transaction(
        Vector(
          ok(r.posted(ra, "doc-review/1", at(4))),
          ok(r.posted(ra, "doc-review/2", at(4))),
          ok(r.posted(rb, "doc-review/1", at(4))),
          ok(r.dropped(ra, at(5))),
          ok(r.dropped(rb, at(5))),
          ok(r.dropped(rb, at(5))),
          ok(r.posted(rb, "doc-review/3", at(5)))
        )
      )
      (answers, offered(p, publicPlace)) ==>
        (Vector(true, false, false, false, true, false, false), Vector())
    }

    test(
      "a reaction keeps its verdict, a later one replacing it, withdrawn only when it is that verdict by that rater"
    ) {
      val p = name("doc-review-reacted")
      val k = keeper(p, terms())
      val v = put(k, "k", "words", 1)
      transaction(ok(k.forReview(v, Day, at(2))))
      val r = reviews.of(p)
      val subject = waiting(p)
      val other = TestAccounts.account("slack:T/U2")
      val (good, bad) = (word("good"), word("not-useful"))
      val answers = transaction(
        Vector(
          ok(r.posted(subject, "doc-reacted/1", at(3))),
          ok(r.reacted("doc-reacted/none", rater, good, at(4))),
          ok(r.reacted("doc-reacted/1", rater, good, at(4))),
          ok(r.reacted("doc-reacted/1", rater, bad, at(5))),
          ok(reviews.of(name("doc-review-reacted-q")).reacted("doc-reacted/1", rater, good, at(6)))
        )
      )
      val kept = labelled(p)
      val refused = transaction(
        Vector(
          ok(r.unreacted("doc-reacted/1", rater, good)),
          ok(r.unreacted("doc-reacted/1", other, bad))
        )
      )
      val withdrawn = transaction(ok(r.unreacted("doc-reacted/1", rater, bad)))
      (answers, kept, refused, withdrawn, labelled(p)) ==> (
        Vector(true, false, true, true, false),
        Vector(DocumentLabel(key("k"), Label.Public, Day, Some(v), bad, rater, at(5))),
        Vector(false, false),
        true,
        Vector()
      )
    }

    test(
      "a label outlives its version, read by labels from a day, earliest day first; the review is no longer offered"
    ) {
      val p = name("doc-review-outlives")
      val k = keeper(p, terms())
      val (a, b, c) = (put(k, "a", "a", 1), put(k, "b", "b", 1), put(k, "c", "c", 1))
      transaction {
        ok(k.forReview(a, Day.plusDays(1), at(2)))
        ok(k.forReview(b, Day, at(2)))
        ok(k.forReview(c, Day.minusDays(1), at(2)))
      }
      val r = reviews.of(p)
      val up = openedUnder(Clearance.of(Label.Public), posting)(ok(r.unposted(publicPlace)))
      transaction {
        up.zipWithIndex.foreach { (s, i) =>
          ok(r.posted(s, s"doc-outlives/$i", at(3)))
          ok(r.reacted(s"doc-outlives/$i", rater, word("good"), at(4)))
        }
      }
      val d = put(k, "d", "d", 5)
      transaction(ok(k.forReview(d, Day, at(5))))
      val before = offered(p, publicPlace)
      put(k, "b", "b again", 6)
      put(k, "d", "d again", 6)
      transaction {
        ok(store.forget(b))
        ok(store.forget(d))
      }
      (
        labelled(p).map(l => (DocKey.value(l.key), l.day, l.version)),
        before,
        offered(p, publicPlace)
      ) ==> (
        Vector(("b", Day, None), ("a", Day.plusDays(1), Some(a))),
        Vector("d"),
        Vector()
      )
    }

    test("labels reads only the labels given on reviews at a label the transaction reads") {
      val p = name("doc-review-labels-above")
      val k = keeper(p, terms())
      val trial = Clearance.of(TestLabels.Trial)
      val high = opened(trial)(
        ok(
          k.write(
            key("high"),
            Label.Public,
            Integrity.Member,
            place("here"),
            text("high"),
            Empty,
            None,
            at(1)
          )
        )
      ) match {
        case Written.Versioned(v, _, _) => v
        case w => throw new java.lang.AssertionError(s"$w")
      }
      opened(trial)(ok(k.forReview(high, Day, at(2))))
      openedUnder(trial, posting) {
        val r = reviews.of(p)
        ok(r.unposted(trialPlace)).foreach(s => ok(r.posted(s, "doc-labels-above/1", at(3))))
        ok(r.reacted("doc-labels-above/1", rater, word("good"), at(4)))
      }
      (
        labelled(p).map(l => DocKey.value(l.key)),
        opened(trial)(ok(reviews.labels(p, Day))).map(l => DocKey.value(l.key))
      ) ==> (Vector(), Vector("high"))
    }

    test("remove deletes a plugin's reviews; another's stay") {
      val (p, q) = (name("doc-review-remove-p"), name("doc-review-remove-q"))
      val (kp, kq) = (keeper(p, terms()), keeper(q, terms()))
      val (vp, vq) = (put(kp, "k", "p", 1), put(kq, "k", "q", 1))
      transaction {
        ok(kp.forReview(vp, Day, at(2)))
        ok(kq.forReview(vq, Day, at(2)))
      }
      transaction {
        ok(reviews.of(p).posted(waiting(p), "doc-remove/p", at(3)))
        ok(reviews.of(p).reacted("doc-remove/p", rater, word("good"), at(3)))
        ok(reviews.of(q).posted(waiting(q), "doc-remove/q", at(3)))
        ok(reviews.of(q).reacted("doc-remove/q", rater, word("good"), at(3)))
      }
      transaction(ok(store.remove(p, at(4))))
      (labelled(p), labelled(q).map(_.version)) ==> (Vector(), Vector(Some(vq)))
    }

    test("forget deletes every review put up before an instant, and no other; how many") {
      val p = name("doc-review-forget")
      val k = keeper(p, terms())
      val (a, b) = (put(k, "a", "a", 1), put(k, "b", "b", 1))
      transaction {
        ok(k.forReview(a, Day, Instant.parse("1990-01-01T00:00:00Z")))
        ok(k.forReview(b, Day, Instant.parse("1990-01-03T00:00:00Z")))
      }
      val forgot = transaction(ok(reviews.forget(Instant.parse("1990-01-02T00:00:00Z"))))
      (forgot, offered(p, publicPlace)) ==> (1, Vector("b"))
    }

    test(
      "a document holding a 4000-character reference is found by it, and not by one differing in its last character"
    ) {
      val p = name("doc-refs-long")
      val k = keeper(p, terms())
      val held = ref("strand", "x" * 3999 + "a")
      val near = ref("strand", "x" * 3999 + "b")
      put(k, "long", "held", 1, refs = Set(held))
      (
        transaction(keys(ok(shelf(p).referring(held, 10)))),
        transaction(keys(ok(shelf(p).referring(near, 10))))
      ) ==> (Vector("long"), Vector())
    }

    test(
      "declassifying gives the application in force with its policy's text and checks when the transaction reads the policy's label, the application with neither when it does not, and none otherwise; NULL checks read as Missing"
    ) {
      val k = keeper(name("doc-declassifying"), terms())
      val (team, bare) = (over("dc-team"), over("dc-bare"))
      val (applied, unapplied, unpolicied) = (file("dc-a"), file("dc-b"), file("dc-c"))
      val held = Label.at(Level.Internal, TestLabels.compartment("dc-team"))
      val grant = Label.at(Level.Confidential, TestLabels.compartment("dc-team"))
      val cleared = Clearance.of(grant)
      val checks = Checks.read(Some(InMemoryDocuments.Checks))
      policyAt(team, 1, Some("drop the venue"), grant, Some(InMemoryDocuments.Checks))
      application(applied, 1, Some(team))
      application(unpolicied, 1, Some(bare))
      def asked(clearance: Clearance, label: Label, pointer: Pointer) =
        opened(clearance)(ok(k.declassifying(label, pointer)))
          .map(i => (i.applied.policy, i.applied.version, i.policy))
      val first = (
        asked(cleared, held, applied),
        asked(Clearance.of(Label.Public), held, applied),
        asked(cleared, Label.at(Level.Internal), applied),
        asked(cleared, Label.Unmapped.join(held), applied),
        asked(cleared, held, unapplied),
        asked(cleared, Label.at(Level.Internal, TestLabels.compartment("dc-bare")), unpolicied)
      )
      policyAt(team, 2, Some("drop the venue and the date"), grant, None)
      val second = asked(cleared, held, applied)
      policyAt(team, 3, None, grant, None)
      (first, second, asked(cleared, held, applied)) ==> (
        (
          Some((DeclassificationPolicy(team, 1), 1, Some(("drop the venue", checks)))),
          Some((DeclassificationPolicy(team, 1), 1, None)),
          None,
          None,
          None,
          None
        ),
        Some(
          (
            DeclassificationPolicy(team, 2),
            1,
            Some(("drop the venue and the date", Left(Checks.Unreadable.Missing)))
          )
        ),
        None
      )
    }

    test(
      "a declassified rewrite is kept at its source's label without the compartment, at its place, under a key core derives, with data naming its source, its file's revision and the versions it was made under, and its audit row, which keeps the attempt that passed"
    ) {
      val p = name("doc-declassified")
      val k = keeper(p, terms())
      val team = over("dk-team")
      val card = file("dk-card")
      val source = Label.at(Level.Internal, TestLabels.compartment("dk-team"))
      policy(team, 1, revoked = false)
      application(card, 1, Some(team))
      val confidential = Clearance.of(Label.at(Level.Confidential))
      val written = opened(confidential) {
        val applied = ok(k.declassifying(source, card))
          .map(_.applied)
          .getOrElse(throw new java.lang.AssertionError("no application"))
        ok(
          k.declassified(
            declassified("the gist", source, card, applied, Declassified.Attempt.Second),
            Integrity.Member,
            at(1)
          )
        )
      }
      val v = written match {
        case Written.Versioned(v, _, _) => v
        case w => throw new java.lang.AssertionError(s"$w")
      }
      val derived = key(s"cards:${ShortHash.of("card")}")
      (
        written.kept,
        opened(confidential)(
          ok(shelf(p).current(derived, Label.at(Level.Internal)))
            .map(d => (DocText.value(d.text), d.place, d.pointer, Declassified.Made.read(d.data)))
        ),
        audited(v).map(a => (DocumentVersionId.value(a.source), a.policy, a.application, a.attempt))
      ) ==> (
        Label.at(Level.Internal),
        Some(
          (
            "the gist",
            place("cards"),
            None,
            Some(Declassified.Made(got(DocumentVersionId.of(1).toRight("")), Revision, team, 1, 1))
          )
        ),
        Some((1L, 1, 1, 2))
      )
    }

    test(
      "a declassified rewrite whose policy or application version is no longer current is refused, writing nothing"
    ) {
      val p = name("doc-declassified-stale")
      val k = keeper(p, terms())
      val team = over("ds-team")
      val card = file("ds-card")
      val source = Label.at(Level.Internal, TestLabels.compartment("ds-team"))
      def applied = transaction(ok(k.declassifying(source, card)))
        .map(_.applied)
        .getOrElse(throw new java.lang.AssertionError("no application"))
      def kept(a: PolicyApplication) =
        transaction(
          k.declassified(declassified("gist", source, card, a), Integrity.Member, at(1))
        )
      val stale = Left(
        StoreError.Invalid("the declassification's policy or application is no longer current")
      )
      policy(team, 1, revoked = false)
      application(card, 1, Some(team))
      val first = applied
      policy(team, 2, revoked = false)
      val policyStale = kept(first)
      val second = applied
      application(card, 2, Some(team))
      val applicationStale = kept(second)
      val internal = Clearance.of(Label.at(Level.Internal))
      (policyStale, applicationStale, opened(internal)(keys(ok(shelf(p).newest(10))))) ==>
        (stale, stale, Vector())
    }

    test("a rewrite at a new label withdraws the one at the old label") {
      val p = name("doc-declassified-moved")
      val k = keeper(p, terms())
      val team = over("dm-team")
      val card = file("dm-card")
      val confidential = Clearance.of(Label.at(Level.Confidential))
      policy(team, 1, revoked = false)
      application(card, 1, Some(team))
      def rewrite(body: String, level: Level): Written = {
        val source = Label.at(level, TestLabels.compartment("dm-team"))
        opened(confidential) {
          val applied = ok(k.declassifying(source, card))
            .map(_.applied)
            .getOrElse(throw new java.lang.AssertionError("no application"))
          ok(k.declassified(declassified(body, source, card, applied), Integrity.Member, at(1)))
        }
      }
      val derived = key(s"cards:${ShortHash.of("card")}")
      def keptAt(level: Level) =
        opened(confidential)(
          ok(shelf(p).current(derived, Label.at(level))).map(d => DocText.value(d.text))
        )
      (
        rewrite("the gist", Level.Internal).kept,
        rewrite("the fuller gist", Level.Confidential).kept,
        keptAt(Level.Internal),
        keptAt(Level.Confidential)
      ) ==> (
        Label.at(Level.Internal),
        Label.at(Level.Confidential),
        None,
        Some("the fuller gist")
      )
    }

    test(
      "a bench row is kept for a version and read back; one under a new screening replaces it, and one for a version not kept is Invalid, keeping nothing"
    ) {
      val k = keeper(name("doc-bench"), terms())
      val team = over("db-team")
      policy(team, 1, revoked = false)
      val v1 = DeclassificationPolicy(team, 1)
      val passing = got(Benched.read(true, ujson.Arr()))
      val failing = got(
        Benched.read(
          false,
          ujson.Arr(
            ujson.Obj("at" -> 0, "kind" -> "accept", "must" -> "absent", "p" -> 0.63, "signal" -> 3)
          )
        )
      )
      val (first, second) =
        (Declassified.Screening.of(Sha256.text("a")), Declassified.Screening.of(Sha256.text("b")))
      val before = transaction(ok(k.benched(v1)))
      transaction(ok(k.keepBenched(new Declassified.BenchRow(v1, first, failing), at(1))))
      val kept = transaction(ok(k.benched(v1)))
      transaction(ok(k.keepBenched(new Declassified.BenchRow(v1, second, passing), at(2))))
      val replaced = transaction(ok(k.benched(v1)))
      val absent = DeclassificationPolicy(team, 2)
      val refused =
        transaction(k.keepBenched(new Declassified.BenchRow(absent, first, passing), at(3)))
      (
        before,
        kept,
        replaced,
        refused.left.map(_.isInstanceOf[StoreError.Invalid]),
        transaction(ok(k.benched(absent)))
      ) ==> (
        None,
        Some((first, failing)),
        Some((second, passing)),
        Left(true),
        None
      )
    }

    test("benchable gives the version in force, and none once it is revoked") {
      val k = keeper(name("doc-benchable"), terms())
      val team = over("dbl-team")
      policy(team, 1, revoked = false)
      policy(team, 2, revoked = false)
      val inForce = transaction(ok(k.benchable(team)))
      policy(team, 3, revoked = true)
      (inForce, transaction(ok(k.benchable(team)))) ==> (
        Vector(DeclassificationPolicy(team, 2)),
        Vector()
      )
    }

    test(
      "checks gives a version's checks, in force or not, only when the transaction reads its policy's label; none for a revocation or a version not kept"
    ) {
      val k = keeper(name("doc-checks"), terms())
      val team = over("dck-team")
      val grant = Label.at(Level.Confidential, TestLabels.compartment("dck-team"))
      policyAt(team, 1, Some("drop the venue"), grant, Some(InMemoryDocuments.Checks))
      policyAt(team, 2, Some("drop the date"), grant, None)
      policyAt(team, 3, None, grant, None)
      def asked(clearance: Clearance, version: Int) =
        opened(clearance)(ok(k.checks(DeclassificationPolicy(team, version))))
          .map(c => (c.policy.version, c.checks))
      val cleared = Clearance.of(grant)
      (
        asked(cleared, 1),
        asked(cleared, 2),
        asked(Clearance.of(Label.Public), 1),
        asked(cleared, 3),
        asked(cleared, 4)
      ) ==> (
        Some((1, Checks.read(Some(InMemoryDocuments.Checks)))),
        Some((2, Left(Checks.Unreadable.Missing))),
        None,
        None,
        None
      )
    }
  }
}
