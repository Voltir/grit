package grit.knowledge.github

import java.time.{LocalTime, ZoneId}

import scala.annotation.tailrec

import grit.core.id.{CorpusName, ScheduleKey}
import grit.core.job.SlotRule
import grit.core.place.Service
import grit.core.triage.Corpus
import grit.core.visibility.{Label, Level}
import grit.knowledge.card.{Card, CardSummary, Scope, UnitKind, UnitVersion}
import grit.knowledge.ref.CardKey
import grit.knowledge.sync.{
  Fetched,
  Held,
  Outcome,
  Step,
  SyncSchedule,
  Synced,
  Unwritten,
  Walk,
  Way
}

import utest.*

/** The walk over a scripted repository, its steps answered in place of a run's moves.
  *
  * The repository, as first written (each SHA a small number, shown as `@n`):
  * {{{
  *   /            docs @10, src @90, README.md @91
  *   docs         a.md @1, b.md @2, guide @20, logo.png @92
  *   docs/guide   c.md @3
  * }}}
  * The source includes `docs`, of files those ending `.md`: `src`, `README.md` and `logo.png`
  * are outside it.
  */
object RepositoryWalkTests extends TestSuite {

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private def sha(n: Int): UnitVersion = right(UnitVersion.of(f"$n%040x"))

  private val service = right(Service.of("github"))
  private val name = right(CorpusName.of("repo"))
  private val corpus = Corpus(name, "the team's repository", service.place, Some(service))
  private val rule = SlotRule.Daily(LocalTime.of(3, 0), ZoneId.of("UTC"))

  private def source(
      maxUnits: Int = 100,
      include: Vector[String] = Vector("docs"),
      extensions: Set[String] = Set("md"),
      keeps: String = "documentation"
  ): GitHubCards = right(
    GitHubCards.of(
      corpus,
      "github",
      "octocat",
      "Hello-World",
      "main",
      include,
      extensions,
      keeps,
      maxUnits,
      None,
      service.place,
      Vector(right(SyncSchedule.of(right(ScheduleKey.of("nightly")), rule, 5)))
    )
  )

  private val floor = Label.at(Level.Confidential)
  private val lower = Label.at(Level.Internal)

  private def file(path: String, v: Int) = Entry(path, UnitKind.File, sha(v))
  private def dir(path: String, v: Int) = Entry(path, UnitKind.Directory, sha(v))

  private def root(docs: Int = 10) =
    Listing.Complete(Vector(dir("docs", docs), dir("src", 90), file("README.md", 91)))
  private def docs(a: Int = 1, b: Int = 2, guide: Int = 20): Vector[Entry] =
    Vector(
      file("docs/a.md", a),
      file("docs/b.md", b),
      dir("docs/guide", guide),
      file("docs/logo.png", 92)
    )
  private val guide = Listing.Complete(Vector(file("docs/guide/c.md", 3)))

  private def text(path: String) = s"The text of $path."
  private def read(path: String, v: Int): Fetched = Fetched.Text(text(path), sha(v))

  private val summary = right(CardSummary.of("What the file holds.", Vector("a term")))

  private def held(path: String, v: Int, label: Label = floor): Held =
    Held.of(name, path, label, sha(v), None)

  /** A directory's card held at `label`, made under `scope`. */
  private def heldDir(path: String, v: Int, label: Label, scope: Scope): Held =
    Held.of(name, path, label, sha(v), Some(scope))

  /** Every card of the repository as first written, kept at `label`, its directories' made
    * under `scope`.
    */
  private def heldAll(label: Label = floor, scope: Scope = source().scope): Vector[Held] = Vector(
    heldDir("docs", 10, label, scope),
    held("docs/a.md", 1, label),
    held("docs/b.md", 2, label),
    heldDir("docs/guide", 20, label, scope),
    held("docs/guide/c.md", 3, label)
  )

  private val paths: Vector[String] =
    Vector("docs", "docs/a.md", "docs/b.md", "docs/guide", "docs/guide/c.md")
  private def pathOf(key: CardKey): String =
    paths.find(CardKey.of(name, _) == key).getOrElse(CardKey.value(key))

  private def at(v: UnitVersion): String = s"@${BigInt(UnitVersion.value(v), 16)}"

  /** A step as a line: its kind, its unit's path and way, its version and labels. A walk of a
    * repository reaches each unit [[Way.Run]], shown as nothing.
    */
  private def shown(step: Step): String = step match {
    case Step.List(path, way) => s"list $path${through(way)}"
    case Step.Read(path, way) => s"read $path${through(way)}"
    case Step.Summarise(path, _, _, v) => s"summarise $path ${at(v)}"
    case Step.Write(card, replacing) =>
      s"write ${card.path} ${at(card.version)}" +
        replacing.fold("")(l => s" replacing ${Label.written(l)}")
    case Step.Relabel(key, from) => s"relabel ${pathOf(key)} from ${Label.written(from)}"
    case Step.Drop(gone) =>
      "drop " + gone.map(h => s"${h.path} at ${Label.written(h.label)}").mkString(", ")
  }

  /** `way` as [[shown]] ends a fetch's line: nothing for [[Way.Run]]. */
  private def through(way: Way): String = way match {
    case Way.Run => ""
    case Way.Through(listing) => s" through $listing"
  }

  /** A run's moves answering from `listings` and `files`; every summary is [[summary]]. */
  private def moves(listings: Map[String, Listing], files: Map[String, Fetched])(
      step: Step
  ): Outcome[Listing] = step match {
    case Step.List(path, _) =>
      listings.get(path).fold(Outcome.Stopped(s"no directory $path"))(Outcome.Listed(_, floor))
    case Step.Read(path, _) =>
      files.get(path).fold(Outcome.Stopped(s"no file $path"))(Outcome.Fetched(_, floor))
    case Step.Summarise(_, _, _, _) => Outcome.Summarised(summary)
    case Step.Write(_, _) | Step.Relabel(_, _) | Step.Drop(_) => Outcome.Kept
  }

  /** Each step of `walk` answered by `answer`, and what its run came to. */
  private def drive(
      walk: Walk[Listing],
      answer: Step => Outcome[Listing]
  ): (Vector[Step], Synced) = {
    @tailrec def go(w: Walk[Listing], steps: Vector[Step]): (Vector[Step], Synced) = w.next match {
      case Left(synced) => (steps, synced)
      case Right(step) =>
        if (steps.size > 100) throw new java.lang.AssertionError(s"no end: ${steps.map(shown)}")
        go(w.after(answer(step)), steps :+ step)
    }
    go(walk, Vector.empty)
  }

  /** The steps, shown, and the result of a run holding `held` over the scripted answers. */
  private def run(
      held: Vector[Held],
      listings: Map[String, Listing],
      files: Map[String, Fetched] = Map.empty,
      perRun: Int = 5,
      maxUnits: Int = 100,
      include: Vector[String] = Vector("docs"),
      extensions: Set[String] = Set("md")
  ): (Vector[String], Synced) = {
    val (steps, synced) = drive(
      RepositoryWalk.start(source(maxUnits, include, extensions), perRun, floor, held),
      moves(listings, files)
    )
    (steps.map(shown), synced)
  }

  /** The steps of a run holding every card as first written, unshown. */
  private def steps(
      listings: Map[String, Listing],
      files: Map[String, Fetched],
      perRun: Int = 5
  ): Vector[Step] =
    drive(RepositoryWalk.start(source(), perRun, floor, heldAll()), moves(listings, files))._1

  private def synced(
      written: Int = 0,
      relabelled: Int = 0,
      withdrawn: Int = 0,
      unchanged: Int = 0,
      left: Int = 0,
      refused: Int = 0,
      unwritten: Vector[Unwritten] = Vector.empty,
      stopped: Option[String] = None
  ) = Synced(name, written, relabelled, withdrawn, unchanged, left, refused, unwritten, stopped)

  /** `docs` changed at `a.md`, now `@4`, whose read answers `a`. */
  private def changedA(a: Fetched): (Map[String, Listing], Map[String, Fetched]) =
    (Map("/" -> root(docs = 11), "docs" -> Listing.Complete(docs(a = 4))), Map("docs/a.md" -> a))

  /** What the card each step writes of the directory at `path` says. */
  private def said(steps: Vector[Step], path: String): Vector[String] = steps.collect {
    case Step.Write(card, _) if card.path == path => card.summary.summary
  }

  /** `docs`, as first written, with `docs/assets` beside it, holding a logo and `raw`, holding
    * an image: nothing beneath it ends `.md`.
    */
  private val withAssets: Map[String, Listing] = Map(
    "/" -> root(),
    "docs" -> Listing.Complete(docs() :+ dir("docs/assets", 93)),
    "docs/assets" -> Listing.Complete(
      Vector(file("docs/assets/logo.png", 95), dir("docs/assets/raw", 94))
    ),
    "docs/assets/raw" -> Listing.Complete(Vector(file("docs/assets/raw/x.png", 96))),
    "docs/guide" -> guide
  )

  /** The scope a directory's card was made under before its form was part of it. */
  private val formless: Scope = Scope.of(Set("include docs", "extension md"))

  val tests = Tests {
    test("an unchanged tree SHA lists nothing under it") {
      val (steps, result) = run(heldAll(), Map("/" -> root()))
      assert(steps == Vector("list /"), result == synced(unchanged = 5))
    }

    test("a changed file is read, summarised and written, and its directory's card after it") {
      val (listings, files) = changedA(read("docs/a.md", 4))
      val (steps, result) = run(heldAll(), listings, files)
      assert(
        steps == Vector(
          "list /",
          "list docs",
          "read docs/a.md",
          "summarise docs/a.md @4",
          "write docs/a.md @4",
          "write docs @11"
        ),
        result == synced(written = 2, unchanged = 3)
      )
    }

    test("a file's card holds its summary, its read's SHA and its pointer; its head is its text") {
      val (listings, files) = changedA(read("docs/a.md", 4))
      val made = steps(listings, files)
      val card = Card.file(
        name,
        "docs/a.md",
        sha(4),
        summary,
        right(source().pointer("docs/a.md"))
      )
      assert(
        made.contains(
          Step.Summarise(
            "docs/a.md",
            source().subject("docs/a.md"),
            text("docs/a.md"),
            sha(4)
          )
        ),
        made.contains(Step.Write(card, None))
      )
    }

    test("a directory's card says what of it the source holds, at its tree's SHA, with no ask") {
      val (listings, files) = changedA(read("docs/a.md", 4))
      val words = "The directory docs holds documentation: a.md and b.md, and more in guide/."
      val card = Card.directory(
        name,
        "docs",
        sha(11),
        source().scope,
        right(CardSummary.of(words, Vector.empty)),
        right(source().pointer("docs"))
      )
      assert(steps(listings, files).lastOption == Some(Step.Write(card, None)))
    }

    test(
      "a directory with nothing in scope beneath it gets no card, and its parent's names it not"
    ) {
      val (walked, result) = drive(
        RepositoryWalk.start(source(), 5, floor, Vector.empty),
        moves(
          withAssets,
          Map(
            "docs/a.md" -> read("docs/a.md", 1),
            "docs/b.md" -> read("docs/b.md", 2),
            "docs/guide/c.md" -> read("docs/guide/c.md", 3)
          )
        )
      )
      assert(
        walked.map(shown).filter(s => s.startsWith("list") || s.startsWith("write")) == Vector(
          "list /",
          "list docs",
          "write docs/a.md @1",
          "list docs/assets",
          "list docs/assets/raw",
          "write docs/b.md @2",
          "list docs/guide",
          "write docs/guide/c.md @3",
          "write docs/guide @20",
          "write docs @10"
        ),
        said(walked, "docs") ==
          Vector("The directory docs holds documentation: a.md and b.md, and more in guide/."),
        result == synced(written = 5)
      )
    }

    test(
      "a held card of a directory with nothing in scope beneath it is dropped, with those beneath it"
    ) {
      val (steps, result) = run(
        heldAll(scope = formless) ++ Vector(
          heldDir("docs/assets", 93, floor, formless),
          heldDir("docs/assets/raw", 94, floor, formless)
        ),
        withAssets
      )
      assert(
        steps == Vector(
          "list /",
          "list docs",
          "list docs/assets",
          "list docs/assets/raw",
          "list docs/guide",
          "write docs/guide @20",
          "write docs @10",
          "drop docs/assets at confidential, docs/assets/raw at confidential"
        ),
        result == synced(written = 2, withdrawn = 2, unchanged = 3)
      )
    }

    test("a directory whose last file in scope goes is dropped, and its parent's names it not") {
      val (walked, result) = drive(
        RepositoryWalk.start(source(), 5, floor, heldAll()),
        moves(
          Map(
            "/" -> root(docs = 11),
            "docs" -> Listing.Complete(docs(guide = 21)),
            "docs/guide" -> Listing.Complete(Vector(file("docs/guide/c.png", 4)))
          ),
          Map.empty
        )
      )
      assert(
        walked.map(shown) == Vector(
          "list /",
          "list docs",
          "list docs/guide",
          "write docs @11",
          "drop docs/guide at confidential, docs/guide/c.md at confidential"
        ),
        said(walked, "docs") ==
          Vector("The directory docs holds documentation: a.md and b.md."),
        result == synced(written = 1, withdrawn = 2, unchanged = 2)
      )
    }

    test(
      "a directory holding no file in scope itself names the directories that do, in its source's words"
    ) {
      val (walked, _) = drive(
        RepositoryWalk.start(source(keeps = "notes or data"), 5, floor, Vector.empty),
        moves(
          Map(
            "/" -> root(),
            "docs" -> Listing.Complete(
              Vector(dir("docs/api", 21), dir("docs/guide", 20), dir("docs/ref", 22)) :+
                file("docs/logo.png", 92)
            ),
            "docs/api" -> Listing.Complete(Vector(file("docs/api/d.md", 4))),
            "docs/guide" -> guide,
            "docs/ref" -> Listing.Complete(Vector(file("docs/ref/e.md", 5)))
          ),
          Map(
            "docs/api/d.md" -> read("docs/api/d.md", 4),
            "docs/guide/c.md" -> read("docs/guide/c.md", 3),
            "docs/ref/e.md" -> read("docs/ref/e.md", 5)
          )
        )
      )
      assert(
        said(walked, "docs") ==
          Vector("The directory docs holds notes or data in api/, guide/ and ref/.")
      )
    }

    test("the head summarised is the text's first 8 KB, never splitting a character") {
      val long = "é" * 5000 // two bytes each
      val (listings, files) = changedA(Fetched.Text("a" + long, sha(4)))
      val heads = steps(listings, files).collect { case Step.Summarise(_, _, head, _) => head }
      assert(heads == Vector("a" + "é" * 4095))
    }

    test("a file gone from a directory listed in full is dropped") {
      val (steps, result) = run(
        heldAll() :+ held("docs/old.md", 5),
        Map("/" -> root(docs = 11), "docs" -> Listing.Complete(docs()))
      )
      assert(
        steps == Vector(
          "list /",
          "list docs",
          "write docs @11",
          "drop docs/old.md at confidential"
        ),
        result == synced(written = 1, withdrawn = 1, unchanged = 4)
      )
    }

    test("a unit outside the source's include and extensions is neither listed nor read") {
      val (steps, _) = run(
        Vector.empty,
        Map("/" -> root(), "docs" -> Listing.Complete(docs()), "docs/guide" -> guide),
        Map(
          "docs/a.md" -> read("docs/a.md", 1),
          "docs/b.md" -> read("docs/b.md", 2),
          "docs/guide/c.md" -> read("docs/guide/c.md", 3)
        )
      )
      assert(
        steps.filter(s => s.startsWith("list") || s.startsWith("read")) ==
          Vector(
            "list /",
            "list docs",
            "read docs/a.md",
            "read docs/b.md",
            "list docs/guide",
            "read docs/guide/c.md"
          )
      )
    }

    test("at perRun summaries the walk stops, and counts the units it saw changed as left") {
      val (steps, result) = run(
        Vector.empty,
        Map("/" -> root(), "docs" -> Listing.Complete(docs()), "docs/guide" -> guide),
        Map("docs/a.md" -> read("docs/a.md", 1), "docs/b.md" -> read("docs/b.md", 2)),
        perRun = 1
      )
      // Left: b.md, guide, and docs' own card.
      assert(
        steps == Vector(
          "list /",
          "list docs",
          "read docs/a.md",
          "summarise docs/a.md @1",
          "write docs/a.md @1"
        ),
        result == synced(written = 1, left = 3)
      )
    }

    test(
      "stopped at perRun, or refusing a new directory at maxUnits, it drops nothing beneath a directory it did not list"
    ) {
      val stale = Vector(held("docs/old.md", 5), held("docs/guide/old.md", 6))
      val listings =
        Map("/" -> root(), "docs" -> Listing.Complete(docs()), "docs/guide" -> guide)
      val files = Map("docs/a.md" -> read("docs/a.md", 1), "docs/b.md" -> read("docs/b.md", 2))
      val (perRun, _) = run(stale, listings, files, perRun = 1)
      val (maxUnits, _) = run(stale, listings, files, maxUnits = 3)
      assert(
        perRun.lastOption == Some("drop docs/old.md at confidential"),
        !maxUnits.contains("list docs/guide"),
        maxUnits.lastOption == Some("drop docs/old.md at confidential")
      )
    }

    test("a Partial directory keeps its cards, and nothing beneath it is dropped") {
      val (steps, result) = run(
        heldAll(),
        Map("/" -> root(docs = 11), "docs" -> Listing.Partial(Vector(file("docs/a.md", 4)))),
        Map("docs/a.md" -> read("docs/a.md", 4))
      )
      assert(
        steps == Vector(
          "list /",
          "list docs",
          "read docs/a.md",
          "summarise docs/a.md @4",
          "write docs/a.md @4"
        ),
        result == synced(written = 1, unwritten = Vector(Unwritten("docs", "listed only in part")))
      )
    }

    test("held at another label, an unchanged unit is relabelled with no read and no ask") {
      val (steps, result) = run(heldAll(lower), Map("/" -> root()))
      assert(
        steps == Vector(
          "list /",
          "relabel docs from internal",
          "relabel docs/a.md from internal",
          "relabel docs/b.md from internal",
          "relabel docs/guide from internal",
          "relabel docs/guide/c.md from internal"
        ),
        result == synced(relabelled = 5)
      )
    }

    test(
      "held at the floor elsewhere than its source's place, an unchanged unit is moved there with no read and no ask"
    ) {
      val elsewhere =
        heldAll().map(h => Held.of(name, h.path, h.label, h.version, h.scope, h.within, true))
      val (steps, result) = run(elsewhere, Map("/" -> root()))
      val from = Label.written(floor)
      assert(
        steps == Vector(
          "list /",
          s"relabel docs from $from",
          s"relabel docs/a.md from $from",
          s"relabel docs/b.md from $from",
          s"relabel docs/guide from $from",
          s"relabel docs/guide/c.md from $from"
        ),
        result == synced(relabelled = 5)
      )
    }

    test(
      "held at another label, a changed unit's old copy is withdrawn in the write replacing it"
    ) {
      val (listings, files) = changedA(read("docs/a.md", 4))
      val (steps, result) = run(heldAll(lower), listings, files)
      assert(
        steps == Vector(
          "list /",
          "list docs",
          "read docs/a.md",
          "summarise docs/a.md @4",
          "write docs/a.md @4 replacing internal",
          "relabel docs/b.md from internal",
          "relabel docs/guide from internal",
          "relabel docs/guide/c.md from internal",
          "write docs @11 replacing internal"
        ),
        result == synced(written = 2, relabelled = 3)
      )
    }

    test("held at another label, a unit gone is dropped at that label") {
      val (steps, _) = run(
        heldAll() :+ held("docs/old.md", 5, lower),
        Map("/" -> root(docs = 11), "docs" -> Listing.Complete(docs()))
      )
      assert(steps.lastOption == Some("drop docs/old.md at internal"))
    }

    test("a unit held at the floor and at another label keeps the floor's, and drops the other") {
      val (steps, result) = run(heldAll() :+ held("docs/b.md", 2, lower), Map("/" -> root()))
      assert(
        steps == Vector("list /", "drop docs/b.md at internal"),
        result == synced(withdrawn = 1, unchanged = 5)
      )
    }

    test("a unit held at the floor goes by the floor's copy, whatever another label's records") {
      val (listings, files) = changedA(read("docs/a.md", 4))
      val (steps, _) = run(heldAll() :+ held("docs/a.md", 4, lower), listings, files)
      assert(
        steps == Vector(
          "list /",
          "list docs",
          "read docs/a.md",
          "summarise docs/a.md @4",
          "write docs/a.md @4",
          "write docs @11",
          "drop docs/a.md at internal"
        )
      )
    }

    test(
      "extensions widened, a directory held under the old scope is listed and its new files read"
    ) {
      val (steps, result) = run(
        heldAll(),
        Map("/" -> root(), "docs" -> Listing.Complete(docs()), "docs/guide" -> guide),
        Map("docs/logo.png" -> read("docs/logo.png", 92)),
        extensions = Set("md", "png")
      )
      assert(
        steps == Vector(
          "list /",
          "list docs",
          "list docs/guide",
          "write docs/guide @20",
          "read docs/logo.png",
          "summarise docs/logo.png @92",
          "write docs/logo.png @92",
          "write docs @10"
        ),
        result == synced(written = 3, unchanged = 3)
      )
    }

    test("extensions narrowed, the cards of the files no longer taken are dropped") {
      val wide = source(extensions = Set("md", "png")).scope
      val (steps, _) = run(
        heldAll(scope = wide) :+ held("docs/logo.png", 92),
        Map("/" -> root(), "docs" -> Listing.Complete(docs()), "docs/guide" -> guide)
      )
      assert(
        steps == Vector(
          "list /",
          "list docs",
          "list docs/guide",
          "write docs/guide @20",
          "write docs @10",
          "drop docs/logo.png at confidential"
        )
      )
    }

    test("include narrowed beneath a directory, that directory's card is dropped with the rest") {
      val (steps, _) = run(
        heldAll(),
        Map("/" -> root(), "docs" -> Listing.Complete(docs()), "docs/guide" -> guide),
        include = Vector("docs/guide")
      )
      assert(
        steps == Vector(
          "list /",
          "list docs",
          "list docs/guide",
          "write docs/guide @20",
          "drop docs at confidential, docs/a.md at confidential, docs/b.md at confidential"
        )
      )
    }

    test("at maxUnits a new unit is not taken, and the cards held are still brought up to date") {
      val (steps, result) = run(
        Vector(held("docs/b.md", 1)),
        Map("/" -> root(), "docs" -> Listing.Complete(docs()), "docs/guide" -> guide),
        Map("docs/a.md" -> read("docs/a.md", 1), "docs/b.md" -> read("docs/b.md", 2)),
        maxUnits = 2
      )
      // Held b.md and new a.md fill it: guide is not taken, and docs' own card waits on it.
      assert(
        steps == Vector(
          "list /",
          "list docs",
          "read docs/a.md",
          "summarise docs/a.md @1",
          "write docs/a.md @1",
          "read docs/b.md",
          "summarise docs/b.md @2",
          "write docs/b.md @2"
        ),
        result == synced(written = 2, refused = 1)
      )
    }

    test(
      "held past maxUnits, the cards last in walk order drop down to it, with the directories above them"
    ) {
      val (steps, result) = run(heldAll(), Map("/" -> root()), maxUnits = 3)
      assert(
        steps == Vector(
          "list /",
          "drop docs at confidential, docs/guide at confidential, docs/guide/c.md at confidential"
        ),
        result == synced(withdrawn = 3, unchanged = 5)
      )
    }

    test("past a lowered maxUnits, a copy a write already withdrew is not dropped again") {
      val (steps, result) = run(
        heldAll().filterNot(_.path == "docs/guide/c.md") :+ held("docs/guide/c.md", 3, lower),
        Map(
          "/" -> root(docs = 11),
          "docs" -> Listing.Complete(docs(guide = 21)),
          "docs/guide" -> Listing.Complete(Vector(file("docs/guide/c.md", 4)))
        ),
        Map("docs/guide/c.md" -> read("docs/guide/c.md", 4)),
        maxUnits = 4
      )
      assert(
        steps.filter(_.startsWith("write")) == Vector(
          "write docs/guide/c.md @4 replacing internal",
          "write docs/guide @21",
          "write docs @11"
        ),
        steps.lastOption == Some("drop docs at confidential, docs/guide at confidential"),
        result == synced(written = 3, withdrawn = 2, unchanged = 2)
      )
    }

    test("a file whose read's SHA differs from its listing's is written at the read's") {
      val (listings, files) = changedA(read("docs/a.md", 5))
      val (steps, _) = run(heldAll(), listings, files)
      assert(steps.slice(3, 5) == Vector("summarise docs/a.md @5", "write docs/a.md @5"))
    }

    test("a run reads at most perRun files, those its reads leave unwritten among them") {
      val (steps, result) = run(
        Vector.empty,
        Map("/" -> root(), "docs" -> Listing.Complete(docs()), "docs/guide" -> guide),
        Map(
          "docs/a.md" -> Fetched.Unversioned("no version"),
          "docs/b.md" -> Fetched.Unversioned("no version")
        ),
        perRun = 1
      )
      (steps.filter(_.startsWith("read")), result.left) ==> (Vector("read docs/a.md"), 3)
    }

    test("an empty file's card is written with no ask, saying it is empty") {
      val made = steps(
        Map("/" -> root(docs = 11), "docs" -> Listing.Complete(docs(a = 4, b = 6))),
        Map("docs/a.md" -> Fetched.Text(" \n", sha(4)), "docs/b.md" -> read("docs/b.md", 6)),
        perRun = 2
      )
      val empty = right(CardSummary.of("An empty file.", Vector.empty))
      assert(
        made.map(shown) == Vector(
          "list /",
          "list docs",
          "read docs/a.md",
          "write docs/a.md @4",
          "read docs/b.md",
          "summarise docs/b.md @6",
          "write docs/b.md @6",
          "write docs @11"
        ),
        made.collectFirst { case Step.Write(card, _) => card.summary } == Some(empty)
      )
    }

    test("a file read with no version is left unwritten, saying why, and so is its directory") {
      val (listings, files) = changedA(Fetched.Unversioned("too large to send"))
      val (steps, result) = run(heldAll(), listings, files)
      assert(
        steps == Vector("list /", "list docs", "read docs/a.md"),
        result == synced(
          unchanged = 3,
          unwritten = Vector(Unwritten("docs/a.md", "too large to send"))
        )
      )
    }

    test("a file whose read was withheld is left unwritten, saying why, and so is its directory") {
      val (listings, files) = changedA(read("docs/a.md", 4))
      val (steps, result) = drive(
        RepositoryWalk.start(source(), 5, floor, heldAll()),
        {
          case Step.Read("docs/a.md", Way.Run) => Outcome.NotTaken("it could not be checked")
          case other => moves(listings, files)(other)
        }
      )
      (steps.map(shown), result) ==> (
        Vector("list /", "list docs", "read docs/a.md"),
        synced(unchanged = 3, unwritten = Vector(Unwritten("docs/a.md", "it could not be checked")))
      )
    }

    test(
      "a walk lists at most maxUnits directories in scope, however many listings are withheld, within calls"
    ) {
      val subdirs = (0 until 6).map(i => dir(s"docs/d$i", 30 + i)).toVector
      val listings = Map(
        "/" -> Listing.Complete(Vector(dir("docs", 10))),
        "docs" -> Listing.Complete(subdirs)
      )
      val cards = source(maxUnits = 2)
      val (steps, result) = drive(
        RepositoryWalk.start(cards, 5, floor, Vector.empty),
        {
          case Step.List(path, _) if path.startsWith("docs/") => Outcome.NotTaken("withheld")
          case other => moves(listings, Map.empty)(other)
        }
      )
      (steps.map(shown), result, steps.size <= cards.calls(5)) ==> (
        Vector("list /", "list docs", "list docs/d0"),
        synced(left = 6, unwritten = Vector(Unwritten("docs/d0", "withheld"))),
        true
      )
    }

    test("the directories on the way to an included path are listed past maxUnits, within calls") {
      val listings = Map(
        "/" -> Listing.Complete(Vector(dir("a", 10))),
        "a" -> Listing.Complete(Vector(dir("a/b", 11))),
        "a/b" -> Listing.Complete(Vector(dir("a/b/c", 12))),
        "a/b/c" -> Listing.Complete(Vector(file("a/b/c/d.md", 13)))
      )
      val cards = source(maxUnits = 1, include = Vector("a/b/c/d.md"))
      val (steps, _) = drive(
        RepositoryWalk.start(cards, 1, floor, Vector.empty),
        moves(listings, Map("a/b/c/d.md" -> read("a/b/c/d.md", 13)))
      )
      val calls = steps.collect { case s @ (Step.List(_, _) | Step.Read(_, _)) => shown(s) }
      (calls, calls.size <= cards.calls(1)) ==> (
        Vector("list /", "list a", "list a/b", "list a/b/c", "read a/b/c/d.md"),
        true
      )
    }

    test(
      "a directory whose listing was withheld is left unwritten, saying why, and nothing beneath it is read or dropped"
    ) {
      val (listings, files) = changedA(read("docs/a.md", 4))
      val (steps, result) = drive(
        RepositoryWalk.start(source(), 5, floor, heldAll()),
        {
          case Step.List("docs", Way.Run) => Outcome.NotTaken("it could not be checked")
          case other => moves(listings, files)(other)
        }
      )
      (steps.map(shown), result) ==> (
        Vector("list /", "list docs"),
        synced(unwritten = Vector(Unwritten("docs", "it could not be checked")))
      )
    }

    test("a stopped step ends the walk at once, saying why, and drops nothing") {
      val (steps, result) = run(heldAll() :+ held("docs/old.md", 5), Map("/" -> root(docs = 11)))
      assert(
        steps == Vector("list /", "list docs"),
        result == synced(stopped = Some("no directory docs"))
      )
    }

    test("an outcome not of its step's kind stops the walk, saying so") {
      val (made, result) =
        drive(RepositoryWalk.start(source(), 5, floor, heldAll()), _ => Outcome.Kept)
      assert(
        made.map(shown) == Vector("list /"),
        result == synced(stopped = Some("a list came to a keep's outcome"))
      )
    }

    test(
      "a reply line counts each kind and those not taken, names at most 5 unwritten, and says why it stopped"
    ) {
      val unwritten = (1 to 7).toVector.map(n => Unwritten(s"docs/$n.png", "a binary file"))
      val full = synced(2, 1, 3, 40, 4, 6, unwritten, Some("the service github failed"))
      assert(
        full.line == "repo: 2 written, 1 relabelled, 3 withdrawn, 40 unchanged, " +
          "11 left for the next run; 6 new units not taken, the source holding its most cards; " +
          "not written: docs/1.png (a binary file), " +
          "docs/2.png (a binary file), docs/3.png (a binary file), docs/4.png (a binary file), " +
          "docs/5.png (a binary file), and 2 more; stopped: the service github failed",
        synced(unchanged = 5).line ==
          "repo: 0 written, 0 relabelled, 0 withdrawn, 5 unchanged, 0 left for the next run"
      )
    }
  }
}
