package grit.knowledge.sync

import java.time.{Instant, LocalTime, ZoneId}

import scala.concurrent.duration.*

import grit.core.act.{Asked, Called, Keeping, MoveError, MoveName, Posed, ScriptedKeeping}
import grit.core.document.{
  DocLabel,
  DocText,
  DocWeight,
  Document,
  DocumentKeeper,
  DocumentTerms,
  InMemoryDocuments
}
import grit.core.durable.Journaled
import grit.core.flow.TestTainted
import grit.core.id.{CorpusName, PluginName, ScheduleKey}
import grit.core.job.{JobRun, SlotRule}
import grit.core.message.Message
import grit.core.place.Service
import grit.core.store.{StoreError, Tx}
import grit.core.tool.ToolName
import grit.core.triage.Corpus
import grit.core.visibility.{Compartment, Label, Level}
import grit.knowledge.card.{CardKey, CardMoves, Phase, UnitVersion}
import grit.knowledge.github.{GitHubCards, SyncSchedule}

import utest.*

/** The sync job's runs over a scripted repository, their moves made by [[ScriptedKeeping]]
  * over one in-memory store, so a second run reads what the first kept.
  *
  * The repository, as first written (each SHA a small number, shown as `@n`):
  * {{{
  *   /       docs @13
  *   docs    a.md @1, b.md @2
  * }}}
  * The source includes `docs`, of files those ending `.md`. A directory's SHA is 10 plus its
  * files', so it changes with any of them.
  */
object SyncTests extends TestSuite {

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private def hex(n: Int): String = f"$n%040x"

  private val service = right(Service.of("github"))
  private val name = right(CorpusName.of("repo"))
  private val corpus = Corpus(name, "the team's repository", service.place, Some(service))
  private val rule = SlotRule.Daily(LocalTime.of(3, 0), ZoneId.of("UTC"))

  private def source(perRun: Int = 5, maxUnits: Int = 100): GitHubCards = right(
    GitHubCards.of(
      corpus,
      "github",
      "octocat",
      "Hello-World",
      "main",
      Vector("docs"),
      Set("md"),
      maxUnits,
      None,
      service.place,
      Vector(right(SyncSchedule.of(right(ScheduleKey.of("nightly")), rule, perRun)))
    )
  )

  private val sync = new Sync(Vector(source()))

  private val plugin = right(PluginName.of("knowledge"))
  private val terms = right(
    DocLabel
      .of("knowledge: what a source holds, and how to read it")
      .flatMap(DocumentTerms.of(_, DocWeight.Unscaled, 30.days, 100))
  )

  private val floor = Label.at(Level.Confidential)
  private val lower = Label.at(Level.Internal)

  /** A directory's answer: a JSON array of `(type, path, sha)` entries. */
  private def listing(entries: (String, String, Int)*): String =
    ujson.write(ujson.Arr.from(entries.map { case (kind, path, n) =>
      ujson.Obj("type" -> kind, "path" -> path, "sha" -> hex(n))
    }))

  /** A file's answer: its SHA's line, then its text. */
  private def file(n: Int, text: String): String =
    s"successfully downloaded text file (SHA: ${hex(n)})\n$text"

  /** The repository's answers by path: `a.md` at `@a` holding `aText`, `b.md` at `@b` when
    * there is one.
    */
  private def repo(
      a: Int = 1,
      b: Option[Int] = Some(2),
      aText: String = "Deploying the app."
  ): Map[String, String] =
    Map(
      "/" -> listing(("dir", "docs", 10 + a + b.getOrElse(0))),
      "docs" -> listing(("file", "docs/a.md", a) +: b.toSeq.map(n => ("file", "docs/b.md", n))*),
      "docs/a.md" -> file(a, aText)
    ) ++ b.map(n => "docs/b.md" -> file(n, "Testing the app."))

  /** Each call's answer: `answers`' text for its `path`, at `raised`' label for that path or
    * else `at`; `Failed` for a path it lacks, or, when `refused`, for every call, saying so.
    */
  private def answer(
      answers: Map[String, String],
      at: Label,
      raised: Map[String, Label],
      refused: Option[String],
      args: ujson.Obj
  ): Either[MoveError, Called] =
    Right(refused.fold {
      val path = args.value.get("path").flatMap(_.strOpt)
      path
        .flatMap(answers.get)
        .fold(Called.Failed("Not Found"))(
          Called.Done(_, path.flatMap(raised.get).getOrElse(at))
        )
    }(Called.Failed(_)))

  /** The text the summary model is sent: its system text, then each user message's. */
  private def sent(system: String, messages: Vector[Message]): String =
    (system +: messages.collect { case Message.User(text) => text }).mkString("\n")

  /** How the summary model replies. */
  private enum Summaries {

    /** `What the file holds.`, with one term. */
    case Fixed

    /** How many `~` and `^` it was sent, with no term. */
    case Counting

    /** A blank summary. */
    case Blank

    /** Capped when sent `path`; otherwise as [[Fixed]]. */
    case Capped(path: String)

    /** Uncleared at `at` when sent `path`; otherwise as [[Fixed]]. */
    case Uncleared(path: String, at: Label)
  }

  /** The reply `summaries` gives to `system` and `messages`, at `raised`' label for the first
    * path they name, or else at `at`.
    */
  private def reply(
      summaries: Summaries,
      at: Label,
      raised: Map[String, Label],
      system: String,
      messages: Vector[Message]
  ): Either[MoveError, Asked[ujson.Value]] = {
    val text = sent(system, messages)
    val label = raised.collectFirst { case (path, l) if text.contains(path) => l }.getOrElse(at)
    def said(summary: String, terms: String*): Either[MoveError, Asked[ujson.Value]] =
      Right(Asked(ujson.Obj("summary" -> summary, "terms" -> ujson.Arr.from(terms)), label))
    summaries match {
      case Summaries.Capped(path) if text.contains(path) => Left(MoveError.Capped)
      case Summaries.Uncleared(path, l) if text.contains(path) => Left(MoveError.Uncleared(l))
      case Summaries.Fixed | Summaries.Capped(_) | Summaries.Uncleared(_, _) =>
        said("What the file holds.", "a term")
      case Summaries.Counting =>
        said(s"${text.count(_ == '~')} tildes and ${text.count(_ == '^')} carets")
      case Summaries.Blank => said(" ")
    }
  }

  /** A run's moves at `at`: calls answered from `answers` (each refused, saying `refused`,
    * when given), asks as `summaries`, keeps into `documents`. A call reading a path in
    * `readAt`, and the ask summarising a path in `summaryAt`, is answered at its label there.
    */
  private def keeping(
      at: Label,
      documents: InMemoryDocuments,
      answers: Map[String, String] = repo(),
      summaries: Summaries = Summaries.Fixed,
      refused: Option[String] = None,
      readAt: Map[String, Label] = Map.empty,
      summaryAt: Map[String, Label] = Map.empty
  ): ScriptedKeeping =
    new ScriptedKeeping(
      sync.limits,
      at,
      plugin,
      terms,
      _ => Left(MoveError.Model("no text ask")),
      (_, _, args) => answer(answers, at, readAt, refused, args),
      // Positional, not named: a named argument is lifted into a local whose inferred type
      // keeps `Vector`'s internal array (docs/capture-checking.md).
      (system, messages) => reply(summaries, at, summaryAt, system, messages),
      _ => Left(MoveError.Model("no judgment")),
      documents
    )

  private def runOf(moves: Keeping^, perRun: Int = 5): String =
    sync.run(
      TestTainted.trusted(JobRun(SyncParams(name, perRun), Instant.EPOCH, Instant.EPOCH)),
      moves
    )

  private def pathOf(d: Document): String =
    d.data.objOpt.flatMap(_.get("path")).flatMap(_.strOpt).getOrElse(s"no path in ${d.data}")

  /** Each document `k` reads, as its unit's path and label, by path. */
  private def held(k: ScriptedKeeping): Vector[(String, Label)] =
    k.kept.map(d => (pathOf(d), d.label)).sortBy(_._1)

  /** What a first run kept at `at` into a new store, and that store. */
  private def firstAt(at: Label): InMemoryDocuments = {
    val documents = new InMemoryDocuments
    val _ = runOf(keeping(at, documents))
    documents
  }

  private def line(
      written: Int = 0,
      relabelled: Int = 0,
      withdrawn: Int = 0,
      unchanged: Int = 0,
      left: Int = 0,
      stopped: Option[String] = None
  ): String =
    s"repo: $written written, $relabelled relabelled, $withdrawn withdrawn, $unchanged " +
      s"unchanged, $left left for the next run" + stopped.fold("")(why => s"; stopped: $why")

  private val clash = "duplicate key value violates unique constraint"

  /** `inner`, and after each keep the documents `inner` reads, as [[held]] shows them; a keep
    * named `failing` is refused as a store's failure, [[clash]], and not made.
    */
  private final class Watched(inner: ScriptedKeeping, failing: Option[MoveName] = None)
      extends Keeping {
    // Holds immutable values; the fake is one test's, read on its one thread.
    @caps.unsafe.untrackedCaptures
    var seen: Vector[Vector[(String, Label)]] = Vector.empty

    def ask[R](name: MoveName, posed: Posed[R]): Either[MoveError, Asked[R]] =
      inner.ask(name, posed)

    def call(
        name: MoveName,
        service: Service,
        tool: ToolName,
        arguments: ujson.Obj
    ): Either[MoveError, Called] = inner.call(name, service, tool, arguments)

    def keep[A <: caps.Pure: Journaled](name: MoveName)(
        body: (DocumentKeeper, Instant) -> Tx^ ?-> Either[StoreError, A]
    ): Either[MoveError, A] =
      if (failing.contains(name)) Left(MoveError.Store(clash))
      else {
        val kept = inner.keep(name)(body)
        seen = seen :+ held(inner)
        kept
      }
  }

  val tests = Tests {
    test("the first run writes a card of each unit, each at the run's floor, and says so") {
      val k = keeping(floor, new InMemoryDocuments)
      val reply = runOf(k)
      (reply, held(k)) ==> (
        line(written = 3),
        Vector(("docs", floor), ("docs/a.md", floor), ("docs/b.md", floor))
      )
    }

    test(
      "a card is kept at the join of its unit's answers' labels, where they rise above the floor"
    ) {
      val (acme, trial) = (right(Compartment.of("acme")), right(Compartment.of("trial")))
      val documents = new InMemoryDocuments
      val k = keeping(
        floor,
        documents,
        readAt = Map("docs/a.md" -> Label.at(Level.Confidential, acme)),
        summaryAt = Map("docs/a.md" -> Label.at(Level.Confidential, trial))
      )
      val reply = runOf(k)
      val both = Label.at(Level.Confidential, acme, trial)
      (reply, held(keeping(both, documents))) ==> (
        line(written = 3),
        Vector(("docs", floor), ("docs/a.md", both), ("docs/b.md", floor))
      )
    }

    test("a file whose SHA changed is rewritten, with its directory; the rest are untouched") {
      val documents = firstAt(floor)
      val before = keeping(floor, documents).kept.map(d => pathOf(d) -> d.version).toMap
      val k = keeping(floor, documents, repo(a = 4))
      val reply = runOf(k)
      val changed = k.kept.filterNot(d => before.get(pathOf(d)).contains(d.version))
      (reply, changed.map(pathOf).sorted) ==>
        (line(written = 2, unchanged = 1), Vector("docs", "docs/a.md"))
    }

    test("a file gone from its directory's listing has its card withdrawn") {
      val documents = firstAt(floor)
      val k = keeping(floor, documents, repo(b = None))
      val reply = runOf(k)
      (reply, held(k)) ==>
        (
          line(written = 1, withdrawn = 1, unchanged = 1),
          Vector(("docs", floor), ("docs/a.md", floor))
        )
    }

    test("a refused first call leaves every held card current, and the reply says why") {
      val documents = firstAt(lower)
      val before = keeping(floor, documents).kept.map(d => (pathOf(d), d.label, d.version))
      val k = keeping(floor, documents, refused = Some("the server is unreachable"))
      val reply = runOf(k)
      (reply, k.kept.map(d => (pathOf(d), d.label, d.version))) ==> (
        line(stopped = Some("listing /: the call to github failed: the server is unreachable")),
        before
      )
    }

    test("an ask refused as capped ends the run, every card kept before it whole") {
      val k = keeping(floor, new InMemoryDocuments, summaries = Summaries.Capped("docs/b.md"))
      val reply = runOf(k)
      (reply, k.kept.map(d => (pathOf(d), DocText.value(d.text), d.pointer.isDefined))) ==> (
        line(
          written = 1,
          left = 1,
          stopped = Some("summarising docs/b.md: the day's recorded spend reached its cap")
        ),
        Vector(("docs/a.md", "What the file holds.\n\nTerms: a term", true))
      )
    }

    test(
      "an ask no model is cleared for ends the run, naming the label, every card kept before it whole"
    ) {
      val confidential = Label.at(Level.Confidential)
      val k = keeping(
        floor,
        new InMemoryDocuments,
        summaries = Summaries.Uncleared("docs/b.md", confidential)
      )
      val reply = runOf(k)
      (reply, k.kept.map(pathOf)) ==> (
        line(
          written = 1,
          left = 1,
          stopped = Some("summarising docs/b.md: no model is cleared for [level: confidential]")
        ),
        Vector("docs/a.md")
      )
    }

    test("a keep refused by the store ends the run, naming the card it was keeping") {
      val inner = keeping(floor, new InMemoryDocuments)
      val k = new Watched(inner, Some(CardMoves.of(Phase.Write, CardKey.of(name, "docs/b.md"))))
      val reply = runOf(k)
      (reply, held(inner)) ==> (
        line(written = 1, left = 1, stopped = Some(s"keeping the card of docs/b.md: $clash")),
        Vector(("docs/a.md", floor))
      )
    }

    test("a failed call ends the run, naming its service and why") {
      val k = keeping(floor, new InMemoryDocuments, repo() - "docs/b.md")
      runOf(k) ==>
        line(
          written = 1,
          left = 1,
          stopped = Some("reading docs/b.md: the call to github failed: Not Found")
        )
    }

    test("the summary model is sent at most the file's first 8 KB") {
      val k = keeping(
        floor,
        new InMemoryDocuments,
        repo(aText = "~" * 8192 + "^" * 100),
        summaries = Summaries.Counting
      )
      val _ = runOf(k)
      k.kept.filter(pathOf(_) == "docs/a.md").map(d => DocText.value(d.text)) ==>
        Vector("8192 tildes and 0 carets")
    }

    test("a summary that does not read ends the run as the model's failure, saying why") {
      val k = keeping(floor, new InMemoryDocuments, summaries = Summaries.Blank)
      (runOf(k), held(k)) ==> (
        line(
          left = 2,
          stopped = Some(
            "summarising docs/a.md: its arguments do not read: a summary is not blank"
          )
        ),
        Vector()
      )
    }

    test("a card held at another label is withdrawn in the keep that replaces it, never apart") {
      val documents = firstAt(lower)
      val inner = keeping(floor, documents, repo(a = 4))
      val k = new Watched(inner)
      val reply = runOf(k)
      (reply, k.seen) ==> (
        line(written = 2, relabelled = 1),
        Vector(
          Vector(("docs", lower), ("docs/a.md", lower), ("docs/b.md", lower)),
          Vector(("docs", lower), ("docs/a.md", floor), ("docs/b.md", lower)),
          Vector(("docs", lower), ("docs/a.md", floor), ("docs/b.md", floor)),
          Vector(("docs", floor), ("docs/a.md", floor), ("docs/b.md", floor))
        )
      )
    }

    test("a run stopped before the keep replacing a card leaves its copy at another label") {
      val documents = firstAt(lower)
      val k = keeping(floor, documents, repo(a = 4), Summaries.Capped("docs/a.md"))
      val _ = runOf(k)
      held(k) ==> Vector(("docs", lower), ("docs/a.md", lower), ("docs/b.md", lower))
    }

    test("the held keep's record reads back as written, each label in its written form") {
      val compound = Label.at(
        Level.Confidential,
        right(Compartment.of("acme")),
        right(Compartment.of("trial"))
      )
      val held = Vector(
        Held.of(name, "docs", Label.Public, right(UnitVersion.of(hex(13))), Some(source().scope)),
        Held.of(name, "docs/a.md", compound, right(UnitVersion.of(hex(1))), None)
      )
      val holding = Holding(compound, held)
      val journaled = summon[Journaled[Holding]]
      val encoded = ujson.read(journaled.encode(holding))
      (
        journaled.decode(journaled.encode(holding)),
        encoded("floor").str +: encoded("held").arr.map(_("label").str).toVector
      ) ==> (Right(holding), Vector("confidential+acme+trial", "public", "confidential+acme+trial"))
    }

    test("a keep's record reads back as written, its label in its written form") {
      val journaled = summon[Journaled[Kept]]
      val kept = Vector(Kept(1, Some(Label.at(Level.Internal))), Kept(3, None))
      (
        kept.map(k => journaled.decode(journaled.encode(k))),
        kept.map(journaled.encode)
      ) ==> (
        kept.map(Right(_)),
        Vector("""{"withdrawn":1,"at":"internal"}""", """{"withdrawn":3}""")
      )
    }

    test("its limits: asks, the largest perRun; calls, that plus the largest maxUnits") {
      val two =
        new Sync(Vector(source(perRun = 5, maxUnits = 40), source(perRun = 30, maxUnits = 20)))
      val l = two.limits
      (l.asks, l.calls, l.judgments) ==> (30, 70, 0)
    }

    test("parameters naming no source of its are refused by the reply, making no move") {
      val other = right(CorpusName.of("wiki"))
      val k = keeping(floor, new InMemoryDocuments)
      (
        sync.run(
          TestTainted.trusted(JobRun(SyncParams(other, 5), Instant.EPOCH, Instant.EPOCH)),
          k
        ),
        k.made
      ) ==>
        ("wiki: no source of this sync's", Vector())
    }

    test("its parameters read back as written") {
      sync.read(sync.write(SyncParams(name, 7))) ==> Right(SyncParams(name, 7))
    }
  }
}
