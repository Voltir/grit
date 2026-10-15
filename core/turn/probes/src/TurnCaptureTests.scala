package grit.turn

import grit.core.durable.Probes
import grit.core.durable.Probes.{classpath, options}

import utest.*

/** What capture checking rejects about the tools a turn is offered, pinned by compiling
  * probe sources against turn with turn's own flags: a [[TurnTooling]]'s own tools act only
  * through the capabilities it names, so a turn cannot be handed a tool that edits unless
  * its type says so, and its hosted tools, which an edge runs, act through nothing here; and
  * what crosses a turn's rounds holds no capability its rounds act through ([[TurnRound]]).
  * The pattern is `grit.core.tool.ToolCaptureTests` (docs/capture-checking.md).
  */
object TurnCaptureTests extends TestSuite {

  private val prelude =
    """package probe
      |import grit.core.host.*
      |import grit.core.model.ModelSettings
      |import grit.core.provider.Models
      |import grit.core.store.{Db, Jot}
      |import grit.core.tool.*
      |import grit.turn.*
      |val spec: ToolSpec[String] = ToolSpec(ToolName("probe"), "A probe.", Args.of((path = Field.text("A path."))).map(_.path))
      |extension (s: ToolSpec[String]) def hosted: Hosted[String] = new Hosted(s, Gate.Free, p => p)
      |def reads(ws: Workspace^): Tool[String]^{ws} =
      |  Tool(spec, Gate.Free, p => p, p => RelPath.of(p).flatMap(ws.read(_, Lines.All)).fold(e => Outcome.Failed(e.toString), c => Outcome.Done(c.show)))
      |def writes(e: Edits^): Tool[String]^{e} =
      |  Tool(spec, Gate.Free, p => p, p => RelPath.of(p).flatMap(e.write(_, "x")).fold(_ => Outcome.Failed("no"), _ => Outcome.Done("ok")))
      |""".stripMargin

  /** The error messages from compiling `body` after the prelude, with every probe below. */
  private def errors(body: String): List[String] = compiled.errors(body)

  /** `source` with its capture sets erased, as code written without capture checking. */
  private def erased(source: String): String =
    source
      .replaceAll("(Toolbox|TurnTooling|Breached)\\[\\{[^}]*\\}\\]", "$1[?]")
      .replaceAll("\\^\\{[^}]*\\}", "")
      .replace("^", "")

  /** The planted breach: [[TurnTooling]]'s shape, its hosted tools allowed to capture. */
  private val breach =
    """final case class Breached[C^](tools: Toolbox[C], operator: Toolbox[C], hosted: Vector[Tool.Offered^], jot: Jot^, budget: TurnLoop.Rounds)
      |""".stripMargin

  /** A toolbox that edits through `e`, handed to `tooling` as the tools of a turn whose own
    * tools act through `ws` alone.
    */
  private val editing =
    """def offered(ws: Workspace^, e: Edits^, box: Toolbox[{ws, e}], j: Jot^, b: TurnLoop.Rounds): TurnTooling[{ws}]^{j} =
      |  TurnTooling(box, Toolbox.Empty, Vector.empty, j, b)
      |""".stripMargin

  /** A tool that edits through `e`, handed to `tooling` as a hosted tool: one the edge runs,
    * which must act through nothing here.
    */
  private def smuggled(tooling: String) =
    s"""def offered(e: Edits^, j: Jot^, b: TurnLoop.Rounds): Option[$tooling[{}]^{j, e}] =
      |  Toolbox.of().toOption.map(box => $tooling(box, Toolbox.Empty, Vector(writes(e)), j, b))
      |""".stripMargin

  /** A principals store over `store`, handed to [[TurnRecord.Reads]], the stores a turn's
    * requests are rebuilt from: one that reads through the workspace `ws` when `store` is
    * `"spying"`, else one that reads nothing.
    */
  private def reads(store: String) =
    s"""def rebuildFrom(ws: Workspace^, rs: TurnRecord.Reads): TurnRecord.Reads^{ws} = {
      |  val none = Left(grit.core.store.StoreError.Invalid("none"))
      |  val pure = new grit.core.store.Principals {
      |    def name(account: grit.core.identity.Account, name: String)(using grit.core.store.Tx^) = Right(())
      |    def speakers(entries: Vector[grit.core.id.EntryId])(using grit.core.store.Tx^) = none
      |  }
      |  val spying = new grit.core.store.Principals {
      |    def name(account: grit.core.identity.Account, name: String)(using grit.core.store.Tx^) = Right(())
      |    def speakers(entries: Vector[grit.core.id.EntryId])(using grit.core.store.Tx^) = {
      |      val _ = RelPath.of("x").flatMap(ws.read(_, Lines.All))
      |      none
      |    }
      |  }
      |  rs.copy(principals = $store)
      |}
      |""".stripMargin

  /** extract over an answer that asks a model through `m`, typed as `tpe`: the tool carries
    * what its answer captures, so it is no pure tool.
    */
  private def extracting(tpe: String) =
    s"""def asking(m: Models^): $tpe =
      |  Extract.tool(None, (_, _, _, _) => { val _ = m.catalog(); Left("unasked") })
      |""".stripMargin

  private def rejected(errs: List[String]): Boolean =
    errs.exists(e => e.contains("Found:") && e.contains("Required:"))

  private val reading =
    """def reading(ws: Workspace^, e: Edits^, j: Jot^, b: TurnLoop.Rounds): Option[TurnTooling[{ws, e}]^{j}] =
      |  Toolbox.of[{ws, e}](reads(ws), writes(e)).toOption.map(box => TurnTooling[{ws, e}](box, Toolbox.Empty, Vector(spec.hosted), j, b))
      |""".stripMargin

  private lazy val compiled = Probes.batch(
    prelude,
    List(
      reading,
      editing,
      smuggled("TurnTooling"),
      breach + smuggled("Breached"),
      reads("pure"),
      reads("spying"),
      extracting("Tool.Offered^{m}"),
      extracting("Tool.Offered")
    )
  )

  /** What a turn's rounds see, inside turn's package, where they are visible. */
  private val roundsPrelude =
    """package grit.turn.probe
      |import grit.core.durable.*
      |import grit.core.tool.*
      |import grit.turn.*
      |""".stripMargin

  /** A round's step, passing on `kept`, of the type `tpe`, as round state. */
  private def roundKept(tpe: String, kept: String) =
    s"""def kept(using Journaled[(TurnRound.Rounds, $tpe)]): Step[TurnRound.Env, TurnRound.Rounds, (TurnRound.Rounds, $tpe)] =
      |  Step[TurnRound.Env, TurnRound.Rounds, (TurnRound.Rounds, $tpe)](StepName("kept"))(r => at => (at, $kept))
      |""".stripMargin

  private val keptSettling = roundKept("TurnTools.Settling^", "r.settling")
  private val keptNow = roundKept("java.time.Instant", "r.now()")

  /** A turn's step of its own, passing on `kept`, of the type `tpe`, on the wire. */
  private def stepKept(tpe: String, kept: String) =
    s"""def held(using Journaled[(grit.core.id.TurnRef, $tpe)]): Step[TurnSteps.Env, grit.core.id.TurnRef, (grit.core.id.TurnRef, $tpe)] =
      |  Step[TurnSteps.Env, grit.core.id.TurnRef, (grit.core.id.TurnRef, $tpe)](StepName("held"))(r => t => (t, $kept))
      |""".stripMargin

  private val keptRounds = stepKept("TurnRound.Env^", "r.rounds")
  private val keptEnv = stepKept("TurnEnv^", "r.env")
  private val keptWeighed = stepKept("Option[TurnWeighing.Weighed]", "r.weigh(t)")

  private lazy val rounds =
    Probes.batch(roundsPrelude, List(keptSettling, keptNow, keptRounds, keptEnv, keptWeighed))

  /** The breaches, their capture sets erased, compiled with capture checking off. */
  private lazy val unchecked = Probes.batch(
    erased(prelude),
    List(reads("spying"), editing, smuggled("TurnTooling"), extracting("Tool.Offered")).map(erased),
    options.filterNot(_.startsWith("-language:experimental."))
  )

  val tests = Tests {
    test("the probe environment is set") {
      assert(classpath.nonEmpty, options.contains("-language:experimental.captureChecking"))
    }

    test("one compile answers every probe, with capture checking on and with it off") {
      (compiled.runs, unchecked.runs) ==> (1, 1)
    }

    test("a turn offered tools through what it names, and pure hosted tools, compiles") {
      val errs = errors(reading)
      assert(errs.isEmpty)
    }

    test("a turn whose tools act through its workspace alone is not handed a tool that edits") {
      assert(rejected(errors(editing)))
    }

    test("a hosted tool that acts through a capability here is rejected") {
      assert(rejected(errors(smuggled("TurnTooling"))))
    }

    test("a tooling whose hosted tools may capture takes that tool") {
      // The breach the test above guards against, planted in a fixture: watched making that
      // test fail, kept so the rejection is known to come from the hosted list's type alone.
      val errs = errors(breach + smuggled("Breached"))
      errs ==> List()
    }

    test("the stores a turn's requests are rebuilt from may not act through a capability") {
      errors(reads("pure")) ==> List()
      assert(rejected(errors(reads("spying"))))
    }

    test("extract's tool carries what its answer captures: a model's caller is no pure tool") {
      errors(extracting("Tool.Offered^{m}")) ==> List()
      assert(rejected(errors(extracting("Tool.Offered"))))
      unchecked.errors(erased(extracting("Tool.Offered"))) ==> List()
    }

    test("a round's state holds what its env's bodies found, never a capability they act through") {
      rounds.errors(keptNow) ==> List()
      assert(rejected(rounds.errors(keptSettling)))
    }

    test("what crosses a turn's steps holds what their bodies found, never what they act through") {
      rounds.errors(keptWeighed) ==> List()
      assert(rejected(rounds.errors(keptRounds)), rejected(rounds.errors(keptEnv)))
    }

    test("capture checking is what rejects a store that acts through a capability") {
      unchecked.errors(erased(reads("spying"))) ==> List()
    }

    test("capture checking is what rejects the tool that edits") {
      val errs = Vector(editing, smuggled("TurnTooling")).flatMap(s => unchecked.errors(erased(s)))
      errs ==> Vector()
    }
  }
}
