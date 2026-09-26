package grit.eval

import grit.core.context.Window
import grit.core.id.EntryId
import grit.core.message.Tokens
import grit.dbos.engine.Engine
import grit.dbos.sql.TestPostgres

import utest.*

/** The eval's harness: the case format, the filler, the database a case is written into,
  * and the score. The scores themselves are the report's business, not a test's.
  */
object EvalTests extends TestSuite {

  private val sample =
    """# what it tests
      |query: one two
      |turn
      |you: one
      |grit: two [must]
      |turn
      |you: three [must]
      |ask
      |you: four?
      |""".stripMargin

  private def parsed(text: String): Case = Case.parse("sample", text).fold(sys.error, identity)

  private lazy val config = TestPostgres.freshDatabase("eval_tests")

  /** An engine on the eval tests' database, closed after `body`. */
  private def withEngine[A](body: Engine^ => A): A = {
    val engine = Engine.open(config, "eval")
    try body(engine)
    finally engine.close()
  }

  val tests = Tests {
    test("a case: its turns, its labels, its ask and its query") {
      parsed(sample) ==> Case(
        "sample",
        "what it tests",
        Vector(
          Vector(Case.Line(true, "one", false), Case.Line(false, "two", true)),
          Vector(Case.Line(true, "three", true))
        ),
        "four?",
        Some("one two")
      )
    }

    test("a malformed case is named by line") {
      Case.parse("x", "you: early\n") ==> Left("x line 1: a message before the first turn")
      Case.parse("x", "turn\nme: hi\n") ==>
        Left("x line 2: expected turn, ask, query:, you: or grit:")
      Case.parse("x", "turn\nask\nturn\n") ==> Left("x line 3: a turn after the ask")
      Case.parse("x", "query: a\nquery: b\n") ==> Left("x line 2: a second query")
      Case.parse("x", "turn\nyou: a\nask\n") ==> Left("x: the ask must be one unlabelled you: line")
    }

    test("filler is the same for the same seed, and a question and an answer per turn") {
      Filler.turns(7, 5) ==> Filler.turns(7, 5)
      assert(Filler.turns(7, 5) != Filler.turns(8, 5))
      assert(Filler.turns(7, 5).forall(t => t.map(_.you) == Vector(true, false)))
    }

    test("a loaded case: one entry per line, the ask recorded as the turn assembled for") {
      withEngine { engine =>
        val plain = Eval.load(engine, config, parsed(sample), Eval.Variant.Plain)
        plain.must.map(EntryId.value) ==> Vector("sample/plain/t0:1", "sample/plain/t1:2")
        plain.messages.size ==> 4
        plain.turn.turnSeq.toString ==> "2"

        val buried = Eval.load(engine, config, parsed(sample), Eval.Variant.Buried)
        // Each of the two turns is followed by its filler turns, two entries each.
        buried.messages.size ==> 4 + 2 * 2 * Eval.FillerPerGap
        // The labelled entries keep their turns' places: the second after the first's filler.
        val second = 1 + Eval.FillerPerGap
        buried.must.map(EntryId.value) ==>
          Vector("sample/buried/t0:1", s"sample/buried/t$second:${2 * second}")
        buried.turn.turnSeq.toString ==> (2 + 2 * Eval.FillerPerGap).toString
      }
    }

    test("the score counts labelled entries held, the window's tokens, and what was missed") {
      withEngine { engine =>
        // Its own name: a case's ids are unique in the database.
        val renamed = Case.parse("sample2", sample).fold(sys.error, identity)
        val loaded = Eval.load(engine, config, renamed, Eval.Variant.Plain)
        val held = Vector(EntryId("sample2/plain/t0:0"), EntryId("sample2/plain/t0:1"))
        // "one" is 1 + 4 tokens, "two" 1 + 4 (CharEstimate).
        Eval.score(loaded, Window(held)) ==>
          Eval.Score(1, 2, Tokens(10), Vector(EntryId("sample2/plain/t1:2")), Vector.empty)
        Eval.run(engine, loaded, Eval.Strategy.Oracle, Eval.NoLive).map(_.got) ==> Right(2)
      }
    }

    test("every case parses, has a query, and linear sees it all at 24k as written") {
      withEngine { engine =>
        Cases.all.foreach { (name, text) =>
          val c = Case.parse(name, text).fold(e => sys.error(e), identity)
          assert(c.turns.flatten.exists(_.must), c.query.nonEmpty)
          val loaded = Eval.load(engine, config, c, Eval.Variant.Plain)
          val all = Eval.run(engine, loaded, Eval.Strategy.Linear(Tokens(24_000)), Eval.NoLive)
          assert(all.exists(s => s.got == s.of))
        }
      }
    }
  }
}
