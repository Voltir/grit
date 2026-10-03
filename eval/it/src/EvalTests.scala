package grit.eval

import grit.core.id.EntryId
import grit.core.message.Tokens
import grit.dbos.engine.{Engine, LiveEngine}
import grit.dbos.sql.TestPostgres

import utest.*

/** A case written into the eval's database, and the score. The scores themselves are the
  * report's business, not a test's.
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
    val engine = LiveEngine.open(config, "eval")
    try body(engine)
    finally engine.close()
  }

  val tests = Tests {
    test(
      "a loaded cross-place case: its others' periods as written, rooted under the case, and scored"
    ) {
      withEngine { engine =>
        val c = Case
          .parse(
            "near",
            """query: invoice
              |place fs:/home/api
              |turn
              |you: invoice fix is TZ [must]
              |place fs:/home/web closed
              |turn
              |you: invoice elsewhere [never]
              |ask
              |you: which invoice fix?
              |""".stripMargin
          )
          .fold(sys.error, identity)
        val loaded = Eval.load(engine, c, Variant.Plain)
        loaded.must.map(EntryId.value) ==> Vector("near/plain/p0/t0:0")
        loaded.never.map(EntryId.value) ==> Vector("near/plain/p1/t0:0")
        loaded.scope.written ==> "fs:/eval/near/plain task:eval/near/plain"
        val open = grit.dbos.sql.LiveDb
          .transaction(config)(engine.periods.openElsewhere(loaded.turn.conversationId))
        open.map(_.map(_.place.written).filter(_.startsWith("fs:/eval/near/"))) ==>
          Right(Vector("fs:/eval/near/plain/home/api"))
        val score = Eval.run(
          engine,
          loaded,
          Eval.Strategy.Retrieval(Tokens(24_000), live = false),
          Eval.NoLive
        )
        score.map(s => (s.got, s.of, s.strays)) ==> Right((1, 1, 0))
      }
    }

    test("a loaded case: one entry per line, the ask recorded as the turn assembled for") {
      withEngine { engine =>
        val plain = Eval.load(engine, parsed(sample), Variant.Plain)
        plain.must.map(EntryId.value) ==> Vector("sample/plain/t0:1", "sample/plain/t1:2")
        plain.messages.size ==> 4
        plain.turn.turnSeq.toString ==> "2"

        val buried = Eval.load(engine, parsed(sample), Variant.Buried)
        // Each of the two turns is followed by its filler turns, two entries each.
        buried.messages.size ==> 4 + 2 * 2 * Variant.FillerPerGap
        // The labelled entries keep their turns' places: the second after the first's filler.
        val second = 1 + Variant.FillerPerGap
        buried.must.map(EntryId.value) ==>
          Vector("sample/buried/t0:1", s"sample/buried/t$second:${2 * second}")
        buried.turn.turnSeq.toString ==> (2 + 2 * Variant.FillerPerGap).toString
      }
    }

    test("the score counts labelled entries held, the window's tokens, and what was missed") {
      withEngine { engine =>
        // Its own name: a case's ids are unique in the database.
        val renamed = Case.parse("sample2", sample).fold(sys.error, identity)
        val loaded = Eval.load(engine, renamed, Variant.Plain)
        val held = Vector(EntryId("sample2/plain/t0:0"), EntryId("sample2/plain/t0:1"))
        // "one" is 1 + 4 tokens, "two" 1 + 4 (CharEstimate).
        Eval.score(loaded, held) ==>
          Eval.Score(1, 2, Tokens(10), Vector(EntryId("sample2/plain/t1:2")), Vector.empty)
        Eval.run(engine, loaded, Eval.Strategy.Oracle, Eval.NoLive).map(_.got) ==> Right(2)
      }
    }

    test("every case parses, has a query, and linear sees it all at 24k as written") {
      withEngine { engine =>
        Cases.all.foreach { (name, text) =>
          val c = Case.parse(name, text).fold(e => sys.error(e), identity)
          assert(c.turns.flatten.exists(_.must), c.query.nonEmpty)
          val loaded = Eval.load(engine, c, Variant.Plain)
          val all = Eval.run(engine, loaded, Eval.Strategy.Linear(Tokens(24_000)), Eval.NoLive)
          assert(all.exists(s => s.got == s.of))
        }
      }
    }
  }
}
