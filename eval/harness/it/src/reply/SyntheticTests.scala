package grit.eval.harness.reply

import grit.core.context.Width
import grit.core.id.TurnRef
import grit.core.message.Tokens
import grit.dbos.engine.{LiveEngine, Reader}
import grit.dbos.sql.TestPostgres
import grit.eval.harness.label.Locator
import grit.eval.{Case, Layout, Load, Variant}

import utest.*

/** grit.eval's cases as a reference over a database they were loaded into as
  * `scripts/eval reference-build` loads them: `invoice` holds its answer in another
  * conversation of its own; `decoy` holds the same words in one of its own and labels nothing.
  */
object SyntheticTests extends TestSuite {

  private def right[E, A](e: Either[E, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why.toString), identity)

  private def parsed(name: String, text: String): Case =
    right(Case.parse(name, text))

  private val invoice = parsed(
    "invoice",
    """query: invoice fix timezone TZ UTC
      |turn
      |you: What about the Monday reports?
      |grit: They run in the batch job.
      |place fs:/home/api
      |turn
      |you: The invoice test is flaky again, only in CI.
      |grit: Pin TZ=UTC in the test JVM. [must]
      |ask
      |you: What fix did I settle on for the flaky invoice test?
      |""".stripMargin
  )

  private val decoy = parsed(
    "decoy",
    """query: invoice fix timezone
      |place fs:/home/web
      |turn
      |you: The invoice fix for the timezone, TZ UTC, again?
      |grit: The invoice fix is TZ=UTC, pinned in the test JVM.
      |ask
      |you: Anything else?
      |""".stripMargin
  )

  /** Every variant of both cases loaded into a database of their own, and the turns each asks
    * in, by `{case}/{variant}`.
    */
  private lazy val built: (grit.dbos.sql.DbConfig, Map[String, TurnRef]) = {
    val config = TestPostgres.freshDatabase("synthetic")
    val engine = LiveEngine.open(config, "eval")
    try {
      val turns = for {
        c <- Vector(invoice, decoy)
        v <- Variant.values.toVector
      } yield s"${c.name}/${v.label}" -> right(
        Layout
          .of(c, v)
          .flatMap(Load.into(engine.jot, engine.conversations, engine.entries, engine.periods))
      )
      (config, turns.toMap)
    } finally engine.close()
  }

  private def withReader[A](body: Reader^ => A): A = {
    val reader = Reader.open(built._1)
    try body(reader)
    finally reader.close()
  }

  val tests = Tests {
    test("a case is found as loaded, and one that labels no [must] entry is left out by name") {
      withReader { reader =>
        val (asked, nothing) = right(Synthetic.found(reader, Vector(invoice, decoy)))
        asked.map(a => (a.name, a.turn)) ==> Vector(
          "invoice/plain" -> built._2("invoice/plain"),
          "invoice/buried" -> built._2("invoice/buried")
        )
        asked.flatMap(_.expected.map {
          case Expect.Holds(Locator.Messages(_, seqs)) => seqs
          case other => List(other)
        }) ==> Vector(List(grit.core.id.EntrySeq(1)), List(grit.core.id.EntrySeq(1)))
        nothing ==> Vector("decoy/plain", "decoy/buried")
      }
    }

    test("a case not in the database is refused, naming it") {
      withReader { reader =>
        val missing = parsed("missing", "query: q\nturn\nyou: a [must]\nask\nyou: b?\n")
        Synthetic.found(reader, Vector(missing)).left.map(_.takeWhile(_ != ':')) ==>
          Left("missing/plain")
      }
    }

    test("a case's window is drawn within its own scope: no other case's conversation is in it") {
      withReader { reader =>
        val (asked, _) = right(Synthetic.found(reader, Vector(invoice)))
        asked.foreach { a =>
          val own = Set(a.turn.conversationId) ++ a.expected.collect {
            case Expect.Holds(Locator.Messages(c, _)) => c
          }
          val shown = right(Synthetic.window(reader, a, Assembled.Shipped, Width.Deployed)).parts
            .map(_.conversation)
            .toSet
          assert(shown.nonEmpty)
          (a.name, shown -- own) ==> (a.name, Set.empty)
        }
      }
    }

    test("a case is judged by its window at a width: held when deployed, missed when narrow") {
      withReader { reader =>
        val (asked, _) = right(Synthetic.found(reader, Vector(invoice)))
        def judged(width: Width) = asked.map(a =>
          a.name -> Reference.judge(
            a.expected.toVector,
            Synthetic.window(reader, a, Assembled.Shipped, width).toOption,
            Set.empty
          )
        )
        judged(Width.Deployed) ==>
          Vector("invoice/plain" -> Judged.Pass, "invoice/buried" -> Judged.Pass)
        // No search ranks anything elsewhere, so no section from elsewhere is shown.
        judged(Width.Within(Tokens(2_000), 0)) ==>
          Vector("invoice/plain" -> Judged.Fail, "invoice/buried" -> Judged.Fail)
      }
    }
  }
}
