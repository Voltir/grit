package grit.eval.harness.reply

import grit.core.context.Width
import grit.core.document.DocWeight
import grit.core.id.TurnRef
import grit.core.message.Tokens
import grit.dbos.engine.LiveEngine
import grit.dbos.internal.{Reader, Scratch}
import grit.dbos.sql.TestPostgres
import grit.eval.harness.label.Locator
import grit.eval.{Case, Layout, Load, Variant}

import utest.*

/** grit.eval's cases as a reference, over a database they were loaded into together, or each
  * apart in its own as a run draws them: `invoice` holds its answer in another
  * conversation of its own; `decoy` holds the same words in one of its own and labels nothing;
  * `closed` holds them in a period of its own since closed, labelled `[never]`.
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

  private val closed = parsed(
    "closed",
    """query: invoice fix timezone TZ UTC
      |place fs:/home/api closed
      |turn
      |you: The invoice test is flaky, only in CI.
      |grit: Pin TZ=UTC in the test JVM. [never]
      |ask
      |you: What fix did I settle on for the flaky invoice test?
      |""".stripMargin
  )

  /** Every variant of the three cases loaded into a database of their own, and the turns each asks
    * in, by `{case}/{variant}`.
    */
  private lazy val built: (grit.dbos.sql.DbConfig, Map[String, TurnRef]) = {
    val config = TestPostgres.freshDatabase("synthetic")
    val engine = LiveEngine.open(config, "eval")
    try {
      val turns = for {
        c <- Vector(invoice, decoy, closed)
        v <- Variant.values.toVector
      } yield s"${c.name}/${v.label}" -> right(
        Layout
          .of(c, v)
          .flatMap(
            Load.into(
              engine.jot,
              engine.conversations,
              engine.entries,
              engine.periods,
              engine.keeper(Load.Plugin, Load.terms(DocWeight.Unscaled))
            )
          )
          .map(_.turn)
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
    test(
      "a build writes every case in each variant, and names those labelling no [must] or [never]"
    ) {
      val engine = LiveEngine.open(TestPostgres.freshDatabase("synthetic_build"), "eval")
      val built =
        try
          Synthetic.build(
            Synthetic.laid(Vector(invoice, decoy, closed)),
            engine.jot,
            engine.conversations,
            engine.entries,
            engine.periods,
            engine.documents,
            engine.keeper
          )
        finally engine.close()
      // invoice: 3 + 2 plain, 83 + 22 buried; decoy and closed each: 1 + 2 plain, 1 + 22 buried.
      built ==> Right(Synthetic.Built(3, 6, 162, Vector("decoy")))
    }

    test("a case is found as loaded, and one labelling no [must] or [never] is left out by name") {
      withReader { reader =>
        val Synthetic.Found(asked, nothing) =
          right(Synthetic.found(reader, Synthetic.laid(Vector(invoice, decoy))))
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

    test("a case's [never] lines are found as omits of their conversation's seqs") {
      withReader { reader =>
        val asked = right(Synthetic.found(reader, Synthetic.laid(Vector(closed)))).asked
        asked.map(a =>
          a.name -> a.expected.map {
            case Expect.Omits(Locator.Messages(c, seqs)) => (c != a.turn.conversationId, seqs)
            case other => other
          }
        ) ==> Vector(
          "closed/plain" -> List((true, List(grit.core.id.EntrySeq(1)))),
          "closed/buried" -> List((true, List(grit.core.id.EntrySeq(1))))
        )
      }
    }

    test("a case not in the database is refused, naming it") {
      withReader { reader =>
        val missing = parsed("missing", "query: q\nturn\nyou: a [must]\nask\nyou: b?\n")
        Synthetic.found(reader, Synthetic.laid(Vector(missing))).left.map(_.takeWhile(_ != ':')) ==>
          Left("missing/plain")
      }
    }

    test(
      "each case is drawn apart, in a database of its own: no other case is there, its window " +
        "shows only its own conversations, and the database is dropped after"
    ) {
      val beside = TestPostgres.freshDatabase("synthetic_beside")
      val prefix = s"synthetic_apart_${ProcessHandle.current().pid()}"
      val laid = Synthetic.laid(Vector(invoice, decoy))
      val drawn = right(Synthetic.apart(beside, prefix, laid) { (db, one) =>
        val engine = LiveEngine.open(db, "eval")
        val built =
          try
            Synthetic.build(
              Vector(one),
              engine.jot,
              engine.conversations,
              engine.entries,
              engine.periods,
              engine.documents,
              engine.keeper
            )
          finally engine.close()
        built.map { _ =>
          val reader = Reader.open(db)
          try {
            val others =
              laid.filterNot(_ == one).filter(o => Synthetic.found(reader, Vector(o)).isRight)
            val strays = right(Synthetic.found(reader, Vector(one))).asked.flatMap { a =>
              val own = Set(a.turn.conversationId) ++ a.expected.collect {
                case Expect.Holds(Locator.Messages(c, _)) => c
              }
              val shown =
                right(Synthetic.window(reader, a, Assembled.Shipped, Width.Deployed)).parts
                  .map(_.conversation)
                  .toSet
              assert(shown.nonEmpty)
              shown -- own
            }
            (s"${one._1.name}/${one._2.label}", others.size, strays.size)
          } finally reader.close()
        }
      })
      drawn ==> Vector(
        ("invoice/plain", 0, 0),
        ("invoice/buried", 0, 0),
        ("decoy/plain", 0, 0),
        ("decoy/buried", 0, 0)
      )
      // Made again under the first's name: the run dropped it.
      (Scratch.create(beside, s"${prefix}_0") match {
        case Right(again) => again.drop()
        case Left(why) => Left(why)
      }) ==> Right(())
    }

    test("a case is judged by its window at a width: held when deployed, missed when narrow") {
      withReader { reader =>
        val asked = right(Synthetic.found(reader, Synthetic.laid(Vector(invoice)))).asked
        def judged(width: Width) = asked.map(a =>
          a.name -> Reference.judge(
            a.expected.toVector,
            Synthetic.window(reader, a, Assembled.Shipped, width).toOption,
            None
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
