package grit.knowledge.topic

import grit.core.durable.Probes
import grit.core.durable.Probes.{erased, heldImpure, options}

import utest.*

/** What capture checking rejects of a sort ([[grit.core.act.Dreaming.sort]]): its candidates'
  * values are pure (`C <: caps.Pure`), so a job cannot hand a sort, or take back from its
  * answer, a capability such as its scan's transaction. A value not declared pure is refused
  * by the bound; one declared pure but holding a transaction, by capture checking. Pinned by
  * compiling probe sources against knowledge with its own flags; `assertCompileError` cannot
  * see capture errors (docs/capture-checking.md).
  */
object SortCaptureTests extends TestSuite {

  private val prelude =
    """package grit.knowledge.probe
      |import grit.core.act.*
      |import grit.core.classify.{Criterion, Decision}
      |import grit.core.id.DocumentVersionId
      |import grit.core.store.Tx
      |import grit.knowledge.ref.TopicKey
      |""".stripMargin

  /** A choice among topics by key, then `new`: accepted, its candidates pure. */
  private val sortsTopics =
    """enum To extends caps.Pure {
      |  case Topic(key: TopicKey)
      |  case Open
      |  case Aside
      |}
      |def sorting: Either[String, Sorting[To, Decision[To]]] =
      |  Sorting.choice("q", 1, 1, 64, Criterion(To.Open, "new", None), Criterion(To.Aside, "aside", None))
      |def placed(m: Dreaming^, item: Dreaming.Item, pinned: Vector[(To, DocumentVersionId)], pool: Vector[(To, DocumentVersionId)], s: grit.core.flow.Tainted[Sorting[To, Decision[To]]]) =
      |  m.sort(MoveName.literal("sort-0"), item, Sorting.Among(pinned, pool, 64), s)
      |""".stripMargin

  /** A candidate of TopicKey's shape, declared pure, holding the scan's transaction. */
  private val candidateHoldsTx =
    """enum Held extends caps.Pure {
      |  case By(tx: Tx^)
      |}
      |""".stripMargin

  /** A sorting whose candidates are a class not declared pure, holding a transaction. */
  private val impureCandidates =
    """final class Holder(val tx: Tx^)
      |def sorting = Sorting.each[Holder]("q", "y", "n", 1, 1, 8)
      |""".stripMargin

  /** Candidates to sort among of a class not declared pure, holding a transaction. */
  private val impureAmong =
    """final class Holder(val tx: Tx^)
      |def among(h: Holder, v: DocumentVersionId) = Sorting.Among[Holder](Vector(h -> v), Vector(), 0)
      |""".stripMargin

  private lazy val compiled = Probes.batch(prelude, List(sortsTopics, candidateHoldsTx))

  /** Compiled apart: the typer rejects them, which ends a run before capture checking. */
  private lazy val typed = Probes.batch(prelude, List(impureCandidates, impureAmong))

  private val unchecked = options.filterNot(_.startsWith("-language:experimental."))

  val tests = Tests {
    test("one compile answers both capture probes: a sort among topics, a candidate holding a tx") {
      compiled.runs ==> 1
    }

    test("a sort among topics by key is accepted, its candidates pure") {
      compiled.errors(sortsTopics) ==> Nil
    }

    test("a candidate declared pure, holding the scan's transaction, is rejected") {
      assert(heldImpure(compiled.errors(candidateHoldsTx)))
    }

    test("capture checking is what rejects the candidate") {
      assert(Probes.errors(erased(prelude + candidateHoldsTx), unchecked).isEmpty)
    }

    test("a sorting over candidates not declared pure is rejected by its bound") {
      typed.errors(impureCandidates) ==> List(
        "Type argument grit.knowledge.probe.Probe0.Holder does not conform to upper bound scala.caps.Pure"
      )
    }

    test("candidates to sort among not declared pure are rejected by its bound") {
      typed.errors(impureAmong) ==> List(
        "Type argument grit.knowledge.probe.Probe1.Holder does not conform to upper bound scala.caps.Pure"
      )
    }
  }
}
