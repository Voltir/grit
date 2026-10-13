package grit.core.classify

import grit.core.durable.Probes

import utest.*

/** What the compiler rejects about the deployment's classifier behind its clearance: one made
  * outside core, which could ask without the check (a typer error), and its classifier carried
  * out of the routed read that chose it (a capture error). Compiled against core.
  */
object ClearedClassifierCaptureTests extends TestSuite {

  private val prelude =
    """package outside
      |import grit.core.classify.*
      |import grit.core.route.{Routed, Uncleared}
      |import grit.core.store.*
      |import grit.core.visibility.*
      |final case class Note(text: String)
      |given StateJson[Note] = StateJson.instance(n => ujson.Obj("note" -> n.text))
      |val urgent = Ask.yesNo[Note]("Is `note` urgent?", None, None)
      |""".stripMargin

  private val site =
    """def site(db: Db^, s: Subject, classifier: ClearedClassifier^): Either[StoreError, Boolean] =
      |  Routed
      |    .reading(db, s)((r, reads) =>
      |      reads.read(Right(Note("x"))).map(n => classifier.asking(r).ask(n, urgent).isRight)
      |    )
      |    .flatten""".stripMargin
  private val extended =
    """def f: ClearedClassifier = new ClearedClassifier {
      |  def upstream: Option[String] = None
      |  def at(routed: Routed): Either[Uncleared, Classifier^] = Right(Classifier.none("unchecked"))
      |  protected def unavailable(u: Uncleared): Classifier = Classifier.none("unchecked")
      |}""".stripMargin
  private val carried =
    """def f(db: Db^, s: Subject, classifier: ClearedClassifier^): Unit = {
      |  val _ = Routed.reading(db, s)((r, _) => classifier.at(r))
      |}""".stripMargin

  private lazy val compiled = Probes.batch(prelude, List(site, extended, carried))

  private def probe(body: String): List[String] = compiled.errors(body)

  val tests = Tests {
    test("control: a site asks the classifier its routed read chose, inside that read") {
      probe(site) ==> Nil
    }

    test("a classifier behind its clearance is not made outside core") {
      probe(extended).map(_.linesIterator.next()) ==> List(
        "Cannot extend sealed trait ClearedClassifier in a different source file"
      )
    }

    test("the classifier a routed read chose is not carried out of that read") {
      probe(carried).exists(e =>
        e.contains("Classifier^") && e.contains("outlives its scope")
      ) ==> true
    }
  }
}
