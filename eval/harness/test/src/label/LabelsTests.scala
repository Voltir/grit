package grit.eval.harness.label

import grit.core.triage.Kind
import grit.eval.harness.corpus.CaseId

import utest.*

/** `labels.json` as the labelling tool writes it. Every id here is synthetic. */
object LabelsTests extends TestSuite {

  private def id(s: String): CaseId = CaseId.read(s).fold(sys.error, identity)

  // The form of eval-label.py's write_labels: json.dump of {"cases": {case: {field: {value,
  // guide, at}}}}, yes/no fields as JSON booleans. Pinned: the tool and this reader share it.
  private val file =
    """{
      |  "cases": {
      |    "C1/1727000000.000100": {
      |      "context": {"at": "2026-10-02T00:00:00Z", "guide": "v1", "value": "short"},
      |      "durable": {"at": "2026-10-02T00:00:00Z", "guide": "v1", "value": true},
      |      "fact": {"at": "2026-10-02T00:00:00Z", "guide": "v1", "value": "none"},
      |      "helps": {"at": "2026-10-02T00:00:00Z", "guide": "v1", "value": false},
      |      "kind": {"at": "2026-10-02T00:00:00Z", "guide": "v1", "value": "decision"},
      |      "place": {"at": "2026-10-02T00:00:00Z", "guide": "v1", "value": "C1/1727000000.000050"},
      |      "waiting": {"at": "2026-10-02T00:00:00Z", "guide": "v2", "value": false}
      |    },
      |    "C1/1727000000.000200": {
      |      "place": {"at": "2026-10-02T00:00:00Z", "guide": "v2", "value": "begins"},
      |      "context": {"at": "2026-10-02T00:00:00Z", "guide": "v2", "value": "ok"}
      |    }
      |  }
      |}""".stripMargin

  val tests = Tests {
    test("every field the tool writes is read, and the guides they were made under") {
      Labels.read(file) ==> Right(
        Labels(
          Map(
            id("C1/1727000000.000100") -> Labelled(
              Some(Kind.Decision),
              Some(false),
              Some(true),
              Some(false),
              Some(Context.Short),
              Some(false),
              Some(Place.Follows(id("C1/1727000000.000050")))
            ),
            id("C1/1727000000.000200") -> Labelled.Blank.copy(
              context = Some(Context.Ok),
              place = Some(Place.Begins)
            )
          ),
          Set("v1", "v2")
        )
      )
    }

    test("a value not of its field's form, or a field the tool never writes, is refused") {
      def one(field: String, value: String) =
        Labels.read(
          s"""{"cases": {"C1/1": {"$field": {"value": $value, "guide": "v1", "at": "x"}}}}"""
        )
      one("kind", "\"musing\"") ==> Left("labels.json: C1/1: kind is not of its form")
      one("durable", "\"yes\"") ==> Left("labels.json: C1/1: durable is not of its form")
      one("note", "\"words\"") ==> Left("labels.json: C1/1: note is not of its form")
    }
  }
}
