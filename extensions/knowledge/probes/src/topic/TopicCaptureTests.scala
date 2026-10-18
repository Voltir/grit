package grit.knowledge.topic

import grit.core.durable.Probes
import grit.core.durable.Probes.{erased, heldImpure, options}

import utest.*

/** What capture checking rejects of a topic: it is data, so a keep that makes one cannot hand
  * its transaction out inside it. A note is a [[TopicNote]], a closed enum, so the only way to
  * slip a capability into a topic is a note type declared as TopicNote is, holding one; capture
  * checking rejects that. Pinned by compiling probe sources against knowledge with its own
  * flags; `assertCompileError` cannot see capture errors (docs/capture-checking.md).
  */
object TopicCaptureTests extends TestSuite {

  private val prelude =
    """package grit.knowledge.probe
      |import java.time.LocalDate
      |import grit.core.act.*
      |import grit.core.document.DocumentKeeper
      |import grit.core.store.{StoreError, Tx}
      |import grit.knowledge.ref.TopicKey
      |import grit.knowledge.topic.*
      |import grit.prose.form.{Block, Doc, Span, Text}
      |""".stripMargin

  /** A keep body making a topic from what it read: accepted, the topic a pure value. */
  private val keepMakesTopic =
    """def pure[A <: caps.Pure](a: A): A = a
      |def body(key: TopicKey): (Unit, DocumentKeeper, java.time.Instant) -> Tx^ ?-> Either[StoreError, Topic] =
      |  (_, _, _) => {
      |    val note: TopicNote = TopicNote.Link(key)
      |    val lead: Doc[TopicNote] = Doc(Vector(Block.Paragraph(Text(Vector(Span("x", Set.empty, Vector(note)))))))
      |    Right(pure(Topic.of(lead, Vector(Passage(LocalDate.of(2026, 1, 1), Lineage.of(Vector.empty), lead)))))
      |  }
      |""".stripMargin

  /** A note of TopicNote's shape, declared as TopicNote is, holding the keep's transaction. */
  private val noteHoldsTx =
    """enum Note extends caps.Pure {
      |  case Held(tx: Tx^)
      |}
      |""".stripMargin

  /** A keep body noting its transaction on a topic's lead. */
  private val keepNotesTx =
    """def body: (Unit, DocumentKeeper, java.time.Instant) -> Tx^ ?-> Either[StoreError, Doc[TopicNote]] =
      |  (_, _, _) => {
      |    val tx = summon[Tx^]
      |    Right(Doc(Vector(Block.Paragraph(Text(Vector(Span("x", Set.empty, Vector(tx))))))))
      |  }
      |""".stripMargin

  private lazy val compiled = Probes.batch(prelude, List(keepMakesTopic, noteHoldsTx))

  /** Compiled apart: the typer rejects it, which ends a run before capture checking. */
  private lazy val typed = Probes.batch(prelude, List(keepNotesTx))

  private val unchecked = options.filterNot(_.startsWith("-language:experimental."))

  val tests = Tests {
    test("the probe environment is set") {
      assert(options.contains("-language:experimental.captureChecking"))
    }

    test("one compile answers both capture probes: a keep's topic, a note holding a tx") {
      compiled.runs ==> 1
    }

    test("a keep body making a topic is accepted, the topic pure") {
      compiled.errors(keepMakesTopic) ==> Nil
    }

    test("a note declared as TopicNote is, holding the keep's transaction, is rejected") {
      assert(heldImpure(compiled.errors(noteHoldsTx)))
    }

    test("capture checking is what rejects the note") {
      assert(Probes.errors(erased(prelude + noteHoldsTx), unchecked).isEmpty)
    }

    test("a keep body noting its transaction on a topic's doc is rejected: a tx is no note") {
      typed.errors(keepNotesTx) ==> List(
        "Found:    (tx : grit.core.store.Tx^)\nRequired: grit.knowledge.topic.TopicNote"
      )
    }
  }
}
