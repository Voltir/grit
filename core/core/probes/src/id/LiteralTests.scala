package grit.core.id

import grit.core.document.{DocLabel, DocText}
import grit.core.durable.Probes
import grit.core.durable.Probes.flowsInto
import grit.core.place.Service
import grit.core.review.ReviewWord
import grit.core.visibility.GroupName

import utest.*

/** What the compiler refuses as a core type's `literal`: a constant its `of` refuses, with that
  * reason, and text that is not a constant; and that a literal's reader is pure, and the
  * constructors `literal` builds through stay private. Pinned by compiling probe sources
  * against core.
  */
object LiteralTests extends TestSuite {

  private val prelude =
    """package probe
      |import scala.quoted.{Expr, Quotes}
      |import grit.core.act.MoveName
      |import grit.core.document.{DocLabel, DocText}
      |import grit.core.id.{DocKey, Harness, JobName, PluginName, QuestionName}
      |import grit.core.place.Service
      |import grit.core.review.ReviewWord
      |import grit.core.visibility.GroupName
      |import grit.std.literal.Literal
      |""".stripMargin

  private val accepted =
    s"""val key: DocKey = DocKey.literal("topic:aside")
       |val text: DocText = DocText.literal("…")
       |val job: JobName = JobName.literal("knowledge-sync")
       |val question: QuestionName = QuestionName.literal("durable")
       |val plugin: PluginName = PluginName.literal("digest")
       |val group: GroupName = GroupName.literal("administrators")
       |val harness: Harness = Harness.literal("claude-code")
       |val label: DocLabel = DocLabel.literal("what a conversation settled")
       |val service: Service = Service.literal("slack")
       |val word: ReviewWord = ReviewWord.literal("merge")
       |val move: MoveName = MoveName.literal("${"a" * 200}")
       |""".stripMargin

  /** Each type's refused constant, its probe, and the reason its `of` gives. */
  private val refused: List[(String, String)] = List(
    """val n: DocKey = DocKey.literal("")""" -> DocKey.of("").swap.getOrElse(""),
    """val n: DocText = DocText.literal(" ")""" -> DocText.of(" ").swap.getOrElse(""),
    """val n: JobName = JobName.literal("Sync")""" -> JobName.of("Sync").swap.getOrElse(""),
    """val n: QuestionName = QuestionName.literal("Durable")""" ->
      QuestionName.of("Durable").swap.getOrElse(""),
    """val n: PluginName = PluginName.literal("Digest")""" ->
      PluginName.of("Digest").swap.getOrElse(""),
    """val n: GroupName = GroupName.literal("  ")""" -> GroupName.of("  ").swap.getOrElse(""),
    """val n: Harness = Harness.literal("Claude")""" -> Harness.of("Claude").swap.getOrElse(""),
    """val n: DocLabel = DocLabel.literal("a\nb")""" -> DocLabel.of("a\nb").swap.getOrElse(""),
    """val n: Service = Service.literal("Slack")""" -> Service.of("Slack").swap.getOrElse(""),
    """val n: ReviewWord = ReviewWord.literal("Merge")""" ->
      ReviewWord.of("Merge").swap.getOrElse(""),
    """val n: MoveName = MoveName.literal("")""" -> "a move's name is not empty",
    s"""val n: MoveName = MoveName.literal("${"a" * 201}")""" ->
      "a move's name is at most 200 characters",
    """val n: MoveName = MoveName.literal("a:record")""" ->
      "a move's name holds no ':' and no control character",
    """val n: MoveName = MoveName.literal("a\tb")""" ->
      "a move's name holds no ':' and no control character"
  )

  private val built = """def n(s: String): DocKey = DocKey.literal(s)"""

  private val impureReader =
    """def checked(text: Expr[String], c: caps.SharedCapability^)(using Quotes): Expr[String] =
      |  Literal.checked(text, s => { val held = c; Right(s) })
      |""".stripMargin

  private val newService = """val s: Service = new Service("slack")"""

  private val newWord = """val w: ReviewWord = new ReviewWord("merge")"""

  private lazy val compiled =
    Probes.batch(
      prelude,
      accepted :: built :: impureReader :: newService :: newWord :: refused.map(_._1)
    )

  val tests = Tests {
    test("control: a constant each type's of accepts compiles") {
      compiled.errors(accepted) ==> Nil
    }

    test("a constant a type's of refuses does not compile, with of's reason") {
      refused.map((probe, _) => compiled.errors(probe)) ==> refused.map((_, why) => List(why))
    }

    test("text that is not a constant does not compile") {
      compiled.errors(built) ==> List("a literal is a string constant")
    }

    test("a literal's reader may hold no capability") {
      assert(flowsInto("{}")(compiled.errors(impureReader)))
    }

    test("the constructors a literal builds through stay private in source") {
      // Each message goes on to name the probe's own object, which the batch numbers.
      val refusals =
        List(newService, newWord).map(compiled.errors(_).map(e => e.take(e.indexOf(" from "))))
      refusals ==> List(
        List("constructor Service cannot be accessed as a member of grit.core.place.Service"),
        List("constructor ReviewWord cannot be accessed as a member of grit.core.review.ReviewWord")
      )
    }
  }
}
