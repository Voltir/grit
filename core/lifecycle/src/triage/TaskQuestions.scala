package grit.lifecycle.triage

import grit.core.allot.{Features, TaskKind}
import grit.core.classify.{Ask, Criterion}

/** The task questions, asked together in one call about [[TriageQuestion.State]]'s
  * `new_message`, `author` and `thread`, with no pool: `kind`, a choice of [[TaskKind]]'s keys,
  * and `hard`, a yes/no, in the words they were calibrated in.
  */
object TaskQuestions {

  /** What the state's fields are, before each question. */
  private val Lead =
    "Read new_message, which author sent to an AI assistant; thread is what came before it. "

  /** Both questions, read into [[Features]] ([[Features.from]]). */
  val Shipped: Ask[TriageQuestion.State, Features] = {
    def kind(k: TaskKind, description: String) = Criterion(k, k.word, Some(description))
    val kinds = Ask.choice[TriageQuestion.State, TaskKind](
      Lead + "What does new_message ask the assistant to do? Analysis computes over structured " +
        "data; investigation reads and reconciles prose and history.",
      kind(TaskKind.Chat, "nothing to do: a greeting, thanks, an acknowledgement or small talk"),
      kind(TaskKind.Lookup, "find or recall one fact, link, name or document"),
      kind(
        TaskKind.Investigation,
        "find out why something happened or what is going on by reading threads, docs, notes, " +
          "logs or tickets"
      ),
      kind(
        TaskKind.Analysis,
        "compute over structured data such as tables, metrics, a database or a spreadsheet: " +
          "query, count, compare or chart"
      ),
      kind(TaskKind.Writing, "draft, rewrite, translate or summarise text"),
      kind(TaskKind.Coding, "write, fix, review or explain code"),
      kind(TaskKind.Decision, "weigh options and recommend or choose one")
    )
    val hard = Lead + "Is what new_message asks hard: work a careful expert would do in several " +
      "steps, or by reading several sources, rather than with one quick answer? A greeting, " +
      "thanks or a single fact is not hard."
    // Its keys are TaskKind's distinct words, so no Left is taken; TaskQuestionsTests builds it.
    kinds
      .fold(d => throw new IllegalStateException(s"kind repeats ${d.key}"), identity)
      .zip(Ask.yesNo[TriageQuestion.State](hard, None, None))
      .map((decision, p) => Features.from(decision, p))
  }
}
