package grit.eval

/** Plausible developer chatter for burying a case's turns, so recency stops finding its
  * labelled entries by accident. Heavy in common words and in the cases' own vocabulary
  * (database, test, run, model, Postgres, turn), so the labelled entries compete with
  * distractors. Ported from the pg_textsearch spike (`recall.py`, its default filler).
  */
object Filler {

  /** `n` filler turns, a question and its answer each; the same `seed` gives the same turns. */
  def turns(seed: Long, n: Int): Vector[Vector[Case.Line]] = {
    val random = new scala.util.Random(seed)
    def pick(from: Vector[String]): String = from(random.nextInt(from.size))
    Vector.fill(n) {
      val subject = pick(Subjects)
      val verb = pick(Verbs)
      def fill(template: String) = template.replace("{s}", subject).replace("{v}", verb)
      Vector(
        Case.Line(you = true, fill(pick(Questions)), must = false),
        Case.Line(you = false, fill(pick(Answers)), must = false)
      )
    }
  }

  private val Subjects: Vector[String] = Vector(
    "the build",
    "the test run",
    "the database",
    "the model",
    "the chat screen",
    "the turn",
    "the Postgres container",
    "the status bar",
    "the provider",
    "the entry store",
    "the assembler",
    "the migration",
    "the probe",
    "the key",
    "the local run",
    "the docker compose file",
    "the scala compiler",
    "the formatter",
    "the workflow",
    "the transcript"
  )

  private val Verbs: Vector[String] = Vector(
    "is failing again",
    "looks fine now",
    "was slow this morning",
    "needs a restart",
    "should be checked before we merge",
    "printed a warning",
    "timed out twice",
    "is using too much memory",
    "has a new setting",
    "works on my machine",
    "broke after the upgrade",
    "is green",
    "hangs when I resize the terminal",
    "costs more than I expected",
    "logs a lot of noise"
  )

  private val Questions: Vector[String] = Vector(
    "Can you look at why {s} {v}?",
    "Is it normal that {s} {v}?",
    "What should we do now that {s} {v}?",
    "Any idea why {s} {v}?",
    "Do you know if {s} {v} for everyone?"
  )

  private val Answers: Vector[String] = Vector(
    "I think {s} {v} because of the last change; I will run it again and see.",
    "Yes, {s} {v}. It is probably the cache, so a clean build should fix it.",
    "Not sure. {s} {v} only sometimes, which makes it hard to test.",
    "Let me check the logs. If {s} {v} again we can open an issue for it.",
    "That is expected: {s} {v} until the next run finishes."
  )
}
