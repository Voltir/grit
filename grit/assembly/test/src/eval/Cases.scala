package grit.assembly.eval

/** The eval's conversations, one text block each, in the format [[Case.parse]] reads:
  *
  *   - `# ...` says what the case is testing;
  *   - `turn` opens a turn, and `you:` / `grit:` lines are its messages;
  *   - a line ending in `[must]` is an entry the window has to contain;
  *   - `ask` is the turn being assembled for: its one `you:` line is recorded before
  *     assembly runs, as the inbox records it.
  *
  * Relabelling is editing a line here.
  */
object Cases {

  val all: Vector[(String, String)] = Vector(
    "recent-fact" ->
      """# The answer is in the last turn: every budget should find it.
        |turn
        |you: What does scripts/reset-db do?
        |grit: Drops and recreates the local databases. Nothing in them is worth keeping yet.
        |turn
        |you: Is the chat screen's transcript scrollable?
        |grit: Yes: the wheel, page keys, and a drag selects text to copy.
        |turn
        |you: Which database should Claude's runs use?
        |grit: grit_agent, never grit, so a run cannot recover and race your own session. [must]
        |ask
        |you: I'm starting a probe. Which database do I point it at?
        |""".stripMargin,
    "distant-decision" ->
      """# The answer is in the first turn and has scrolled away: small budgets lose it.
        |turn
        |you: Postgres or SQLite for the entry store?
        |grit: Postgres 18. DBOS needs it anyway, and we want concurrent writers from every edge. [must]
        |turn
        |you: What should the modules be called?
        |grit: core, dbos, turn, models, assembly and app, each with one job.
        |turn
        |you: Braces or significant indentation?
        |grit: Braces. The compiler flag -no-indent makes indentation syntax an error.
        |turn
        |you: How do we format?
        |grit: scalafmt, with the dialect override that lets it parse capture syntax.
        |ask
        |you: Remind me why we didn't go with SQLite?
        |""".stripMargin,
    "revised-decision" ->
      """# An early choice is reversed; only the reversal answers the question.
        |turn
        |you: How should the live tests get a database?
        |grit: Point them at the docker-compose Postgres; it is already running.
        |turn
        |you: A test run just wiped my local rows.
        |grit: Then the tests move to Testcontainers: a throwaway Postgres per run, never the local one. [must]
        |turn
        |you: Does that need anything installed?
        |grit: Docker, and the image docker-compose.yml names, so both use one Postgres version.
        |ask
        |you: Where do the live tests get their database from now?
        |""".stripMargin,
    "two-facts" ->
      """# Two facts far apart, both needed: linear keeps the recent one and drops the other.
        |turn
        |you: Anything I should know about the OpenRouter key?
        |grit: It has a low spending limit, so every run should stay under a few cents. [must]
        |turn
        |you: How does a turn survive a crash?
        |grit: Each turn is a DBOS workflow; a restart replays its recorded steps.
        |turn
        |you: What does the chat screen show while a turn runs?
        |grit: A thinking line at the end of the transcript, and the status bar says so.
        |turn
        |you: Which model are we testing with?
        |grit: gpt-oss-20b through OpenRouter; a short turn costs a small fraction of a cent. [must]
        |ask
        |you: Can I afford a fifty-turn probe run on the current model?
        |""".stripMargin
  )
}
