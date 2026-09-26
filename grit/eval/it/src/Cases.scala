package grit.eval

/** The eval's conversations, one text block each, in the format [[Case.parse]] reads:
  *
  *   - `# ...` says what the case is testing;
  *   - `query:` is the search query a model might write for the ask, used by retrieval
  *     when no model writes one (the spike's agent-style queries: they name what the
  *     earlier messages contain, not the answer);
  *   - `turn` opens a turn, and `you:` / `grit:` lines are its messages;
  *   - a line ending in `[must]` is an entry the window has to contain;
  *   - `ask` is the turn being assembled for: its one `you:` line is recorded before
  *     assembly runs, as the inbox records it.
  *
  * A cross-place case ([[crossPlace]]) also writes other conversations:
  *
  *   - `place fs:/home/nick/api` starts another conversation at that place (under the case's
  *     own root), whose `turn`s follow, its period open; `place … closed` closes it after
  *     them; `carried: …` is a standing line its closing carries (and closes it); `reopen`
  *     starts its second period, open; `here` returns to the case's own turns;
  *   - `scope …` is the scope a window draws within (places separated by spaces), everywhere
  *     under the case's root by default;
  *   - a line ending in `[never]` is an entry the window must not hold.
  *
  * Relabelling is editing a line here.
  */
object Cases {

  val all: Vector[(String, String)] = Vector(
    "recent-fact" ->
      """# The answer is in the last turn: every budget should find it.
        |query: which Postgres database should a probe or manual test run connect to: the local dev database or a separate database for Claude's agent runs, GRIT_DATABASE_URL, avoid clobbering the user's session
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
        |query: why did we choose Postgres instead of SQLite for the entry store: database decision, trade-offs, DBOS durable workflows, concurrency, multiple writers, Postgres 18
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
        |query: how do the live test suites get their Postgres database now: docker-compose local database versus a throwaway Testcontainers container, test setup change after tests wiped local rows
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
        |query: cost of a fifty-turn probe run on the current model: OpenRouter API key spending limit and budget, which model is configured for testing, price per turn in cents, tokens per turn
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

  /** Cases with other conversations beside their own ([[Case.Elsewhere]]): where recall from
    * elsewhere is magic, where it is weird, and where the design means it to miss.
    */
  val crossPlace: Vector[(String, String)] = Vector(
    "answer-next-door" ->
      """# A new session in web; the answer is only in api's open period. The first turn's query finds it.
        |query: flaky invoice test fix timezone TZ UTC test JVM CI
        |place fs:/home/nick/api
        |turn
        |you: The invoice test is flaky again, only in CI.
        |grit: It's the timezone: InvoiceDateTests builds LocalDate.now(), and CI runs in UTC. Pin TZ=UTC in the test JVM. [must]
        |turn
        |you: What about the Monday reports?
        |grit: They run in the batch job, and nothing there depends on the local date.
        |ask
        |you: What fix did I settle on for the flaky invoice test?
        |""".stripMargin,
    "same-words-other-project" ->
      """# The answer is in this conversation's own earlier turns; api's open period is loud with the same words.
        |query: database migration rollback plan staging schema version
        |turn
        |you: If the staging migration fails, how do we roll back?
        |grit: Restore the snapshot taken before the migration; the schema version table then says 41 again. [must]
        |turn
        |you: Anything else for the release?
        |grit: Tag it after the migration passes on staging.
        |turn
        |you: Who approves the release?
        |grit: The on-call lead, in the release channel.
        |place fs:/home/nick/api
        |turn
        |you: The api database migration adds an index; any rollback plan needed? [never]
        |grit: A migration that only adds an index rolls back by dropping it; the staging schema version stays. [never]
        |turn
        |you: And the migration's run time on staging?
        |grit: About two minutes on staging, most of it building the index.
        |ask
        |you: Remind me of the rollback plan if the staging migration fails.
        |""".stripMargin,
    "closed-next-door" ->
      """# The answer was in api's period, now closed: the close is the time boundary, so it is not a candidate.
        |query: flaky invoice test fix timezone TZ UTC
        |place fs:/home/nick/api closed
        |turn
        |you: The invoice test is flaky, only in CI.
        |grit: Pin TZ=UTC in the test JVM: CI runs in UTC. [never]
        |turn
        |you: Thanks.
        |grit: You're welcome.
        |ask
        |you: What fix did I settle on for the flaky invoice test?
        |""".stripMargin,
    "out-of-scope" ->
      """# The answer is in an open period outside the scope (a nightly task, scope fs only).
        |query: nightly backup failed disk full volume
        |scope fs:/
        |place task:nightly/backup
        |turn
        |you: Why did the nightly backup fail?
        |grit: The backup volume was full; old snapshots were pruned and it ran again. [never]
        |ask
        |you: Why did the nightly backup fail last night?
        |""".stripMargin,
    "two-open-one-relevant" ->
      """# Two open periods elsewhere: one holds the answer; the other only shares a word.
        |query: grpc deadline exceeded timeout gateway retry setting
        |place fs:/home/nick/api
        |turn
        |you: The gateway keeps logging DEADLINE_EXCEEDED on the grpc calls.
        |grit: Raise the gateway's grpc deadline to 5s and add one retry; the backend's p99 is 3.8s. [must]
        |place fs:/home/nick/web
        |turn
        |you: Should the web form show a timeout message?
        |grit: Yes, after 10 seconds, with a retry button. [never]
        |ask
        |you: What did I decide about the grpc deadline for the gateway?
        |""".stripMargin,
    "carried-next-door" ->
      """# The design's gap: api settled the fix in a period since closed, carried in its balance, and api is open again on something else. Nothing reaches it.
        |query: flaky invoice test fix timezone TZ UTC
        |place fs:/home/nick/api
        |turn
        |you: The invoice test is flaky again, only in CI.
        |grit: Pin TZ=UTC in the test JVM: CI runs in UTC. [must]
        |carried: The flaky InvoiceDateTests is fixed by pinning TZ=UTC in the test JVM
        |reopen
        |turn
        |you: Can we speed up the api build?
        |grit: Cache the dependency resolution between CI runs.
        |ask
        |you: What fix did I settle on for the flaky invoice test?
        |""".stripMargin
  )
}
