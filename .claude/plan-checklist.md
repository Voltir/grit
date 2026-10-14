# Plan checklist

The house rules no script checks. The gate enforces warnings, the law, format, doc length and
replay; these hold only if a plan states them and its review checks them. A planner answers each
that applies in its plan; a reviewer checks each against the plan and says which it checked.

- **Decision records** (`docs/decisions/README.md`, read in full). A new decision passes all four
  threshold tests or gets no record. An existing record within 30 days of its acceptance is
  revised in place (Status `revised (date)`, the commit saying what changed); after that it is
  never edited, and only a new record supersedes it.
- **Every table bounded.** Each new table, column or recurring writer, a step recorded every round
  included, states its deletion path and its steady-state size.
- **Schema.** A change to `schema.sql` comes with the `ALTER` for an existing database, tested
  against a database built from the previous schema.
- **One home per fact.** A fact the plan adds to the docs lives in one place and is linked from
  the others. No counts in docs: totals go in commit messages.
- **Committed text states rules by reason**: no milestone or step names, review dates, local paths,
  or the names of any one deployment or organisation.
- **Model spend.** Each run that calls a real model is estimated in cents, a few cents at most,
  with the cheap model and the `max_tokens` it uses.
- **A person sees it**: a real-use run with the real model, a restart mid-turn, and a person's
  waiting time.
- **Park, don't widen.** What surfaced and is not in scope is listed as parked, not folded in.
