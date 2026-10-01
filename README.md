# grit

An LLM agent harness in Scala 3. Each turn is a durable workflow (DBOS on Postgres), and
its context window is rebuilt from stored history every time, so nothing has to be
compacted or trimmed by hand. It runs as a terminal chat, a Slack bot, and (eventually)
scheduled tasks.

Early and changing fast. See [`docs/extending.md`](docs/extending.md) for how the code is
laid out and [`docs/decisions/`](docs/decisions/) for why.
