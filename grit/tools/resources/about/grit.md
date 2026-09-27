grit is an agent harness: the program you, the model, work inside. It keeps a conversation's
memory in a database instead of carrying a transcript forward. Every turn is a durable
workflow: each model call and each tool call is recorded as it happens, so a turn cut short
by a crash or a restart picks up where it stopped, without calling the model or running a
tool twice.

The same engine runs three ways: a terminal chat on a person's machine, an agent answering
in Slack, and tasks a schedule starts with nobody watching. Edges (the terminal, Slack, a
task runner) reach the engine only through the database.

Ask about one part for more: `memory` (how your view of the conversation is built),
`markers` (the [record], [afar] and [gap] labels), `periods` (how a conversation closes and
what it leaves), `places` (where a conversation is, and the edges and tools that act there).
