# grit.core

The domain and the seams everything else is written against. No DBOS, no JDBC driver on its
classpath; implementations live in the modules that need the libraries (CLAUDE.md, rule 8).

In dependency order:

- **`id`** — the opaque ids (`ConversationId`, `EntryId`, `TurnSeq`, `WorkflowId`,
  `SourceId`, `ToolCallId`) and `TurnRef`. Imports nothing in core.
- **`message`** — the model's vocabulary: `Message`, `AssistantBlock`, `StopReason`,
  `Tokens`, `Usage`. ← `id`
- **`store`** — what is kept and the transaction it is kept under: `Tx`, `Db`, `Entry`,
  its `Payload` and their codec `PayloadJson`, `EntryStore`, `EntrySearch`, `Conversation`, `Origin`,
  `ConversationStore`, `UsageLedger`, `StoreError`. ← `id`, `message`
- **`durable`** — `Durable` and `Journaled`: steps that survive a crash. ← `id`, `store`
- **`context`**, **`provider`**, **`inbox`** — the seams the engine plugs into:
  `ContextAssembler` (and the `Window` it builds), `Provider`, `Inbox`. Each names only
  the packages above, never another of the three.

No source file sits at core's root, and no two packages import each other in a circle:
`scripts/enola-law.sh` fails on a new import cycle.

The test tree mirrors it: the in-memory fakes other modules' tests use are
`store.InMemoryEntryStore`, `store.InMemoryUsageLedger` and `durable.InMemoryDurable`.
`TestTx` lives in package `grit.dbos.sql`, because the `null` it holds is legal only inside
the DBOS quarantine (rule 6).
