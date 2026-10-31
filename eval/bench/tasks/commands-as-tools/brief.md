# C3. Commands as turn tools

From [the plan](artifact:grit/plan-m19) (its "Commands are not tools" finding and the Shared design line "A command offered to a turn is the same command"; Settled with wisdom 2: a turn's keep lands without approval, the tools are `Hosted.requiring(outside)` at least, author the asker with the turn's session named). New branch `m19-c3-commands` in the fixer worktree, **from main** (C2a/C2b are on an unmerged branch waiting for Nick; C3 is independent of them). coder-opus: **plan first** (Elision, Capture and separation, Sharing), keep it as the `## Plan` of note `grit/note-m19-c3-plan`, stop for wisdom's review, then build. Nick is asleep: wisdom answers design questions. C3 adds model-facing tool descriptions, so its PR waits for Nick's look before merge.

## Goal

The projects plugin's commands are offered to a turn as tools, and a command run by a turn is the same command a person runs, not a copy.

- **Reads:** a turn reads through `Hosted.reading`/`Bound.Reading`, with `ReadingCommand.run` inside `Reads.read`.
- **Keeps:** through a new `Bound.Keeping` sibling whose `Tx^` stays inside the step, with a probe that rejects letting it escape.
- **Integrity:** `believed` is the round's `Gated.level` met with the room's cap (ADR 0036). A scripted-model test pins a keep's believed level.
- **Which commands:** reads, keep, move and new; not withdraw, migrate, claim or release.
- **A new ADR replaces 0048** (its title, "no model and no turn", becomes false).
- `PluginCommands.offered` has no caller today; either it becomes the caller-facing path or it is deleted.
- Tool descriptions are model-facing and may be repeated to a person: plain words.

## Signatures

Settle in the plan, every new or changed public signature with its Scaladoc as written (rule 10). Prefer one command path a person's edge and a turn share.

## Reads

Before the first edit (root: the fixer worktree, compile first):

- `show {"symbols":["PluginCommand","PluginCommands","Hosted","Bound","ReadingCommand"],"docs":true}`
- `uses {"symbols":["PluginCommands.offered","Hosted.reading","Bound.Reading"]}`
- `area {"names":["projects"]}`, then `show` the commands the plan touches
- `docs/decisions/0048-*.md`, `0036-*.md`, `docs/decisions/README.md`

At the point of use: the existing tests of `Bound.Reading` and of the projects commands, by name prefix; `docs/capture-checking.md` for the probe.

## Commits

Each behaviour red first; then bare `scripts/check`, no warnings, `scripts/check --only-unused`. The manager runs the it tier. Report hashes, reds, window used.

## Cited: grit/plan-m19 (M19 plan: grit's agent on a project)

# M19 plan: grit's agent on a project

M19 ([milestone](artifact:grit/m19-grit-agent-on-a-project)). Process and its words: [how work is run](artifact:grit/roadmap-how-work-is-run). Drafted 2026-10-29. Nick agreed the shape, added a direct Anthropic provider and made caching a priority; a turn's session and brief moved to M20 (Nick, 2026-10-29); wisdom's review is folded in. Nick's go given the same day.

**Exit:** the real-use run. In kalandra's chat TUI, Nick and grit triage the open notes or draft a plan. grit reads and writes the store through the project commands as tools. Its window is projected at the conversation's start and grows between boundaries on a cached prefix. Real model through Anthropic's API, a restart mid-turn, cost stated in cents.

## Where the code stands (main 322bdc56)

- **Commands are not tools.** The project commands are `PluginCommand`s (ADR 0048). They take a tool's `ToolSpec`, but they run as a `Principal.Person` in a room under `Tx^`, with `believed` set by the edge. `PluginCommands.offered` has no caller in grit.
- **The window is fresh every turn, and so is the system prompt.** `ContextAssembler`'s doc says the window is "never carried from the last one". `TurnShown.requestOf` ends each turn's system prompt with a dated line (`TurnShown.answering`, ADR 0016), and it places sections, documents and pieces *before* the window's entries. `LinearAssembler` keeps "the most recent whole turns that fit", so at budget it slides.
- **One provider.** `grit.models` speaks OpenRouter alone. `ModelRequest` has no cache field. `ToolUse.Required` (forced tool choice) is what `Shaping` uses.
- **A turn knows no session.** ADR 0050 anticipates one ("a grit turn can record its own"). A claim's holder is a directory's slot, kept by every session started there.
- **kalandra runs no turns:** `Main.Unread`, `Placer.Off`, `Linear(8000)`. The chat is in unpublished `grit.app` and imports `grit.dbos.engine`; its engine entry points are `private[grit]`.

## Shared design

- **ADR 0052: a window is drawn from the store at a boundary and carried, append-only, until the next. Nothing before a boundary is redrawn.** The boundaries are a start, a clear, a compaction and the limit. Between boundaries, ADR 0040's steps (`ask:needs` … `ask:relevance`) still run each turn. What they admit is appended after the carried entries, never placed before them. The limit is set per deployment through the existing `Assembly` budget, capped by the model's context. At the limit the window is cleared and re-projected, never slid. The ADR revises the thesis line in CLAUDE.md and the trait doc.
- **Nothing per-turn sits before the messages.** The dated line moves out of the system prompt and into the turn's own message (ADR 0016 is revised in place). Within a turn, what a gate adds is a note kept in place, and system and tools are fixed per turn, so a gated turn keeps its cache (ADR 0036).
- **Two providers, chosen per model.** Anthropic's Messages API sits beside OpenRouter in `grit.models`, behind the same `Provider`. The catalog routes each use (turn, query, Jev, classifiers) to a model and that model to its provider, so one deployment mixes them. Nick has Claude API credits ($100 a month) for the direct path, and OpenRouter stays for other models. Anthropic-specific facts are verified against the current API docs in the coder's plan, never recalled:
  - prompt caching through `cache_control` breakpoints (at most four), confirmed by `usage.cache_read_input_tokens`;
  - **append-only history**: current models refuse an edited history that carries thinking blocks (preserved thinking). Thinking blocks are kept as the turn recorded them and sent back unchanged between boundaries; a boundary is a new history. ADR 0052's rule is the same rule, so the two are designed together;
  - **no forced tool choice** on current models: `ToolUse.Required` maps to `auto` with `strict` tools, or to structured outputs, in that provider;
  - a `refusal` stop reason is an expected failure in the return type (rule 3).
  - Whether it goes through the official Java SDK, quarantined in `grit.models`, or through raw `java.net.http` as OpenRouter does, is settled in the coder's plan by which shares more and keeps the quarantine.
- **A command offered to a turn is the same command.** A turn reads through `Hosted.reading`/`Bound.Reading`, with `ReadingCommand.run` inside `Reads.read`. A turn keeps through a new `Bound.Keeping` sibling whose `Tx^` stays inside the step (with a probe). `believed` is the round's `Gated.level` met with the room's cap (ADR 0036). **A new ADR replaces 0048**, because its title ("no model and no turn") becomes false.
- **Moved to [M20](artifact:grit/m20-grit-codes):** a turn's session (through `Sessions.record`, inheriting its directory slot's claim, ADR 0050) and the brief at a boundary (kept as a projects document version on claim, packed by id in `Window.documents`; the digest by `SessionDigest.of`), as wisdom shaped them. M19's run needs neither: a turn reads a brief through `project_brief` when it wants one.
- **Bounded:** no new table. The per-turn window record (`TurnSteps.windowRecorded`) becomes a delta: the carried record's entry id plus the appended range. Recorded thinking blocks are counted in that bound. Deletion is the journal's raw purge.
- **Caching is a gate, not a hope** (Nick, 2026-10-29: "very important, get it right"). Every model call records `cache_read`/`cache_write` tokens from `usage` beside its cost, through both providers. A test pins byte-identical requests across turns of one window: the request a turn sends begins with the bytes of the last one's prefix (tools, then system, then carried messages). Silent invalidators are listed and ruled out by that test: tool order, JSON key order, timestamps, ids, and per-turn text placed before the messages. **Target, measured in the real-use run:** on every turn after a window's first, at least 80% of input tokens are cache reads; a miss is diagnosed before the run counts.
- **Signatures are reviewed where they are written.** Checkpoints C1, C2a and C3 start with a coder plan carrying Elision, Capture-and-separation and Sharing sections, which wisdom reviews before code.

## Checkpoints (hours from M18)

M18 ran about 27 active hours against 19 planned. Coder median 49 min; 1–2.5 h a checkpoint end to end; close 1.3 h.

- **C0. The proving ground** (1.5 h, manager): republish the proving-ground deployment on main's jars. M18's ALTERs on the proving-ground database need Nick's go. Watch it for an hour. Beside it: kalandra's capture records a scout `find`/`area` with no `named` ([note](artifact:grit/note-digest-reads-name-symbols)); coder-haiku.
- **C0b. A server that loses its engine lock serves again** (1.5 h; dbos, kit; Nick, 2026-10-30). Found in C0's log: a database restart left the proving ground up with no engine until a manual restart (`grit/backlog-engine-lock-lost-outage`). Option (a): wait for the lock, then serve again.
- **C1a. A cacheable request** (2 h; core, turn). The dated line leaves the system prompt; `ModelRequest.stable` marks the prefix; `Usage` gains cache writes; every round tags the same message (`TurnVerdict.tagged`, a history edit today). Plan in `grit/plan-m19-c1-anthropic-provider`, reviewed in `grit/note-m19-c1-wisdom-review`.
- **C1c. The Anthropic provider** (2.5 h; models). Raw `java.net.http` with HTTP plumbing shared with OpenRouter; thinking kept and replayed, `Required` mapped to `auto`, `refusal`, cost from published rates, `cache_control` at the marks.
- **C1d. Routing per model, and the live probe** (1.5 h; models, kit). The reserved upstream `api.anthropic.com`; `ANTHROPIC_API_KEY`; the prefix-identity test; Haiku 5.5 cache reads and the `drop_block` check on Haiku and Sonnet, under a cent.
- **C1b. Caching through OpenRouter, and an append-only gate** (2 h; models, turn). `cache_control` for Anthropic models on OpenRouter; a gate's lines in a trailing message and its tools offered from round 1, so a gated turn keeps its cache; cache writes in `usage_ledger` (ALTER: Nick's go).
- **C2a. The assembler projects at a boundary** (2.5 h; core, assembly; ADR 0052). The trait expresses a boundary and a carried window. Linear clears and re-projects at the limit.
- **C2b. The turn carries its window** (2.5 h; turn; `Durable.patch`). The carried window, thinking blocks included, is read in a step; recall is appended after it; the window record is a delta. The prefix-identity test runs over a scripted multi-turn conversation. `TurnReplayTests` gate; the rehearsal (ADR 0041) is checked.
- **C3. Commands as turn tools** (3 h; core, act, kit, projects; the ADR replacing 0048). `Bound.Keeping` and its probe, the integrity rule, and a scripted-model test of a keep's believed level.
- **C4. The chat as a published extension** (3.5 h; tui, app, kit). `extensions/chat` over core, tui and edge, with no kit and no dbos, shaped like `grit.slack`. A trait over opening the engine; the open-or-attach entry made public in kit. `grit.tui` published. `mcp-arch.yaml`, `enola-intent.yaml`, `scripts/enola-plant` and CLAUDE.md's table updated. A change a person sees: Nick looks before merge.
- **C5. kalandra runs turns** (3.5 h with the run; kalandra). An Anthropic key, a placer, its project tools offered (reads, keep, move, new; not withdraw, migrate, claim or release), a `Budget` cap, an `Assembly` budget that holds a conversation, and `kalandra chat`. **Real-use run** with Nick, reporting the cache-read share per turn against the 80% target.
- **C6. Close** (2 h): `--milestone`, the janitors, ADRs held to the code, note triage, and the recall query set scored once as the baseline ([recall note](artifact:grit/note-store-recall-measured-then-tuned)).

**Above the cut line: about 28 h**, past the 24 h ceiling. Wisdom split C1 into three windows (C1a, C1c, C1d), the append-only gate joined C1b, and C0b was added for a live bug; caching is Nick's priority.

**Below the cut line:** [one store listener](artifact:grit/backlog-one-store-listener); [the chat follows EdgeWatch](artifact:grit/backlog-chat-follow-on-edgewatch); compaction with a model summary; recall tuning.

Build order: C0; C1a; C1c; C0b; C1d; C1b; C2a; C2b; C3 (independent of C1 and C2); C4; C5; C6.

## Settled with wisdom (2026-10-29), for Nick's go

1. **ADR 0052's wording** is as in Shared design. The limit is per deployment, capped by the model.
2. **A turn's keep** lands without approval: reads are free, keeps and moves are reversible through history. The tools are `Hosted.requiring(outside)` at least. Author: the asker, with the turn's session named.
3. **The chat's home:** `extensions/chat` over core, tui and edge; kit keeps the engine entry, made public.

## Decided by Nick (2026-10-29)

- **Models:** probes and tests on Haiku 5.5; the real-use run on Sonnet 5.5 with a 25¢ cap, on the Claude API credits. Anthropic's rates (cached 2026-10-06): Haiku 5.5 $0.10/$0.50 per M tokens, Sonnet 5.5 $2/$10 (cache reads $0.10, per the live pricing page read 2026-10-29; the skill's cached $0.20 was stale).
- **C0's ALTERs** on the proving-ground database: go.
- **The plan and the three answers settled with wisdom:** go.
