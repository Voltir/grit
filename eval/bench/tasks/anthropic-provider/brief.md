# C1c. The Anthropic provider

Commit 3 of C1's plan (in `grit/plan-m19-c1-anthropic-provider`; Elision in `grit/note-m19-c1-plan-review`; facts in `grit/note-anthropic-api-facts`): `AnthropicProvider` over raw `java.net.http`, with `ProviderHttp`/`ToolArguments` extracted from the OpenRouter provider and shared. Wisdom's review (`grit/note-m19-c1-wisdom-review`): it records `upstream = Some("api.anthropic.com")`; `drop_block` is sent per model, as the live check in C1d decides (M1). After C1a.

## Cited: grit/plan-m19-c1-anthropic-provider (C1a. A cacheable request: the dated line, the prefix mark, cache writes, one tag)

**Split (2026-10-30, wisdom's review `grit/note-m19-c1-wisdom-review`):** this checkpoint is now **C1a**: commits 1–2 of the plan below, plus the `TurnVerdict.tagged` fix (every round tags the same message, red first), with the review's S2, S3 (state the C2b hazard), S4 and S5. Commit 3 is **C1c** (`grit/plan-m19-c1c-anthropic-provider`); commits 4–5 are **C1d** (`grit/plan-m19-c1d-routing-and-probe`). The gate's append-only fix is in **C1b**.

From [the plan](artifact:grit/plan-m19) (Shared design: "Two providers, chosen per model" and "Caching is a gate, not a hope"). Nick, 2026-10-29: caching is very important; get it right. Branch `m19-c1-anthropic`, in the fixer worktree.

## Goal

grit calls Anthropic's Messages API directly, beside OpenRouter, chosen per model by the catalog. A request's prefix is cacheable and cached. Done when:

1. Nothing per-turn sits before the messages: `TurnShown.answering`'s dated line leaves `system` for the turn's own message (ADR 0016 revised in place, its title still true).
2. `ModelRequest` says where its stable prefix ends. The Anthropic provider sets `cache_control` there (at most four breakpoints) and records cache reads **and writes** in `Usage`. Today `Usage` has `cachedInput` only.
3. The Anthropic provider covers:
   - tools, `strict`;
   - thinking blocks: kept in `AssistantBlock.Reasoning.replay` and sent back unchanged;
   - `ToolUse.Required` mapped, since current models refuse a forced tool choice;
   - a `refusal` stop reason as an expected `ProviderError`;
   - streaming;
   - usage and cost in cents.
4. A `Models` routes each `Pinned` to its provider by the profile's upstream. One deployment mixes OpenRouter and Anthropic.
5. **The prefix-identity test:** for a scripted turn's second call, the request's serialized bytes before the last breakpoint equal the first call's. It names and rules out tool order, JSON key order, and dated or per-call text before the messages.
6. A live probe on `claude-haiku-5-5`, under a cent, shows cache reads on its second call, above the minimum cacheable length. Its cost is reported.

## Plan first

Write a plan into this artifact before any code, with **Elision**, **Capture and separation** and **Sharing** sections (CLAUDE.md, "Plan before implementing"). Wisdom reviews it. It must settle:

- **The official Anthropic Java SDK (`com.anthropic`), or raw `java.net.http` as `OpenRouterProvider` uses.** Weigh what each shares with `OpenRouterJson` and the quarantine (rule 8: only `grit.models` and `grit.mcp` speak HTTP). Verify the SDK's API with `javap` on the jar, never from memory.
- How a stable prefix is expressed in `ModelRequest`. A mark is pure data, so `Shaping`, the rehearsal (ADR 0041) and the stub providers keep working unchanged.
- Where the dated line goes. A `Message.User` before the turn's own? Anthropic's mid-conversation system messages are Anthropic-only, so they're not the core form.
- **The facts, checked against Anthropic's current docs** (the `claude-api` skill, then its live sources) and quoted in the plan with the date read:
  - the endpoint, headers and version;
  - `cache_control` placement and the minimum cacheable length per model;
  - preserved thinking's rule against an edited history;
  - what replaces forced `tool_choice`;
  - the `usage` fields, refusal and stop details, and the rates.

## Reads (main 322bdc56; through scout)

- Before the first edit: `Provider`, `ModelRequest`, `ToolUse`, `Usage`, `AssistantBlock`, `Models`, `OpenRouterModels`, `OpenRouterProvider`, `OpenRouterJson` (only the request builder and the usage reader at first), `TurnShown.answering`, `TurnShown.requestOf`.
- At the point of use: `Shaping` (`ToolUse.Required`), `Catalog`/`Pinned`/`Profile` (how a pin names its upstream), `Launch` (where `Models` is built from the environment and where `StubModels` is the fallback), `UsageTests`, `OpenRouterModelsTests`, `TurnShownTests`.

## Commits

One item each, in order:

1. The dated line out of `system`.
2. The prefix mark and cache writes in `Usage`.
3. The Anthropic provider.
4. Routing `Models` and its wiring in `Launch` (`ANTHROPIC_API_KEY` read beside `OPENROUTER_API_KEY`).
5. The prefix-identity test and the live probe.

If commit 1 changes a step's output encoding, it goes under `Durable.patch` (ADR 0004; `TurnReplayTests` gate).

## Not here

- `cache_control` through OpenRouter (C1b).
- A carried window (C2a/C2b). The identity test here covers one turn's calls, and C2b extends it across turns.

## Plan

Coder, 2026-10-29, on main 322bdc56. Every Anthropic fact used here is quoted with its source and date in [the facts note](artifact:grit/note-anthropic-api-facts), together with the SDK evidence. The review sections (Elision, Capture and separation, Sharing, checklist, turned down, parked) are in [the plan's review note](artifact:grit/note-m19-c1-plan-review).

**Decisions**

- **Raw `java.net.http` + ujson, not the SDK.**
  - The SDK (3.3.0, checked with `javap`) is 40 MB, pulls in Kotlin, Jackson, OpenTelemetry and OkHttp, and retries by default.
  - OkHttp would be an egress path the law's `java.net.http` rule cannot see.
  - The identity test needs grit to own the bytes.
  - Raw HTTP shares `post`, `guarded`, `transient`, the SSE line reader and the tool-argument parsing with OpenRouter.
- **Routing:** the direct API is the upstream `Upstream.AnthropicApi`, spelled `api.anthropic.com`.
  - `ModelRef`, `CatalogJson`, `Party` and the clearances (ADR 0031) are unchanged.
  - The model is written `anthropic/claude-haiku-5-5`, and the provider sends what follows `anthropic/`.
  - The bare slug `anthropic` stays OpenRouter's.
  - A value containing `:` is out, because `ClearedClassifier` relies on no upstream holding one.
- **Prefix mark:** `ModelRequest.stable: Int = 0`.
  - The Anthropic provider places one breakpoint after `system` (which covers the tools) and one after message `stable`: two of the four allowed.
  - No breakpoint goes at the request's end, since the turn's tail (`LastCall`) is volatile.
- **The dated line** becomes its own `Message.User`, just before the turn's own messages, its words unchanged.
- **`Required` maps to `auto` on Anthropic.**
  - The brief's premise is partly wrong: Haiku 5.5 accepts `any`, but it then skips thinking. Opus 5.5 and Sonnet 5.5 return a 400.
  - `Shaping.shaped` already asks again when a reply makes no call.
- **Thinking** is `adaptive` with `display: "summarized"`. Each block goes back verbatim in its place, with `block_binding.prefix_mismatch_behavior: "drop_block"` under `thinking-binding-controls-2026-08-01`.
  - **Why `drop_block`:** a gate's per-round system lines or added tools edit history. On an account enforced for preserved thinking that is a 400 once a thinking block follows; with `drop_block` the earlier thinking is lost instead.
- **Cost from published rates** (`AnthropicRates`). Note that plan-m19's Sonnet 5.5 cache-read rate ($0.20) is stale: the live page says $0.10.
- **No schema change.** Cache writes live in the assistant entry's payload.

**Commits** (each red test named first; the suites that run after it)

1. **The dated line out of `system`.**
   - `TurnShown.requestOf` places `Message.User(answering(...))` before `Shown.turn(mine, named)`.
   - ADR 0016 is revised in place; its title holds.
   - **Red:** `TurnShownTests` "a turn's system prompt is its fragments alone; the line saying when its message was said is the user message just before its own". The pins in `TurnTests` and `TurnHostedTests` move.
   - **Replay is unaffected:** the request is no step's output, since steps record the reply. `TurnReplayTests` runs anyway.
   - **Runs:** `scripts/check grit.turn`.
2. **The prefix mark, and cache writes in `Usage`.**
   - `ModelRequest.stable` is set in `TurnShown.loopShape` to every message the store built, before `TurnLoop.told`'s additions. Under `InLastResult` it stops before the first edited result.
   - `cachedInput` is renamed to `cacheRead` by a script, with the site counts in the commit message, and `cacheWrite: Tokens = Tokens.Zero` is appended.
   - `PayloadJson` writes `cacheWrite` only when non-zero and keeps the stored key `cachedInput`, so recorded outputs re-encode byte-identically.
   - **Red:**
     - `UsageTests` "a sum adds cache reads and writes";
     - `PayloadJsonTests` "an assistant's cache writes are written only when there are some, and read as none when absent";
     - `TurnLoopTests` "the loop's request marks stable every message but the last call's note".
   - **Runs:** `grit.core`, `grit.turn`, and `TurnReplayTests`.
3. **The Anthropic provider.**
   - New: `AnthropicJson` (pure), `AnthropicStream`, `AnthropicRates`, `AnthropicProvider`.
   - Extracted first, behaviour unchanged: `ProviderHttp` (`post`, `guarded`, `transient`) and `ToolArguments.read`.
   - Each `Reasoning.replay` is sent only in its own provider's form. OpenRouter sends an array; Anthropic sends a `thinking` or `redacted_thinking` object.
   - Each grit `Message` becomes one wire message, never regrouped: regrouping would rewrite earlier bytes when a note is appended. The API combines consecutive same-role turns.
   - **Red, in `AnthropicJsonTests`:**
     - the exact request JSON and its breakpoints;
     - strict tools, and `Required` sent as `auto`;
     - the replay verbatim, and the other provider's form skipped;
     - effort, read from `output_config`;
     - each stop reason, with a refusal read as `Refused` naming its category;
     - usage, where `input` = uncached + read + write, and its cost.
   - **Also red:** `AnthropicStreamTests` (the fold, an error event); `AnthropicRatesTests` (Haiku's two cards, split at 100,000 tokens); `OpenRouterJsonTests` (the replay guard).
   - **Runs:** `grit.models`.
4. **`CatalogModels`, and its wiring in `Launch`.**
   - `OpenRouterModels` becomes `CatalogModels(keys, seed, db, settings)`.
   - `Secrets` reads `ANTHROPIC_API_KEY`, refusing a blank one, and refuses a policy rung whose key is unset (`Keys.missing`). With neither key it is `StubModels`, as today.
   - **Red:**
     - `CatalogModelsTests` "a pin served by api.anthropic.com goes to Anthropic, any other to OpenRouter; one without its key refuses every call naming the variable";
     - `SecretsTests` "a policy with an api.anthropic.com rung and no ANTHROPIC_API_KEY is refused, naming it".
   - **Runs:** `grit.models`, `grit.kit`, then the reviewer runs `scripts/it grit.app.it`.
5. **The identity test, and the live probe.**
   - **Red:** `TurnPrefixTests` (in `grit.turn.test`, which already sees `grit.models`) "a turn's second call repeats its first call's cached prefix byte for byte". A scripted, classified turn makes a tool call and then replies; the test asserts that `AnthropicJson.prefix(first)` is a byte prefix of `AnthropicJson.prefix(second)`. The message names tool order, JSON key order, and dated or per-call text.
   - **It is red on `TurnVerdict.tagged`,** which tags round 1's last user message only, so round 2 edits round 1. The fix tags every round's request at the same message.
   - **Probe:** `AnthropicCacheProbe` (a `runMain`) makes two calls on `claude-haiku-5-5`, with a fixed system of about 1,500 tokens, `stable = 1` and `max_tokens` 256.
     - It shows call 2's `cacheRead` > 0, and prints each call's usage and cost in cents.
     - Estimate: under 0.1¢. It is run once in the build round.
     - It saves a streamed and a plain response under `test/resources`, and `AnthropicStreamTests` reads them.
   - **Runs:** `grit.turn`, `grit.models`, then `scripts/check --no-unused`.

**Questions for wisdom, then Nick**

1. Routing by the reserved upstream `api.anthropic.com`, with the model written `anthropic/<id>`. Recommended: it is the brief's "by the profile's upstream", and the stripped prefix is its one wart.
2. The dated line as a separate `Message.User`, its words unchanged. Recommended. It loses system authority; the alternative is a grit section tag, which means editing Base's legend.
3. Cache writes kept per call only in the payload until C1b brings the ledger column's ALTER. Recommended.
4. `drop_block` on every Anthropic request. Recommended.

## Cited: grit/note-anthropic-api-facts (Anthropic Messages API facts (read 2026-10-29))

# Anthropic Messages API: the facts grit's provider is built on

Read 2026-10-29 for [C1](artifact:grit/plan-m19-c1-anthropic-provider), from the `claude-api` skill (cached 2026-10-06) and its live sources on platform.claude.com; where they disagree the live page wins and is named. Re-read before relying on a figure.

## Anthropic API facts (read 2026-10-29)

- **Endpoint and headers.** `POST https://api.anthropic.com/v1/messages` with `x-api-key: <key>`, `content-type: application/json` and `anthropic-version` (source: "Using the Messages API").
  - **Version `2026-10-21`.** The migration guide (`api/migration-2026-10-21`) says: "If you call the API without an SDK, set the `anthropic-version` header to `2026-10-21`."
  - **Its breaking changes don't touch grit.** They are document blocks in assistant messages, citations in user messages, and `temperature`/`top_p`/`top_k` rejected. grit sends none of these.
  - **`x-api-key` still works.** "The Claude API accepts a key in either header."
- **`cache_control`** (source: "Prompt caching").
  - **Order and placement.** "Prompt caching references the entire prompt: `tools`, `system`, and `messages` (in that order), up to and including the block designated with `cache_control`." It can go on a tool definition, a `system` text block, or a message content block. "Thinking blocks cannot be cached directly with `cache_control`."
  - **At most four:** "You can define up to 4 cache breakpoints".
  - **Lookback:** "The lookback window is 20 blocks … On the Claude API, a run of consecutive `tool_use` blocks counts as one position, and so does a run of consecutive `tool_result` blocks."
  - **Minimum:** "512 tokens for … Claude Opus 5.5, … Claude Sonnet 5.5, … and Claude Haiku 5.5". Shorter prompts "are processed without caching, and no error is returned." Haiku 5.5's new tokenizer counts about 30% more tokens than Haiku 4.5's for the same text (skill, "Migrating to Claude Haiku 5.5").
  - **Markers are not edits.** Adding, moving or removing `cache_control` markers is not a history edit (skill, preserved thinking, Breaking change 3). The skill's diff procedure strips markers before comparing prefixes.
- **`usage`** (source: "Prompt caching").
  - **Fields:** `input_tokens` ("not read from or used to create a cache"), `cache_creation_input_tokens`, `cache_read_input_tokens`, and `cache_creation.{ephemeral_5m_input_tokens, ephemeral_1h_input_tokens}`.
  - **Total:** "`total_input_tokens = cache_read_input_tokens + cache_creation_input_tokens + input_tokens`".
  - **Streaming:** "The token counts shown in the `usage` field of the `message_delta` event are *cumulative*" ("Streaming messages").
- **Rates per MTok** (source: "Pricing" page).
  - **Haiku 5.5, prompts up to 100,000 tokens:** input $0.10, 5m cache write $0.125, 1h write $0.20, cache hit $0.01, output $0.50.
  - **Haiku 5.5, prompts over 100,000:** $0.50 / $0.625 / $1 / $0.05 / $2.50. "A request's prompt length counts all of its input tokens, including cache reads and cache writes."
  - **Sonnet 5.5:** $2 / $2.50 / $4 / **$0.10** / $10.
  - **The plan's rate for Sonnet is wrong.** plan-m19 records Sonnet 5.5 cache reads at $0.20, from the skill's 2026-10-06 cache. The live page says $0.10 ("0.05x the base input price").
- **Thinking.**
  - **Haiku 5.5 thinks by default.** It "runs adaptive thinking when the request doesn't set `thinking`". Thinking text is omitted by default unless `display: "summarized"` is set. `budget_tokens` returns a 400. Effort is `low`…`max`, default `medium` (skill, Haiku 5.5 section).
  - **Block shapes:** `{"type":"thinking","thinking","signature"}` and `{"type":"redacted_thinking","data"}`. "Blocks must be passed back unmodified, or the API returns a 400" (Messages API reference).
- **Preserved thinking** (skill, Haiku 5.5 "Breaking change 5", and Fable 5.1 "Breaking change 3").
  - **The rule.** "A Claude Haiku 5.5 thinking block stays valid only while everything sent before it is unchanged: a request that sends a thinking block back after a change to `system`, `tools`, or earlier `messages` returns a 400 … enforced by default for accounts created on or after August 31, 2026."
  - **The escape hatch.** `thinking.block_binding.prefix_mismatch_behavior: "drop_block"` under `anthropic-beta: thinking-binding-controls-2026-08-01` drops "the first mismatched block and every thinking block after it", and the request proceeds. It works only with adaptive thinking on Haiku 5.5.
  - **What doesn't count as an edit:** reordering `tools` without changing them ("confirm at launch"), changing `tool_choice` or effort, and moving `cache_control`.
- **Forced tool choice** (skill, "Forced tool use removed").
  - **Rejected on Opus 5.5 and Sonnet 5.5.** "`tool_choice: {type: "any"}` and `{type: "tool", name: ...}` return a 400 … Use `{type: "auto"}` plus an explicit instruction naming the tool, `strict: true` on the tool".
  - **The brief's premise is partly wrong:** "Claude Haiku 5.5 accepts a forced `tool_choice` … but the response then starts with the tool call and has no `thinking` block."
  - **Unaffected:** `{type: "none"}`.
  - **`strict`** is a top-level boolean on the tool ("When true, guarantees schema validation on tool names and inputs").
- **Refusal** (source: "Using the Messages API", and the skill's refusal section).
  - **Shape:** HTTP 200 with `stop_reason: "refusal"` and `stop_details` (`type: "refusal"`, `category`, which may be `null`, and an optional `explanation`). "Branch on `stop_reason`, never on `stop_details`."
  - **Haiku 5.5 has no fallback:** "There is no server-side fallback for Claude Haiku 5.5 - don't add the `fallbacks` parameter".
  - **Other stop reasons:** `end_turn`, `max_tokens`, `stop_sequence`, `tool_use`, `pause_turn`, `refusal`, `model_context_window_exceeded` (the SDK's `StopReason` constants, by `javap`).
- **Streaming** ("Streaming messages").
  - **Events:** `message_start` (a Message with empty content), then per block `content_block_start`, `content_block_delta` (`text_delta`, `input_json_delta` `partial_json`, `thinking_delta`, and a `signature_delta` "just before the `content_block_stop`"), and `content_block_stop`; then `message_delta` and `message_stop`.
  - **Ping and error events.** There can be "any number of `ping` events", and errors mid-stream look like `data: {"type": "error", "error": {"type": "overloaded_error", …}}`.
  - **Unknown events.** "New event types may be added, and your code should handle unknown event types gracefully."
- **A `role: "system"` message inside `messages`** is supported on Haiku 5.5 with no beta header. It is Anthropic-only, so the core form stays a `Message.User` (see "Turned down").

## SDK or raw HTTP

Checked by `javap` on `com.anthropic:anthropic-java-core:3.3.0` (the latest on Maven Central, published 2026-10-23), with its POMs read.

- **The API is complete.** `MessageCreateParams.Builder` has `system(...)`, `addTool`, `toolChoice(ToolChoiceAuto|Any|Tool|None)`, `thinking(ThinkingConfigAdaptive|...)`, `cacheControl(CacheControlEphemeral)` and `outputConfig`. `Usage` has `cacheCreationInputTokens()`, `cacheReadInputTokens()` and `cacheCreation()`. `Message` has `stopDetails(): Optional<RefusalStopDetails>`. `StopReason.REFUSAL` exists.
- **The non-beta `ThinkingConfigAdaptive` has only `display`:** `block_binding` needs the beta namespace.
- **What it costs.** The core jar is 40 MB. It pulls Kotlin stdlib and kotlin-reflect, Jackson (core, databind, kotlin, jdk8, jsr310), OpenTelemetry API, jsonschema-generator and standardwebhooks. The client module adds OkHttp 4.12. The SDK retries by default (`maxRetries` 2), but `Provider.complete`'s contract says the caller makes any retry.

**Raw HTTP, because:**

1. **It keeps the quarantine.** The law guards `java.net.http.*` to `grit.models` and `grit.mcp` (rule 8). OkHttp would be a second HTTP stack whose imports no law rule names, so an egress path the gate cannot see.
2. **It shares more with OpenRouter.** OpenRouter's `post`/`guarded`/`transient`, the SSE line reading, tool-argument parsing and error description can each become one helper both use. An SDK provider shares none of it and translates ujson to and from Jackson at every boundary.
3. **grit must own the bytes.** The prefix-identity test needs `AnthropicJson.request` to be a pure function to ujson, which keeps insertion order. Under the SDK, the bytes are Jackson's, written inside OkHttp.
4. **The SDK wouldn't save beta work.** The two betas grit sends (`block_binding`, and later mid-conversation system and tool changes) are beta-namespace types in the SDK anyway.

## Cited: grit/note-m19-c1-plan-review (C1 plan: Elision, Capture, Sharing and the rest)

# C1 plan: review sections

The Elision, Capture and separation, Sharing, checklist, Turned down and Parked sections of [C1's plan](artifact:grit/plan-m19-c1-anthropic-provider), kept apart because the checkpoint's body has a size cap. Written 2026-10-29. Facts: [the facts note](artifact:grit/note-anthropic-api-facts).

**Elision** (each Scaladoc as it will be written)

- **`ModelRequest(system, messages, tools = Vector.empty, use = ToolUse.Auto, stable: Int = 0)`.** Its doc gains: "`stable` is how many of `messages`, from the first, the caller's later requests in the same exchange repeat unchanged: with `tools` and `system` they are the prefix a provider may cache. 0 keeps only `tools` and `system`; a count past the end covers every message. A mark that overclaims costs a cache write, never a different reply."
- **`Usage(input, output, cacheRead, costUsd, cacheWrite: Tokens = Tokens.Zero)`:** "What one model call consumed. `input` is every input token, `cacheRead` of them read from the provider's prompt cache and `cacheWrite` written to it (0 where the provider does not say). `costUsd` is the provider's own figure, or grit's from its published rates, when either is known." `cacheWrite` goes last so the 201 positional sites compile.
- **`ToolUse.Required`:** "The model is asked to call one of them: forced where the provider can (OpenRouter's `tool_choice: "required"`); Anthropic's current models refuse a forced choice, so there it is `auto`, and a reply may make no call." The old doc would have become false.
- **`AssistantBlock.Reasoning`:** "… `replay` is the provider's own form, sent back verbatim on later calls of the same turn by a provider that reads that form, and skipped by any other."
- **`Upstream.AnthropicApi: Upstream`:** "`api.anthropic.com`: Anthropic's Messages API, reached directly rather than through OpenRouter. A pair it serves names its model `anthropic/` and Anthropic's own id (`anthropic/claude-haiku-5-5`)." The docs of `Upstream` and `ModelId` gain the same clause.
- **`AnthropicProvider(key: String, pinned: Pinned) extends Provider`:** "[[Provider]] over Anthropic's Messages API, one request per call, streamed or not, no retries, under `pinned`'s model (its id after `anthropic/`), budget and effort, thinking adaptive and summarised, earlier thinking sent back as its settings' replay says. Caches `tools` and `system`, and a request's first `stable` messages, with a breakpoint after each. A failure to connect, a timeout or a connection lost is `Unavailable`; an HTTP error is classed as [[AnthropicJson.error]] says; a decline by the model's safeguards is `Refused` naming its category; a model not under `anthropic/` is `Refused` with no call made."
- **`AnthropicJson`:**
  - `request(model, maxTokens, effort, replay, request): ujson.Value`;
  - `response(body): Either[ProviderError, Message.Assistant]`: "`input` counts uncached, read and written tokens; cost by [[AnthropicRates.cost]]; a `refusal` is `Refused` naming its category";
  - `error(status, body): ProviderError`;
  - `prefix(request): String`: "The bytes Anthropic's cache keys the request's last breakpoint on: its tools, system and first `stable` messages as `request` writes them, without `cache_control`."
- **`AnthropicStream.fold(lines, onDelta): Either[ProviderError, ujson.Value]`:** "… an `error` event is `Unavailable` when `overloaded_error` or `api_error`, else `Refused`; unknown events are skipped."
- **`AnthropicRates.cost(model: String, usage: Usage): Option[BigDecimal]`:** "What Anthropic bills for `usage` on `model`, in USD, at its published rates read on 2026-10-29, cache writes at the 5-minute rate; `None` for a model it has no rates for. A Claude Haiku 5.5 prompt over 100,000 tokens, reads and writes counted, takes the long-context card."
- **`Keys(openRouter: Option[String], anthropic: Option[String])`:** "Each gateway's key, `None` when its variable is unset; `toString` shows neither." `missing(policy): Option[String]`: "the variable the first rung of `policy` lacks a key for, by its upstream; `None` when every rung has one."
- **`CatalogModels(keys, seed, db: Db^, settings) extends Models`:** `OpenRouterModels`'s doc, plus "a pin served by [[Upstream.AnthropicApi]] calls Anthropic with `keys.anthropic`, any other OpenRouter with `keys.openRouter`; a pin whose key is `None` gets a provider refusing every call, naming the variable."

**Every signature passes from its doc alone.** The check changed:

- `Required`'s doc, rewritten so it stays true;
- the replay form guard, so a fallback across providers can't send garbage;
- `stable`'s total semantics, and the cost of a wrong mark;
- routing by an upstream value instead of a `ModelRef` field, which would allow a direct pair at an OpenRouter upstream;
- `AnthropicProvider` taking `(key, pinned)`, so there is no `AnthropicConfig` copying `OpenRouterConfig`.

No doc line is deleted.

**Capture and separation**

- `AnthropicProvider` is a `SharedCapability` holding its own `HttpClient`, like `OpenRouterProvider`. `CatalogModels` captures `db: Db^`, as before.
- `AnthropicJson`, `AnthropicRates`, `ToolArguments`, `Keys`, `stable` and `cacheWrite` hold nothing. `AnthropicStream` is pure but for `onDelta`.
- There is no new trait or type parameter. `ModelRequest` stays capability-free, so `Shaping`, the rehearsal's `Taps` and the stubs are untouched.
- No step body and no `Tx` is involved, and there is no `caps.unsafe`.
- **No probe:** there is no new claim for the compiler to enforce. The HTTP quarantine is the law's existing rule, and the new files sit in `grit.models`.

**Sharing**

- `ProviderHttp` and `ToolArguments` are extracted once, for both providers. The SSE `data:` line reader is shared too, if `OpenRouterStream.fold`'s framing separates; the build says which.
- `CatalogModels` generalises `OpenRouterModels` rather than sitting beside it.
- Reused as they are: `Upstream`, `Party`, `ModelRef`, `CatalogJson`, `ReasoningReplay` (Details widened to "the provider's own form, verbatim"), `ProviderError` (a refusal is `Refused`), `StopReason` (`Other` for `stop_sequence`, `pause_turn` and `model_context_window_exceeded`), and `Delta`.
- `AnthropicRates` has no production prior art. The eval's `Rates` infers prices from the ledger, which is a different idea.
- Names searched: "Gateway", "CatalogModels" and "Keys"; the only `Keys` are two test classes.

**Checklist**

- **ADRs:** 0016 and 0031 are revised in place (0031 says the upstream `api.anthropic.com` is Anthropic's API reached directly). There is no new ADR.
- **Tables:** no new table and no schema change. Thinking signatures grow assistant payloads, within plan-m19's bound and the raw purge.
- **Spend:** about 0.1¢ for the one probe run. No test calls a model.
- **Tiers:** wire forms, routing and the identity test are in the unit tier; recorded responses hold the stream reader to the real API.
- **Reads:** for each commit, the scout queries for what it edits, run before the first edit:
  - 1: `requestOf`, `answering`, `TurnShownTests`;
  - 2: `Usage`, `ModelRequest`, `loopShape`, `told`, `PayloadJson.writeUsage` and `readUsage`, and `uses Usage.cachedInput`;
  - 3: `OpenRouterProvider` and `OpenRouterJson` bodies, and `OpenRouterStream.fold` at the point of use;
  - 4: `OpenRouterModels`, `Secrets`, `Launch.launched`;
  - 5: `TurnVerdict.tagged`, `TurnFixtures.Scripted`, `StreamingProbe`.

**Turned down**

- The SDK.
- A `ModelRef` gateway field, or an `Upstream` enum: illegal pairs, a `Party` change, and about 40 sites.
- The upstream `anthropic`, which is OpenRouter's slug.
- A `role: "system"` message: Anthropic-only.
- Prefixing the dated line to the person's own message.
- `any` on Haiku 5.5, which drops its thinking.
- Structured outputs for `Required`, which would mean synthesised calls.
- Splitting `ModelRequest.messages`, which 117 suites build.
- Top-level automatic caching, or a breakpoint at the request's end: either writes the volatile tail every time, and nothing reads it.

**Parked**

- A `cache_write_tokens` column in the ledger (an ALTER for Nick).
- Recording `input_transformations` (dropped thinking).
- An append-only gate (`role: "system"` lines, `tool_addition`).
- A whole turn against a fake serving the recordings.
- A seed profile for the direct Haiku pair (`strict` Enforced).
- An Anthropic `TokenEstimator`.
- The eval `Rates`, which misreads cache writes.
- The 1-hour TTL.

## Cited: grit/note-m19-c1-wisdom-review (C1 plan: wisdom's review)

# C1 plan: wisdom's review (2026-10-30)

Reviewed `grit/plan-m19-c1-anthropic-provider`, `grit/note-m19-c1-plan-review` and `grit/note-anthropic-api-facts` at main 322bdc56.

## Must

- **M1. `drop_block` per model.** The facts note says the beta works only with adaptive thinking on Haiku 5.5, and the real-use run is on Sonnet 5.5. A live check under a cent (Haiku and Sonnet) decides whether the header is sent per model. Never send it unconditionally.
- **M2. A gate wipes the whole cache, not just thinking.** Verified in `TurnShown.gatedOffer`/`loopShape`: a gated round appends `TurnGate.Guidance`/withdrawal lines to `system` and prepends the `ReadHidden`/`unasked` schemas, so every breakpoint after `system` misses. `drop_block` only avoids the 400. **The fix:** the gate's lines go in a trailing `Message.User` after the stable mark, and `ReadHidden`/`unasked` are offered from round 1 whenever gates are possible, so tools and system never change mid-turn. It goes in C1b, with a prefix test.
- **M3. Three windows, not one.** Commits 1–2 plus the `tagged` fix; commit 3 (the provider, with `ProviderHttp`/`ToolArguments` extracted from 21 KB of OpenRouter code); commits 4–5. Each ends green.

## Should

- **S1. Routing by the reserved upstream `api.anthropic.com`: recommended.** `Upstream.Slug`, `ModelId.Rule` and `Party.of` verified; ADR 0031's party is exact. The provider records `Message.Assistant.upstream = Some("api.anthropic.com")`, and its doc says so. The docs of `Upstream` and `ModelId` change.
- **S2. `stable: Int`: as planned.** C2b may need a second mark, and two breakpoints stay free for it. The doc says a negative count is treated as 0.
- **S3. The dated line: the replay claim holds** (the request is never a step output; ADR 0041's taps persist nothing). **C2b hazard:** `answering` is synthesised from `mine.minBy(seq).createdAt` just before `Shown.turn`, so turn N's line is absent from turn N+1's carried history. Either every shown turn's first message carries its date stably, or C2b solves it. The plan says so now.
- **S4. `tagged`: verified, and the fix moves to commit 2.** A red case: every round tags the same message, and the tool is not offered again after round 1 (`TurnVerdict.anomaly`).
- **S5. `Usage`: as planned.** `+` sums writes. The field-order wart is documented. For C5's measure, cache reads come from the ledger and writes from payloads until the ledger's ALTER (C1b, Nick's go).

## Nits

- The replay guard by shape is parked as "a tagged replay".
- The SDK evidence is sound.
- The capture section is correct.
