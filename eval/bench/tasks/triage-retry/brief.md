# M21 B: a triage step retried after its decision was kept still starts its turn

Checkpoint B of [the M21 plan](artifact:grit/414). Internal (no wording, nothing a person sees but a lost reply returning): merges on review. Branch `triage-retry`, from `origin/main`. Planned 1 h for the reproduction, one more for the fix if it is red.

## Goal

Settle [the suspected lost turn](artifact:grit/577), which wisdom inferred from the code and nobody has reproduced. Triage's `Speak.consider` decides a heard message and keeps the decision (`SpeechStore.decided`) in one transaction. If that transaction commits and the step's result is not recorded (a crash between), DBOS runs the step again: the ledger now holds the message's own `Drafting` row, `Speech.decide`'s drafting check may read it as another turn drafting (`Silence.Unanswered`), `decided` answers `false` (kept before), and `consider` returns the fresh `Held`, so `Triage.flow` starts no turn although the kept row says `Drafting`.

First a test that runs `consider` twice for one heard message in two transactions, as a retried step would, and pins that the second answers the decision the first kept. If it is green as the code stands, the note is wrong: say why in the report (which check excludes the message's own turn), keep the test only if it pins a contract no other test does, and stop. If it is red, fix it.

## Signatures

No public signature changes are expected. The contract to pin, wherever it is cheapest to state once: **`Speak.consider` is idempotent for one heard message**: asked again after its decision was kept, it answers that kept decision, whatever the ledger has gained from it. Two shapes for the fix; choose by which shares more with the occasion path, which already excludes its own turn (`t.turn != turn` in the occasion's drafting check):

1. the heard message's drafting check in `Speech.decide` excludes the root's own turn, as the occasion's does: one rule for both roots; or
2. `consider` answers the decision that stands when `decided` answers `false` (read back from the store).

Prefer 1 if both roots can share one check; say in the report why if not. If either needs a changed signature in `grit.core.speech`, stop and report the signature with its Scaladoc before building it.

Replay: `Triage.flow`'s step names, order and recorded encodings must not change (ADR 0004); `LifecycleReplayTests` is the gate.

## Reads

_Before the first edit_ (scout): `show Speak.consider, Speech.decide` with `body`, `SpeechStore.decided`, `Silence`; `uses Silence.Unanswered`.

_At the point of use:_ `tests` of `grit.lifecycle.triage.TriageTests` by the names that mention speech or drafting, and its fixture for a heard message and a speech store; `SpeechContract` (the spec `SpeechStore`'s fake and SQL share) if the fix lands in the store; `Triage.flow`'s body only to confirm what it does with `Held`.

## Commits

1. `tests:` the retried `consider`, red for the stated reason (quote the failing comparison in the message), or green with the reason the note is wrong.
2. If red: the fix, one commit, the test green; `scripts/check` on the lifecycle and core suites touched, then bare once; `LifecycleReplayTests` green.

Report: red or green and why; the fix's shape and what it shares; gate totals; `scripts/kb-delta`; actual hours.

## Cited: grit/577 (A triage step retried after its consider transaction committed may hold its own turn Unanswered)

# A triage step retried after its consider transaction committed may hold its own turn Unanswered

Found by wisdom reviewing [`plan-occasions-consolidation`](artifact:grit/574), 2026-11-20; inferred from the bodies, not reproduced.

Triage's `Speak.consider` decides a heard message and keeps the decision (`SpeechStore.decided`) in one transaction. If that transaction commits and the step's result is not recorded (a crash between), the step re-runs: the ledger now holds the message's own `Drafting` row, and `Speech.decide`'s drafting check (`Silence.Unanswered`) does not exclude the heard message's own turn, as the occasion vector does (`t.turn != turn`). So the re-run decides `Held(Unanswered(itself))`; `decided` returns `false` (kept before), and `consider` returns the fresh `Held`, so `Triage.flow` starts no turn although the kept row says `Drafting`.

To check: reproduce in `TriageTests` (or the lifecycle replay harness) by committing `consider` and re-running it; if red, exclude the root's own turn in the shared drafting check, or have `kept` return the decision that stands when `false`.

## Cited: grit/414 (M21 plan: a second machine takes work)

# M21 plan: a second machine takes work

**Draft 4, 2026-11-20, by the manager: [wisdom's review](artifact:grit/585) of draft 3 (go with changes) is folded in; waits for Nick's go.** It replaces the draft Nick approved on 2026-11-10 (version 4353 of this artifact), most of which the three milestones since built another way. Process and its words: [how work is run](artifact:grit/252). Milestone: [a second machine takes work](artifact:grit/263). Keys of older notes keep the number M22.

**Goal.** A Claude Code session on Nick's laptop takes ready checkpoints from the store and builds them while the desktop builds others; the desktop's manager makes work ready, reads each report and reviews the pushed branch. Nothing is copied between machines by hand. The reason is memory: the desktop has 24 GB and 4 cores, and crashed twice (2026-11-20) with two builds and a deployment running.

**Exit:** two real checkpoints built at once, one on each machine, reviewed and merged from the desktop.

## Checkpoints, the cut line and the stop rule

Hours from the last three milestones' actuals (1–2.5 h a checkpoint, 1.5× on anything that touched `ProjectCommands`, already applied below). Every checkpoint records its actual hours.

|  | What | Hours |
|---|---|---|
| [C0](artifact:grit/417) | The laptop spike, with Nick at it, finished: tools, cold compile, a session, sleep. Now also the reach (Design 1) and a credential for the laptop made by hand as today. No code merged. | 2.5 |
| B | [The triage retry](artifact:grit/577): reproduce it red in `TriageTests`. The hour is the reproduction; a fix, if it is red, is one more. Not this milestone's subject, but a suspected lost turn does not wait. | 1 |
| P | [`ProjectCommands` split and the projects coverage gaps](artifact:grit/580), first because K1 and K2 edit that file. | 2 |
| K1 | `project_claim` takes `oldest` (Design 2). | 2.5 |
| K2 | A claim is held by one session (Design 3): `Claims`, its SQL, the schema, `EdgeSessions`, `Briefing`, the plugin. Carries an `ALTER` and one line a person reads. | 5 |
| T | The `/take` skill, and join's checks of what a coder needs (Designs 5, 6). | 2.5 |
| J | Janitors over P–T: `overlap-janitor` on the production symbols, `test-janitor` on the changed tests; findings validated before any fix. | 1.5 |
| R | The real-use run (Verification), its two checkpoints' building included: J's fixes on the desktop, K3 on the laptop. Then how-work-is-run says how the manager marks work ready and how many coders each machine runs. | 3 |
| C | Close. | 2 |
| **cut line** |  | **22** |
| K3 | A role names who may hold it (Design 4). Built by R as its laptop checkpoint, so it lands unless R is cut. | (in R) |
| M | Minted credentials as a command: an account, a machine named at the mint, an expiry; how one is retired. Nick placed it in this milestone; below the line here, which is his to confirm. | 2.5 |
| H | The headless take loop under a user unit; its permission mode and allowlist Nick's, from what R's sessions stopped to ask. | 3 |
| S | TLS in the listener, in a module of its own, and `Hosts` admitting `https://` origins. | 5 |

- **Stop rule:** if K2 lands past 13 h actual, R builds J's fixes on the laptop and K3 goes to the next milestone.
- **Order:** C0 can run any day Nick is at the laptop; nothing above T needs it. B, P, K1 and K2 are built on the desktop, one coder at a time.

## Where things stand (grit main 64f658f3)

Built since the first draft, so no longer planned here:

- **A machine has a name, by its credential** (`MachineName`, `Callers.of`, tokens kept as SHA-256 digests); a holder is `<machine>/<dir segment>-<hash>[/<type>/<session hash>]` (`ClaimHolders.of`).
- **Hooks reach the edge over HTTP** (`/events`, `/brief`, `/holder`), and **joining is one command** (`/join/claude-code`, `extensions/claudecode/shell/join.sh`).
- **`ready` is a checkpoint state** (method v7), checked by `sections-present`; person-only moves are method data.
- **The project-scoped lock** (`Claims.lock`), taken by every keeping command and by claim and release.

Not built: claiming by search; a claim's session; a role bound to an agent type; any way for another machine to reach the edge; the take skill; join checking a coder's tools; the laptop itself beyond the desktop's network change ([C0's findings](artifact:grit/418)).

**The constraint that changed the reach.** `CommandsEdge.of` refuses an address that is not loopback, by Nick's answer of 2026-11-14 ([E5](artifact:grit/458)): a credentialed edge listens on loopback only until TLS, since a credential would cross the network in clear. The first draft's open LAN port is therefore gone.

## Design

1. **The laptop reaches the edge through an SSH forward it opens** (`ssh -N -L 7317:127.0.0.1:7317 <desktop>`, kept up by a user unit with `ServerAliveInterval` and `ExitOnForwardFailure`). On the laptop the edge is at its own `127.0.0.1:7317`: the `Host` check, the join script, the hooks and the MCP entry are unchanged, the credential crosses encrypted, and grit gains no code; the laptop's token is one more line of kalandra's configuration (`Callers.of`). It costs `sshd` in the desktop's WSL and a Windows firewall rule for port 22 on the private profile, in place of 7317. The laptop's key is restricted in `authorized_keys` (`restrict,port-forwarding,permitopen="127.0.0.1:7317"`), so it is no shell on the desktop. C0 checks port 22 is free of a Windows sshd: mirrored mode shares ports. A forward that is down is what a hook already handles: lines wait on the laptop and post later (ADR 0050 knows a repeat by its ordinal). Wisdom: no third shape is better. **Nick's call** (he turned a tunnel down on 2026-11-10, before credentials existed).
2. **Taking is a claim by search.** `project_claim` takes `oldest` (`project`, `kind`, `state`) in place of `key`: of that kind's artifacts in that state that no live claim holds, the one that entered the state first, ties by id, found and claimed in one transaction under the project's lock (`ProjectClaim.run` locks before `claims.live`), so of two machines asking at once the second waits, then finds the next. "Entered the state" is read from the versions the store keeps (`written_at` of the version that moved it there), one history read per candidate; when that version has been collected, the earliest kept version in that state. No new column.
3. **A claim is held by one session at a time** ([the rule](artifact:grit/477)).
   - A claim records the session that holds it. **Its lease becomes that session's**: once a claim has a session, `Claim.active` reads that session's last event alone, not every session of the holder; today a crashed session's claim never lapses while another works in the same checkout. One clock still.
   - **Who writes the session:** a claim made by a session's own command carries it; a claim made for a holder before its session exists (the manager claiming for a coder) has none, and the holder's first session to record events takes it, in the `/events` recording (`Sessions.record`). `/brief` stays a GET that writes nothing.
   - **The brief** goes to that session alone. `EdgeSessions.Briefed` gains a case for a claim another session holds: one line (who holds it, how lately active, and the exact takeover command with this session's id). A second session claiming without takeover is answered `Claimed.HeldBy(claim)`, so a renewal never moves the session silently.
   - **Succession:** a recorded close frees the claim for the holder's next session; a crash records none, so the lapse frees it; and `project_claim` with `takeover` and a session's id moves the subject's claim to that session at once, **whatever its holder**, when the claim's `by` is the caller's person. So a milestone's manager goes on from the other machine, whose holder is another. Sessions are believed lower, so takeover is the person's own command line, gated and worded as person-only moves are (`ProjectCommands.moverOf`, `PersonOnlyBy`).
   - **A turn's path is unchanged:** `TurnAssembly.opening` reads `briefOf(holder, at)` for a conversation, which one session works; K2's coder plan shows no step input or output changes, or stops for an ADR 0004 answer.
4. **A role names who may hold it**, as method data: a field of `RoleView` with a default, as `covers` is, shaped like `Movers`: `Any | Session | Agents(nonempty)`. `coder` for `coder-opus` and `coder-haiku`, `manager` for a top-level session; `Any` is absent from the digest, so older method versions read as they did. The plugin learns a holder's agent type from a reader in core beside `ClaimHolder` (holder to optional agent type), held to `ClaimHolders.of` by a round-trip law: `grit.projects` may not name `grit.commands.machine`. A claim for a holder the role does not admit is refused, naming the roles that do. (K3: below the line, built in R.)
5. **Taking is a skill a normal session runs.** `/take`, checked in beside grit's other skills: go on with what a coder of this session holds, else claim `oldest` `ready` checkpoint for the coder's holder and stop, saying so, when there is none; move it to `building`; branch from the base the brief names; start a fresh coder (the SubagentStart hook injects its brief); when the coder ends, check the claim is still this session's, push the branch, move to `review`, release, take the next. A coder that ends without its check passing: release and leave the checkpoint in `building`, which with no claim is the manager's signal. The desktop's manager uses the same skill in place of its by-hand claim and start.
6. **Join says what a coder needs and lacks:** the JDK, glibc 2.39 (Mill's launcher), Mill, the scout binary, Docker with Postgres publishing its port, a git credential that can push, the edge answering with the caller's machine name. It checks and names; it installs nothing. How the scout binary, `CLAUDE.local.md`, the agent definitions and memory reach the laptop is C0's finding.

## Turned down

- **TLS now** (S): the right answer once someone else's machine or a sandbox joins; about 5 h, a certificate both curl and Claude Code's MCP client must trust, and nothing this exit needs. Below the line.
- **An open LAN port, unauthenticated or with the credential in clear:** refused by the edge by design.
- **Postgres across the LAN, the laptop attaching as an edge (ADR 0015):** needed for `grit work` on the laptop, not for Claude Code coders.
- **A `project_take` command:** `project_claim` with a search in front; one command, one refusal list.
- **The session as part of a top-level holder** (so two sessions never share one): a new session could then not go on with its predecessor's claim, which is why holders are by directory.
- **A lease stored on the claim** in place of one read from sessions: two clocks for one fact. The `ALTER` adds the session only.
- **Role by convention** (the skill claims the right role): a wisdom agent claimed as `coder` was a real mistake, and the method already holds who may move.

## Elision (the coder plans refine these; each is a tool's description, which is its doc)

- `project_claim`: "Claims an artifact for a session or agent to work in a role. Give `key`, or `oldest` (`project`, `kind`, `state`): of that kind's artifacts in that state that no one holds, the one that entered it first. With `takeover` and `session`, the claim on `key` is moved to that session from whoever holds it. Refused: both or neither of `key` and `oldest`; `oldest` finding none ('nothing to take'); a claim another session of the holder has, naming it and the takeover that moves it; `takeover` of a claim someone else made, or by a session rather than its person; and as today." Callable from this alone. The check changed the design three times: "oldest" needed its clock stated, `takeover` needed who may, and takeover had to name a session since the command line has none.
- `Claim.session: Option[SessionId]`: "The session holding it; none until a session of its holder records an event, which takes it." `Claim.active`: "…the last event of its session, or while it has none, of any session working as `holder`…".
- `Claims.claim` gains `session: Option[SessionId]`: from the caller's session when a session gives the command, from `takeover`'s argument, else none. `Claimed.HeldBy(claim: Claim)`, apart from `Holding` (the holder on another subject), as `Taken` carries its claim.
- `EdgeSessions.Briefed` gains the held-by-another case, and `Briefing.briefed` renders its one line; `Routes.brief` is unchanged in what it writes: nothing.
- `RoleView.holders: Holders` (`Any | Session | Agents`), default `Any`; the core reader of a holder's agent type, named in K3's coder plan after its synonym search.
- The `/take` skill's description: "Take ready checkpoints from the project store and build each with a fresh coder, until none is ready."

## Capture and separation

- `oldest` reads and claims under one `Tx^`, which does not leave the command; the lock is `Claims.lock`'s. A kit `it` test makes a second claim wait on a held lock, then take the next artifact, and is seen red with the lock removed.
- `Claims` stays `caps.Pure` with `(using Tx^)` on each method; `Claim`, `Holders` and the new fields are pure data. No new trait, no type parameter, no new capability, no `caps.unsafe`: so no probe is needed, and none is added.
- Not compiler-enforced, tested: a brief is served only to the claim's session (`/brief` for another session of the holder answers the one line and writes nothing); takeover by a session, or of a claim another person made, is 409.

## Sharing

- `oldest` reuses `project_list`'s filter, `ProjectClaim.run`'s lock and `Claims.claim`; the one line reuses what `project_claims` renders for a claim.
- Takeover's gate and wording are `ProjectCommands.moverOf` and `PersonOnlyBy`, as person-only moves use them; no second notion of "a person, not their session".
- `Holders` sits in `RoleView` beside `covers`, read by the same `MethodJson`, and is shaped like `Movers`; a holder's agent type has one writer (`ClaimHolders.of`) and one reader, held together by a law.
- `/take` runs the coder definitions and the brief hook both machines already have; the manager's by-hand steps become the skill's, so there is one procedure.
- Words: `take` for the skill; "machine", never "worker" ([the vocabulary](artifact:grit/458)). Synonyms are searched at each coder plan for the names it adds (`*Session*` on claims, `*Holders*`, `*AgentType*`).

## Bounds and schema

No new table. One `ALTER`: a nullable `session` column on the claims table, a session id's text under the same `CHECK` on its length that session ids carry elsewhere in the schema; tested against a database built from the previous schema; Nick is given the statement before it reaches `grit_kalandra`. Claims stay one per subject and one per holder, deleted on release or the session sweep, moved on takeover. `oldest` reads at most the kept history of each candidate (20 versions) under the lock. The laptop's unposted event lines are bounded as the hook already bounds them. No model spend: coders run on the subscription.

**Decision records.** K2 changes whose lease a claim's is: ADR 0049 (claims) is revised in place, and ADR 0050 where it says a holder's sessions keep its claims; K2's coder plan confirms which sentences. Nothing else here clears the bar.

## Verification

- Unit and `it` per checkpoint, each new behaviour red first; the law; `ProjectsLiveTests` for claim, takeover and the role refusal through `SqlPluginCommands`.
- kalandra: the method's next version declared (roles' agent types), `restore-test`, then `bin/kalandra deploy` with its proof inside five minutes.
- **Real-use run (R):** two real checkpoints at once, the laptop's taken by `/take`: marked ready on the desktop, built, pushed, moved to review, its report read, reviewed and merged from the desktop. First the plain path. Then: the forward dropped mid-checkpoint and restored (lines arrive late, none lost); the laptop's session closed mid-checkpoint and a new one started (`/take` goes on with what its coder holds); a second session opened in the manager's checkout (one line, no brief), then `takeover`.
- Measured: wall-clock for the pair against the same two built one after the other; free memory on each machine at peak; every prompt the laptop's session stopped on.

## Risks

- **The subscription's limits, not memory, may bound parallel coders.** R measures it; it is the case for the cheaper-coders milestone.
- **A laptop that sleeps:** its claim lapses after 2 h with no event, and the forward must come back by itself. C0 tries both.
- **K2 touches every session's brief.** A fault there shows a manager no brief. It lands with the previous behaviour one revert away and is proven on kalandra before any coder depends on it.

## Waits for Nick

Design 1's reach; K3 below the line and built in R, and M below the line though placed in this milestone; the `ALTER`; the one line another session is shown, the takeover refusal and the role refusal's wording; the method's next version declared live; the deploy; C0, at the laptop.

## Parked

- `grit work` behind the same steps; the laptop attached as an edge; hosted tool calls across machines; `Namespace.Fs` as a named machine. The next milestone, with spawn.
- [Agents as principals](artifact:grit/259); a `holder` checked on `project_move`; an agent under its own credential computing another holder than its parent claimed ([beyond loopback](artifact:grit/484)); people joining by a realm's login ([authentication](artifact:grit/415)).
- A client stalled in its headers (`maxReqTime`, `maxConnections`): with S.
- The DeepSeek provider and its bench: its own milestone.

## Retires

- The manager's by-hand claim, holder lookup and coder start in how-work-is-run: the skill's steps.
- "One session to a checkout while anything shared is changed" (the stopgap in [the rule's note](artifact:grit/477)).
- `project_claim`'s sentence that claims made at the same moment fail "with a store error": under the lock the later one waits.
- The first draft's C1–C4, and its `KALANDRA_MACHINE`, `/spool` and method-v5 text, with this rewrite.
