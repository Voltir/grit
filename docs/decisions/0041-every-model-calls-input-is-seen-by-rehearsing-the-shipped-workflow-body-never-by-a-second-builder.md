# 0041. Every model call's input is seen by rehearsing the shipped workflow body, never by a second builder

Status: accepted (2026-10-17)

Context: to judge what grit shows a model, a person needs each call's input as the engine
would send it now: triage's questions, a placement's, the turn's query writer and relevance
judge, and the turn's own request. The calls are made inside workflow bodies, each building its
input from stores, settings and earlier calls in the chain. Turned down: a builder per family
that assembles a call's input outside its workflow (each a second copy of its family's code,
silently wrong after the first change to the original, and blind to a new call until someone
writes its builder); reading recorded workflows only (shows what was sent then, never what a
message would get today, and nothing before a first real run); running the real engine on a
copy of the database (minutes per look, paid calls, and a copy to keep current).

Decision:

- **A rehearsal runs the shipped bodies.** The kit's `Rehearse` launches the deployment's
  workflows exactly as a live launch does (`Launch.asking`), on a `Rehearsing` session over the
  live database: one transaction, every write a savepoint of it, rolled back when the session
  ends. Each workflow the message starts runs once, in the engine's order, through a `Durable`
  that runs every step and records nothing (`Rehearsal`).
- **Calls are seen at the taps.** The rehearsal's `Models` and classifier keep each call with
  the step it was made in and answer it from a stand-in (`Answers`, pure, so a dry rehearsal
  can hold no model), until the turn's own call (`Turn.Step.answering`), which is kept and
  never answered. What a rehearsal shows is what the taps were asked, by part, each with the
  label its step's record or the turn's route record names.
- **A new call joins with no edit to the rehearsal.** Made through its env's `Models` or
  classifier inside a step of a shipped body, it is on the tape under its step's name.

Consequences:

- A family builds its call's input through its env's capabilities inside a step body, and
  never through a path the session's transaction cannot see: a second connection, a thread
  that outlives its step, a store opened outside the env. Such a path would read the database
  without the rehearsal's own writes, and show an input the engine would not send.
- A rehearsal holds row locks for at most its session's cap, and leaves nothing but the
  sequence values its inserts drew.
- What the rehearsal shows is only as faithful as its stand-ins: a stub answer steers the rest
  of the chain, and the output says which answers stood in.
- Enforced by probes (a session, its reads and its writes never leave its block; a dry
  rehearsal's answers hold no provider) and by an integration test that the input a dry
  rehearsal shows equals what the engine's run sends (`RehearseLiveTests`).
