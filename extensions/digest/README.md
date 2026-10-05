# grit.digest

Implements `Plugin` (what a deployment can supply:
[`docs/extending.md`](../../docs/extending.md#extension-points), where `Digest` is the
worked example).

The hello-world plugin (ADR 0011), kept as documents (ADR 0028). `Digest` keeps one document
per room (`Origin.room`: a TUI session's directory, a Slack channel, a task's name) where
conversations closed, kept at that room: its newest `Digest.Lines` closings' lines, newest
first, each when it closed, where, why, and its headline (its outcome, or its prose's first
sentence); none for a period closed unearned (ADR 0020). Its data holds the same lines with
their close ordinals, so a closing posted again writes nothing. A room's document is written
as of its newest closing's close, never a clock.

Retrieval draws a room's document into a window whose scope reaches the room, ranked beside
entries in one pool and budget, headed by Digest's label. Its terms (`Digest.Terms`): the
label "digest: conversations closed here most recently", weight 1, kept 30 days after a
version stops being current, at most 1000 rooms. **Weight 1 is a placeholder, not a ranking
result**: it leaves the document index's scores unscaled until the eval measures a weight.

`Digest` exports `Activity`, its lines read back from its documents, newest first across
rooms. `Digest.RecentActivity` is its tool over it: `recent_activity`, which every turn is
offered (the kit offers any plugin's tools the same way), free because it only reads. On with
`GRIT_PLUGINS=digest`; enabled on a database with closed periods, it posts them all from its
cursor at the start, and a new version posts them all again.

One idea, so one package, `grit.digest`. Names only `grit.core`.
