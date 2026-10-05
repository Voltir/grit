# grit.digest

Implements `Plugin` (what a deployment can supply:
[`docs/extending.md`](../../docs/extending.md#extension-points), where `Digest` is the
worked example).

The hello-world plugin (ADR 0011), and the interface's first consumer: `Digest` keeps one
document per closed period, keyed by its close ordinal, holding when it closed, where, why,
and its headline (its outcome, or its prose's first sentence); none for a period closed
unearned (ADR 0020). Its documents are a cache: each is deleted
with the closing it was posted from, and a new version rebuilds them from the closings kept. `Digest.RecentActivity` is
its tool: `recent_activity`, which every turn is offered (the kit offers any plugin's tools
the same way), free because it only reads, listing the newest lines. On with `GRIT_PLUGINS=digest`; enabled on a database with closed periods, it posts
them all from its cursor at the start.

One idea, so one package, `grit.digest`. Names only `grit.core`.
