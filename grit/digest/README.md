# grit.digest

The hello-world plugin (ADR 0011), and the interface's first consumer: `Digest` keeps one
document per closed period, keyed by its close ordinal, holding when it closed, where, why,
and its headline (its outcome, or its prose's first sentence). `Digest.recentActivity` is
its surface: the `recent_activity` tool, free because it only reads, listing the newest
lines. On with `GRIT_PLUGINS=digest`; enabled on a database with closed periods, it posts
them all from its cursor at the start.

One idea, so one package, `grit.digest`. Names only `grit.core`.
