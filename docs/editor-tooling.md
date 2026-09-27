# Build and editor tooling

Moved out of `CLAUDE.md` on 2026-09-23: it is troubleshooting for Metals/VSCode, needed when
something is wrong rather than every session.


**Mill's lock is per-build-directory, and Metals does not share ours.** Mill takes an
exclusive lock on a build directory for the duration of an evaluation, but the `mill --bsp`
server VSCode starts builds into `.bsp/out/`, with its own `.bsp/out/mill-out-lock`. Verify
any time with `fuser -v out/mill-out-lock .bsp/out/mill-out-lock` -- different holders. So
a CLI `./mill` run cannot block Metals, and Metals cannot block it. Only two CLI
invocations contend with each other. Since Mill 1.2 that takes the
`mill-separate-bsp-output-dir: true` header at the top of `build.mill`: without it BSP
shares `out/` and the CLI's daemon, its launcher never notices Metals closing stdin, and a
Metals cancel mid-request restarts the daemon under every CLI run. With it, each BSP
connection is its own JVM, which exits when Metals closes stdin or cancels it.

**A "compiling" spinner that never resolves is almost never a lock.** It is usually a stale
Metals progress item -- a failed or superseded compile can orphan the notification, and
VSCode keeps counting against a task that already returned. Check before restarting
anything:

```bash
grep 'time: compiled' .metals/metals.log | tail    # real compiles finish in seconds
tail -40 .metals/metals.log
```

If the log shows `done compiling` and a recent `buildTargetCompile took 0 msec` heartbeat,
the build is idle and green. Clear the spinner with **Metals: Restart build server**. Long
silent stretches in the log -- no requests at all for tens of minutes -- mean the VSCode
extension host stalled, not that Mill was blocked.

**Keep the editor out of the build directories.** `out/` is ~150MB and `.bsp/out/` churns
on every compile; watching them is what stalls the extension host. `.vscode/settings.json`
excludes them. Mill uses `out/`, **not** `target/` -- an sbt-style `**/target/**` exclude
matches nothing here and silently leaves both trees watched.

**Keep BSP out of loose `.scala` files.** Mill's BSP offers every `.scala` file outside a
module's sources as a single-file script module, and Metals compiles each on import. The
`bspScriptIgnore` line in `build.mill`'s header adds `**/.local/` to Mill's defaults (which
already skip `src/`, `out/`, `target/`); without it, the vendored examples in `.local/` were
compiled, and failed, on every build-server start. Check the effective list with
`./mill --meta-level 1 show bspScriptIgnoreAll`, and extend the header if a new tree of
non-module Scala lands in the repository.

`.bsp/out/mill-no-daemon/` accumulates a sandbox directory per BSP process and is never
reaped by Mill. It has run into five figures in the spike. If it grows into the thousands,
stop the build server first, then clear it -- deleting them under a live server kills it:

```bash
ls .bsp/out/mill-no-daemon | wc -l
rm -rf .bsp/out/mill-no-daemon/*
```

`.bloop/` is a leftover from the pre-Mill-BSP setup and is not used.

**One BSP JVM per connection, and it exits when Metals lets go.** With the separate
output directory (above), a BSP launch is a native launcher and one `MillBspMain` JVM.
The JVM exits within a second of its stdin closing, whether Metals closes it, cancels the
connection (SIGTERM, 200ms, SIGKILL on the launcher, then the pipe closes) or exits; a
second server beside a live one neither waits for it nor kills it. So grit needs no
wrapper around Mill's BSP launch: Mill 1.0/1.1's BSP lock and kill-other, which made one
necessary, are gone. Killing only the launcher while the pipe stays open leaves the JVM
running until the pipe closes.

On Metals `1.6.8+57-ee0e9da8-SNAPSHOT`, Metals opened two connections at start, seconds
apart, and kept the first idle until it exited: one extra ~0.5GB `MillBspMain` per
Metals session, parented to Metals, is that and not a leak. An orphan is a `MillBspMain`
whose parent is `/init` or a Metals that is gone. They ignore SIGTERM; `kill -9` them:

```bash
ps -eo pid,ppid,etime,rss,args | grep '[M]illBspMain' | cut -c1-120
```

## Metals for grit's own development

**Registering the MCP server.** Metals serves MCP over HTTP; `.mcp.json` registers it for
Claude Code as `grit-metals`, with the port Metals wrote to `.metals/mcp.json`. The port
has stayed the same across Metals restarts; if Metals ever picks another, copy the new one
into `.mcp.json`. `list-modules` is the cheapest check that it answers. What agents use it
for is in [`CLAUDE.md`](../CLAUDE.md#metals).

**`inspect` ignores `module`.** It resolves the symbol in the target of `fileInFocus`, or,
without one, in the first build target Metals lists, which need not see the symbol. So
`inspect {fqcn: "grit.app.chat.Commands", module: "grit.app"}` returns nothing although
its footer says `Inspected from 'grit.app' module`, and with `fileInFocus` set to
`grit/app/src/chat/Commands.scala` it lists the object. `get-docs` and `get-usages`
honour `module`. For a generic class, `inspect` lists only the companion's members; use
`get-docs`.

**Metals serves this checkout, not a git worktree.** Its `compile-*` and `test` build the
main checkout's sources. In a worktree, type-check with the narrowest
`./mill` target, `grit.<module>.compile` or `grit.<module>.test.compile`,
which answers in about a second on a warm build directory. Metals' read queries are still
right there for code the worktree's branch has not changed.
