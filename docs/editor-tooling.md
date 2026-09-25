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

`.bsp/out/mill-no-daemon/` accumulates a sandbox directory per BSP process and is never
reaped by Mill. It has run into five figures in the spike. If it grows into the thousands,
stop the build server first, then clear it -- deleting them under a live server kills it:

```bash
ls .bsp/out/mill-no-daemon | wc -l
rm -rf .bsp/out/mill-no-daemon/*
```

`.bloop/` is a leftover from the pre-Mill-BSP setup and is not used.

**grit is on Mill 1.3, which has no BSP lock and no kill-other**, so neither failure below
can happen here; the wrapper passes Mill ≥ 1.2 through untouched (1.2+ rejects
`--bspNoKillOther`). The rest of this section is for projects still on Mill 1.0/1.1.

**If the build server crash-loops (SIGTERM/SIGKILL cycles in `.metals/metals.log`)**, it is the
Mill 1.0/1.1 BSP kill-other behavior dueling with Metals reconnects — not a build error. A
global wrapper (`~/.local/bin/mill-bsp-wrapper`, activated via `MILL_EXECUTABLE_PATH` in
`~/.profile`) injects `--bspNoKillOther` to defuse it, and supervises each BSP server so it
is killed when its client closes stdin or cancels it: Mill 1.1.x never exits on its own
then, and waiting servers used to pile up at one JVM per 60s Metals retry. Full
diagnosis, symptom signatures, and undo instructions in
[`mill-bsp-nokill-workaround.md`](../mill-bsp-nokill-workaround.md).
