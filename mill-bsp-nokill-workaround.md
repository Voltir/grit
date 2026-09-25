# Mill BSP crash-loop with Metals — diagnosis and workaround

*Written 2026-09-02 after a full morning of debugging. Read this before touching
anything in `.bsp/` or the Metals extension settings.*

## Symptom

VS Code (Scala/Metals) crash-loops when opening a Mill project: the build server
dies and restarts every ~2 seconds, forever. `.metals/metals.log` shows:

```
BSP server: bsp] Sent SIGTERM to process 1316364
BSP server: Received SIGTERM, exiting
BSP server: bsp] Sent SIGKILL to process 1316364
1-buildInitialize] buildInitialize took 2082 msec
Running BSP server List(..., --bsp, --jobs, 0)   <- repeat, forever
```

## Root cause (confirmed in Mill 1.1.8 source)

1. Metals opens a BSP connection; something makes it open a **second** connection
   ~2s later (or retry after a 60s `build/initialize` timeout).
2. Mill 1.x BSP servers enforce single ownership of a workspace: on startup, if
   the BSP lock is held, the new server **SIGTERMs — then SIGKILLs — the previous
   server** (`MillBuildServer.initLock`, `killOther = true` by default,
   `MillMain0.scala:357`).
3. The killed server is the one Metals is talking to → Metals times out
   (`Timeout waiting for 'build/initialize' response`) → retries → the new server
   kills the current one → infinite duel.

Two failure modes depending on configuration:

- **Default (kill-other):** fast kill-loop, ~2s per cycle, hundreds of `pid-*`
  dirs pile up in `.bsp/out/mill-no-daemon/`.
- **With `--bspNoKillOther`, before 2026-09-25:** one leaked JVM per minute. See
  the next section; the wrapper now prevents it.

## Second failure: the leak (found 2026-09-25)

`--bspNoKillOther` made a new server *wait* for the lock. Metals' log showed, every
60s, `Another Mill BSP server is running with PID … waiting for it to be done...`,
then `Timeout waiting for 'build/initialize' response`, then a fresh
`Running BSP server`. 23 `mill.daemon.MillBspMain` JVMs had piled up, 11 of them
about 13h old, all parented to `/init`, and the lock holder was one whose client
was gone. Build import never finished, so every Metals MCP query timed out.

Why the JVMs never exited (Mill 1.1.10 source, Metals `ee0e9da8`):

1. The lock is taken **inside `build/initialize`** (`Endpoints.buildInitialize` →
   `MillBuildServer.doneInitializingBuild` → `initLock` → blocking `bspLock.lock()`),
   so a waiting server never answers and Metals' hard-coded 60s timeout fires
   (`BuildServerConnection.initialize`, `get(60, SECONDS)`; there is no setting).
2. On the timeout Metals runs `SystemProcess.cancel`: `destroy()` (SIGTERM), 200ms,
   `destroyForcibly()` (SIGKILL) — **on its direct child only**. That child is
   Mill's native launcher, which starts the `MillBspMain` JVM with inherited stdio
   and `destroyOnExit = false` (`MillProcessLauncher.configureRunMillProcess`).
   The launcher dies; the JVM is orphaned.
3. The JVM then sees EOF on stdin, but in 1.1.x EOF only ends the JSON-RPC listener
   (`BspWorkerImpl`, "Shutting down executor"). The main loop waits for a session
   result that only `build/exit` completes, and the initialize thread stays blocked
   on the lock. SIGTERM does nothing either: `MillMain0` installs a handler that
   prints `Received SIGTERM, exiting` and carries on.

The same holds for the holder: when its Metals goes away (VS Code window reload),
it keeps running, and keeps the lock, forever. With kill-other that was masked
because the next server killed it; with wait-for-lock every retry queues behind it.

Reproduced in a scratch project with the old wrapper: holder A plus waiter B,
Metals-style cancel of B → B's JVM reparented to `/init`, still waiting; after A's
client exited, A's JVM too.

Cleanup, if it ever happens again: `kill -9` the `MillBspMain` JVMs (they ignore
SIGTERM). The lock is an OS file lock, so it goes with its holder; the
`.bsp/out/mill-bsp-Metals-lock` and `mill-active-bsp-Metals.json` files are harmless.

## The workaround (installed 2026-09-02)

Three pieces:

1. **`~/.local/bin/mill-bsp-wrapper`** — resolves the project's Mill version
   (`MILL_VERSION` env → `.mill-version` in cwd/ancestors → newest in
   `~/.cache/mill/download`) and finds the matching native binary. Anything but a
   `--bsp` launch on Mill ≥ 1.0 is `exec`ed unchanged (0.12.x doesn't know the flag).
   A `--bsp` launch on Mill ≥ 1.0 gets `--bspNoKillOther` appended and is
   **supervised** (since 2026-09-25) so it cannot outlive its client:
   - Mill runs under `setsid`, so the native launcher and its JVM are one process
     group, with stdin relayed through a FIFO by a `cat` the wrapper owns.
   - When the client closes stdin, or the wrapper gets SIGTERM/SIGINT/SIGHUP, the
     wrapper SIGTERMs the group and SIGKILLs it after `MILL_WRAPPER_KILL_GRACE`
     seconds (default 2; the JVM ignores SIGTERM).
   - A watchdog (`tail --pid=<wrapper>`) does the same if the wrapper is SIGKILLed,
     which is what Metals' cancel does 200ms after its SIGTERM.
   - If Mill exits on its own (`build/exit`), the wrapper exits with Mill's code.
   Only the wrapper's own child group is ever signalled, never another client's
   server.
2. **`~/.profile`** exports `MILL_EXECUTABLE_PATH=$HOME/.local/bin/mill-bsp-wrapper`.
   Mill's BSP connection-file generator (`BSP.scala:37`) writes this path into
   every project's `.bsp/mill-bsp.json`, and Mill's launcher defers to an
   externally-set value (`MillProcessLauncher.scala:90`) — so all Mill projects
   get the fix automatically.
3. **`.bsp/mill-bsp.json`** in affected projects points `argv[0]` at the wrapper.
   Metals reuses this file as long as the mill version matches, so it needed a
   one-time hand-edit (deleting it also works — it gets regenerated via the
   wrapper once the env var is in the VS Code server's environment).

## New projects

- **No `.bsp/mill-bsp.json` yet:** after the VS Code WSL window has been restarted once
  (so `MILL_EXECUTABLE_PATH` is in the server env), Metals generates it pointing at the
  wrapper automatically — do nothing.
- **Json already exists:** Metals reuses it while the Mill version matches, so it needs a
  one-time edit:

  ```bash
  python3 - <<'EOF'
  import json
  p = "PATH/TO/PROJECT/.bsp/mill-bsp.json"
  d = json.load(open(p))
  d["argv"][0] = "/home/nickchilders/.local/bin/mill-bsp-wrapper"
  json.dump(d, open(p, "w"), separators=(",", ":"))
  EOF
  ```

- **Verify:** `grep "Running BSP server" PROJECT/.metals/metals.log | tail` should show
  `List(/home/nickchilders/.local/bin/mill-bsp-wrapper, --bsp, --jobs 0)`. (The injected
  `--bspNoKillOther` is invisible on that line — the wrapper appends it when exec'ing the
  real binary; use `MILL_WRAPPER_DEBUG=1` to see it.)

Projects on Mill 0.12.x need nothing — the wrapper detects the version and does not inject
the flag there.

## Debugging / testing the wrapper

```bash
# dry-run: show what would be executed without running Mill
MILL_WRAPPER_BIN=/bin/echo MILL_WRAPPER_DEBUG=1 mill-bsp-wrapper --bsp --jobs 0 </dev/null

# a supervised BSP server is a process group led by the native launcher
ps -eo pid,ppid,pgid,etime,args | grep -E 'mill-bsp-wrapper|native-linux-amd64 --bsp|MillBspMain' | grep -v grep

# check the env var reached the VS Code server's world
bash -lc 'echo $MILL_EXECUTABLE_PATH'
```

The 2026-09-25 leak test drove the wrapper from a Python script in a scratch Mill
1.1.10 project: holder A initialized; waiter B queued on the lock; then B was given
a Metals-style cancel (SIGTERM, 200ms, SIGKILL, close stdin), or just had stdin
closed, or A had stdin closed. With the new wrapper B's JVM was gone within the
grace period in every case, B got the lock within ~2s when A's client went away,
`build/exit` still returned exit code 0, and no JVM, `cat` or FIFO was left
behind once the driver exited.

## How to undo (once upstream is fixed)

1. Remove the `MILL_EXECUTABLE_PATH` export from `~/.profile`.
2. Delete `~/.local/bin/mill-bsp-wrapper`.
3. Delete each project's `.bsp/mill-bsp.json` (regenerates pointing at the real
   binary).

## Upstream

- [Mill #7451](https://github.com/com-lihaoyi/mill/issues/7451) — "Mill crash
  loops Metals calling SIGKILL..." Closed 2026-09-17 as fixed "by 1.1.8", but
  without an identified fix; the kill-other code is unchanged in 1.1.10. The Metals
  maintainer points at [metals#8867](https://github.com/scalameta/metals/pull/8867)
  (merged 2026-09-18: shut down a session that failed to connect). Nick's snapshot
  `1.6.8+57-ee0e9da8` predates it, and it still only signals the direct child.
- **Mill 1.2 fixes the leak at the source:** in `1.2.0-RC1` and `1.3.0-M1`, stdin
  EOF ends the session (`BSP-Shutdown-Listener-Thread` completes `shutdownPromise`),
  and the BSP lock and `killOther` are gone: by default BSP shares the daemon and
  `out/` with the CLI. Once 1.2.0 is final, upgrading `.mill-version` should make
  both halves of this wrapper unnecessary; verify before removing it.
- Metals side: the duplicate-connection trigger was observed on a **snapshot**
  build (`1.6.8+50-40d2323f-SNAPSHOT`). If this recurs, try switching the Scala
  extension to the stable release channel before debugging further.

## Related facts that were true when this was written

- Latest stable Mill: 1.1.8 (2026-08-07); 1.2.0-RC1 was the only newer build.
- Mill 1.1.8's `killOther` default is set in `MillMain0.scala`; the hidden
  `--bspNoKillOther` CLI flag is documented as "If the BSP lock is hold by
  another process, wait for it to release the lock".
- Mill daemons trap SIGTERM; `kill -9` is required for cleanup.
- Metals' 60s `build/initialize` timeout is hard-coded (20s for Bloop); no user
  setting changes it.
- Re-checked 2026-09-23 after moving to Mill 1.1.10: `--bspNoKillOther` is still accepted
  (an unknown flag is rejected with "Cannot resolve"), so the wrapper still applies.
