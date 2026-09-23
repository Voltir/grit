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
- **With `--bspNoKillOther` but stale locks:** each abandoned server queues on
  the BSP lock and never answers `build/initialize`; Metals retries every 60s,
  leaking one zombie Mill daemon per minute. If you see `waiting for it to be
  done...` repeatedly plus `Timeout waiting for 'build/initialize'`, kill all
  Mill processes and delete `.bsp/out/mill-bsp-Metals-lock`,
  `.bsp/out/mill-active-bsp-Metals.json` (Zombie daemons also ignore SIGTERM —
  use `kill -9`).

## The workaround (installed 2026-09-02)

Three pieces:

1. **`~/.local/bin/mill-bsp-wrapper`** — resolves the project's Mill version
   (`MILL_VERSION` env → `.mill-version` in cwd/ancestors → newest in
   `~/.cache/mill/download`), then execs the matching native binary, appending
   `--bspNoKillOther` **only** for `--bsp` launches on Mill ≥ 1.0 (the flag
   doesn't exist in 0.12.x and would break those projects).
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
MILL_WRAPPER_BIN=/bin/echo MILL_WRAPPER_DEBUG=1 mill-bsp-wrapper --bsp --jobs 0

# check the env var reached the VS Code server's world
bash -lc 'echo $MILL_EXECUTABLE_PATH'
```

## How to undo (once upstream is fixed)

1. Remove the `MILL_EXECUTABLE_PATH` export from `~/.profile`.
2. Delete `~/.local/bin/mill-bsp-wrapper`.
3. Delete each project's `.bsp/mill-bsp.json` (regenerates pointing at the real
   binary).

## Upstream

- [Mill #7451](https://github.com/com-lihaoyi/mill/issues/7451) — "Mill crash
  loops Metals calling SIGKILL..." (open at time of writing; the reporter saw it
  with `MILL_NO_SEPARATE_BSP_OUTPUT_DIR` + Metals V2, but the kill-other
  mechanism is the same).
- Metals side: the duplicate-connection trigger was observed on a **snapshot**
  build (`1.6.8+50-40d2323f-SNAPSHOT`). If this recurs, try switching the Scala
  extension to the stable release channel before debugging further.

## Related facts that were true when this was written

- Latest stable Mill: 1.1.8 (2026-08-07); 1.2.0-RC1 was the only newer build.
- Mill 1.1.8's `killOther` default is set in `MillMain0.scala`; the hidden
  `--bspNoKillOther` CLI flag is documented as "If the BSP lock is hold by
  another process, wait for it to release the lock".
- Mill daemons trap SIGTERM; `kill -9` is required for cleanup.
- Re-checked 2026-09-23 after moving to Mill 1.1.10: `--bspNoKillOther` is still accepted
  (an unknown flag is rejected with "Cannot resolve"), so the wrapper still applies.
