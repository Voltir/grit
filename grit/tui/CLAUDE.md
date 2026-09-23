# grit.tui

The terminal UI. Ported 2026-09-23 from `../Spikes/tui-spike-slate`, where it was the
library `slate`; the name is retired and so is the generality — grit is its only consumer,
and it is tailored to the harness. **[`README.md`](README.md) is the design**; every rule
below exists because something in it was paid for. Comments citing "ROADMAP" decisions
dated before 2026-09-23 refer to the spike's table, archived unedited at
`.local/history/slate/ROADMAP.md`. New decisions go in grit's `roadmap/decisions.md`.

## Where it sits

Mill module `grit.tui`, depending on `grit.core` and nothing else — no external
dependencies; the terminal seam is hand-rolled over FFM, not JLine. `grit.tui.examples`
holds everything runnable (`Demo2` is the gate's target until `grit.app`'s screen exists).

Four groups, and the groups are the layer table:

- **`model`** — `surface`, `text`, `input`, `select`, `block`: the pure cell model and the
  event vocabulary. Imports no other group.
- **`components`** — `View` (`render(size)` paints exactly the box it is given;
  `measure(avail)` only *reports*), `layout`, `widget`, `overlay`, `pane`, `editor`.
  The API surface. Imports only `model`.
- **`wire`** — `paint`, `term`, and input's `Decoder`: bytes out and bytes in. Imports only
  `model`.
- **`runtime`** — the loop, `Effect`, `Scheduler`, and the `std` layer. The only group
  allowed to name all three.

`components` and `wire` are siblings and must not name each other: that is what keeps the
surface grit's screens are written against separate from the terminal underneath.
**`grit.core` may be imported from `components` and `runtime` only** — never `model` or
`wire`, so the cell model and the terminal seam stay testable with no domain fixtures.
Import direction is enforced twice, deliberately: by enola's three `tui-*` rules in
`enola-intent.yaml` (gated by `scripts/enola-law.sh`), and by `test/src/QuarantineTests.scala`'s
source scan, which also catches the fully-qualified reference with no import that enola
cannot see. Content rules — no function in `Effect`, escape bytes in three files, the
terminal touched in one package — are the test's alone.

## Commands

```bash
./mill grit.tui.test
scripts/tui-gate                          # six pty scenarios, 89 checks, against Demo2
scripts/tui-gate modal                    # one scenario
./mill --no-daemon --no-build-lock grit.tui.examples.runMain grit.tui.examples.Demo2
./mill grit.tui.examples.runMain grit.tui.examples.Snapshot 30 100   # one frame, no tty
```

A compile landing while an example is up rewrites `out/grit/tui/.../classes` underneath the
live JVM; restart after recompiling.

## Design rules that are not negotiable

Each was paid for in `../Spikes/tui-spike-layoutz`. Do not relax one without reading its FINDINGS.

1. **Address every line absolutely** (`ESC[<row>;1H`). `stty raw` clears `ONLCR`, so a bare
   LF is a line feed with no carriage return and the frame paints as a diagonal staircase.
2. **Never write into the last column** (`paintCols = cols - 1`, or handle deferred wrap
   explicitly). Deferred wrap parks the cursor there and a following `ESC[K` erases the
   character just written.
3. **`Effect` is a sealed ADT of plain data — no case may contain a function.** A thunk in a
   command launders capabilities past capture checking; this is what makes `update`'s purity
   a property of the types rather than of a test.
4. **Timers are cancellable.** Nothing self-re-arms without a cancel handle or a generation
   stamp. An uncancellable self-re-arming chain forks every time its trigger condition is
   left and re-entered, and doubles.
5. **Both selection bounds project onto a row identically.** Asymmetric projection paints
   every row *after* the selection while leaving the copied text correct — invisible to
   anything that inspects the model or the clipboard.
6. **A drag belongs to the pane it began in, and that pane clamps it.** No selection may
   escape a modal into what is behind it.
7. **`view` never wraps and never allocates per cell in the hot path.** Wrapping happens in
   `update`, threading a cache keyed by `(contentId, revision, width)`. The one wrap that
   now happens during layout is `Editor.measure`, which a `Region.Fit` asks for: it is over
   the *draft*, not a document, and bounded by the prompt's own height. Rule 7 is about
   documents; do not read this as a licence to measure one.
8. **Restore terminal state in reverse, idempotently.** It is called from cleanup, from a
   `finally`, and from a shutdown hook.
9. **Never fight the user's escape hatches.** Shift-drag stays unbound.

## Verify by painting, not by diffing bytes

The lesson that matters most. Balanced mode sequences and a frame of exactly N lines prove
nothing about whether it *paints*; correct clipboard content proves nothing about what the
user was *shown*. Three of the four bugs in the layoutz spike were invisible to byte-level
checks and to 64 passing tests.

So the test suite ships a VT model — CUP / LF / CR / EL / ED / wrap / SGR — and asserts
against the painted grid: the frame never scrolls, is always exactly `rows` lines, is never
wider than `cols - 1`, and **the reverse-video cells equal what the selection model says**.
A prototype lived at `../Spikes/tui-spike-layoutz` scratch (`emu.py`); the Scala port is
`grit/tui/test/src/wire/paint/Vt.scala`.

Prefer `./mill grit.tui.examples.runMain grit.tui.examples.Snapshot 30 100` over a real run — it renders the same pure view with no
terminal. When the real thing is needed, give it a pty *with a size* (an unsized pty reports
0x0) and script mouse bytes on stdin. SGR reports are `ESC [ < b ; col ; row M` (`m` for
release), 1-based:

```bash
E=$'\033'
{ sleep 8
  printf "%s" "${E}[<0;21;21M${E}[<32;21;15M${E}[<32;20;2M"   # press, drag, park at top
  sleep 2; printf "%s" "${E}[<0;20;2m"                         # release -> copy
  sleep 1; printf '\021'                                       # ctrl-q
} | setsid script -qc "stty rows 30 cols 100; $RUNNER" /dev/null > /tmp/run.txt 2>&1
```

`scripts/tui-gate [span|one|modal|thumb|popup|resize]` is this recipe, checked in and repeatable — read it
before rebuilding one by hand. What follows is what it does and why.

**Pass `--ticker false` when capturing.** Mill forwards stdin fine and the app quits
normally through it, but the ticker draws progress onto the same pty and lands in the
capture. Call mill directly for a capture:

```bash
./mill --no-daemon --no-build-lock --ticker false grit.tui.examples.runMain grit.tui.examples.Smoke
```

Allow generous startup: a no-daemon run compiles first, so sleep ~12s before scripting
input or the keystrokes arrive before the app is listening. To take mill out of the
picture entirely (fastest iteration, cleanest capture), build a direct runner and re-make
it after each recompile:

```bash
CP=$(./mill show grit.tui.examples.runClasspath 2>/dev/null | grep -oP '"q?ref:v[01]:[0-9a-f]+:\K[^"]+' | tr '\n' ':' | sed 's/:$//')
"$GRIT_JDK_HOME/bin/java" --sun-misc-unsafe-memory-access=allow \
  --enable-native-access=ALL-UNNAMED -cp "$CP" grit.tui.examples.Smoke
```

Frames are delimited by `ESC[?2026h` … `ESC[?2026l`, one per frame. Check the OSC 52 payload
(`ESC ] 52;c;<base64>`) for what was copied, and count `?1049h`/`l`, `?1002h`/`l`,
`?1006h`/`l` — each must appear exactly once or teardown is leaking terminal state.

**Clean up afterwards, every time.** Mill forks the app JVM as a child, so killing the
launcher leaves a headless demo alive holding ~400 MB:

```bash
P=examples.De; pgrep -af "${P}mo2"    # any with no tty are leaked
P=examples.De; pkill -9 -f "${P}mo2"  # -9: the app ignores SIGTERM in raw mode
```

The split is not superstition: a plain `pkill -f 'examples.Demo2'` matches the shell running the
pkill and kills the rest of your own command line with it.

**And it matches the developer's own demo.** A script that cleans up after itself must
kill only what *it* started — `scripts/tui-gate` stamps each JVM it forks with `--gate-<pid>-<epoch>`
and matches that. The interactive form above is for a human who knows what is running;
never put it at the end of a script.

## Scala specifics

- **Capture checking is on project-wide.** Iterator-producing combinators are the risk area:
  `args.sliding(2).collectFirst { … }` is rejected with `Illegal capture reference`; indexed
  `Vector` code is the way out.
- `caps.Capability` is **sealed** — `caps.SharedCapability` is the extension point for making
  the terminal a tracked capability.
- `-no-indent` makes significant indentation a compile error. Braces everywhere.
- Virtual threads are load-bearing (blocked timers and the input reader). JDK 26 means
  `synchronized` no longer pins a carrier (JEP 491), so runtime locks can stay simple — but
  do not assume that on an older JDK.
