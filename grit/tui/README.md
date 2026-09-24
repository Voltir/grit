# grit.tui

A fullscreen terminal UI library for Scala 3, in the shape of
[layoutz](https://github.com/matthieucourt/layoutz) — the same small, declarative surface —
but built for an app that has taken the alternate screen and owes the terminal's job back.

This began as the fourth spike in the line, `slate`, ported into grit on 2026-09-23. `../../../Spikes/tui-spike-layoutz` answered *can layoutz carry a
fullscreen transcript with drag-scroll-copy?* — yes, but only by injecting a custom
`Terminal`, and **three of the four bugs it found came from layoutz's own render loop and
scheduler**. `../../../Spikes/tui-spike-fullscreen` built the cell-grid answer on JLine and produced the
`Surface`/`Painter` model this starts from. grit.tui is the merge: layoutz's ergonomics on the
fullscreen spike's foundations.

## Why not keep building on layoutz

Measured on the finished layoutz spike: **4 files imported it, 12 symbols total**, for a
1.0 MB jar. 1188 of its 2018 lines already imported nothing from it. The Elm architecture we
actually wanted was our own code the whole time.

What was layoutz's — a three-thread runtime and a string-joining layout — was what fought us:

| layoutz | cost |
|---|---|
| `Element` is `def render: String` | a String has no per-cell identity, so it cannot be hit-tested, diffed, or asked where a container put it. `Element.width` literally re-renders. |
| render loop writes bare `"\n"` per line | `stty raw` clears `ONLCR`, so the frame painted as a diagonal staircase |
| render loop appends `ESC[K` per line | a line exactly `cols` wide left the deferred-wrap flag set, and the erase ate its last character |
| `Cmd.afterMs` is `Future { Thread.sleep(d); … }` | timers cannot be cancelled and each pins a pool thread; a self-re-arming chain forked and doubled on every round trip |
| `Cmd.fire(=> Unit)` | a thunk inside a command launders capabilities straight past capture checking |
| full-frame rewrite, string-compared | no cell diff; ~3 KB per change over WSL's conpty |

The one that decides it: because `Element` cannot report where anything landed, layout
constants get hardcoded (`transcriptTop = 1`, `gutter = 3`). That is survivable for three
stacked elements and fatal the moment a component sits *beside* another — and hit-testing is
what selection *is*.

## The five ideas

**1. A frame is cells, not a string.**

```scala
final case class Cell(ch: Char, style: Style)
final case class Surface(size: Size, cells: Vector[Cell], panes: Vector[Placement])
                                                        // writes clip, never wrap;
                                                        // blit appends placements
final case class Frame(surface: Surface, cursor: Option[Pos])
```

A component is a **function from state to cells**, composed with `blit` at an offset. Not a
widget framework, not a class hierarchy — `grit.tui` provides the surface algebra and the
runtime, and the app provides functions. `blit` with a `PaneId` records the rect it painted
(innermost last, carried placements translated); `Hit.paneAt` walks the map backwards for
the topmost pane and its pane-local coordinates.

A component never chooses its own size — `render(size)` paints exactly the box it is
handed, at every size, and the test suite asserts that over every view in the library. What
it may do is *report*, through `measure(avail)`, and a region may act on the report:

```scala
PromptPane -> Region.Fit(min = 3, upTo = 0.5)   // the editor says how tall its draft is;
                                                // the region says how tall it may get
```

`Fixed` and `Flex` are declarations; `Fit` is the one that asks. The split is the point —
the editor knows how many rows its draft wraps to and nothing about whether it is allowed
to use them, and a prompt in a `Fixed` region still paints in exactly the box it always
did. A layout holding a `Fit` cannot be resolved without its children, and says so rather
than guessing: `Regions.placed(size)` is the way in.

**2. Rendering yields a placement map, so hit-testing is its inverse.**

`blit` records where it put things. A frame therefore carries `Vector[Placement]` —
`(PaneId, Rect)`, innermost last. `Hit.paneAt(frame, pos)` walks it backwards and returns
the topmost pane plus a *pane-local* position. This is the thing layoutz structurally could
not do, and it is what makes the next idea possible.

**3. A selection belongs to the pane the drag began in, and that pane clamps it.**

Every scrollable pane owns its own document coordinate space and its own selection:

```scala
final case class Drag(pane: PaneId, anchor: DocPos, head: DocPos)
```

Press routes through `paneAt` and binds the drag to that pane for its whole life. Every
later motion is converted into *that* pane's coordinates and clamped to its rect, so
dragging out of an open modal cannot escape into the transcript behind it, and autoscroll
scrolls the pane that owns the drag. Copy asks the pane to serialise its own selection, so
a modal copies modal text and a transcript copies transcript text, with no special cases at
the call site.

Two properties carried forward from the layoutz spike, both hard-won:

- **`DocPos` indexes logical, unwrapped text.** Scrolling moves the anchor and never the
  selection; copying is a slice with trimmed ends and performs no wrapping at all, so the
  clipboard gets what the author wrote rather than what an 80-column viewport broke it to.
- **Both selection bounds project onto a row identically.** Treating them asymmetrically
  paints every row *after* the selection. The copied text stays correct throughout, so only
  reading painted cells catches it.

**4. `Effect` is data. Never a thunk.**

The capture-checking finding from the layoutz spike, stated as a rule. Making the terminal a
`caps.SharedCapability` there failed at the injection seam *and nowhere else* —
`Cmd.fire(term.copyOut(text))` was accepted, and a capability-free `update` signature happily
returned a command holding a closure over it.

So: **no `Effect` case may contain a function.** A sealed ADT of plain data structurally
cannot capture a capability, which makes "`update` is pure" a fact about the types rather
than a fact about a test. Only the interpreter — which holds the terminal capability — turns
an `Effect` into an action, and its signature says so.

Work outside the terminal follows the same rule. `Effect.ToHost(msg)` is plain data: the
runtime hands `msg` to the `Host` the embedding program supplied (`TuiApp.run(app, host)`),
which holds whatever capabilities the work needs and answers, on a thread of its own, by
offering a message back through its `Mailbox`. grit's chat screen sends turns this way
(`grit.app.ChatHost`).

**5. Timers are cancellable, and the scheduler is virtual-thread native.**

A `ScheduledExecutorService` on one platform thread dispatching onto
`Executors.newVirtualThreadPerTaskExecutor()`: cancellable handles, and a blocked timer costs
a continuation rather than a carrier. JDK 26 also means `synchronized` no longer pins a
carrier (JEP 491), so the runtime's locks are free to stay simple.

Nothing self-re-arms without either a cancel handle or a generation stamp. The layoutz spike
needed generation counters *because* its timers could not be cancelled; here they become a
belt-and-braces choice rather than the only defence.

## The standard layer: `grit.tui.runtime.std`

The `App` trait is four pure functions and no opinions. The `std` layer is the opinion: quit,
resize delivery, scroll, and the whole pointer/selection lifecycle are machinery every
pane-driven app was about to copy out of `Demo2`, so the library states them once and the app
states only its own decisions.

It is not one trait but **six mixins over one seam**. `StdBase` holds the state access and the
two chains; `Hotkeys`, `Ambient`, `Scrolling`/`Selecting`, `Modals`, `Prompting`/`Completing`
and `FreeKeys` each own their hooks, their routing layer, and the `Std` messages those hooks
read. `StdApp` assembles all six, so an app that wants everything names one trait:

```scala
enum Msg { case Submit; case ToggleStream; case Tick /* the genuinely app's own */ }
object DemoApp extends StdApp[State, Msg]
```

An app that wants less names the traits it wants, and owes the rest nothing:

```scala
object Viewer extends StdBase[State, Msg] with Ambient[State, Msg] with Selecting[State, Msg] {
  val layers = Vector(ambientLayer, pageLayer, pointerLayer)
  val steps  = Vector(ambientStep, scrollStep, pointerStep)
}
```

Those three abstract members — `onModalClose`, `withEditor`, `withPopup` — are the whole
argument for the split: while the layer was one indivisible trait, an app with no modal still
had to say what closing its modal meant.

`update` and `onInput` are final and already written, and both are a chain of values: `layers`
walked until one claims an input, `steps` walked until one claims a `Std` message. The
symmetry is what makes the layer divisible — a mixin cannot own a hook unless it also owns the
message that reads it, since `Std.EditTo`'s handler calls `withEditor`. The price is honest:
a `Vector[Step]` is not checked for exhaustiveness the way one match over `Std` was, and the
test suite stands where the compiler did.

Both vectors are written out in the assembly rather than accumulated through `super`, because
trait linearization would otherwise let the order of the `with` clauses decide routing
precedence — and precedence is the entire difference between the two halves of binding:
hotkeys before capture (quit, the app's modes), free keys only after every component has
declined (Enter, Tab). The default order is the one Demo2 stated by hand: hotkeys, the
terminal's own facts (a resize, the shift-drag the user's terminal owns), the modal's capture,
the popup's claims, the editor, the free keys, paging, and the pointer.

**Every hook is a pure function by type.** The hooks are declared `A -> B` — the
capture-checked *pure* function type — and never `A => B` (which is `(A -> B)^{cap}`, a
function that may capture anything). Declared as function *values*, a hook that closes over
the terminal or the scheduler is a compile error naming the capability it captured; declared
as methods, a capability reference would fold silently into the enclosing object. `Effect`
remains the only channel to the world, so the hooks are projections over plain data and the
handlers are `State -> (State, Effect[Std | Own])` value-to-value functions.

What the layer cannot know — which pane a scrollbar's wheel redirects to, what a submission
means, what a finished drag says — is exactly what the hooks are for: `onPress` claims a press
(the thumb drag is not a text drag), `onResize` lays out, `onCopy` writes the status note.
A hook that turns out to have exactly one possible implementation across Demo2 and grit's
first screen collapses into the layer; a hook that fights a grit need gets deleted like any
opinion that loses to evidence.

The core `App` seam is typed the same way: `update` and `onInput` are `A -> B` members, and
`view` is a curried `State -> (Size -> Frame)`, so an app written without the standard layer
gets the same compile error for a capability-reaching lambda. The members are `val`s on
purpose: a `def` member is a method, and a method called from inside a pure lambda captures
`this` — the machine's own dispatch could not have been typed `A -> B` if it had to reach its
hooks through method calls.

## Performance goals

- **`view` is O(viewport), never O(document).** Wrapping threads a cache keyed by
  `(contentId, revision, width)` and is computed in `update`, not in `view`.
- **The painter emits changed cells only**, coalescing horizontally adjacent runs that share
  a style, one `CUP` per run, `SGR` only on style change.
- **One write per frame**, wrapped in `ESC[?2026h`/`l`, addressed absolutely.
- Measured against `../../../Spikes/tui-spike-layoutz` and `../../../Spikes/tui-spike-fullscreen`: bytes/frame at rest
  and streaming, keypress→paint latency.

## Rules the terminal taught us

Non-negotiable, each paid for in the layoutz spike:

1. **Address every line absolutely** (`ESC[<row>;1H`). Never rely on `ONLCR`; `stty raw`
   clears it and a bare LF paints a staircase.
2. **Never write into the last column.** Deferred wrap leaves the cursor there and a
   following `ESC[K` erases the character just written. The rule lives in exactly one
   place: the runtime hands apps the paintable screen (`Size.screen`), so a frame is
   already one column narrower than the terminal and nothing downstream compensates.
3. **Restore in reverse**, and make it idempotent — it will be called from cleanup, from a
   `finally`, and from a shutdown hook.
4. **Verify by painting, not by diffing bytes.** Mode sequences balanced and a frame of
   exactly N lines prove nothing about whether it paints; the right text on the clipboard
   proves nothing about what the user was shown. A VT model (CUP/LF/CR/EL/ED/wrap/SGR) in
   the test suite is worth more than another assertion about bytes — a ~50-line one found
   three of the four bugs next door.
5. **Never fight the user's escape hatches.** Shift-drag stays unbound so the terminal's own
   selection still works.

## Layout

One Mill module, `grit.tui`, in four groups — `model`, `components`, `wire`, `runtime` —
plus `grit.tui.examples` for everything runnable. The group table, what each may import,
and how it is enforced are in [`CLAUDE.md`](CLAUDE.md).

Escape bytes are spelled in exactly three places and nowhere else: `wire/paint/Ansi` (out),
`wire/input/Decoder` (in), and `wire/term/` (mode setting). Binding — what an input
*means* — is not in the library at all: it needs the app's message type and the state the
event arrives in, so the app owns it.
