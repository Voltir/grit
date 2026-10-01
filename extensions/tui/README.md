# grit.tui

A fullscreen terminal UI library for Scala 3, in the shape of
[layoutz](https://github.com/matthieucourt/layoutz) — the same small, declarative surface —
but built for an app that has taken the alternate screen and owes the terminal's job back.

This began as the fourth spike in the line, `slate`, ported into grit on 2026-09-23. `../../../Spikes/tui-spike-layoutz` answered *can layoutz carry a
fullscreen transcript with drag-scroll-copy?* — yes, but only by injecting a custom
`Terminal`, and **three of the four bugs it found came from layoutz's own render loop and
scheduler**. `../../../Spikes/tui-spike-fullscreen` built the cell-grid answer on JLine and produced the
`Surface`/`Painter` model this starts from. grit.tui is the merge: layoutz's ergonomics on the
fullscreen spike's foundations. Its app model -- one tree that carries its handlers -- came
from two spikes (the view tree, and a pure self type on the app).

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

## The design

### A frame is cells, not a string

```scala
final case class Cell(ch: Char, style: Style)
final case class Surface(size: Size, cells: Vector[Cell])   // writes clip, never wrap
final case class Frame(surface: Surface, cursor: Option[Pos])
```

A leaf component is a `View`: `render(size)` paints exactly the box it is handed, at every
size (the test suite asserts it over every view in the library), and `measure(avail)` only
*reports* what it would like. A box may act on the report -- `Region.Fit(min = 3, upTo =
0.5)` over an `Editor` is a prompt that grows with its draft: the editor knows how many rows
its draft wraps to and nothing about whether it may use them; the region knows the policy.

### An app is three pure functions, and its screen is one tree

```scala
trait App[S, M <: caps.Pure] { self: App[S, M]^{} =>
  def init: (S, Effect[M])
  def update(msg: M, state: S): (S, Effect[M])
  def view(state: S): Node[M]
}
```

`Node[M]` is Elm's `Html msg`: boxes (`column`/`row` of `fixed`/`flex`/`fit` children),
leaves (`paint(view)`, a document pane, the editor), and the handlers, carried *in* the
tree -- `grounded(style)` under any node, so the cells it leaves unset take a colour
rather than the terminal's; `onKeyFirst`/`onKey`/`onPress` on any node, `onScroll`/`onSelect`/`onCopy`/`onClick` on a
document pane (a click is a press released where it began, named by the document position
under it), `onEdit` on the editor, a `dialog` over a node, a `floating` popup beside
one, and `wide(cols)(wide, narrow)`, which picks between two by the width it is laid out
at (the app never sees the terminal's size). A component is a value: `Scroller` is its state, its messages, `update` and `view`,
nested into a parent with `Node.map`. There is no layout pass in the app, no placement map,
no pane registry, and no hook to implement.

Every handler is a **pure function value** (`A -> M`, the capture-checked pure arrow), so the
tree is data plus pure functions and `Effect` stays the only way out.

### Purity is a fact about the app's type

The self type `App[S, M]^{}` says an app *object* captures nothing. An app whose members
reach a terminal, a scheduler or a global capability -- through a field, a helper method, a
mixin or a `using` parameter -- is rejected **where it is defined**. That is why `update` and
`view` can be plain methods, and why a handler built in one of an app's methods is pure with
no ceremony. (Without the self type, `this` in a trait is `^{any}`, and a method reference
folds whatever it touches into the object.)

`M <: caps.Pure` closes the other way in: a message a `Host` delivers could otherwise carry a
capability to `update`. An app declares its message type pure (`enum Msg extends
caps.Pure`), and a capability-typed case is then a compile error where it is declared.

What neither catches: an untracked Java effect (`println`, a static), a capability minted
inside `update` by a pure-signature factory, and mutable state -- a `var` or a mutable
collection is a pure type, and separation checking, which would see it, is off in
`grit.tui` ([ADR 0003](../../docs/decisions/0003-durable-is-exclusive-under-separation-checking.md)).
`AppCaptureTests` pins each property with a compiler probe.

### Layout happens when the runtime paints

The runtime lays the tree out at the size it is painting (`Paint`), paints every leaf into
its box, and keeps what it painted (`Painted`): the targets in paint order, the focus path,
and every handler lifted into the root's message type. Input is routed against that
(`Route`), so a press maps back through exactly the viewport the user was looking at.

The one piece of layout that must persist between frames is wrapping, and the runtime owns
it: the wrap memo (`DocMemo`) keeps each document's rows **keyed by the blocks themselves**
-- reused when the block at a position is `eq` to the one they came from, then when it is
`==`, re-wrapped otherwise. There is no revision to forget to bump, so a reply that takes
the thinking line's place cannot be painted with the thinking line's rows. A document is
wrapped once; after that a frame costs the rows it shows. A pane is found across frames by
its `PaneKey`; a second pane painted under a key already taken paints an error in its box.

### Handlers see what was painted, so a batch re-lays out between inputs

A handler closes over what its paint captured -- the editor it edits, the scroll position
it scrolls from -- and answers with the *new* value, computed against that. The runtime
handles a burst of input (a line pasted or typed in one write, mouse motion) as one batch
with one paint after it, so the second key of a batch would otherwise build on the first
key's *input* rather than its result: the prompt kept only the last key typed. So before
each input in a batch the loop lays the current state out again, without painting (`Loop.fresh`,
microseconds); a drag under a grab is the exception and is routed against what was painted,
which is what the user sees. The gate's `burst` scenario pins this.

The alternative, handlers answering with deltas ("scroll by 3") that compose across a batch,
was turned down: turning a delta into a position needs the layout facts, and those are the
runtime's, not the app's.

### Precedence is position

The mouse goes to the topmost thing under it. Keys go down the focus path through every
`onKeyFirst` (capture: hotkeys, a popup's arrows), then back up through every `onKey`
(bubble: the editor's own keys first, then Enter-submits). A dialog is a barrier: nothing
beneath it sees a key, a press or the wheel. So a hotkey that must work over a dialog is
bound *outside* it -- `screen.dialog(...).onKeyFirst(hotkeys)` -- and one bound inside the
dialog's base silently stops while it is open. The focus path ends at the focused leaf; on a
screen with none, it ends at the innermost `On` that holds nothing focused.

### A drag belongs to the pane it began in

The press binds the drag to its pane (`Grab`) for its whole life; every later motion is read
through *that* pane's viewport, clamped to its rect, so a drag out of a modal cannot select
the transcript behind it (rule 6), and autoscroll scrolls the pane that owns the drag. A
drag the terminal never releases -- the pointer left the window under mode 1002 -- is ended
by the runtime's deadline and copied, flagged as expired. Two properties from the layoutz
spike hold throughout:

- **`DocPos` indexes logical, unwrapped text.** Scrolling moves the anchor and never the
  selection, and copying is a slice of the document, so the clipboard gets what the author
  wrote rather than what a viewport broke it to.
- **Both selection bounds project onto a row identically.** Treating them asymmetrically
  paints every row *after* the selection while the copied text stays correct, so only
  reading painted cells catches it.

### `Effect` is data. Never a thunk.

The capture-checking finding from the layoutz spike, stated as a rule. Making the terminal a
`caps.SharedCapability` there failed at the injection seam *and nowhere else* —
`Cmd.fire(term.copyOut(text))` was accepted, and a capability-free `update` signature happily
returned a command holding a closure over it.

So: **no `Effect` case may contain a function.** A sealed ADT of plain data structurally
cannot capture a capability, which makes "`update` is pure" a fact about the types rather
than a fact about a test. Only the interpreter — which holds the terminal capability — turns
an `Effect` into an action, and its signature says so.

Work outside the terminal follows the same rule. `Effect.ToHost(msg)` is plain data: the
runtime hands `msg` to the `Host` the embedding program supplied (`Runtime.run(app, host)`),
which holds whatever capabilities the work needs and answers, on a thread of its own, by
offering a message back through its `Mailbox`. grit's chat screen sends turns this way
(`grit.app.chat.ChatHost`).

### Timers are cancellable, and the scheduler is virtual-thread native.

A `ScheduledExecutorService` on one platform thread dispatching onto
`Executors.newVirtualThreadPerTaskExecutor()`: cancellable handles, and a blocked timer costs
a continuation rather than a carrier. JDK 26 also means `synchronized` no longer pins a
carrier (JEP 491), so the runtime's locks are free to stay simple.

Nothing self-re-arms without either a cancel handle or a generation stamp. The layoutz spike
needed generation counters *because* its timers could not be cancelled; here they become a
belt-and-braces choice rather than the only defence. The runtime's own two timers -- the
drag autoscroll and the escaped-drag deadline -- are data too (`Timer.Arm`/`Disarm`),
returned by routing and armed by the loop.

### An app that throws does not freeze the screen

A throwable from `update`, `view` or a handler in the tree -- a class that failed to load
mid-render, say -- is caught by the loop and kept as data (`Fault`): the message or input
is dropped, a view that threw leaves the last frame that painted on screen, the bottom row
says what failed until the next key, and input goes on being handled, so the app's quit
key still quits. The host is told (`Host.fault`) and logs it. Only an interrupt and running
out of memory still end the loop.

## Testing an app

`Headless` is the loop with no terminal, as a value: an app stepped through exactly the
steps the runtime takes at a fixed size, its effects kept rather than performed, the screen
painted on demand. A test types keys and delivers host messages, then reads the painted rows
-- `grit.app.chat.ChatScreenTests` is the pattern. The runtime's own timers are only recorded;
`tick` fires one.

## Rules the terminal taught us

Non-negotiable, each paid for in the layoutz spike:

1. **Address every line absolutely** (`ESC[<row>;1H`). Never rely on `ONLCR`; `stty raw`
   clears it and a bare LF paints a staircase.
2. **Never write into the last column.** Deferred wrap leaves the cursor there and a
   following `ESC[K` erases the character just written. The rule lives in exactly one
   place: the runtime hands apps the paintable screen (`Size.screen`), so a frame is
   already one column narrower than the terminal and nothing downstream compensates.
   The owed column is still *coloured*: the painter ends a row whose last cell it
   painted with that cell's background and an erase to the end of the line, which
   writes no glyph there (`Painter`).
3. **Restore in reverse**, and make it idempotent — it will be called from cleanup, from a
   `finally`, and from a shutdown hook.
4. **Verify by painting, not by diffing bytes.** Mode sequences balanced and a frame of
   exactly N lines prove nothing about whether it paints; the right text on the clipboard
   proves nothing about what the user was shown. A VT model (CUP/LF/CR/EL/ED/wrap/SGR) in
   the test suite is worth more than another assertion about bytes — a ~50-line one found
   three of the four bugs next door.
5. **Never fight the user's escape hatches.** Shift-drag stays unbound so the terminal's own
   selection still works.

The painter emits changed cells only, coalescing adjacent runs that share a style: one write
per frame, wrapped in `ESC[?2026h`/`l`, every line addressed absolutely.

## Layout

One Mill module, `grit.tui`, in four groups — `model`, `components`, `wire`, `runtime` —
plus `grit.tui.examples` for everything runnable. Each group is a set of subpackages in one
dependency order (in `runtime`: `app` ← `render` ← `route` ← `loop`). The group table, the
orders, what each may import, and how it is enforced are in [`CLAUDE.md`](CLAUDE.md).

Escape bytes are spelled in exactly three places and nowhere else: `wire/paint/Ansi` (out),
`wire/input/Decoder` (in), and `wire/term/` (mode setting). Binding — what an input
*means* — is the app's: it is the handlers the app puts in its tree, in its own message
type. The library routes; it binds nothing but the mechanics every pane shares (selection,
the wheel, the thumb, page keys on a focused pane).
