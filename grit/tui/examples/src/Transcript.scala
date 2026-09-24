package grit.tui.examples

import grit.tui.model.block.Block
import grit.tui.model.select.Doc

/** The transcript model of the grit mock: the blocks a conversation holds and how the
  * list grows.
  *
  * The colours and the chunk vocabulary are [[Palette]]'s -- the demo's, not the
  * library's. grit.tui knows where a tool call's name ends; it does not know that a failed
  * one is rose. Everything below reads as a role because this file says what a role is.
  *
  * This file is deliberately quarantined (see QuarantineTests in `grit.tui.test`): it
  * imports nothing from `grit.tui.wire`, so the transcript model is unit-testable with no
  * terminal in the loop -- grit's stated quarantine test for its own model, and one of
  * the two new phase 1 gate oracles. What lands here is data in, data out: a `Doc` of
  * `Block`s, the streaming tick, and what a submitted prompt adds.
  */
object Transcript {

  private val words = Vector(
    "signature",
    "capability",
    "viewport",
    "anchor",
    "placement",
    "revision",
    "deadline",
    "projection",
    "clamp",
    "transcript"
  )

  private def word(rev: Long): String = words((rev % words.length).toInt)

  private def wordsFor(rev: Long, n: Int): String =
    (0 until n).map(w => words(((rev + w) % words.length).toInt)).mkString(" ")

  /** The transcript the mock opens with: every block shape grit's transcript holds --
    * a greeting, a separator before a submitted prompt, the prompt, a finished tool
    * call with its result, the diff it produced, and the reply. Enough rows that a
    * drag past the top edge has screens to pull in.
    */
  def seeded: Doc =
    Doc(
      Vector(
        Palette.assistant(
          "ready. type a prompt and press enter; tab completes slash commands; alt-m opens the help modal; alt-s pauses this streamer."
        ),
        Palette.separator,
        Palette.user("what held the glyph boundary in Width?"),
        Palette.thinking(
          "Width measures per code point, so the cut has to resolve to a glyph start. Checking offsetAtColumn."
        ),
        Block
          .tool("Read", "grit/tui/src/model/text/Width.scala")
          .styled(Palette.tool)
          .finish(
            ok = true,
            "96 lines",
            Vector(
              "def of(c: Char): Int = c match",
              "  case combining => 0",
              "  case wide      => 2"
            )
          ),
        Block.Diff(
          Vector(
            "+ val col = Width.offsetAtColumn(line, w)",
            "- val col = math.min(line.length, w)",
            "+ // a column inside a wide glyph resolves to the glyph's start"
          ),
          style = Palette.diff
        ),
        Palette.assistant(
          "the cut resolves to the start of the glyph it lands in: a wide glyph is never split, and a row that cannot fit the whole glyph comes up a column short. that is the honest price of measuring in display columns, and the same rule every widget's chrome truncates by."
        ),
        Palette.separator,
        Palette.user("and the wrap cache?"),
        Palette.assistant(
          "keyed by the blocks themselves: a block's rows are reused while the block is unchanged, so a streaming tail re-wraps one entry and a reply that replaces the thinking line wraps afresh, with no revision to keep in step."
        ),
        Palette.separator,
        Palette.user("what happens when i select across a wrapped row?"),
        Palette.assistant(
          "both bounds project through one function into display columns, so the mask and the copy cannot disagree. drag to select and release: the clipboard holds what the author wrote, not what a width happened to break it into. drag past the top or bottom edge and the view autoscrolls -- the anchor never moves, however many screens go by."
        ),
        Palette.assistant(
          "the scrollbar on the right is a widget: grab its thumb and drag it, and the view follows the geometry, not a stored offset."
        ),
        Palette.assistant(
          "expansion is a block-internal change: opening a tool call rewrites rows inside that block alone, so every sibling position and every sibling cache key is bit-identical before and after. the block, not the line, is what a document position indexes."
        ),
        Palette.separator,
        Palette.user("and the layout?"),
        Palette.assistant(
          "a stack down the screen -- one row of header, a flexible body, three rows of prompt, one of status -- and the body splits into the transcript and a one-column scrollbar. regions are served in declaration order, flexes share what remains with the first absorbing the rounding, and a demand that cannot be met is clamped in place and reported, never stolen and never painted outside its parent."
        ),
        Palette.separator,
        Palette.user("what happens to a drag that leaves the window?"),
        Palette.assistant(
          "it is given a named deadline. under mode 1002 a pointer that leaves the window stops reporting entirely -- no motion, no release, nothing to wait for -- so each extend re-arms one timer that never matures while the drag is live and is the only thing pending when it escapes. an autoscroll tick re-arms it only when the tick actually moved the view, so a drag held off the bottom scrolls to the end of the document and then expires instead of ticking forever."
        ),
        Palette.separator,
        Palette.user("why is the app dimmed under the modal instead of cleared?"),
        Palette.assistant(
          "because it is frozen, not gone: every glyph outside the frame survives and only its style changes. capture, though, is a routing rule and not a paint rule -- painting something opaque over a pane does not stop input reaching it, so the modal's route type has no case meaning give it to the app beneath, and an unclaimed input is swallowed by construction rather than by every binding site remembering to guard."
        ),
        Palette.separator,
        Palette.user("and the popup?"),
        Palette.assistant(
          "the contrast: its route does have a pass-through case, because a popup that ate the keystrokes filtering it would be one you could not type into. its window over the matches is derived from the selection rather than stored, so there is no offset to fall out of step -- the list scrolls because the selection moved, never the other way round."
        ),
        Palette.separator,
        Palette.user("why does the wrap cache key on the entry and not the document?"),
        Palette.assistant(
          "because scroll state is per pane. two panes can hold the same document at different widths -- a transcript and its modal, say -- and a document-level revision would have each new stream tick invalidate both of them. the cost is inherited: a document that reorders or deletes entries must bump revisions, because the index alone will not notice."
        ),
        Palette.separator,
        Palette.user("why does the status bar truncate instead of wrap?"),
        Palette.assistant(
          "chrome truncates, content wraps. the status bar, the hint strip and the popup truncate because chrome that reflowed would change the transcript's height -- one row more and everything below it moves, including the row you are reading. the prompt is the exception and wraps, because a user has to see what they typed."
        ),
        Palette.separator,
        Palette.user("what does the scrollbar's thumb know?"),
        Palette.assistant(
          "only geometry. content rows, a window onto them, and an offset -- clamped into the document, so a stale offset after a shrink cannot point outside it. the thumb's length is the window's share, pinned to both ends of its travel, and dragging is the same arithmetic run backwards: a row of the track names a row of the document, endpoints exact and monotone in between."
        ),
        Palette.separator,
        Palette.user("one last one: why is nothing here a callback?"),
        Palette.assistant(
          "because effects are data. the model returns a description of what should happen, the runtime interprets it, and no case of it carries a function -- a thunk would launder a capability past the capture checker. the terminal is a tracked capability, the scheduler is one, and the app's own messages ride along as values. that is the whole discipline."
        )
      )
    )

  /** One streamer tick: usually more words on the last entry, sometimes a new one.
    *
    * The append bumps one block's revision, which re-wraps exactly that block and
    * leaves every other cached row alone -- the reason a `Block` carries a revision.
    */
  def stream(doc: Doc, rev: Long): Doc = {
    // The streamer owns only the `grit>` line it started. A submission's `you>` block
    // may be the document's last, and a tick that glued stream words onto it -- or, at
    // a six-rev boundary, replaced it wholesale -- erased the user's message. When the
    // tail is not its own, the tick starts a fresh line instead of touching it.
    val own = doc.blocks.lastOption.collect {
      case t: Block.Text if t.text.startsWith("grit> ") => t
    }
    (own, rev % 6) match {
      // `copy`, not a fresh `Block.Text`: the tail's spans and ground are its own and a
      // rebuilt block would drop them, un-dressing the line mid-stream.
      case (Some(t), r) if r != 0 =>
        doc.updated(doc.length - 1, t.copy(text = t.text + " " + word(rev)))
      case _ => doc.append(Palette.assistant(s"$rev: " + wordsFor(rev, 6)))
    }
  }

  /** A submitted prompt: a separator before it, then the prompt itself. */
  def submitted(doc: Doc, draft: String): Doc =
    doc.append(Block.Separator()).append(Palette.user("" + draft))

  /** The last tool block, for expansion. */
  def toggleExpansion(doc: Doc): Doc = {
    val i = doc.blocks.lastIndexWhere(_.isInstanceOf[Block.Tool])
    if (i < 0) { doc }
    else {
      val t = doc.blocks(i).asInstanceOf[Block.Tool]
      doc.updated(i, t.withExpanded(!t.expanded))
    }
  }
}
