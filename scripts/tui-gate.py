"""Read a scripted pty capture of grit.tui.examples.Demo (or grit.app) and check what it painted.

Byte-level assertions can only see the wire's own rules -- balanced modes, no bare LF.
Three of the four bugs in the layoutz spike were invisible to those and to 64 passing
tests, so this replays the capture through a small VT model (CUP / EL / ED / SGR) and
asserts against the resulting *grid*: which cells carry reverse video, and whether they
say the same thing the clipboard does.

Usage: gate.py <capture> <rows> <cols> <scenario>
"""

import base64
import re
import sys

# Mirrors Demo.help. If that document changes, change this: the modal scenario asserts
# the clipboard is a contiguous slice of it and of nothing else.
HELP = """This is a second document pane, drawn over the
transcript with its own scroll position and its
own document.

Start a drag in here and run the pointer down
over the transcript: the selection stays in this
pane and clamps to this document. A drag belongs
to the pane it began in.

Esc or alt-m closes."""


class Vt:
    """Enough of a terminal to answer 'what does the screen say'."""

    def __init__(self, rows, cols):
        self.rows, self.cols = rows, cols
        self.grid = [[(" ", False)] * cols for _ in range(rows)]
        # Each cell's background, None for the terminal's own: kept beside the grid so
        # the cells stay (glyph, reverse) pairs for every oracle that reads them.
        self.bgs = [[None] * cols for _ in range(rows)]
        self.r = self.c = 0
        self.rev = False
        self.bg = None
        self.max_row = 0
        self.max_col = 0
        self.scrolled = False
        self.bare_lf = 0
        self.cr = 0

    def feed(self, s):
        i = 0
        while i < len(s):
            ch = s[i]
            if ch == "\x1b":
                m = re.match(r"\x1b\[([0-9;?]*)([A-Za-z])", s[i:])
                if m:
                    self._csi(m.group(1), m.group(2))
                    i += m.end()
                    continue
                osc = re.match(r"\x1b\][^\x07\x1b]*(\x07|\x1b\\)", s[i:])
                i += osc.end() if osc else 1
                continue
            if ch == "\n":
                self.bare_lf += 1
                self.r += 1
            elif ch == "\r":
                self.cr += 1
                self.c = 0
            else:
                if 0 <= self.r < self.rows and 0 <= self.c < self.cols:
                    self.grid[self.r][self.c] = (ch, self.rev)
                    self.bgs[self.r][self.c] = self.bg
                    self.max_col = max(self.max_col, self.c + 1)
                else:
                    self.scrolled = True
                self.c += 1
                if self.c >= self.cols:
                    self.scrolled = True
            i += 1

    def _csi(self, params, final):
        if final == "H":
            a = [int(x) for x in params.split(";") if x] or [1, 1]
            while len(a) < 2:
                a.append(1)
            self.r, self.c = a[0] - 1, a[1] - 1
            self.max_row = max(self.max_row, a[0])
        elif final == "m":
            # Extended colour spans several params -- 38;2;r;g;b and 38;5;n -- so this
            # walks with an index and skips them wholesale. Consuming them one at a time
            # would read a red channel of 7 as reverse video and a zero channel as a
            # reset, and the mask oracle would be quietly wrong rather than loudly.
            codes = params.split(";") if params else ["0"]
            i = 0
            while i < len(codes):
                code = codes[i]
                step = 1
                if code in ("38", "48"):
                    kind = codes[i + 1] if i + 1 < len(codes) else ""
                    step = 5 if kind == "2" else 3 if kind == "5" else 2
                    if code == "48":
                        self.bg = tuple(codes[i + 2:i + step]) if kind == "2" else "indexed"
                elif code in ("", "0"):
                    self.rev = False
                    self.bg = None
                elif code == "27":
                    self.rev = False
                elif code == "49":
                    self.bg = None
                elif code == "7":
                    self.rev = True
                i += step
        elif final == "K":
            # Background colour erase: the erased cells take the current background.
            for c in range(self.c, self.cols):
                self.grid[self.r][c] = (" ", False)
                self.bgs[self.r][c] = self.bg
        elif final == "J":
            self.grid = [[(" ", False)] * self.cols for _ in range(self.rows)]
            self.bgs = [[None] * self.cols for _ in range(self.rows)]

    def body_highlight(self, top, bottom):
        """The reverse-video text of rows [top, bottom), row by row.

        The title and status bars are reverse by design, so the mask oracle looks only at
        the pane rows between them.
        """
        out = []
        for r in range(top, bottom):
            run = "".join(ch for ch, rev in self.grid[r] if rev)
            if run:
                out.append(run)
        return out


def thumb_top(v, cols):
    """The first row of the scrollbar's thumb block, or None if it is not on screen.

    The track is the body's last column: the screen's own last column is never written
    (rule 2), so the scrollbar lives at cols - 2.
    """
    col = cols - 2
    for r in range(len(v.grid)):
        if v.grid[r][col][0] == "\u2503":
            return r
    return None


def main():
    path, rows, cols, scenario = sys.argv[1], int(sys.argv[2]), int(sys.argv[3]), sys.argv[4]
    raw = open(path, "rb").read().decode("utf-8", "replace")
    checks = []

    def check(ok, label, detail=""):
        checks.append((bool(ok), label, detail))

    for mode in ("1049", "1002", "1006", "2004", "1004"):
        h, l = raw.count("\x1b[?%sh" % mode), raw.count("\x1b[?%sl" % mode)
        check(h == 1 and l == 1, "?%s taken once and given back once" % mode, "%d/%d" % (h, l))

    frames = re.findall(r"\x1b\[\?2026h(.*?)\x1b\[\?2026l", raw, re.S)
    # A reload with no input paints twice: the empty screen, then the loaded transcript.
    least = 2 if scenario == "reload" else 4
    check(len(frames) >= least, "frames are synchronised output", "%d frames" % len(frames))

    vt = Vt(rows, cols)
    snapshots = []
    per_frame = []
    ones = []
    # The thumb is read off the *accumulated* screen, not off one frame. A frame is a
    # diff, so a frame that did not repaint the scrollbar has no thumb in it at all --
    # and a per-frame reading therefore reports only the moves, which is satisfied by a
    # transcript that is merely streaming. Reading the screen asks where the thumb *is*.
    thumbs = []
    # The screen as text after each frame, for the scenarios whose oracle is what was
    # *written* rather than what was highlighted. Accumulated, like the thumb: a frame is
    # a diff, and a frame that did not repaint the prompt says nothing about it.
    screens = []
    for f in frames:
        before = vt.max_row, vt.max_col
        vt.feed(f)
        one = Vt(rows, cols)
        one.feed(f)
        ones.append(one)
        per_frame.append((one.max_row, one.max_col))
        del before
        snapshots.append(vt.body_highlight(1, rows - 1))
        thumbs.append(thumb_top(vt, cols))
        screens.append(
            ["".join(vt.grid[r][c][0] for c in range(cols)) for r in range(rows)])

    check(vt.bare_lf == 0, "no bare LF (stty raw clears ONLCR)", "%d" % vt.bare_lf)
    check(vt.cr == 0, "no CR", "%d" % vt.cr)
    check(not vt.scrolled, "the frame never scrolls and never wraps")
    check(vt.max_row <= rows, "no row past the last", "max %d" % vt.max_row)
    check(vt.max_col <= cols - 1, "the last column is never written", "max %d" % vt.max_col)

    if scenario == "resize":
        # The app is told its size through one path -- a Resize input -- at startup and at
        # every SIGWINCH. Proof that the signal reached the loop is that the frames after
        # it are painted to the new size and stop addressing rows that no longer exist.
        full = [i for i, (r, _) in enumerate(per_frame) if r == rows]
        small = [i for i, (r, c) in enumerate(per_frame) if 0 < r <= 20 and c <= 59]
        check(bool(full), "the app painted the original size first", "%d frames" % len(full))
        check(bool(small) and (not full or small[-1] > full[-1]),
              "SIGWINCH reached the loop: later frames are painted to the new size",
              "%d frames at 20x60" % len(small))
        check(all(r <= 20 for r, _ in per_frame[small[0]:]) if small else False,
              "no frame after the resize addresses a row that no longer exists")
        width = max(len(label) for _, label, _ in checks)
        failed = 0
        for ok, label, detail in checks:
            failed += 0 if ok else 1
            print("  %s  %-*s %s" % ("ok  " if ok else "FAIL", width, label, detail))
        return 1 if failed else 0

    if scenario == "burst":
        # Twelve characters in one write reach the loop as one batch of keys, routed before
        # anything is repainted. Each must build on the one before it: a handler captured
        # by the last paint edits the draft *that paint* saw, so without a fresh layout per
        # input the prompt keeps only the last key ("i").
        final = screens[-1] if screens else []
        prompt = final[rows - 3].strip(" \u2502") if final else ""
        check("gate says hi" in prompt, "a burst typed in one write lands whole in the prompt",
              repr(prompt[:40]))
        payloads = re.findall(r"\x1b\]52;c;([A-Za-z0-9+/=]*)\x1b\\", raw)
        check(len(payloads) == 0, "no drag, so no clipboard write", "%d" % len(payloads))
        width = max(len(label) for _, label, _ in checks)
        failed = 0
        for ok, label, detail in checks:
            failed += 0 if ok else 1
            print("  %s  %-*s %s" % ("ok  " if ok else "FAIL", width, label, detail))
        return 1 if failed else 0

    if scenario in ("chat", "reload"):
        # grit.app with the stub model, whose reply to a message is "stub reply to: " and
        # the message. The oracle is what the transcript *says*, read off the accumulated
        # screen: the store's contents, painted.
        final = screens[-1] if screens else []
        text = "\n".join(row.rstrip() for row in final)
        asked = [i for i, row in enumerate(final) if "ᛗ gate says hi" in row]
        answered = [i for i, row in enumerate(final) if "ᚨ stub reply to: gate says hi" in row]
        check(bool(asked), "the message is painted in the transcript")
        check(bool(answered), "its reply is painted in the transcript")
        check(bool(asked) and bool(answered) and answered[0] > asked[0],
              "the reply is below the message it answers")
        check("thinking" not in text, "no turn is left in progress on the final screen")
        check(bool(final) and "ᛁ idle" in final[rows - 1],
              "the status bar settles on idle")
        payloads = re.findall(r"\x1b\]52;c;([A-Za-z0-9+/=]*)\x1b\\", raw)
        if scenario == "reload":
            check(len(payloads) == 0, "no drag, so no clipboard write", "%d" % len(payloads))
        else:
            # A drag from the rune of the message to the end of its reply: the copy is
            # what was written, never a rune or a rail painted beside it.
            copied = base64.b64decode(payloads[-1]).decode("utf-8", "replace") if payloads else ""
            check(len(payloads) == 1, "one drag, one clipboard write", "%d" % len(payloads))
            check("gate says hi" in copied and "stub reply to: gate says hi" in copied,
                  "the drag copied the message and its reply", repr(copied[:60]))
            check(not any(ch in copied for ch in "▌ᛗᚨ"),
                  "and no rune or rail beside them", repr(copied[:60]))
        # The terminal's last column is never written (rule 2), but it must still be
        # coloured: left alone it keeps the terminal's own background, a stripe down the
        # right edge of a screen grounded in the theme's colour.
        stripe = [r for r in range(rows) if vt.bgs[r][cols - 1] != vt.bgs[r][cols - 2]]
        check(not stripe, "the last column takes the ground of the cell before it",
              "rows %s" % stripe[:8])
        if scenario == "chat":
            # The prompt's text row is the middle of its box: rows - 3.
            typed = [i for i, g in enumerate(screens) if "gate says hi" in g[rows - 3]]
            check(bool(typed), "the prompt showed the message as it was typed")
            check(bool(final) and "gate says hi" not in final[rows - 3],
                  "and was cleared by the submission")
        else:
            # Nothing was typed in this run, so anything in the transcript came from the
            # store. The screen paints a ward while the engine opens; the exchange must be
            # there within a few frames of the ward leaving, and never in the prompt.
            opened = [i for i, g in enumerate(screens)
                      if not any("opening the engine" in row for row in g)]
            after = screens[opened[0]:] if opened else []
            first = next((i for i, g in enumerate(after) if any("ᛗ " in row for row in g)), None)
            typed = any("gate says hi" in g[rows - 3] for g in screens)
            check(first is not None and first <= 3 and not typed,
                  "the exchange was loaded, not typed",
                  "frame %s after the ward" % first)
        width = max(len(label) for _, label, _ in checks)
        failed = 0
        for ok, label, detail in checks:
            failed += 0 if ok else 1
            print("  %s  %-*s %s" % ("ok  " if ok else "FAIL", width, label, detail))
        return 1 if failed else 0

    if scenario == "columns":
        return columns(checks, check, raw, rows, cols, screens, snapshots, ones)
    if scenario == "turn-modal":
        return turn_modal(checks, check, raw, rows, screens)
    if scenario == "stream":
        return stream(checks, check, rows, screens)
    if scenario == "palette":
        return palette(checks, check, raw, frames, screens)
    if scenario == "tabs":
        return tabs(checks, check, raw, rows, cols, screens)

    payloads = re.findall(r"\x1b\]52;c;([A-Za-z0-9+/=]*)\x1b\\", raw)
    if scenario in ("thumb", "popup"):
        check(len(payloads) == 0, "no drag, so no clipboard write", "%d" % len(payloads))
    else:
        check(len(payloads) == 1, "exactly one clipboard write", "%d" % len(payloads))
    copied = base64.b64decode(payloads[-1]).decode("utf-8", "replace") if payloads else ""

    # The mask lives only while the button is down, so the frame that matters is the last
    # one that had any, which is the frame painted immediately before the release copied.
    highlighted = []
    for snap in snapshots:
        if snap:
            highlighted = snap
    painted = "".join(highlighted)
    squeeze = lambda s: re.sub(r"\s+", "", s)

    if scenario == "span":
        check(copied.count("\n") + 1 > rows, "the selection spans more than one viewport",
              "%d lines, %d chars" % (copied.count("\n") + 1, len(copied)))
        # The chrome is header 1 + prompt 3 + status 1 = 5 rows, so the pane is rows - 5.
        # A drag that fills it lights every row except the separator rules, which copy as
        # empty lines and so never highlight -- count them from the masked frame's grid.
        rules = 0
        masked = [i for i, s in enumerate(snapshots) if s]
        if masked:
            g = ones[masked[-1]].grid
            for r in range(1, rows - 4):
                cells = [g[r][c][0] for c in range(0, cols - 2)]
                if cells and all(ch == "\u2500" for ch in cells):
                    rules += 1
        check(len(highlighted) + rules >= rows - 5, "the highlight fills the pane",
              "%d rows, %d rules" % (len(highlighted), rules))
        check(squeeze(copied).startswith(squeeze(painted)) and len(painted) > 0,
              "the painted mask is the visible prefix of the clipboard",
              "%d of %d chars" % (len(painted), len(copied)))
    elif scenario == "one":
        check(squeeze(painted) == squeeze(copied) and len(painted) > 0,
              "the highlighted cells are exactly what was copied",
              "%d painted / %d copied" % (len(painted), len(copied)))
    elif scenario == "modal":
        check(copied in HELP and len(copied) > 0,
              "the clipboard is a slice of the modal's own document",
              repr(copied[:40]))
        check("grit>" not in copied and "you>" not in copied,
              "no transcript text escaped into the selection (rule 6)")
    elif scenario == "popup":
        # The chrome is header 1 + prompt 3 + status 1, so the prompt's text row is the
        # middle of its box: rows - 3. Nothing here reads a mask -- the whole scenario is
        # keyboard, and what it asserts is that four keystrokes moved through four
        # different layers (free key, popup, popup, free key) and came out as a command
        # that ran.
        listed = [i for i, g in enumerate(screens) if any("/clear" in row for row in g)]
        check(bool(listed), "Tab opened the completion list", "%d frames" % len(listed))
        took = [i for i, g in enumerate(screens) if "/stream" in g[rows - 3]]
        check(bool(took), "the prompt took the completion Enter chose", "%d frames" % len(took))
        check(bool(took) and bool(listed) and took[0] > listed[0],
              "and took it after the list was open, not before")
        final = screens[-1] if screens else []
        check(bool(final) and not any("/clear" in row for row in final),
              "the list closed behind the choice")
        # `/stream` is one of the three slash commands that delegate to a message the app
        # already has. If the table and the hotkey ever diverge again, this is what says so.
        check(bool(final) and "paused" in final[0],
              "the submitted command ran: the header says paused", repr(final[0][-20:]))
    elif scenario == "thumb":
        # The scrollbar is the body's last column -- the screen's last column is never
        # written, so the track lives at cols - 2. The thumb must *travel*: pressed near
        # the foot of the track and released near its head, it has to end up where the
        # pointer left it and stay there while the transcript keeps streaming. And none
        # of it may paint a selection or touch the clipboard: the widget drag never
        # started a text drag.
        #
        # "The thumb moved" and "the transcript scrolled" are not enough on their own,
        # and this oracle used to stop there. A demo whose thumb press does nothing at
        # all still passes both, because the streamer scrolls the tailing transcript and
        # drags the thumb down the track by itself -- which is exactly what a mutant that
        # broke the press proved. Travel is what only a thumb drag can produce.
        tops = [t for t in thumbs if t is not None]
        seen = sorted(set(tops))
        check(len(seen) > 1, "the thumb moved with the drag", str(seen))
        check(bool(tops) and max(tops) >= (rows - 5) // 2,
              "the thumb was at the foot of the track before the press",
              "max row %s" % (max(tops) if tops else None))
        check(bool(tops) and tops[-1] <= 8,
              "the thumb ended where the pointer released it, and stayed",
              "row %s" % (tops[-1] if tops else None))
        check(all(not snap for snap in snapshots),
              "a thumb drag paints no selection")

    width = max(len(label) for _, label, _ in checks)
    failed = 0
    for ok, label, detail in checks:
        failed += 0 if ok else 1
        print("  %s  %-*s %s" % ("ok  " if ok else "FAIL", width, label, detail))
    return 1 if failed else 0


# Mirrors TurnPanel.Cols. If that changes, change this.
PANEL_COLS = 38
# Mirrors ChatScreen's panel: the row of tab pills, then a gap, then the document.
PANEL_TOP = 2


def columns(checks, check, raw, rows, cols, screens, snapshots, ones):
    """grit.app beside its turn panel. The body is rows 1 .. rows - 5 (header 1; prompt 3
    and status 1 below). The panel is the last PANEL_COLS columns before the screen's own
    unwritten last one (TurnPanel.Cols); the transcript is everything left of it.
    """
    body = range(1, rows - 4)
    final = screens[-1] if screens else []
    divider = cols - 1 - PANEL_COLS
    left = lambda g: [g[r][:divider] for r in body]
    right = lambda g: [g[r][divider:] for r in body]
    check(any(any(" TURN " in row for row in right(g)) for g in screens),
          "the turn panel stands beside the transcript", "from column %d" % divider)

    # The wheel over the panel: the frame where its title scrolls away, and the
    # transcript in that frame as it was in the frame before.
    panel_moved = next((i for i in range(1, len(screens))
                        if "TURN" in right(screens[i - 1])[PANEL_TOP]
                        and "TURN" not in right(screens[i])[PANEL_TOP]),
                       None)
    check(panel_moved is not None, "the wheel over the panel scrolled the panel")
    check(panel_moved is not None and left(screens[panel_moved]) == left(screens[panel_moved - 1]),
          "and left the transcript where it was")
    # The wheel over the transcript, afterwards: its rows move, the panel's do not.
    later = range((panel_moved or 0) + 1, len(screens))
    text_moved = next((i for i in later
                       if left(screens[i]) != left(screens[i - 1])
                       and not any(snapshots[i])), None)
    check(text_moved is not None, "the wheel over the transcript scrolled the transcript")
    check(text_moved is not None and right(screens[text_moved]) == right(screens[text_moved - 1]),
          "and left the panel where it was")

    # The drag, begun in the transcript and released over the panel: rule 6 across columns.
    payloads = re.findall(r"\x1b\]52;c;([A-Za-z0-9+/=]*)\x1b\\", raw)
    check(len(payloads) == 1, "exactly one clipboard write", "%d" % len(payloads))
    copied = base64.b64decode(payloads[-1]).decode("utf-8", "replace") if payloads else ""
    check("message" in copied or "stub reply" in copied,
          "the clipboard holds transcript text", repr(copied[:40]))
    check(not any(w in copied for w in ("TURN", "assemble", "window", "recalled")),
          "and nothing of the panel's")
    masked = [i for i, snap in enumerate(snapshots) if snap]
    lit = [(r, c) for r in body for c in range(cols)
           if masked and ones[masked[-1]].grid[r][c][1]]
    check(bool(lit) and all(c < divider for _, c in lit),
          "the highlight never crossed the divider", "%d cells" % len(lit))

    check(bool(final) and "still here" in final[rows - 3], "the prompt still takes keys")
    return report(checks)


def turn_modal(checks, check, raw, rows, screens):
    """grit.app with a slow stub: the running turn opened by a click, then a turn pinned."""
    has = lambda g, s: any(s in row for row in g)
    opened = [i for i, g in enumerate(screens) if has(g, "\u256d\u2500 turn 2")]
    check(bool(opened), "a click on the thinking line opened the running turn",
          "%d frames" % len(opened))
    check(bool(opened) and has(screens[opened[0]], "\u16d7 second"),
          "the opened turn says what was asked")
    check(bool(opened) and not any("x" in screens[i][rows - 3] for i in opened),
          "the dialog took the key: nothing reached the prompt")
    closed = next((i for i in range(opened[-1] + 1, len(screens))
                   if not has(screens[i], "\u256d\u2500 turn")), None) if opened else None
    check(closed is not None, "Escape closed it")
    after = range(closed or len(screens), len(screens))
    pinned = next((i for i in after
                   if has(screens[i], "TURN 1") and has(screens[i], "esc: latest")), None)
    check(pinned is not None, "a click on the first reply pinned its turn to the panel")
    back = next((i for i in range((pinned or len(screens)) + 1, len(screens))
                 if has(screens[i], "TURN 2") and not has(screens[i], "esc: latest")), None)
    check(back is not None, "Escape let it go: the panel follows the latest turn again")
    payloads = re.findall(r"\x1b\]52;c;([A-Za-z0-9+/=]*)\x1b\\", raw)
    check(len(payloads) == 0, "clicks copy nothing", "%d" % len(payloads))
    return report(checks)


# Mirrors Theme.Frost.ground and Theme.TokyoNight.ground, as the painter writes a
# background. If those change, change these.
FROST_GROUND = "48;2;13;19;25"
TOKYO_NIGHT_GROUND = "48;2;26;27;38"


def palette(checks, check, raw, frames, screens):
    """grit.app's command palette, driven by keys alone (script_palette)."""
    has = lambda g, s: any(s in row for row in g)
    after = lambda start, ok: next((i for i in range(start, len(screens)) if ok(screens[i])), None)
    listed = after(0, lambda g: has(g, "/theme  switch the colour theme")
                   and has(g, "/quit   leave grit"))
    check(listed is not None, "/ opened the list of every command")
    filtered = after((listed or 0) + 1, lambda g: has(g, "/theme  switch")
                     and not has(g, "leave grit"))
    check(listed is not None and filtered is not None, "typing filtered it to /theme")
    themes = after((filtered or 0) + 1, lambda g: has(g, "tokyo-storm") and has(g, "nightshade"))
    check(filtered is not None and themes is not None,
          "Enter on /theme offered the themes as a second list")
    # The prompt held "/theme tokyo-n" until the choice ran it and cleared it.
    chose = after((themes or 0) + 1,
                  lambda g: not has(g, "/theme ") and not has(g, "\u2502tokyo-night"))
    check(themes is not None and chose is not None, "Enter on a theme closed the list")
    before = "".join(frames[:themes or 0])
    since = "".join(frames[chose:]) if chose is not None else ""
    check(FROST_GROUND in before and TOKYO_NIGHT_GROUND not in before,
          "the screen was painted in Frost before the choice")
    check(TOKYO_NIGHT_GROUND in since, "and in Tokyo Night after it")
    reopened = after((chose or len(screens)) + 1, lambda g: has(g, "leave grit"))
    check(reopened is not None, "/ opened the list again")
    final = screens[-1] if screens else []
    check(reopened is not None and not has(final, "leave grit"), "Escape dismissed it")
    check(not has(final, "\u16d7 ") and not any(has(g, "thinking") for g in screens),
          "nothing typed reached the transcript, and no turn ran")
    check(not any(has(g, "no command") for g in screens), "no draft failed as a command")
    payloads = re.findall(r"\x1b\]52;c;([A-Za-z0-9+/=]*)\x1b\\", raw)
    check(len(payloads) == 0, "no drag, so no clipboard write", "%d" % len(payloads))
    return report(checks)


def tabs(checks, check, raw, rows, cols, screens):
    """grit.app's panel tabs (script_tabs): ctrl-t, then a click on each pill."""
    divider = cols - 1 - PANEL_COLS
    panel = lambda g: "\n".join(g[r][divider:] for r in range(1, rows - 4))
    on_turn = lambda g: "TURN 1" in panel(g) and "SESSION" not in panel(g)
    on_session = lambda g: ("SESSION  · 1 turn" in panel(g) and "TURN 1" not in panel(g)
                            and "said     2 messages" in panel(g))
    after = lambda start, ok: next((i for i in range(start, len(screens)) if ok(screens[i])), None)
    pills = screens[-1][1][divider:] if screens else ""
    check("▐ turn ▌" in pills or "▐ session ▌" in pills,
          "the pills stand across the top of the panel", repr(pills.strip()))
    first = after(0, on_turn)
    check(first is not None, "the panel opened on the turn tab")
    keyed = after((first or 0) + 1, on_session)
    check(first is not None and keyed is not None,
          "ctrl-t showed the session: its turns and messages")
    check(keyed is not None and "grit/stub" in panel(screens[keyed]),
          "and the model that answered, by role")
    back = after((keyed or len(screens)) + 1, on_turn)
    check(keyed is not None and back is not None, "a click on the turn pill showed the turn")
    again = after((back or len(screens)) + 1, on_session)
    check(back is not None and again is not None, "a click on the session pill showed the session")
    final = screens[-1] if screens else []
    check(bool(final) and on_session(final), "and the panel stayed there")
    check(bool(final) and "▐ session ▌" in final[1][divider:],
          "its pill is the lit one")
    check(bool(final) and final[rows - 3].strip(" │ᚦ") == "", "no key reached the prompt",
          repr(final[rows - 3].strip() if final else ""))
    check(bool(final) and any("ᚨ stub reply to: tabs one" in row for row in final),
          "the transcript kept the exchange")
    payloads = re.findall(r"\x1b\]52;c;([A-Za-z0-9+/=]*)\x1b\\", raw)
    check(len(payloads) == 0, "clicks copy nothing", "%d" % len(payloads))
    return report(checks)


def stream(checks, check, rows, screens):
    """grit.app with a slow stub that streams its reply word by word."""
    asked = "rune one two three four five six seven eight nine ten eleven twelve"
    whole = "stub reply to: " + asked
    body = lambda g: "\n".join(g[1:rows - 4])
    partial = [i for i, g in enumerate(screens)
               if "\u16a8 stub reply to:" in body(g) and whole not in body(g)
               and "answering" in g[rows - 1]]
    check(bool(partial), "the reply was painted part-written while the model answered",
          "%d frames" % len(partial))
    final = screens[-1] if screens else []
    check(bool(final) and body(final).count(whole) == 1,
          "the recorded reply replaced it, once")
    check(bool(final) and "thinking" not in body(final) and "\u258d" not in body(final),
          "no spinner and no caret left behind")
    return report(checks)


def report(checks):
    width = max(len(label) for _, label, _ in checks)
    failed = 0
    for ok, label, detail in checks:
        failed += 0 if ok else 1
        print("  %s  %-*s %s" % ("ok  " if ok else "FAIL", width, label, detail))
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
