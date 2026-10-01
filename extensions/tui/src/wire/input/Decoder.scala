package grit.tui.wire.input

import grit.tui.model.input.*

/** An incremental, pure decoder from terminal characters to [[Input]].
  *
  * A read can split an escape sequence anywhere, and a pure function cannot block to ask
  * for one more character -- so the partial sequence is carried in the value. Feeding a
  * script in one read, in two, or one character at a time yields the same events;
  * `DecoderTests` sweeps every split to hold that.
  *
  * Two things are deliberately *not* decided here:
  *
  *   - **How long to wait for an ambiguous ESC.** ESC prefixes every sequence, so a
  *     trailing ESC could be the Escape key or the start of an arrow. The decoder holds
  *     it; [[flush]] is the reader saying the wait is over. The clock lives with the
  *     capability, not in the model.
  *   - **What an input means.** See [[Input]].
  *
  * Input is characters, not bytes: UTF-8 decoding belongs to the reader, so a multi-byte
  * character is never split across a [[feed]].
  */
final case class Decoder(pending: String) {

  /** True when no partial sequence is being held -- the stream is at a clean boundary. */
  def pendingIsEmpty: Boolean = pending.isEmpty

  /** The events completed by `chars`, and the decoder carrying whatever prefix of a
    * sequence was left over.
    */
  def feed(chars: String): (Vector[Input], Decoder) = Decoder.run(pending + chars)

  /** The events implied by giving up on the held prefix: a bare ESC is the Escape key,
    * anything else is reported raw. The returned decoder holds nothing.
    */
  def flush: (Vector[Input], Decoder) =
    if (pending.isEmpty) { (Vector.empty, this) }
    else if (pending == Decoder.Esc) { (Vector(Input.Keyboard(Key.Escape)), Decoder.empty) }
    else { (Vector(Input.Keyboard(Key.Unknown(pending))), Decoder.empty) }
}

object Decoder {

  /** A decoder holding nothing. */
  val empty: Decoder = Decoder("")

  private val Esc = "\u001b"
  private val Csi = Esc + "["
  private val PasteStart = Csi + "200~"
  private val PasteEnd = Csi + "201~"

  /** Longest parameter run accepted before a sequence is written off as junk. Without it
    * a stream that never sends a final byte grows the held prefix without bound.
    */
  private val MaxParams = 64

  private def isParam(c: Char): Boolean = c >= ' ' && c <= '?'
  private def isFinal(c: Char): Boolean = c >= '@' && c <= '~'
  private def isPrintable(c: Char): Boolean = c >= ' ' && c != '\u007f'

  /** What one sequence at an index turned out to be: an event and where to resume, or
    * a prefix that needs more characters before it can be decided.
    */
  private enum Step {
    case Emit(input: Input, next: Int)
    case Incomplete
  }

  private def run(buf: String): (Vector[Input], Decoder) = {
    val out = Vector.newBuilder[Input]
    var i = 0
    var held = -1 // where an incomplete sequence starts, once one is found
    while (i < buf.length && held < 0) {
      if (buf.charAt(i) == '\u001b') {
        escape(buf, i) match {
          case Step.Emit(input, next) => { out += input; i = next }
          case Step.Incomplete => held = i
        }
      } else {
        out += Input.Keyboard(plain(buf.charAt(i)))
        i += 1
      }
    }
    (out.result(), Decoder(if (held >= 0) buf.substring(held) else ""))
  }

  /** One character that arrived without an escape prefix. */
  private def plain(c: Char): Key = c match {
    case '\r' | '\n' => Key.Enter
    case '\t' => Key.Tab
    case '\b' | '\u007f' => Key.Backspace
    case _ if c >= '\u0001' && c <= '\u001a' => Key.Ctrl(('a' + c - 1).toChar)
    case _ if isPrintable(c) => Key.Printable(c)
    case _ => Key.Unknown(c.toString)
  }

  /** The sequence starting at `i`, which is an ESC. */
  private def escape(buf: String, i: Int): Step = {
    if (i + 1 >= buf.length) { Step.Incomplete }
    else {
      val c = buf.charAt(i + 1)
      if (c == '[') { csi(buf, i) }
      else if (c == 'O') { ss3(buf, i) }
      // Two in a row: the first can only have been the key.
      else if (c == '\u001b') { Step.Emit(Input.Keyboard(Key.Escape), i + 1) }
      else if (isPrintable(c)) { Step.Emit(Input.Keyboard(Key.Alt(c)), i + 2) }
      else { Step.Emit(Input.Keyboard(Key.Unknown(buf.substring(i, i + 2))), i + 2) }
    }
  }

  private def ss3(buf: String, i: Int): Step = {
    if (i + 2 >= buf.length) { Step.Incomplete }
    else {
      val key = buf.charAt(i + 2) match {
        case 'A' => Key.Up()
        case 'B' => Key.Down()
        case 'C' => Key.Right()
        case 'D' => Key.Left()
        case 'H' => Key.Home()
        case 'F' => Key.End()
        case _ => Key.Unknown(buf.substring(i, i + 3))
      }
      Step.Emit(Input.Keyboard(key), i + 3)
    }
  }

  private def csi(buf: String, i: Int): Step = {
    // A paste is framed, not terminated by its own final byte: the body is arbitrary
    // text and must never be decoded, so it is lifted out before anything else looks at
    // it. An unterminated paste is held -- the terminal owes us the closing frame.
    if (buf.startsWith(PasteStart, i)) {
      val body = i + PasteStart.length
      val end = buf.indexOf(PasteEnd, body)
      if (end < 0) { Step.Incomplete }
      else { Step.Emit(Input.Paste(buf.substring(body, end)), end + PasteEnd.length) }
    } else {
      var j = i + 2
      while (j < buf.length && isParam(buf.charAt(j))) { j += 1 }
      if (j - (i + 2) > MaxParams) {
        Step.Emit(Input.Keyboard(Key.Unknown(buf.substring(i, j))), j)
      } else if (j >= buf.length || !isFinal(buf.charAt(j))) { Step.Incomplete }
      else {
        Step.Emit(dispatch(buf.substring(i, j + 1), buf.substring(i + 2, j), buf.charAt(j)), j + 1)
      }
    }
  }

  /** A complete CSI sequence, as an event. `raw` is the whole thing including ESC, so an
    * unrecognised sequence reports what actually arrived.
    */
  private def dispatch(raw: String, params: String, fin: Char): Input =
    if (params.startsWith("<") && (fin == 'M' || fin == 'm')) {
      MouseParse.sgr(params.substring(1), fin) match {
        case Some(e) => Input.Mouse(e)
        case None => Input.Keyboard(Key.Unknown(raw))
      }
    } else if (params.isEmpty && fin == 'I') { Input.Focus(true) }
    else if (params.isEmpty && fin == 'O') { Input.Focus(false) }
    else {
      // xterm modifier encoding: the second parameter `m` carries shift/alt/ctrl in
      // `m - 1` as bits 1/2/4, with `m` absent or 1 meaning none.
      val parts = params.split(';')
      val head = parts(0)
      val mods =
        if (parts.length < 2) Mods.none
        else {
          val m = parts(1)
            .takeWhile(_.isDigit)
            .foldLeft(0)((acc, c) => math.min(acc * 10 + (c - '0'), 99))
          val bits = math.max(0, m - 1)
          Mods(shift = (bits & 1) != 0, alt = (bits & 2) != 0, ctrl = (bits & 4) != 0)
        }
      val key = fin match {
        case 'A' => Key.Up(mods)
        case 'B' => Key.Down(mods)
        case 'C' => Key.Right(mods)
        case 'D' => Key.Left(mods)
        case 'H' => Key.Home(mods)
        case 'F' => Key.End(mods)
        case 'Z' => Key.BackTab
        case '~' =>
          head match {
            case "1" | "7" => Key.Home(mods)
            case "3" => Key.Delete(mods)
            case "4" | "8" => Key.End(mods)
            case "5" => Key.PageUp(mods)
            case "6" => Key.PageDown(mods)
            case _ => Key.Unknown(raw)
          }
        case _ => Key.Unknown(raw)
      }
      Input.Keyboard(key)
    }
}
