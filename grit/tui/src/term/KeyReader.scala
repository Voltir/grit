package grit.tui.term

import grit.tui.key.Key

import org.jline.keymap.{BindingReader, KeyMap}
import org.jline.terminal.Terminal
import org.jline.utils.InfoCmp.Capability

/** Decodes terminal input into `Key` values.
  */
final class KeyReader(terminal: Terminal) {
  import KeyReader.Token

  private val bindingReader = new BindingReader(terminal.reader())
  private val keyMap: KeyMap[Token] = KeyReader.keyMap(terminal)

  /** Read one keystroke, blocking until a complete sequence arrives. */
  def read(): Key = bindingReader.readBinding(keyMap) match {
    case Token.SelfInsert =>
      val s = bindingReader.getLastBinding
      if (s == null || s.isEmpty) Key.Unknown else Key.Printable(s.charAt(0))

    case Token.PasteStart =>
      // The library reads through to the closing sequence, so a pasted newline never
      // reaches the binding table and cannot be mistaken for a submission.
      val text = bindingReader.readStringUntil(KeyReader.PasteEnd)
      Key.Paste(if (text == null) "" else text)

    case Token.CtrlChar(c) => Key.ctrl(c)
    case Token.Simple(k) => k
    case null => Key.Unknown
  }
}

object KeyReader {

  /** The escape byte, spelled as a code point so no literal control character
    * ends up in the source.
    */
  private val Esc: String = 27.toChar.toString

  /** Enable and disable the terminal's bracketed-paste mode.
    *
    * Without this the terminal never emits the paste-start sequence, and every paste
    * arrives as bare keystrokes — a pasted newline then reads as a submission.
    */
  val EnablePaste: String = Esc + "[?2004h"
  val DisablePaste: String = Esc + "[?2004l"

  private val PasteStart: String = Esc + "[200~"
  private val PasteEnd: String = Esc + "[201~"

  private enum Token derives CanEqual {
    case SelfInsert
    case PasteStart
    case CtrlChar(c: Char)
    case Simple(key: Key)
  }

  private def keyMap(terminal: Terminal): KeyMap[Token] = {
    val result = new KeyMap[KeyReader.Token]

    def bind(t: Token, seq: String): Unit =
      if (seq != null && !seq.isEmpty) { result.bind(t, seq: java.lang.CharSequence) }

    def bindKey(key: Key, seqs: String*): Unit =
      seqs.foreach(s => bind(Token.Simple(key), s))

    // Control characters. Enter arrives as CR in raw mode; accept LF too.
    bindKey(Key.Enter, "\r", "\n")
    bindKey(Key.Tab, "\t")
    bindKey(Key.Backspace, KeyMap.del(), "\b")
    bindKey(Key.Escape, KeyMap.esc())

    // A lone ESC is a prefix of every sequence above. Wait this long before deciding
    // the user really pressed Escape.
    result.setAmbiguousTimeout(80L)
    return result
  }
}
