package grit.tui.key

enum Key derives CanEqual {
  case Printable(c: Char)
  case Enter, Backspace, Delete, Home, End
  case Up, Down, Left, Right
  case WordLeft, WordRight
  case Tab, BackTab, Escape
  case Ctrl(c: Char)
  case Paste(text: String)
  case Unknown
}

object Key {
  def ctrl(c: Char): Key = Ctrl(c.toLower)
}
