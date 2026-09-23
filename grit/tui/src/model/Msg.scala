package grit.tui.model

sealed trait Msg derives CanEqual

object Msg {
  case object Tick extends Msg
  case object Quit extends Msg
}
