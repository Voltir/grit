package grit.prose.form

/** An output mode: how one consumer shows prose -- the model's markdown, a terminal's blocks,
  * a Slack message. A function of the doc and of what it was built with, and nothing else.
  */
trait Renderer[+Out] {
  def render(doc: Doc[Nothing]): Out
}
