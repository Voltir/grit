package grit.prose.form

/** An output mode: how one edge shows prose. The terminal's renders to its transcript's
  * blocks; a Slack edge's would render to a message. A renderer is a function of the doc
  * and of what it was built with -- a theme, say -- and nothing else.
  */
trait Renderer[+Out] {
  def render(doc: Doc): Out
}
