package grit.core.classify

/** How a state of type `S` is sent to a classifier. Questions about an `S` name the fields of
  * this JSON in their instructions, so its field names are part of `S`'s contract and are
  * documented where `S` is.
  */
trait StateJson[S] {
  def json(state: S): ujson.Value
}

object StateJson {
  def instance[S](f: S -> ujson.Value): StateJson[S] = new StateJson[S] {
    def json(state: S): ujson.Value = f(state)
  }

  def apply[S](using s: StateJson[S]): StateJson[S] = s
}
