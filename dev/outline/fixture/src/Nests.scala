package grit.outline.fixture

/** Two nested owners defining one name. */
object Nests {
  object Left {
    def same: Int = 1
  }
  object Right {
    def same: Int = 2
  }
}

/** A trait an anonymous class implements. */
trait Greeter {
  def greet: String
}

/** A greeter as an anonymous class. */
object Greeters {
  val polite: Greeter = new Greeter {
    def greet: String = "hello"
  }
}
