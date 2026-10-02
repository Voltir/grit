package grit.eval.harness.main

/** The eval harness's command line, run through `scripts/eval`. */
object Main {

  def main(args: Array[String]): Unit = {
    System.err.println("usage: scripts/eval <command> ...  (no command yet)")
    sys.exit(2)
  }
}
