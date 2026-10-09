package grit.outline.read

import scala.annotation.experimental
import scala.quoted.Quotes
import scala.tasty.inspector.Inspector
import scala.tasty.inspector.Tasty
import scala.tasty.inspector.TastyInspector
import scala.util.control.NonFatal

object Probe {
  def facts(tasty: Vector[os.Path], deps: Vector[os.Path]): Vector[String] = {
    val buffer = scala.collection.mutable.ListBuffer[String]()
    val inspector = new Inspector {
      def inspect(using q: Quotes)(tastys: List[Tasty[q.type]]): Unit = {
        import q.reflect.*
        for (tastyFile <- tastys) {
          val tree = tastyFile.ast
          val rootSym = tree.symbol
          val traverser = new TreeTraverser {
            override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
              tree match {
                case t: ClassDef =>
                  emitFact(t, t.symbol, t.pos)
                case t: DefDef =>
                  emitFact(t, t.symbol, t.pos)
                case t: ValDef =>
                  emitFact(t, t.symbol, t.pos)
                case t: TypeDef =>
                  emitFact(t, t.symbol, t.pos)
                case _ => ()
              }
              super.traverseTree(tree)(owner)
            }

            private def emitFact(defTree: Tree, sym: Symbol, pos: Position): Unit = {
              val fullName = sym.fullName
              val flags = sym.flags.show
              val path = pos.sourceFile.path
              val startLine = pos.startLine + 1
              val endLine = pos.endLine + 1
              val start = pos.start
              val src = pos.sourceCode.map(_.take(40).replace('\n', ' ')) match {
                case Some(s) => s
                case None => ""
              }
              val doc = try {
                if (sym.docstring.isDefined) { "true" }
                else { "false" }
              } catch {
                case e: Exception => s"error:${e.getMessage.take(20)}"
              }
              val info = try {
                sym.info.show
              } catch {
                case e: Exception => s"error:${e.getMessage.take(20)}"
              }
              val shown = try {
                defTree.show.take(120).replace('\n', ' ').replace('\r', ' ')
              } catch {
                case NonFatal(e) => s"error:${e.toString.take(60)}"
              }
              val line =
                s"$fullName | $flags | $path | $startLine-$endLine | start=$start | src=$src | doc=$doc | info=$info | tree=$shown"
              buffer += line
            }
          }
          traverser.traverseTree(tree)(rootSym)
        }
      }
    }
    val depsStr = deps.map(_.toString).toList
    val tastyStr = tasty.map(_.toString).toList
    val ok = TastyInspector.inspectAllTastyFiles(tastyStr, Nil, depsStr)(inspector)
    buffer.toVector :+ s"ok=$ok"
  }

  def main(args: Array[String]): Unit = {
    val (depsStr, tastyPaths) = args.toList match {
      case depsStr :: rest => (depsStr, rest)
      case _ => sys.error("usage: Probe <deps-classpath> <tasty-files...>")
    }
    val deps =
      if (depsStr.isEmpty) { Vector.empty }
      else {
        depsStr.split(":").map(p => os.Path(java.nio.file.Paths.get(p).toAbsolutePath)).toVector
      }
    val tasty =
      tastyPaths.map(p => os.Path(java.nio.file.Paths.get(p).toAbsolutePath)).toVector
    val result = facts(tasty, deps)
    result.foreach(println)
  }
}
