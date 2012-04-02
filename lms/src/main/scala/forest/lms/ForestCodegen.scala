package forest.lms

import virtualization.lms.common._
import js._
import java.io.PrintWriter

trait ForestJSCodegen extends JSCodegen {
  val IR: ForestExp
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any])(implicit stream: PrintWriter): Unit = node match {
    case Tag(name, attrs) => {
      emitValDef(sym, "document.createElement('%s')".format(name))
    }
  }
}

trait ForestScalaCodegen extends ScalaGenBase {
  val IR: ForestExp
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any])(implicit stream: PrintWriter): Unit = node match {
    case Tag(name, attrs) => {
      emitValDef(sym, "\"<%s></%s>\"".format(name, name))
    }
    case _ => super.emitNode(sym, node)
  }
}