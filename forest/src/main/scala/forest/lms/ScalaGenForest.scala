package forest.lms

import virtualization.lms.common._
import java.io.PrintWriter


/**
 * Scala code generator for `ForestExp` expressions
 */
// TODO I should extend a more general trait than ScalaGenEffect
trait ScalaGenForest extends ScalaGenEffect with ScalaGenListOps2 { // this: ScalaGenListOps2 =>
  val IR: ForestExp with ListOps2Exp
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any])(implicit stream: PrintWriter): Unit = node match {

    case Tag(name, children, attrs, _) => {
      val attrsFormatted = (for ((name, value) <- attrs) yield {
        // value is a list of string literals or symbols
        // TODO dramatically reduce the number of concatenations
        if (value.isEmpty) {
          "\" " + name + "\""
        } else {
          val v = value.map {
            case Const(lit) => "\"" + lit.toString.replace("\"", "\\\"") + "\""
            case s: Sym[_] => quote(s)
          }.mkString(" + ")
          "\" " + name + "=\\\"\" + " + v + " + \"\\\"\""
        }
      }).mkString(" + ")
      children match {
        case Def(ConstList(xs)) if xs.isEmpty =>
          emitValDef(sym, "\"<%s\" + %s + \" />\"".format(name, if (attrsFormatted == "") "\"\"" else attrsFormatted))
        case _ =>
          emitValDef(sym, "\"<%s\" + %s + \">\" + %s.mkString + \"</%s>\"".format(name, if (attrsFormatted == "") "\"\"" else attrsFormatted, quote(children), name))
      }
    }

    case Text(content) => emitValDef(sym, content.map(quote).mkString(" + "))

    case Tree(root) => emitValDef(sym, quote(root))

    case TreeRoot(tree) => emitValDef(sym, quote(tree))

    case _ => super.emitNode(sym, node)
  }

  // On Scala backend, trees are just strings
  override def remap[A](m: Manifest[A]) = {
    if (m == manifest[Tree]) {
      "String"
    } else {
      super.remap(m)
    }
  }
}

trait ScalaGenForestPkg extends ScalaGenForest with ScalaGenProxy with ScalaGenModules {
  val IR: ForestPkgExp
}