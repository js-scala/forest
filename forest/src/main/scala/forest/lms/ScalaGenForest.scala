package forest.lms

import virtualization.lms.common._
import java.io.PrintWriter


/**
 * Scala code generator for `ForestExp` expressions
 */
trait ScalaGenForest extends ScalaGenEffect {
  val IR: ForestStringExp with ListOps2Exp
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit = node match {

    case Tag(name, children, attrs, _) => {
      val attrsFormatted = (for ((name, value) <- attrs) yield {
        // value is a list of string literals or symbols
        // TODO dramatically reduce the number of concatenations. Use lower transformations?
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
      val attrsQuoted = if (attrsFormatted == "") "\"\"" else attrsFormatted
      children match {
        case Left(children) => {
          if (children.isEmpty) {
            emitValDef(sym, "\"<%s\" + %s + \" />\"".format(name, attrsQuoted))
          } else {
            emitValDef(sym, "\"<%s\" + %s + \">\" + %s + \"</%s>\"".format(name, attrsQuoted, children.map(quote).mkString(" + "), name))
          }
        }
        case Right(children) =>
          emitValDef(sym, "\"<%s\" + %s + \">\" + %s.mkString + \"</%s>\"".format(name, attrsQuoted, quote(children), name))
      }
    }

    case Text(content) => emitValDef(sym, "(" + content.map(quote).mkString(" + ") + ").replace(\"<\", \"&lt;\")")

    case ForestTree(root) => emitValDef(sym, quote(root))

    case TreeRoot(tree) => emitValDef(sym, quote(tree))

    case _ => super.emitNode(sym, node)
  }

}

trait ScalaGenForestXml extends ScalaGenEffect {
  val IR: ForestXmlExp with ListOps2Exp
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit = node match {
    case Tag(name, children, attrs, _) => {
      val attrsFormatted = (for ((name, value) <- attrs) yield {
        // value is a list of string literals or symbols
        if (value.isEmpty) {
          " " + name + "='" + name + "'"
        } else {
          val v = value.map {
            case Const(lit) => "\"" + lit.toString.replace("\"", "\\\"") + "\""
            case s: Sym[_] => quote(s)
          }.mkString(" + ")
          " " + name + "={" + v + "}"
        }
      }).mkString(" ")
      children match {
        case Left(children) => {
          if (children.isEmpty) {
            emitValDef(sym, "<%s%s />".format(name, attrsFormatted))
          } else {
            children match {
              case child :: Nil =>
                emitValDef(sym, "<%s%s>{%s}</%s>".format(name, attrsFormatted, quote(child), name))
              case _ =>
                emitValDef(sym, "<%s%s>{%s}</%s>".format(name, attrsFormatted, children.map(quote), name))
            }
          }
        }
        case Right(children) =>
          emitValDef(sym, "<%s%s>{%s}</%s>".format(name, attrsFormatted, quote(children), name))
      }
    }

    case Text(content) => {
      emitValDef(sym, "{xml.Text(%s)}".format(content.map(quote).mkString(" + ")))
    }

    case ForestTree(root) => {
      emitValDef(sym, quote(root))
    }

    case TreeRoot(tree) => {
      emitValDef(sym, quote(tree))
    }

    case _ => super.emitNode(sym, node)

  }

}

trait ScalaGenForestPkg extends ScalaGenForest with ScalaGenFunctions with ScalaGenProxy with ScalaGenModules with ScalaGenListOps2 {
  val IR: ForestStringPkgExp
}

trait ScalaGenForestXmlPkg extends ScalaGenForestXml with ScalaGenFunctions with ScalaGenProxy with ScalaGenModules with ScalaGenListOps2 {
  val IR: ForestXmlPkgExp
}
