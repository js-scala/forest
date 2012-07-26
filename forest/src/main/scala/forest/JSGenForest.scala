package forest

import scala.js.JSExp
import scala.js.JSGen
import scala.js.JSGenProxy

/**
 * JavaScript code generator for `ForestExp` expressions
 */
trait JSGenForest extends JSGen with JSGenListOps2 {
  val IR: ForestExp with JSExp with ListOps2Exp
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit = node match {

    case Tag(name, children, attrs) => {
      // Create the element
      emitValDef(sym, s"document.createElement('$name');")
      // Add its attributes
      for ((name, value) <- attrs) {
        val v = if (value.isEmpty) {
          "'$name'" // Attribute with no value: give it an arbitrary value (because Element#setAttribute does not support attributes with no value)
        } else {
          value.map(quote).mkString("+")
        }
        stream.println(s"${quote(sym)}.setAttribute('$name', $v);")
      }
      // Append its children nodes
      children match {
        // Fold constant lists during the staging phase
        case Left(children) => {
          for (child <- children) {
            stream.println("%s.appendChild(%s);".format(quote(sym), quote(child)))
          }
        }
        // Otherwise loop on children and append them one by one
        case Right(children) => {
          val x = fresh[Int]
          stream.println("for (var %s = 0 ; %s < %s.length ; %s++) {".format(quote(x), quote(x), quote(children), quote(x)))
          stream.println("%s.appendChild(%s[%s]);".format(quote(sym), quote(children), quote(x)))
          stream.println("}")
        }
      }
    }

    case Text(content) =>
      emitValDef(sym, "document.createTextNode(%s);".format(content.map(quote).mkString("+")))

    case _ => super.emitNode(sym, node)
  }
}

trait JSGenForestPkg extends JSGenForest with JSGenProxy with JSGenModules with JSGenStruct {
  val IR: ForestPkgExp
}
