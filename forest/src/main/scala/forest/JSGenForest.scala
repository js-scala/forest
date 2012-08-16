package forest

import scala.virtualization.lms.common._
import scala.js._

/**
 * JavaScript code generator for `ForestExp` expressions
 */
trait JSGenForest extends JSGenEffect with QuoteGen {
  val IR: ForestExp
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit = node match {

    case Tag(name, xmlns, children, attrs) => {
      // Create the element
      emitValDef(sym, "document.createElementNS('" + xmlns + "', '" + name + "')")
      // Add its attributes
      for ((name, value) <- attrs) {
        stream.println(s"%s.setAttribute('$name', %s);".format(quote(sym), quote(value)))
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
      emitValDef(sym, q"document.createTextNode($content)")

    case _ => super.emitNode(sym, node)
  }

}

trait JSGenForestPkg extends JSGenForest with JSGenListOps with JSGenObjectOps {
  val IR: ForestPkgExp
}
