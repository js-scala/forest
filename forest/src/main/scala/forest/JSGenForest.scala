package forest

import scala.virtualization.lms.common._
import scala.js._

/**
 * JavaScript code generator for `ForestExp` expressions
 */
trait JSGenForest extends JSGenBase {
  val IR: ForestExp
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit = node match {

    case Tag(name, children, attrs) => {
      // Create the element
      emitValDef(sym, s"document.createElement('$name')")
      // Add its attributes
      for ((name, value) <- attrs) {
        stream.println(s"${quote(sym)}.setAttribute('$name', ${quote(value)});")
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
      emitValDef(sym, "document.createTextNode(%s)".format(quote(content)))

    case _ => super.emitNode(sym, node)
  }

  def emitSource[A : Manifest](args: List[Sym[_]], body: Block[A], name: String, out: java.io.PrintWriter) = emitSourceAnyArity(args, body, name, out)
}

trait JSGenForestPkg extends JSGenEffect with JSGenForest with JSGenIfThenElse with JSGenListOps with JSGenStringOps with JSGenObjectOps with JSGenProxy with JSGenModules with JSGenStruct {
  val IR: ForestPkgExp
}
