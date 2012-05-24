package forest.lms

import virtualization.lms.common._
import js._
import java.io.PrintWriter

/**
 * JavaScript code generator for `ForestExp` expressions
 */
// TODO I should not extend JSGen but a more general trait
trait JSGenForest extends JSGen with JSGenListOps2 { //this: JSGenListOps2 => // FIXME Why can’t I depend on JSGenListOps2 instead of mixing it?
  val IR: ForestExp with JSExp with ListOps2Exp
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any])(implicit stream: PrintWriter): Unit = node match {

    case Tag(name, children, attrs, ref) => {
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
        case Def(ConstList(children)) => {
          for (child <- children) {
            stream.println("%s.appendChild(%s);".format(quote(sym), quote(child)))
          }
        }
        // Otherwise loop on children and append them one by one
        case children => {
          // TODO I’d like to reuse code from JSArrays
          val x = fresh[Int]
          stream.println("for (var %s = 0 ; %s < %s.length ; %s++) {".format(quote(x), quote(x), quote(children), quote(x)))
          stream.println("%s.appendChild(%s[%s]);".format(quote(sym), quote(children), quote(x)))
          stream.println("}")
        }
      }
    }

    case Text(content) =>
      emitValDef(sym, "document.createTextNode(%s);".format(content.map(quote).mkString("+")))

    case Tree(root) => {
      def collectRefs(rootNode: Exp[Node]): Map[String, Exp[Node]] = rootNode match {
        case Def(Tag(_, children, _, ref)) => {
          ref.map(_ -> rootNode).toMap// TODO ++ children.flatMap(collectRefs)
        }
        // FIXME What’s this case?
        case Def(Reflect(Tag(_, children, _, ref), _, _)) => {
          ref.map(_ -> rootNode).toMap
        }
        case _ => sys.error("Really?")
      }
      val refs = collectRefs(root)
      if (refs.isEmpty) {
        // No reference found: just return the root node itself. FIXME Return an object `{ root: root }`?
        emitValDef(sym, quote(root))
      } else {
        // Otherwise return a literal object containing all references. TODO Reuse JSLiteral
        emitNode(sym, JSLiteralDef(refs.toList)) // FIXME Is it the right way to reuse JSLiteral code generator?
      }
    }

    case TreeRoot(tree) => emitValDef(sym, quote(tree) + ".root") // TODO, really read the tree definition

    case _ => super.emitNode(sym, node)
  }
}

trait JSGenForestPkg extends JSGenForest with JSGenFields with JSGenProxy with JSGenModules {
  val IR: ForestPkgExp
}