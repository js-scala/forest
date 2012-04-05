package forest.lms

import virtualization.lms.common._
import js._
import java.io.PrintWriter

trait ForestJSCodegen extends JSGenBase {
  val IR: ForestExp
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any])(implicit stream: PrintWriter): Unit = node match {
    case Tag(name, children, attrs, ref) => {
      // Create the element
      emitValDef(sym, s"document.createElement('${name}');")
      // Add its attributes
      for ((name, value) <- attrs) {
        val v = if (value.isEmpty) {
          "'${name}'"
        } else {
          value.map(quote).mkString("+")
        }
        stream.append(s"${quote(sym)}.setAttribute('${name}', ${v});"+"\n")
      }
      // Append its children nodes
      for (child <- children) {
        stream.append(s"${quote(sym)}.appendChild(${quote(child)});"+"\n")
      }
    }
    case Tree(root) => {
      def collectRefs(rootNode: Exp[Node]): List[(String, Exp[Node])] = rootNode match {
        case Def(Tag(_, children, _, ref)) => {
          ref.map((_,rootNode)).toList ++ children.flatMap(collectRefs)
        }
        case _ => sys.error("You loose.")
      }
      val refs = collectRefs(root)
      if (refs.isEmpty) {
        emitValDef(sym, quote(root))
      } else {
        val jsObject = refs.map { case (n, s) => s"'${n}':${quote(s)}" }.mkString(",")
        emitValDef(sym, s"{${jsObject}};")
      }
    }
    case _ => super.emitNode(sym, node)
  }
}

trait ForestScalaCodegen extends ScalaGenBase {
  val IR: ForestExp
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any])(implicit stream: PrintWriter): Unit = node match {
    case Tag(name, children, attrs, _) => {
      val attrsFormatted = (for ((name, value) <- attrs) yield {
          // value is a list of string literals or symbols
          val v = if (value.isEmpty) {
            name
          } else {
            value.map { _ match {
                case Const(lit) => lit.toString
                case s: Sym[_] => s"$${${quote(s)}}"
              }
            }.mkString
          }
          s""" ${name}="${v}"""".format(name, v)
        }).mkString
      if (children.isEmpty) {
        emitValDef(sym, "s\"\"\"<%s%s />\"\"\"".format(name, attrsFormatted))
      } else {
        val childrenFormatted = children.map(child => s"$${${quote(child)}}").mkString
        emitValDef(sym, "s\"\"\"<%s%s>%s</%s>\"\"\"".format(name, attrsFormatted, childrenFormatted, name))
      }
    }
    case Tree(root) => {
      emitValDef(sym, quote(root))
    }
    case _ => super.emitNode(sym, node)
  }
}