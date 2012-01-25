package forest.backends

import scalax.io.Output
import forest.ast.Document
import scalax.file.Path
import forest.ast._
import forest.SymbolGenerator

class JsDom extends Backend {
  
  private val symbols = new SymbolGenerator
  
  override def generate(document: Document, names: List[String], targetDir: Path) {
    (targetDir / (names.mkString(".") + ".js")).write(
        """|%s
           |%s = function (%s) {
           |  %s
           |}""".stripMargin.format(
               namespace(names),
               names.mkString("."),
               (for ((name, _) <- document.parameters) yield name).mkString(", "),
               node(document.tree, None)))
  }
  
  /**
   * Generate the namespace of the target
   * @param names Parts of the namespace. *At least one element*.
   */
  def namespace(names: List[String]): String = {
    val root :: ns = names
    def sub(names: List[String], processed: List[String]): String = names match {
      case n :: ns => {
        val p = processed :+ n
        val current = p.mkString(".")
        "if (typeof %s == 'undefined') { %s = {}; }\n".format(current, current) + sub(ns, p)
      }
      case Nil => ""
    }
    "if (typeof %s == 'undefined') { var %s = {}; }\n".format(names.head, names.head) + sub(names.tail, List(root))
  }
  
  def node(n: Node, parentName: Option[String]): String = n match {
    case Tag(name, children, attrs, ref) => appendOrReturn(parentName, symbols.fresh()) { varName =>
      val out = new collection.mutable.StringBuilder
      out.append("var %s = document.createElement('%s');\n".format(varName, name))
      for ((n, v) <- attrs) {
        val value = if (v.isEmpty) {
          "'%s'".format(n)
        } else {
          textContent(v)
        }
        out.append("%s.setAttribute('%s', %s);\n".format(varName, n, value))
      }
      for (child <- children) {
        out.append(node(child, Some(varName)))
      }
      out.toString
    }
    case Text(t) => appendOrReturn(parentName, symbols.fresh()) { varName =>
      "var %s = document.createTextNode(%s)\n".format(varName, textContent(t))
    }
    case For(it, seq, body) => {
      val out = new collection.mutable.StringBuilder
      val i, length = symbols.fresh()
      out.append("for (var %s = 0, %s = %s.length ; %s < %s ; %s++) {\n".format(i, length, seq.path, i, length, i))
      out.append("%s = %s[%s];\n".format(it, seq.path, i))
      for (n <- body) {
        out.append(node(n, parentName))
      }
      out.append("}\n")
      out.toString
    }
    case If(cond, then, maybeElse) => {
      val out = new collection.mutable.StringBuilder
      out.append("if (%s) {\n".format(expr(cond)))
      for (n <- then) {
        out.append(node(n, parentName))
      }
      out.append((for (`else` <- maybeElse) yield (
          "} else {\n"
        + (`else`.map { node(_, parentName) }).mkString
        + "}\n"
      )) getOrElse (
        "}\n"
      ))
      out.toString
    }
    case Call(tmpl, args) => appendOrReturn(parentName, symbols.fresh()) { varName =>
      "var %s = %s(%s);\n".format(varName, tmpl, args.map(expr).mkString(", "))
    }
  }
  
  def textContent(ts: List[TextContent]): String = ts.map {
      case RawText(t) => "'%s'".format(escapeQuotes(t))
      case e: Expr => expr(e)
    }.mkString("+")
  
  def expr(e: Expr): String = e match {
    case Data(p) => p
    case InlineIf(c, t, e) => "((%s) ? (%s) : (%s))".format(c.path, expr(t), e.map(expr).getOrElse("''"))
    case Literal(l) => "'%s'".format(escapeQuotes(l))
  }
  
  def escapeQuotes(s: String) = s.replace("'", "\\'")
  
  def appendOrReturn(parentName: Option[String], name: String)(content: String => String) = (
      content(name)
    + ((for (pName <- parentName) yield {
        "%s.appendChild(%s);\n".format(pName, name)
      }) getOrElse {
        "return %s;\n".format(name)
      })
  )
}