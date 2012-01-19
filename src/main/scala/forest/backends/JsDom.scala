package forest.backends

import scalax.io.Output
import forest.ast.Document
import scalax.file.Path
import forest.ast._
import forest.SymbolGenerator

class JsDom extends Backend {
  
  private val symbols = new SymbolGenerator
  
  override def generate(source: Path, document: Document, targetDir: Path) {
    (targetDir / (source.simpleName + ".js")).write(
        """|var %s = function (%s) {
           |  %s
           |}""".stripMargin.format(
               source.simpleName,
               (for ((name, _) <- document.parameters) yield name).mkString(", "),
               node(document.tree, None)))
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
      )) getOrElse (
        "}\n"
      ))
      out.toString
    }
    case Call(c) => appendOrReturn(parentName, symbols.fresh()) { varName =>
      "var %s = %s;\n".format(varName, c)
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