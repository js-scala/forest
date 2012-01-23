package forest.backends

import scalax.file.Path
import forest.ast._

/**
 * A compiler to Scala code, generating a function producing a String containing the DOM
 */
class ScalaText extends Backend {
  
  override def generate(document: Document, namespace: List[String], targetDir: Path) {
    (targetDir / (namespace.last + ".scala")).write(
        """|
           |object %s {
           |  def apply(%s): String = {
           |    val out  = new collection.mutable.StringBuilder
           |    %s
           |    out.toString
           |  }
           |}""".stripMargin.format(
                 namespace.last,
                 (for ((name, kind) <- document.parameters) yield name + ": " + kind.getOrElse("Any")).mkString(", "),
                 node(document.tree)
               )
    )
  }
  
  def node(n: Node): String = n match {
    case Tag(n, cs, as, r) => {
      val out = new collection.mutable.StringBuilder
      if (as.isEmpty) {
        out.append("out.append(\"<%s>\")\n".format(n))
      } else {
        out.append("out.append(\"<%s\")\n".format(n))
        for ((n, v) <- as) {
          out.append("out.append(\" %s\")\n".format(n))
          if (!v.isEmpty) {
            out.append("""|out.append("=\"" + %s + "\"")
                          |""".stripMargin.format(textContent(v)))
          }
        }
        out.append("out.append(\">\")\n")
      }
      for (c <- cs) {
        out.append(node(c))
      }
      out.append("out.append(\"</%s>\")\n".format(n))
      out.toString
    }
    case Text(content) => "out.append(%s)\n".format(textContent(content))
    case For(it, seq, body) => {
      val out = new collection.mutable.StringBuilder
      out.append("for (%s <- %s) {\n".format(it, seq.path))
      for (n <- body) {
        out.append(node(n))
      }
      out.append("}\n")
      out.toString
    }
    case If(c, t, e) => {
      val out = new collection.mutable.StringBuilder
      out.append("if (%s) {\n".format(expr(c)))
      for (n <- t) {
        out.append(node(n))
      }
      out.append((for (`else` <- e) yield (
            "} else {\n"
          + (`else`.map(node)).mkString
          + "}\n"
       )) getOrElse (
           "}\n"
       ))
       out.toString
    }
    case Call(tmpl, args) => "out.append(%s(%s))\n".format(tmpl, args.map(expr).mkString(", "))
  }
  
  def textContent(content: List[TextContent]): String = content.map {
      case RawText(t) => "\"\"\"%s\"\"\"".format(t)
      case e: Expr => expr(e)
    }.mkString(" + ")
  
  def expr(e: Expr): String = e match {
    case Data(p) => p
    case InlineIf(c, t, e) => ("if (%s) { %s }" + e.map { e => " else { %s }".format(expr(e)) }.getOrElse("")).format(c.path, expr(t))
    case Literal(l) => "\"\"\"%s\"\"\"".format(l)
  }
}