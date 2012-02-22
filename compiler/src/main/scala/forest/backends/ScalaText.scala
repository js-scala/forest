package forest.backends

import scalax.file.Path
import forest.ast._

/**
 * A compiler to Scala code, generating a function producing a String containing the DOM
 */
class ScalaText(runtime: String) extends Backend {
  
  override def generate(document: Document, namespace: List[String], targetDir: Path) {
    val pkgName = if (namespace.size > 1) namespace.take(namespace.size - 1).mkString(".") else "__root__"
    (targetDir / (namespace.mkString(".") + ".scala")).write(
        """|package %s
           |
           |object %s extends %s {
           |  def apply(%s): String = {
           |    val out  = new collection.mutable.StringBuilder
           |    %s
           |    out.toString
           |  }
           |}""".stripMargin.format(
                 pkgName,
                 namespace.last,
                 runtime,
                 (for ((name, kind) <- document.parameters) yield name + ": Data").mkString(", "),
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
      out.append("for (%s <- _iterate(%s)) {\n".format(it, path(seq.path)))
      for (n <- body) {
        out.append(node(n))
      }
      out.append("}\n")
      out.toString
    }
    case If(c, t, e) => {
      val out = new collection.mutable.StringBuilder
      out.append("if (_test(%s)) {\n".format(expr(c)))
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
      case e: Expr => "_show(%s)".format(expr(e))
    }.mkString(" + ")
  
  def expr(e: Expr): String = e match {
    case Data(p) => path(p)
    case InlineIf(c, t, e) => ("if (_test(%s)) { %s }" + e.map { e => " else { %s }".format(expr(e)) }.getOrElse("")).format(path(c.path), expr(t))
    case Literal(l) => "\"\"\"%s\"\"\"".format(l)
  }
  
  def path(p: String): String =
    p.split("[.]") reduce ((acc, field) => """_get(%s, "%s")""".format(acc, field))
}