package forest.backend

import forest.ast._
import scalax.file.Path

class Lms {
  def generate(document: Document, namespace: List[String], targetDirectory: Path) {
    val pkgName = if (namespace.size > 1) namespace.take(namespace.size - 1).mkString(".") else "__root__"
    val templateName = namespace.last
    (targetDirectory / (namespace.mkString(".") + ".scala")).write(
        """|package %s
           |
           |import forest.lms._
           |
           |trait %s extends ForestPkg {
           |  import collection.immutable.{List => SList}
           |  trait %s {
           |    def apply(%s): Rep[Tree] = {
           |      tree(%s)
           |    }
           |  }
           |  val %s = module[%s]
           |}""".stripMargin.format(
                 pkgName,
                 templateName,
                 templateName,
                 (for ((name, kind) <- document.parameters) yield "%s: Rep[%s]".format(name, kind)).mkString(", "),
                 q(document.tree),
                 templateName,
                 templateName
               )
    )
  }

  implicit val quoteNode: Quote[Node] = new Quote[Node] {
    override def quote(node: Node) = node match {
      case Tag(name, children, attrs, ref) => {
        "tag(%s, %s, %s, %s)".format(q(name), q(children), q(attrs), q(ref))
      }
      case Text(content) => "text(%s)".format(content.map(c => q(c)).mkString(", "))
      case If(cond, thenPart, elsePart) => {
        "if (%s) %s else %s".format(q(cond), q(thenPart), q(elsePart))
      }
      case For(it, seq, body) => "%s.flatMap{ %s => %s }".format(q(seq), it, q(body))
      case Call(callee, args) => "%s(%s)".format(callee, args.map(arg => q(arg)).mkString(", "))
    }
  }

  implicit val quoteTextContent = new Quote[TextContent] {
    override def quote(txt: TextContent) = txt match {
      case RawText(txt) => q(txt)
      case e: Expr => quoteExpr.quote(e)
    }
  }
  
  implicit val quoteExpr: Quote[Expr] = new Quote[Expr] {
    override def quote(expr: Expr) = expr match {
      case Data(path) => path
      case InlineIf(cond, thenPart, elsePart) => "if (%s) %s else %s".format(q(cond), q(thenPart), q(elsePart))
      case Literal(value) => q(value)
    }
  }

  def q[A : Quote](a: A): String = implicitly[Quote[A]].quote(a)
}

object Lms extends Lms