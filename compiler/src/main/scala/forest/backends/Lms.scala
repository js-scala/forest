package forest.backends

import forest.ast._
import scalax.file.Path

class Lms extends Backend {
  override def generate(document: Document, namespace: List[String], targetDirectory: Path) {
    val pkgName = if (namespace.size > 1) namespace.take(namespace.size - 1).mkString(".") else "__root__"
    (targetDirectory / (namespace.mkString(".") + ".scala")).write(
        """|package %s
           |
           |import forest.ast._
           |import virtualization.lms.common.Base
           |
           |trait %s extends Base {
           |  def apply(%s): Rep[Node] = {
           |    %s
           |  }
           |}""".stripMargin.format(
                 pkgName,
                 namespace.last,
                 (for ((name, kind) <- document.parameters) yield "%s: Rep[%s]".format(name, kind)).mkString(", "),
                 q(document.tree)
               )
    )
  }

  implicit val quoteNode: Quote[Node] = new Quote[Node] {
    override def quote(node: Node) = node match {
      case Tag(name, children, attrs, ref) => {
        "Tag(%s, %s, %s, %s)".format(q(name), q(children), q(attrs), q(ref))
      }
      case Text(content) => "Text(%s)".format(q(content))
      case If(cond, thenPart, elsePart) => {
        "If(%s, %s, %s)".format(q(cond), q(thenPart), q(elsePart))
      }
    }
  }

  implicit val quoteTextContent = new Quote[TextContent] {
    override def quote(txt: TextContent) = txt match {
      case RawText(txt) => "RawText(%s)".format(q(txt))
      case e: Expr => quoteExpr.quote(e)
    }
  }
  
  implicit val quoteExpr: Quote[Expr] = new Quote[Expr] {
    override def quote(expr: Expr) = expr match {
      case Data(path) => "Data(%s)".format(path)
    }
  }

  def q[A : Quote](a: A): String = implicitly[Quote[A]].quote(a)
}