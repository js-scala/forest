package forest.backends

import forest.ast._
import scalax.file.Path

class Lms extends Backend {
  override def generate(document: Document, namespace: List[String], targetDirectory: Path) {
    val pkgName = if (namespace.size > 1) namespace.take(namespace.size - 1).mkString(".") else "__root__"
    (targetDirectory / (namespace.mkString(".") + ".scala")).write(
        """|package %s
           |
           |object %s {
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

  implicit val quoteNode = new Quotable[Node] {
    override def quote(node: Node) = node match {
      case Tag(name, children, attrs, ref) => {
        "Tag(%s, %s, %s, %s)".format(q(name), q(children), q(attrs), q(ref))
      }
    }
  }

  def q[A : Quotable](a: A): String = implicitly[Quotable[A]].quote(a)
}