package forest.lms

import virtualization.lms.common._

/**
 * Virtualizable AST of a forest document.
 */
trait Forest extends Base {

  sealed trait Node

  sealed trait TextContent
  case class Literal(value: String) extends TextContent
  case class Expr[A](value: Rep[A]) extends TextContent

  def tag(name: String, attrs: Map[String, List[TextContent]]): Rep[Node]

  implicit def toLiteral(str: String) = Literal(str)
  implicit def toExpr[A](expr: Rep[A]) = Expr(expr)
}

trait ForestExp extends Forest with BaseExp {

/*  sealed trait Node

  case class TextNode(content: List[TextContent]) extends Exp[forest.ast.Node] Node

  sealed trait TextContent

  case class Literal(str: String) extends TextContent
  case class Expr[T](expr: Exp[T]) extends TextContent

  implicit def unit[T <: Node](n: T): Exp[T]*/

  case class Tag(name: String, attrs: Map[String, List[TextContent]]) extends Def[Node]

  override def tag(name: String, attrs: Map[String, List[TextContent]]) = Tag(name, attrs)
}