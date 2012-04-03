package forest.lms

import virtualization.lms.common._

/**
 * Virtualizable AST of a forest document.
 */
trait Forest extends Base {

  type Node

  def tag(name: String, children: List[Rep[Node]], attrs: Map[String, List[Rep[Any]]]): Rep[Node]

}

trait ForestExp extends Forest with BaseExp {

  sealed trait Node
  case class Tag(name: String, children: List[Rep[Node]], attrs: Map[String, List[Rep[Any]]]) extends Def[Node]

  override def tag(name: String, children: List[Rep[Node]], attrs: Map[String, List[Rep[Any]]]) = Tag(name, children, attrs)

}