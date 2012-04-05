package forest.lms

import virtualization.lms.common._

/**
 * Virtualizable AST of a forest document.
 */
trait Forest extends Base {

  type Node

  def tag(name: String, children: List[Rep[Node]], attrs: Map[String, List[Rep[Any]]], ref: Option[String]): Rep[Node]

}

trait ForestExp extends Forest with BaseExp {

  sealed trait Node
  case class Tag(name: String, children: List[Rep[Node]], attrs: Map[String, List[Rep[Any]]], ref: Option[String]) extends Def[Node]

  override def tag(name: String, children: List[Rep[Node]], attrs: Map[String, List[Rep[Any]]], ref: Option[String]) = Tag(name, children, attrs, ref)

}