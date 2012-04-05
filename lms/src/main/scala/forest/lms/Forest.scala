package forest.lms

import virtualization.lms.common._

/**
 * Virtualizable AST of a forest document.
 */
trait Forest extends Base {

  type Node
  type Tree

  def tag(name: String, children: List[Rep[Node]], attrs: Map[String, List[Rep[Any]]], ref: Option[String]): Rep[Node]

  def tree(root: Rep[Node]): Rep[Tree]
}

trait ForestExp extends Forest with BaseExp {

  sealed trait Node
  case class Tag(name: String, children: List[Exp[Node]], attrs: Map[String, List[Exp[Any]]], ref: Option[String]) extends Def[Node]

  case class Tree(root: Exp[Node]) extends Def[Tree]

  override def tag(name: String, children: List[Exp[Node]], attrs: Map[String, List[Exp[Any]]], ref: Option[String]) = Tag(name, children, attrs, ref)
  override def tree(root: Exp[Node]) = Tree(root)
}