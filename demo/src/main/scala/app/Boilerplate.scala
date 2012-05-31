package app

import scala.virtualization.lms.common.Base
import forest.lms._
import js._


trait PersonOps extends Base {
  def infix_children(p: Rep[Person]): Rep[List[String]]
}

trait PersonOpsExp extends PersonOps with FieldsExp {
  override def infix_children(p: Exp[Person]): Exp[List[String]] = Field[Person, List[String]](p, "children")
}

trait PersonInScala extends PersonOps with JSInScala {
  override def infix_children(p: Person): List[String] = p.children
}