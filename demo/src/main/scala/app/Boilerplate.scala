package app


import scala.virtualization.lms.common.Base
import forest.lms._
import js._
import views.Form

trait PersonOps extends Base {

  implicit def repToPersonOps(p: Rep[Person]) = new PersonOps(p)

  class PersonOps(p: Rep[Person]) {
    def children: Rep[List[String]] = person_children(p)
  }

  def person_children(p: Rep[Person]): Rep[List[String]]

}

trait PersonOpsExp extends PersonOps with FieldsExp {
  override def person_children(p: Exp[Person]): Exp[List[String]] = Field[Person, List[String]](p, "children")
}

trait PersonInScala extends PersonOps with JSInScala {
  override def person_children(p: Person): List[String] = p.children
}



trait FormInScala extends Form with ForestInScalaPkg { this: PersonOps =>

  override def create[A : Manifest]: A = {
    if (manifest[A] equals manifest[Form]) (new Form {}).asInstanceOf[A]
    else super.create[A]
  }

}


trait JSGenForestPkg extends JSGenForest with JSGenFields with JSGenProxy with JSGenModules {
  val IR: ForestPkgExp
}