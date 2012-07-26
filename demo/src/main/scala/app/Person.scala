package app

case class Person(children: List[String])

object Person {

  def edit(p: Person): String =
    """|<!DOCTYPE html>
       |<html>
       |  <head></head>
       |  <body>
       |    %s
       |    <script src="views.js"></script>
       |    <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.7.2/jquery.min.js"></script>
       |    <script src="person.js"></script>
       |  </body>
       |</html>""".stripMargin.format(views.Form.person(p)("root"))

}


import scala.virtualization.lms.common.Base
import forest._
import js._

trait PersonOps extends Base with Fields {
  class PersonOps(implicit person: Rep[Person]) extends Fields[Person] {
    def children = field[List[String]]("children")
  }
  implicit def repPersonToOps(p: Rep[Person]) = new PersonOps()(p)
}
