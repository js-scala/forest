package app

import forest.lms._
import views.Form

case class Person(children: List[String])

object Person extends Form with ForestInScalaPkg with PersonInScala with FormInScala {

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
       |</html>""".stripMargin.format(Form.person(p))

}
